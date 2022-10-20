use std::collections::HashMap;

use swc_ecma_ast::*;

use crochet_ast::types::{
    self as types, TCallable, TFnParam, TGeneric, TIndexAccess, TObjElem, TObject, TVar, Type,
    TypeKind,
};
use crochet_infer::{get_type_params, set_type_params, Context, Subst, Substitutable};

use crate::parse_dts::infer_ts_type_ann;

// TODO: rename this replace_refs
pub fn replace_aliases(
    t: &Type,
    type_param_decl: &TsTypeParamDecl,
    ctx: &Context,
) -> Result<Type, String> {
    let mut type_params: Vec<TVar> = vec![];
    let type_param_map: HashMap<String, TVar> = type_param_decl
        .params
        .iter()
        .map(|tp| {
            // NOTE: We can't use .map() here because infer_ts_type_ann
            // returns a Result.
            let constraint = match &tp.constraint {
                Some(constraint) => {
                    let t = infer_ts_type_ann(constraint, ctx)?;
                    Some(Box::from(t))
                }
                None => None,
            };
            let tv = TVar {
                id: ctx.fresh_id(),
                constraint,
            };
            type_params.push(tv.clone());
            Ok((tp.name.sym.to_string(), tv))
        })
        .collect::<Result<HashMap<String, TVar>, String>>()?;

    let t = set_type_params(t, &type_params);
    Ok(replace_aliases_rec(&t, &type_param_map))
}

// TODO: rename this replace_refs_rec
fn replace_aliases_rec(t: &Type, map: &HashMap<String, TVar>) -> Type {
    match &t.kind {
        TypeKind::Generic(TGeneric { t, type_params }) => {
            // TODO: create a new `map` that adds in `type_params`
            Type {
                kind: TypeKind::Generic(TGeneric {
                    t: Box::from(replace_aliases_rec(t, map)),
                    type_params: type_params.to_owned(),
                }),
            }
        }
        TypeKind::Var(_) => t.to_owned(),
        TypeKind::App(types::TApp { args, ret }) => Type {
            kind: TypeKind::App(types::TApp {
                args: args.iter().map(|t| replace_aliases_rec(t, map)).collect(),
                ret: Box::from(replace_aliases_rec(ret, map)),
            }),
        },
        TypeKind::Lam(types::TLam { params, ret }) => Type {
            kind: TypeKind::Lam(types::TLam {
                params: params
                    .iter()
                    .map(|param| TFnParam {
                        t: replace_aliases_rec(&param.t, map),
                        ..param.to_owned()
                    })
                    .collect(),
                ret: Box::from(replace_aliases_rec(ret, map)),
            }),
        },
        TypeKind::Lit(_) => t.to_owned(),
        TypeKind::Keyword(_) => t.to_owned(),
        TypeKind::Union(types) => Type {
            kind: TypeKind::Union(types.iter().map(|t| replace_aliases_rec(t, map)).collect()),
        },
        TypeKind::Intersection(types) => Type {
            kind: TypeKind::Intersection(
                types.iter().map(|t| replace_aliases_rec(t, map)).collect(),
            ),
        },
        TypeKind::Object(obj) => {
            let elems: Vec<TObjElem> = obj
                .elems
                .iter()
                .map(|elem| match elem {
                    TObjElem::Call(lam) => {
                        let params: Vec<TFnParam> = lam
                            .params
                            .iter()
                            .map(|t| TFnParam {
                                t: replace_aliases_rec(&t.t, map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(lam.ret.as_ref(), map);

                        TObjElem::Call(TCallable {
                            params,
                            ret: Box::from(ret),
                            type_params: lam.type_params.to_owned(),
                        })
                    }
                    TObjElem::Constructor(lam) => {
                        let params: Vec<TFnParam> = lam
                            .params
                            .iter()
                            .map(|t| TFnParam {
                                t: replace_aliases_rec(&t.t, map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(lam.ret.as_ref(), map);

                        TObjElem::Constructor(TCallable {
                            params,
                            ret: Box::from(ret),
                            type_params: lam.type_params.to_owned(),
                        })
                    }
                    TObjElem::Index(index) => {
                        let t = replace_aliases_rec(&index.t, map);
                        TObjElem::Index(types::TIndex {
                            t,
                            ..index.to_owned()
                        })
                    }
                    TObjElem::Prop(prop) => {
                        println!("replacing prop.t");
                        println!("prop.t = {:#}", prop.t);
                        let t = replace_aliases_rec(&prop.t, map);
                        TObjElem::Prop(types::TProp {
                            t,
                            ..prop.to_owned()
                        })
                    }
                })
                .collect();
            Type {
                kind: TypeKind::Object(TObject { elems }),
            }
        }
        TypeKind::Ref(alias) => match map.get(&alias.name) {
            Some(tv) => Type {
                kind: TypeKind::Var(tv.to_owned()),
            },
            None => t.to_owned(),
        },
        TypeKind::Tuple(types) => Type {
            kind: TypeKind::Tuple(types.iter().map(|t| replace_aliases_rec(t, map)).collect()),
        },
        TypeKind::Array(t) => Type {
            kind: TypeKind::Array(Box::from(replace_aliases_rec(t, map))),
        },
        TypeKind::Rest(t) => Type {
            kind: TypeKind::Rest(Box::from(replace_aliases_rec(t, map))),
        },
        TypeKind::This => Type {
            kind: TypeKind::This,
        },
        TypeKind::KeyOf(t) => Type {
            kind: TypeKind::KeyOf(Box::from(replace_aliases_rec(t, map))),
        },
        TypeKind::IndexAccess(TIndexAccess { object, index }) => Type {
            kind: TypeKind::IndexAccess(TIndexAccess {
                object: Box::from(replace_aliases_rec(object, map)),
                index: Box::from(replace_aliases_rec(index, map)),
            }),
        },
        TypeKind::Mutable(t) => Type {
            kind: TypeKind::Mutable(Box::from(replace_aliases_rec(t, map))),
        },
    }
}

pub fn merge_types(t1: &Type, t2: &Type) -> Type {
    let tp1 = get_type_params(t1);
    let tp2 = get_type_params(t2);

    if tp1.len() != tp2.len() {
        panic!("Mismatch in type param count when attempting to merge type");
    }

    // Creates a mapping from type params in t2 to those in t1
    let subs: Subst = tp2
        .into_iter()
        .map(|tv| tv.id.to_owned())
        .zip(tp1.iter().map(|tv| Type {
            kind: TypeKind::Var(tv.to_owned()),
        }))
        .collect();

    // Unwrap qualified types since we return a qualified
    // type if there are any type qualifiers.
    let t1 = if let TypeKind::Generic(TGeneric { t, .. }) = &t1.kind {
        t
    } else {
        t1
    };
    let t2 = if let TypeKind::Generic(TGeneric { t, .. }) = &t2.kind {
        t
    } else {
        t2
    };

    // Updates type variables for type params to match t1
    let t2 = t2.apply(&subs);

    let type_params = tp1;
    let t = match (&t1.kind, &t2.kind) {
        (TypeKind::Object(obj1), TypeKind::Object(obj2)) => {
            let elems: Vec<_> = obj1
                .elems
                .iter()
                .cloned()
                .chain(obj2.elems.iter().cloned())
                .collect();

            Type {
                kind: TypeKind::Object(TObject { elems }),
            }
        }
        (_, _) => todo!(),
    };

    if type_params.is_empty() {
        t
    } else {
        Type {
            kind: TypeKind::Generic(TGeneric {
                t: Box::from(t),
                type_params,
            }),
        }
    }
}
