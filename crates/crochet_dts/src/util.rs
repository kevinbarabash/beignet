use std::collections::HashMap;

use swc_ecma_ast::*;

use crochet_ast::types::{
    self as types, TCallable, TFnParam, TGeneric, TIndexAccess, TMappedType, TObjElem, TObject,
    TVar, Type, TypeKind,
};
use crochet_infer::{get_type_params, set_type_params, Context, Subst, Substitutable};

use crate::parse_dts::infer_ts_type_ann;

// TODO: rename this replace_refs
// TODO: use the same technique we use in infer_type_ann.rs, as this stands, it
// doesn't handle type param shadowing.
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

    // QUESTION: Do we need to call `set_type_params` before calling `replace_aliases_rec`
    let t = set_type_params(t, &type_params);
    Ok(replace_aliases_rec(&t, &type_param_map))
}

// TODO: update this to use Visitor from crochet_infer, which should probably
// be moved into the crochet_ast crate.
// TODO: rename this replace_refs_rec
// NOTE: This is only used externall by replace_aliases_rec.
pub fn replace_aliases_rec(t: &Type, type_param_map: &HashMap<String, TVar>) -> Type {
    let kind = match &t.kind {
        TypeKind::Generic(TGeneric { t, type_params }) => {
            // TODO: create a new `map` that adds in `type_params`
            TypeKind::Generic(TGeneric {
                t: Box::from(replace_aliases_rec(t, type_param_map)),
                type_params: type_params.to_owned(),
            })
        }
        TypeKind::Var(tvar) => match &tvar.constraint {
            Some(constraint) => TypeKind::Var(TVar {
                constraint: Some(Box::from(replace_aliases_rec(constraint, type_param_map))),
                ..tvar.to_owned()
            }),
            None => t.kind.to_owned(),
        },
        TypeKind::App(types::TApp { args, ret }) => TypeKind::App(types::TApp {
            args: args
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
            ret: Box::from(replace_aliases_rec(ret, type_param_map)),
        }),
        TypeKind::Lam(types::TLam { params, ret }) => TypeKind::Lam(types::TLam {
            params: params
                .iter()
                .map(|param| TFnParam {
                    t: replace_aliases_rec(&param.t, type_param_map),
                    ..param.to_owned()
                })
                .collect(),
            ret: Box::from(replace_aliases_rec(ret, type_param_map)),
        }),
        TypeKind::Lit(_) => return t.to_owned(),
        TypeKind::Keyword(_) => return t.to_owned(),
        TypeKind::Union(types) => TypeKind::Union(
            types
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
        ),
        TypeKind::Intersection(types) => TypeKind::Intersection(
            types
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
        ),
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
                                t: replace_aliases_rec(&t.t, type_param_map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(lam.ret.as_ref(), type_param_map);

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
                                t: replace_aliases_rec(&t.t, type_param_map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(lam.ret.as_ref(), type_param_map);

                        TObjElem::Constructor(TCallable {
                            params,
                            ret: Box::from(ret),
                            type_params: lam.type_params.to_owned(),
                        })
                    }
                    TObjElem::Index(index) => {
                        let t = replace_aliases_rec(&index.t, type_param_map);
                        TObjElem::Index(types::TIndex {
                            t,
                            ..index.to_owned()
                        })
                    }
                    TObjElem::Prop(prop) => {
                        let t = replace_aliases_rec(&prop.t, type_param_map);
                        TObjElem::Prop(types::TProp {
                            t,
                            ..prop.to_owned()
                        })
                    }
                })
                .collect();
            TypeKind::Object(TObject { elems })
        }
        TypeKind::Ref(alias) => match type_param_map.get(&alias.name) {
            Some(tv) => {
                return Type {
                    kind: TypeKind::Var(tv.to_owned()),
                    // TODO: update provenance to indicate that the ref was replace with type variable
                    // ideally we should also be able to say where the type variable came from, I guess
                    // we can look at its provenance.
                    provenance: None,
                    mutable: t.mutable,
                };
            }
            None => return t.to_owned(),
        },
        TypeKind::Tuple(types) => TypeKind::Tuple(
            types
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
        ),
        TypeKind::Array(t) => TypeKind::Array(Box::from(replace_aliases_rec(t, type_param_map))),
        TypeKind::Rest(t) => TypeKind::Rest(Box::from(replace_aliases_rec(t, type_param_map))),
        TypeKind::This => TypeKind::This,
        TypeKind::KeyOf(t) => TypeKind::KeyOf(Box::from(replace_aliases_rec(t, type_param_map))),
        TypeKind::IndexAccess(TIndexAccess { object, index }) => {
            TypeKind::IndexAccess(TIndexAccess {
                object: Box::from(replace_aliases_rec(object, type_param_map)),
                index: Box::from(replace_aliases_rec(index, type_param_map)),
            })
        }
        TypeKind::MappedType(mapped) => {
            TypeKind::MappedType(TMappedType {
                t: Box::from(replace_aliases_rec(&mapped.t, type_param_map)),
                type_param: TVar {
                    id: mapped.type_param.id,
                    constraint: mapped.type_param.constraint.as_ref().map(|constraint| {
                        Box::from(replace_aliases_rec(constraint, type_param_map))
                    }),
                },
                ..mapped.to_owned()
            })
        }
    };

    Type {
        kind,
        ..t.to_owned()
    }
}

// TODO: rename to merge_interface_types
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
        .zip(
            tp1.iter()
                .map(|tv| Type::from(TypeKind::Var(tv.to_owned()))),
        )
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

            Type::from(TypeKind::Object(TObject { elems }))
        }
        (_, _) => todo!(),
    };

    if type_params.is_empty() {
        t
    } else {
        Type::from(TypeKind::Generic(TGeneric {
            t: Box::from(t),
            type_params,
        }))
    }
}
