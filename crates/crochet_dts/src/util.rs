use std::collections::HashMap;

use swc_ecma_ast::*;

use crochet_infer::{get_type_params, set_type_params, Context, Subst, Substitutable};
use crochet_types::{
    self as types, TCallable, TFnParam, TGeneric, TIndexAccess, TObjElem, TObject, TVar, Type,
};

// TODO: rename this replace_refs
pub fn replace_aliases(t: &Type, type_param_decl: &TsTypeParamDecl, ctx: &Context) -> Type {
    let mut type_params: Vec<i32> = vec![];
    let type_param_map: HashMap<String, i32> = type_param_decl
        .params
        .iter()
        .map(|tp| {
            let id = ctx.fresh_id();
            type_params.push(id);
            (tp.name.sym.to_string(), id)
        })
        .collect();

    let t = set_type_params(t, &type_params);
    replace_aliases_rec(&t, &type_param_map)
}

// TODO: rename this replace_refs_rec
fn replace_aliases_rec(t: &Type, map: &HashMap<String, i32>) -> Type {
    match t {
        Type::Generic(TGeneric { t, type_params }) => {
            // TODO: create a new `map` that adds in `type_params`
            Type::Generic(TGeneric {
                t: Box::from(replace_aliases_rec(t, map)),
                type_params: type_params.to_owned(),
            })
        }
        Type::Var(_) => t.to_owned(),
        Type::App(types::TApp { args, ret }) => Type::App(types::TApp {
            args: args.iter().map(|t| replace_aliases_rec(t, map)).collect(),
            ret: Box::from(replace_aliases_rec(ret, map)),
        }),
        Type::Lam(types::TLam { params, ret }) => Type::Lam(types::TLam {
            params: params
                .iter()
                .map(|param| TFnParam {
                    t: replace_aliases_rec(&param.t, map),
                    ..param.to_owned()
                })
                .collect(),
            ret: Box::from(replace_aliases_rec(ret, map)),
        }),
        Type::Lit(_) => t.to_owned(),
        Type::Keyword(_) => t.to_owned(),
        Type::Union(types) => {
            Type::Union(types.iter().map(|t| replace_aliases_rec(t, map)).collect())
        }
        Type::Intersection(types) => {
            Type::Intersection(types.iter().map(|t| replace_aliases_rec(t, map)).collect())
        }
        Type::Object(obj) => {
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
            Type::Object(TObject { elems })
        }
        Type::Ref(alias) => match map.get(&alias.name) {
            Some(id) => Type::Var(TVar {
                id: id.to_owned(),
                quals: None,
            }),
            None => t.to_owned(),
        },
        Type::Tuple(types) => {
            Type::Tuple(types.iter().map(|t| replace_aliases_rec(t, map)).collect())
        }
        Type::Array(t) => Type::Array(Box::from(replace_aliases_rec(t, map))),
        Type::Rest(t) => Type::Rest(Box::from(replace_aliases_rec(t, map))),
        Type::This => Type::This,
        Type::KeyOf(t) => Type::KeyOf(Box::from(replace_aliases_rec(t, map))),
        Type::IndexAccess(TIndexAccess { object, index }) => Type::IndexAccess(TIndexAccess {
            object: Box::from(replace_aliases_rec(object, map)),
            index: Box::from(replace_aliases_rec(index, map)),
        }),
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
        .zip(tp1.iter().map(|id| {
            Type::Var(TVar {
                id: id.to_owned(),
                quals: None,
            })
        }))
        .collect();

    // Unwrap qualified types since we return a qualified
    // type if there are any type qualifiers.
    let t1 = match t1 {
        Type::Generic(TGeneric { t, .. }) => t,
        t => t,
    };
    let t2 = match t2 {
        Type::Generic(TGeneric { t, .. }) => t,
        t => t,
    };

    // Updates type variables for type params to match t1
    let t2 = t2.apply(&subs);

    let type_params = tp1;
    let t = match (t1, t2) {
        (Type::Object(obj1), Type::Object(obj2)) => {
            let elems: Vec<_> = obj1
                .elems
                .iter()
                .cloned()
                .chain(obj2.elems.iter().cloned())
                .collect();

            Type::Object(TObject { elems })
        }
        (_, _) => todo!(),
    };

    if type_params.is_empty() {
        t
    } else {
        Type::Generic(TGeneric {
            t: Box::from(t),
            type_params,
        })
    }
}
