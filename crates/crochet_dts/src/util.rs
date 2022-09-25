use std::collections::HashMap;

use swc_ecma_ast::*;

use crochet_infer::{get_type_params, set_type_params, Context, Subst, Substitutable};
use crochet_types::{self as types, TFnParam, TLam, TObjElem, TObject, Type};

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

fn replace_aliases_rec(t: &Type, map: &HashMap<String, i32>) -> Type {
    match t {
        Type::Var(_) => t.to_owned(),
        Type::App(types::TApp { args, ret }) => Type::App(types::TApp {
            args: args.iter().map(|t| replace_aliases_rec(t, map)).collect(),
            ret: Box::from(replace_aliases_rec(ret, map)),
        }),
        Type::Lam(types::TLam {
            params,
            ret,
            type_params,
        }) => Type::Lam(types::TLam {
            params: params
                .iter()
                .map(|param| TFnParam {
                    t: replace_aliases_rec(&param.t, map),
                    ..param.to_owned()
                })
                .collect(),
            ret: Box::from(replace_aliases_rec(ret, map)),
            type_params: type_params.to_owned(),
        }),
        Type::Prim(_) => t.to_owned(),
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

                        TObjElem::Call(TLam {
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

                        TObjElem::Constructor(TLam {
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
                        let t = replace_aliases_rec(&prop.t, map);
                        TObjElem::Prop(types::TProp {
                            t,
                            ..prop.to_owned()
                        })
                    }
                })
                .collect();
            Type::Object(TObject {
                elems,
                ..obj.to_owned()
            })
        }
        Type::Ref(alias) => match map.get(&alias.name) {
            Some(id) => Type::Var(*id),
            None => t.to_owned(),
        },
        Type::Tuple(types) => {
            Type::Tuple(types.iter().map(|t| replace_aliases_rec(t, map)).collect())
        }
        Type::Array(t) => Type::Array(Box::from(replace_aliases_rec(t, map))),
        Type::Rest(t) => Type::Rest(Box::from(replace_aliases_rec(t, map))),
        Type::This => Type::This,
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
        .zip(tp1.iter().map(|id| Type::Var(*id)))
        .collect();

    // Updates type variables for type params to match t1
    let t2 = t2.apply(&subs);

    match (t1, t2) {
        (Type::Object(obj1), Type::Object(obj2)) => {
            let elems: Vec<_> = obj1
                .elems
                .iter()
                .cloned()
                .chain(obj2.elems.iter().cloned())
                .collect();

            Type::Object(TObject {
                elems,
                type_params: obj1.type_params.to_owned(),
            })
        }
        (_, _) => todo!(),
    }
}
