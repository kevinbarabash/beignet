use defaultmap::*;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use crochet_types::*;

use super::context::{Context, Env};
use super::substitutable::{Subst, Substitutable};

pub fn normalize(sc: &Scheme, ctx: &Context) -> Scheme {
    let body = &sc.t;
    let keys = body.ftv();
    let mut keys: Vec<_> = keys.iter().cloned().collect();
    keys.sort_unstable();
    let mapping: HashMap<i32, Type> = keys
        .iter()
        .enumerate()
        .map(|(index, key)| (key.to_owned(), Type::Var(index as i32)))
        .collect();

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    // We should also add it to TObjElem (and structs used by its enums.  This will help us filter out
    // type variables that are bound to the object element as opposed to the encompassing object type.
    fn norm_type(t: &Type, mapping: &HashMap<i32, Type>, ctx: &Context) -> Type {
        match &t {
            Type::Var(id) => match mapping.get(id) {
                Some(t) => t.to_owned(),
                // If `id` doesn't exist in `mapping` we return the original type variable.
                // In this situation, it should appear in some other list of qualifiers.
                None => t.to_owned(),
            },
            Type::App(app) => {
                let args: Vec<_> = app
                    .args
                    .iter()
                    .map(|arg| norm_type(arg, mapping, ctx))
                    .collect();
                let ret = Box::from(norm_type(&app.ret, mapping, ctx));
                Type::App(TApp { args, ret })
            }
            Type::Lam(lam) => {
                let params: Vec<_> = lam
                    .params
                    .iter()
                    .map(|param| TFnParam {
                        t: norm_type(&param.t, mapping, ctx),
                        ..param.to_owned()
                    })
                    .collect();
                let ret = Box::from(norm_type(&lam.ret, mapping, ctx));
                Type::Lam(TLam { params, ret })
            }
            Type::Prim(_) => t.to_owned(),
            Type::Lit(_) => t.to_owned(),
            Type::Keyword(_) => t.to_owned(),
            Type::Union(types) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = types.iter().map(|t| norm_type(t, mapping, ctx)).collect();
                Type::Union(types)
            }
            Type::Intersection(types) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types: Vec<_> = types.iter().map(|t| norm_type(t, mapping, ctx)).collect();
                simplify_intersection(&types)
            }
            Type::Object(elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| match elem {
                        TObjElem::Call(qlam) => {
                            let params: Vec<_> = qlam
                                .t
                                .params
                                .iter()
                                .map(|param| TFnParam {
                                    t: norm_type(&param.t, mapping, ctx),
                                    ..param.to_owned()
                                })
                                .collect();
                            let ret = Box::from(norm_type(&qlam.t.ret, mapping, ctx));
                            TObjElem::Call(TCall {
                                t: TLam { params, ret },
                                ..qlam.to_owned()
                            })
                        }
                        TObjElem::Constructor(qlam) => {
                            let params: Vec<_> = qlam
                                .t
                                .params
                                .iter()
                                .map(|param| TFnParam {
                                    t: norm_type(&param.t, mapping, ctx),
                                    ..param.to_owned()
                                })
                                .collect();
                            let ret = Box::from(norm_type(&qlam.t.ret, mapping, ctx));
                            TObjElem::Constructor(TConstructor {
                                t: TLam { params, ret },
                                ..qlam.to_owned()
                            })
                        }
                        TObjElem::Index(index) => TObjElem::Index(TIndex {
                            key: index.key.clone(),
                            mutable: index.mutable,
                            scheme: Scheme {
                                t: norm_type(&index.scheme.t, mapping, ctx),
                                ..index.scheme.to_owned()
                            },
                        }),
                        TObjElem::Prop(prop) => TObjElem::Prop(TProp {
                            name: prop.name.clone(),
                            optional: prop.optional,
                            mutable: prop.mutable,
                            scheme: Scheme {
                                t: norm_type(&prop.scheme.t, mapping, ctx),
                                ..prop.scheme.to_owned()
                            },
                        }),
                    })
                    .collect();
                Type::Object(elems)
            }
            Type::Alias(TAlias { name, type_params }) => {
                let type_params = type_params
                    .clone()
                    .map(|params| params.iter().map(|t| norm_type(t, mapping, ctx)).collect());
                Type::Alias(TAlias {
                    name: name.to_owned(),
                    type_params,
                })
            }
            Type::Tuple(types) => {
                let types = types.iter().map(|t| norm_type(t, mapping, ctx)).collect();
                Type::Tuple(types)
            }
            Type::Array(t) => Type::Array(Box::from(norm_type(t, mapping, ctx))),
            Type::Rest(arg) => Type::Rest(Box::from(norm_type(arg, mapping, ctx))),
            Type::This => Type::This,
        }
    }

    Scheme {
        qualifiers: (0..keys.len()).map(|x| x as i32).collect(),
        t: norm_type(body, &mapping, ctx),
    }
}

pub fn generalize(env: &Env, t: &Type) -> Scheme {
    // ftv() returns a Set which is not ordered
    // TODO: switch to an ordered set
    let mut qualifiers: Vec<_> = t.ftv().difference(&env.ftv()).cloned().collect();
    qualifiers.sort_unstable();
    Scheme {
        qualifiers,
        t: t.clone(),
    }
}

pub fn generalize_type_map(env: &HashMap<String, Type>, t: &Type) -> Scheme {
    // ftv() returns a Set which is not ordered
    // TODO: switch to an ordered set
    let mut qualifiers: Vec<_> = t.ftv().difference(&env.ftv()).cloned().collect();
    qualifiers.sort_unstable();
    Scheme {
        qualifiers,
        t: t.clone(),
    }
}

// TODO: make this recursive
// TODO: handle optional properties correctly
// Maybe we can have a function that will canonicalize objects by converting
// `x: T | undefined` to `x?: T`
pub fn simplify_intersection(in_types: &[Type]) -> Type {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|t| match &t {
            Type::Object(elems) => Some(elems),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, HashSet<Scheme>> = defaulthashmap!();
    for elems in obj_types {
        for elem in elems {
            match elem {
                // What do we do with Call and Index signatures
                TObjElem::Call(_) => todo!(),
                TObjElem::Constructor(_) => todo!(),
                TObjElem::Index(_) => todo!(),
                TObjElem::Prop(prop) => {
                    props_map[prop.name.clone()].insert(prop.scheme.clone());
                }
            }
        }
    }

    let mut elems: Vec<TObjElem> = props_map
        .iter()
        .map(|(name, types)| {
            let schemes: Vec<_> = types.iter().cloned().collect();
            let scheme: Scheme = if types.len() == 1 {
                schemes[0].clone()
            } else {
                let qualifiers: Vec<_> = schemes
                    .iter()
                    .flat_map(|scheme| scheme.qualifiers.clone())
                    .collect();
                let types: Vec<Type> = schemes.iter().map(|scheme| scheme.t.clone()).collect();
                Scheme {
                    qualifiers,
                    t: Type::Intersection(types),
                }
            };
            TObjElem::Prop(TProp {
                name: name.to_owned(),
                // TODO: determine this field from all of the TProps with
                // the same name.  This should only be optional if all of
                // the TProps with the current name are optional.
                optional: false,
                mutable: false,
                scheme,
            })
        })
        .collect();
    // How do we sort call and index signatures?
    elems.sort_by_key(|elem| match elem {
        TObjElem::Call(_) => todo!(),
        TObjElem::Constructor(_) => todo!(),
        TObjElem::Index(_) => todo!(),
        TObjElem::Prop(prop) => prop.name.clone(),
    }); // ensure a stable order

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|t| !matches!(t, Type::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    if !elems.is_empty() {
        out_types.push(Type::Object(elems));
    }
    // TODO: figure out a consistent way to sort types
    // out_types.sort_by_key(|t| t.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0].clone()
    } else {
        Type::Intersection(out_types)
    }
}

fn flatten_types(t: &Type) -> Vec<Type> {
    match &t {
        Type::Union(types) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![t.to_owned()],
    }
}

pub fn union_types(t1: &Type, t2: &Type) -> Type {
    union_many_types(&[t1.to_owned(), t2.to_owned()])
}

pub fn union_many_types(ts: &[Type]) -> Type {
    let types: Vec<_> = ts.iter().flat_map(flatten_types).collect();

    let types_set: HashSet<_> = types.iter().cloned().collect();

    let prim_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| matches!(t, Type::Prim(_)))
        .collect();

    let lit_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| match &t {
            // Primitive types subsume corresponding literal types
            Type::Lit(lit) => match lit {
                TLit::Num(_) => !prim_types.contains(&Type::Prim(TPrim::Num)),
                TLit::Bool(_) => !prim_types.contains(&Type::Prim(TPrim::Bool)),
                TLit::Str(_) => !prim_types.contains(&Type::Prim(TPrim::Str)),
            },
            _ => false,
        })
        .collect();

    let keyword_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| matches!(t, Type::Keyword(_)))
        .collect();

    let rest_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| !matches!(t, Type::Prim(_) | Type::Lit(_) | Type::Keyword(_)))
        .collect();

    let types: Vec<_> = prim_types
        .iter()
        .chain(lit_types.iter())
        .chain(keyword_types.iter())
        .chain(rest_types.iter())
        .cloned()
        .collect();

    if types.len() > 1 {
        Type::Union(types)
    } else {
        types[0].clone()
    }
}

pub fn compose_subs(s2: &Subst, s1: &Subst) -> Subst {
    let mut result: Subst = s1.iter().map(|(id, tv)| (*id, tv.apply(s2))).collect();
    result.extend(s2.to_owned());
    result
}

// subs are composed from left to right with ones to the right
// being applied to all of the ones to the left.
pub fn compose_many_subs(subs: &[Subst]) -> Subst {
    subs.iter()
        .fold(Subst::new(), |accum, next| compose_subs(&accum, next))
}

// If are multiple entries for the same type variable, this function merges
// them into a union type (simplifying the type if possible).
fn compose_subs_with_context(s1: &Subst, s2: &Subst) -> Subst {
    let mut result: Subst = s2.iter().map(|(i, t)| (*i, t.apply(s1))).collect();
    for (i, t) in s1 {
        match result.get(i) {
            Some(t1) => {
                let t = union_types(t, t1);
                result.insert(*i, t)
            }
            None => result.insert(*i, t.to_owned()),
        };
    }
    result
}

pub fn compose_many_subs_with_context(subs: &[Subst]) -> Subst {
    subs.iter().fold(Subst::new(), |accum, next| {
        compose_subs_with_context(&accum, next)
    })
}
