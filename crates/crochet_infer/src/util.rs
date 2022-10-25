use array_tool::vec::*;
use defaultmap::*;
use std::collections::{BTreeSet, HashMap};
use std::iter::Iterator;

use crochet_ast::types::*;

use crate::context::{Context, Env};
use crate::substitutable::{Subst, Substitutable};

fn get_mapping(t: &Type) -> HashMap<i32, Type> {
    let mut mapping: HashMap<i32, Type> = t
        .ftv()
        .iter()
        .enumerate()
        .map(|(index, key)| {
            (
                key.id,
                Type {
                    kind: TypeKind::Var(TVar {
                        id: index as i32,
                        constraint: key.constraint.clone(),
                    }),
                    provenance: None,
                },
            )
        })
        .collect();

    let type_params = get_type_params(t);
    let offset = mapping.len();
    for (index, tp) in type_params.iter().enumerate() {
        mapping.insert(
            tp.id,
            Type {
                kind: TypeKind::Var(TVar {
                    id: (offset + index) as i32,
                    constraint: tp.constraint.clone(),
                }),
                provenance: None,
            },
        );
    }

    mapping
}

pub fn close_over(s: &Subst, t: &Type, ctx: &Context) -> Type {
    let empty_env = Env::default();
    normalize(&generalize(&empty_env, &t.to_owned().apply(s)), ctx)
}

pub fn normalize(t: &Type, ctx: &Context) -> Type {
    let mapping = get_mapping(t);

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    // We should also add it to TObjElem (and structs used by its enums.  This will help us filter out
    // type variables that are bound to the object element as opposed to the encompassing object type.
    fn norm_type(t: &Type, mapping: &HashMap<i32, Type>, ctx: &Context) -> Type {
        match &t.kind {
            TypeKind::Generic(TGeneric {
                t: inner_t,
                type_params,
            }) => {
                // NOTE: For now we don't bother normalizing qualified types since
                // mappings from higher up may cause type variables to be reassigned
                // to the incorrect type.  In order to fix this, we'd have to run
                // `normalize` itself on each qualified type in the tree.
                Type { kind: TypeKind::Generic(TGeneric {
                    t: Box::from(norm_type(inner_t, mapping, ctx)),
                    type_params: type_params
                        .iter()
                        .map(|tp| match mapping.get(&tp.id) {
                            Some(t) => {
                                if let TypeKind::Var(tv) = &t.kind {
                                    tv.to_owned()
                                } else {
                                    panic!("Expected a type variable in mapping when update type_params");
                                }
                            },
                            None => tp.to_owned(),
                        })
                        .collect(),
                }), provenance: None}
            }
            TypeKind::Var(tv) => match mapping.get(&tv.id) {
                Some(t) => t.to_owned(),
                // If `id` doesn't exist in `mapping` we return the original type variable.
                // In this situation, it should appear in some other list of qualifiers.
                None => t.to_owned(),
            },
            TypeKind::App(app) => {
                let args: Vec<_> = app
                    .args
                    .iter()
                    .map(|arg| norm_type(arg, mapping, ctx))
                    .collect();
                let ret = Box::from(norm_type(&app.ret, mapping, ctx));
                Type {
                    kind: TypeKind::App(TApp { args, ret }),
                    provenance: None,
                }
            }
            TypeKind::Lam(lam) => {
                let params: Vec<_> = lam
                    .params
                    .iter()
                    .map(|param| TFnParam {
                        t: norm_type(&param.t, mapping, ctx),
                        ..param.to_owned()
                    })
                    .collect();
                let ret = Box::from(norm_type(&lam.ret, mapping, ctx));
                Type {
                    kind: TypeKind::Lam(TLam { params, ret }),
                    provenance: None,
                }
            }
            TypeKind::Lit(_) => t.to_owned(),
            TypeKind::Keyword(_) => t.to_owned(),
            TypeKind::Union(types) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = types.iter().map(|t| norm_type(t, mapping, ctx)).collect();
                Type {
                    kind: TypeKind::Union(types),
                    provenance: None,
                }
            }
            TypeKind::Intersection(types) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types: Vec<_> = types.iter().map(|t| norm_type(t, mapping, ctx)).collect();
                simplify_intersection(&types)
            }
            TypeKind::Object(obj) => {
                let elems = obj
                    .elems
                    .iter()
                    .map(|elem| match elem {
                        TObjElem::Call(call) => {
                            let params: Vec<_> = call
                                .params
                                .iter()
                                .map(|param| TFnParam {
                                    t: norm_type(&param.t, mapping, ctx),
                                    ..param.to_owned()
                                })
                                .collect();
                            let ret = Box::from(norm_type(&call.ret, mapping, ctx));
                            TObjElem::Call(TCallable {
                                params,
                                ret,
                                // TODO: normalize type_params?
                                type_params: call.type_params.to_owned(),
                            })
                        }
                        TObjElem::Constructor(call) => {
                            let params: Vec<_> = call
                                .params
                                .iter()
                                .map(|param| TFnParam {
                                    t: norm_type(&param.t, mapping, ctx),
                                    ..param.to_owned()
                                })
                                .collect();
                            let ret = Box::from(norm_type(&call.ret, mapping, ctx));
                            TObjElem::Constructor(TCallable {
                                params,
                                ret,
                                // TODO: normalize type_params?
                                type_params: call.type_params.to_owned(),
                            })
                        }
                        TObjElem::Index(index) => TObjElem::Index(TIndex {
                            key: index.key.to_owned(),
                            mutable: index.mutable,
                            t: norm_type(&index.t, mapping, ctx),
                        }),
                        TObjElem::Prop(prop) => TObjElem::Prop(TProp {
                            name: prop.name.to_owned(),
                            optional: prop.optional,
                            mutable: prop.mutable,
                            t: norm_type(&prop.t, mapping, ctx),
                        }),
                    })
                    .collect();
                Type {
                    kind: TypeKind::Object(TObject { elems }),
                    provenance: None,
                }
            }
            TypeKind::Ref(TRef { name, type_args }) => {
                let type_args = type_args
                    .clone()
                    .map(|params| params.iter().map(|t| norm_type(t, mapping, ctx)).collect());
                Type {
                    kind: TypeKind::Ref(TRef {
                        name: name.to_owned(),
                        type_args,
                    }),
                    provenance: None,
                }
            }
            TypeKind::Tuple(types) => {
                let types = types.iter().map(|t| norm_type(t, mapping, ctx)).collect();
                Type {
                    kind: TypeKind::Tuple(types),
                    provenance: None,
                }
            }
            TypeKind::Array(t) => Type {
                kind: TypeKind::Array(Box::from(norm_type(t, mapping, ctx))),
                provenance: None,
            },
            TypeKind::Rest(arg) => Type {
                kind: TypeKind::Rest(Box::from(norm_type(arg, mapping, ctx))),
                provenance: None,
            },
            TypeKind::This => Type {
                kind: TypeKind::This,
                provenance: None,
            },
            TypeKind::KeyOf(t) => Type {
                kind: TypeKind::KeyOf(Box::from(norm_type(t, mapping, ctx))),
                provenance: None,
            },
            TypeKind::IndexAccess(TIndexAccess { object, index }) => Type {
                kind: TypeKind::IndexAccess(TIndexAccess {
                    object: Box::from(norm_type(object, mapping, ctx)),
                    index: Box::from(norm_type(index, mapping, ctx)),
                }),
                provenance: None,
            },
            TypeKind::Mutable(t) => Type {
                kind: TypeKind::Mutable(Box::from(norm_type(t, mapping, ctx))),
                provenance: None,
            },
        }
    }

    // NOTE: normalize() is usually called on a type that's already been generalized so we
    // don't need to re-generalize the result type here.
    norm_type(t, &mapping, ctx)
}

pub fn generalize(env: &Env, t: &Type) -> Type {
    let mut type_params = get_type_params(t);

    // We do this to account for type params from the environment.
    // QUESTION: Why is it okay to ignore the keys of env?
    type_params.append(&mut t.ftv().uniq_via(env.ftv(), |a, b| a.id == b.id));

    if type_params.is_empty() {
        return t.to_owned();
    }

    set_type_params(t, &type_params)
}

// TODO: make this recursive
// TODO: handle optional properties correctly
// Maybe we can have a function that will canonicalize objects by converting
// `x: T | undefined` to `x?: T`
pub fn simplify_intersection(in_types: &[Type]) -> Type {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|t| match &t.kind {
            TypeKind::Object(elems) => Some(elems),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, BTreeSet<Type>> = defaulthashmap!();
    for obj in obj_types {
        for elem in &obj.elems {
            match elem {
                // What do we do with Call and Index signatures
                TObjElem::Call(_) => todo!(),
                TObjElem::Constructor(_) => todo!(),
                TObjElem::Index(_) => todo!(),
                TObjElem::Prop(prop) => {
                    props_map[prop.name.clone()].insert(prop.t.clone());
                }
            }
        }
    }

    let mut elems: Vec<TObjElem> = props_map
        .iter()
        .map(|(name, types)| {
            let types: Vec<_> = types.iter().cloned().collect();
            let t: Type = if types.len() == 1 {
                types[0].clone()
            } else {
                Type {
                    kind: TypeKind::Intersection(types),
                    provenance: None,
                }
            };
            TObjElem::Prop(TProp {
                name: name.to_owned(),
                // TODO: determine this field from all of the TProps with
                // the same name.  This should only be optional if all of
                // the TProps with the current name are optional.
                optional: false,
                mutable: false,
                t,
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
        .filter(|t| !matches!(&t.kind, TypeKind::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    if !elems.is_empty() {
        out_types.push(Type {
            kind: TypeKind::Object(TObject { elems }),
            provenance: None,
        });
    }
    // TODO: figure out a consistent way to sort types
    // out_types.sort_by_key(|t| t.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0].clone()
    } else {
        Type {
            kind: TypeKind::Intersection(out_types),
            provenance: None,
        }
    }
}

fn flatten_types(t: &Type) -> Vec<Type> {
    match &t.kind {
        TypeKind::Union(types) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![t.to_owned()],
    }
}

pub fn union_types(t1: &Type, t2: &Type) -> Type {
    union_many_types(&[t1.to_owned(), t2.to_owned()])
}

pub fn union_many_types(ts: &[Type]) -> Type {
    let types: Vec<_> = ts.iter().flat_map(flatten_types).collect();

    let types_set: BTreeSet<_> = types.iter().cloned().collect();

    let keyword_types: BTreeSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| matches!(&t.kind, TypeKind::Keyword(_)))
        .collect();

    let lit_types: BTreeSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| match &t.kind {
            // Primitive types subsume corresponding literal types
            TypeKind::Lit(lit) => match lit {
                TLit::Num(_) => !keyword_types.contains(&Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                }),
                TLit::Bool(_) => !keyword_types.contains(&Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                }),
                TLit::Str(_) => !keyword_types.contains(&Type {
                    kind: TypeKind::Keyword(TKeyword::String),
                    provenance: None,
                }),
            },
            _ => false,
        })
        .collect();

    let other_types: BTreeSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| !matches!(&t.kind, TypeKind::Lit(_) | TypeKind::Keyword(_)))
        .collect();

    let types: Vec<_> = keyword_types
        .iter()
        .chain(lit_types.iter())
        .chain(other_types.iter())
        .cloned()
        .collect();

    if types.len() > 1 {
        Type {
            kind: TypeKind::Union(types),
            provenance: None,
        }
    } else {
        types[0].clone()
    }
}

pub fn compose_subs(s2: &Subst, s1: &Subst) -> Subst {
    let mut result: Subst = s1
        .iter()
        .map(|(tv, t)| (tv.to_owned(), t.apply(s2)))
        .collect();
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
    let mut result: Subst = s2
        .iter()
        .map(|(tv, t)| (tv.to_owned(), t.apply(s1)))
        .collect();
    for (tv, t) in s1 {
        match result.get(tv) {
            Some(t1) => {
                let t = union_types(t, t1);
                result.insert(tv.to_owned(), t)
            }
            None => result.insert(tv.to_owned(), t.to_owned()),
        };
    }
    result
}

pub fn compose_many_subs_with_context(subs: &[Subst]) -> Subst {
    subs.iter().fold(Subst::new(), |accum, next| {
        compose_subs_with_context(&accum, next)
    })
}

pub fn get_property_type(prop: &TProp) -> Type {
    let t = prop.t.to_owned();
    match prop.optional {
        true => Type {
            kind: TypeKind::Union(vec![
                t,
                Type {
                    kind: TypeKind::Keyword(TKeyword::Undefined),
                    provenance: None,
                },
            ]),
            provenance: None,
        },
        false => t,
    }
}

pub fn get_type_params(t: &Type) -> Vec<TVar> {
    match &t.kind {
        TypeKind::Generic(gen) => gen.type_params.to_owned(),
        _ => vec![],
    }
}

pub fn set_type_params(t: &Type, type_params: &[TVar]) -> Type {
    // If there are no type params then the returned type shouldn't be
    // a qualified type.
    if type_params.is_empty() {
        return t.to_owned();
    }

    match &t.kind {
        TypeKind::Var(_) => t.to_owned(),
        TypeKind::Generic(TGeneric {
            t,
            // NOTE: `generalize_type` is responsible for merge type params so
            // it's safe to ignore `type_params` here.
            type_params: _,
        }) => Type {
            kind: TypeKind::Generic(TGeneric {
                t: t.to_owned(),
                type_params: type_params.to_owned(),
            }),
            provenance: None,
        },
        _ => Type {
            kind: TypeKind::Generic(TGeneric {
                t: Box::from(t.to_owned()),
                type_params: type_params.to_owned(),
            }),
            provenance: None,
        },
    }
}
