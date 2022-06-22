use defaultmap::*;

use std::collections::{HashMap, HashSet};
use std::iter::Iterator;
use std::convert::identity;
use std::panic;

use crate::types::*;

use super::context::{Context, Env};
use super::substitutable::Substitutable;

pub fn normalize(sc: &Scheme, ctx: &Context) -> Scheme {
    let body = &sc.ty;
    let keys = body.ftv();
    let mut keys: Vec<_> = keys.iter().cloned().collect();
    keys.sort_unstable();
    let mapping: HashMap<i32, Type> = keys
        .iter()
        .enumerate()
        .map(|(index, key)| {
            (
                key.to_owned(),
                Type {
                    id: index as i32,
                    frozen: false,
                    variant: Variant::Var,
                    flag: None,
                },
            )
        })
        .collect();

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    fn norm_type(ty: &Type, mapping: &HashMap<i32, Type>, ctx: &Context) -> Type {
        match &ty.variant {
            Variant::Var => mapping.get(&ty.id).unwrap().to_owned(),
            Variant::Lam(LamType { params, ret }) => {
                let params: Vec<_> = params
                    .iter()
                    .map(|param| norm_type(param, mapping, ctx))
                    .collect();
                Type {
                    variant: Variant::Lam(LamType {
                        params,
                        ret: Box::from(norm_type(ret, mapping, ctx)),
                    }),
                    ..ty.to_owned()
                }
            }
            Variant::Prim(_) => ty.to_owned(),
            Variant::Lit(_) => ty.to_owned(),
            Variant::Union(types) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type {
                    variant: Variant::Union(types),
                    ..ty.to_owned()
                }
            }
            Variant::Intersection(types) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types: Vec<_> = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                simplify_intersection(&types, ctx)
            }
            Variant::Object(props) => {
                let props = props
                    .iter()
                    .map(|prop| TProp {
                        name: prop.name.clone(),
                        optional: prop.optional,
                        // NOTE: we don't use prop.get_type(ctx) here because we're tracking
                        // the optionality of the property in the TProp that's returned.
                        ty: norm_type(&prop.ty, mapping, ctx),
                    })
                    .collect();
                Type {
                    variant: Variant::Object(props),
                    ..ty.to_owned()
                }
            }
            Variant::Alias(AliasType { name, type_params }) => {
                let type_params = type_params.clone().map(|params| {
                    params
                        .iter()
                        .map(|ty| norm_type(ty, mapping, ctx))
                        .collect()
                });
                Type {
                    variant: Variant::Alias(AliasType {
                        name: name.to_owned(),
                        type_params,
                    }),
                    ..ty.to_owned()
                }
            }
            Variant::Tuple(types) => {
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type {
                    variant: Variant::Tuple(types),
                    ..ty.to_owned()
                }
            }
            Variant::Rest(arg) => Type {
                variant: Variant::Rest(Box::from(norm_type(arg, mapping, ctx))),
                ..ty.to_owned()
            },
            Variant::Member(MemberType { obj, prop }) => Type {
                variant: Variant::Member(MemberType {
                    obj: Box::from(norm_type(obj, mapping, ctx)),
                    prop: prop.to_owned(),
                }),
                ..ty.to_owned()
            },
        }
    }

    Scheme {
        qualifiers: (0..keys.len()).map(|x| x as i32).collect(),
        ty: norm_type(body, &mapping, ctx),
    }
}

pub fn generalize(env: &Env, ty: &Type) -> Scheme {
    // ftv() returns a Set which is not ordered
    // TODO: switch to an ordered set
    let mut qualifiers: Vec<_> = ty.ftv().difference(&env.ftv()).cloned().collect();
    qualifiers.sort_unstable();
    Scheme {
        qualifiers,
        ty: ty.clone(),
    }
}

// TODO: make this recursive
// TODO: handle optional properties correctly
// Maybe we can have a function that will canonicalize objects by converting 
// `x: T | undefined` to `x?: T`
pub fn simplify_intersection(in_types: &[Type], ctx: &Context) -> Type {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|ty| match &ty.variant {
            Variant::Object(props) => Some(props),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, HashSet<Type>> = defaulthashmap!();
    for props in obj_types {
        for prop in props {
            props_map[prop.name.clone()].insert(prop.ty.clone());
        }
    }

    let mut props: Vec<TProp> = props_map
        .iter()
        .map(|(name, types)| {
            let types: Vec<_> = types.iter().cloned().collect();
            let ty: Type = if types.len() == 1 {
                types[0].clone()
            } else {
                ctx.intersection(types)
            };
            TProp {
                name: name.to_owned(),
                // TODO: determine this field from all of the TProps with
                // the same name.  This should only be optional if all of
                // the TProps with the current name are optional.
                optional: false,
                ty,
            }
        })
        .collect();
    props.sort_by_key(|prop| prop.name.clone()); // ensure a stable order

    let obj_type = ctx.object(props);

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|ty| !matches!(ty.variant, Variant::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    out_types.push(obj_type);
    out_types.sort_by_key(|ty| ty.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0].clone()
    } else {
        ctx.intersection(out_types)
    }
}

// Returns true if t2 admits all values from t1.
pub fn is_subtype(t1: &Type, t2: &Type, ctx: &Context) -> Result<bool, String> {
    match (&t1.variant, &t2.variant) {
        (Variant::Lit(lit), Variant::Prim(prim)) => Ok(matches!(
            (lit, prim),
            (Lit::Num(_), Primitive::Num)
                | (Lit::Str(_), Primitive::Str)
                | (Lit::Bool(_), Primitive::Bool)
        )),
        (Variant::Object(props1), Variant::Object(props2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            let result: Result<Vec<_>, String> = props2
                .iter()
                .map(|prop2| {
                    if prop2.optional {
                        Ok(true)
                    } else {
                        let inner_result: Result<Vec<_>, String> = props1
                            .iter()
                            .map(|prop1| {
                                Ok(prop1.name == prop2.name
                                    && is_subtype(&prop1.get_type(ctx), &prop2.get_type(ctx), ctx)?)
                            })
                            .collect();
                        Ok(inner_result?.into_iter().any(identity))
                    }
                })
                .collect();
            Ok(result?.into_iter().all(identity))
        }
        (Variant::Tuple(types1), Variant::Tuple(types2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            if types1.len() < types2.len() {
                panic!("t1 contain at least the same number of elements as t2");
            }
            let result: Result<Vec<_>, _> = types1
                .iter()
                .zip(types2.iter())
                .map(|(t1, t2)| is_subtype(t1, t2, ctx))
                .collect();
            Ok(result?.into_iter().all(identity))
        }
        (Variant::Union(types), _) => {
            let result: Result<Vec<_>, _> =
                types.iter().map(|t1| is_subtype(t1, t2, ctx)).collect();
            Ok(result?.into_iter().all(identity))
        }
        (_, Variant::Union(types)) => {
            let result: Result<Vec<_>, _> =
                types.iter().map(|t2| is_subtype(t1, t2, ctx)).collect();
            Ok(result?.into_iter().any(identity))
        }
        (_, Variant::Alias(AliasType {name, ..})) => {
            match ctx.types.get(name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    is_subtype(t1, &scheme.ty, ctx)
                }
                None => panic!("Can't find alias '{name}' in context"),
            }
        }
        (Variant::Alias(AliasType { name, .. }), _) => {
            match ctx.types.get(name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    is_subtype(&scheme.ty, t2, ctx)
                }
                None => panic!("Can't find alias '{name}' in context"),
            }
        }
        (t1, t2) => Ok(t1 == t2),
    }
}
