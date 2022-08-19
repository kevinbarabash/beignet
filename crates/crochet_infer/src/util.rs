use defaultmap::*;

use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use super::context::{Context, Env};
use super::substitutable::{Subst, Substitutable};
use super::types::*;

pub fn normalize(sc: &Scheme, ctx: &Context) -> Scheme {
    let body = &sc.ty;
    let keys = body.ftv();
    let mut keys: Vec<_> = keys.iter().cloned().collect();
    keys.sort_unstable();
    let mapping: HashMap<i32, Type> = keys
        .iter()
        .enumerate()
        .map(|(index, key)| (key.to_owned(), Type::Var(index as i32)))
        .collect();

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    fn norm_type(ty: &Type, mapping: &HashMap<i32, Type>, ctx: &Context) -> Type {
        match &ty {
            Type::Var(id) => mapping.get(id).unwrap().to_owned(),
            Type::App(app) => {
                let args: Vec<_> = app
                    .args
                    .iter()
                    .map(|arg| norm_type(arg, mapping, ctx))
                    .collect();
                let ret = Box::from(norm_type(&app.ret, mapping, ctx));
                Type::App(AppType { args, ret })
            }
            Type::Lam(lam) => {
                let params: Vec<_> = lam
                    .params
                    .iter()
                    .map(|param| TFnParam {
                        ty: norm_type(&param.ty, mapping, ctx),
                        ..param.to_owned()
                    })
                    .collect();
                let ret = Box::from(norm_type(&lam.ret, mapping, ctx));
                Type::Lam(LamType { params, ret })
            }
            Type::Wildcard => ty.to_owned(),
            Type::Prim(_) => ty.to_owned(),
            Type::Lit(_) => ty.to_owned(),
            Type::Union(types) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type::Union(types)
            }
            Type::Intersection(types) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types: Vec<_> = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                simplify_intersection(&types, ctx)
            }
            Type::Object(props) => {
                let props = props
                    .iter()
                    .map(|prop| TProp {
                        name: prop.name.clone(),
                        optional: prop.optional,
                        mutable: prop.mutable,
                        // NOTE: we don't use prop.get_type(ctx) here because we're tracking
                        // the optionality of the property in the TProp that's returned.
                        ty: norm_type(&prop.ty, mapping, ctx),
                    })
                    .collect();
                Type::Object(props)
            }
            Type::Alias(AliasType { name, type_params }) => {
                let type_params = type_params.clone().map(|params| {
                    params
                        .iter()
                        .map(|ty| norm_type(ty, mapping, ctx))
                        .collect()
                });
                Type::Alias(AliasType {
                    name: name.to_owned(),
                    type_params,
                })
            }
            Type::Tuple(types) => {
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type::Tuple(types)
            }
            Type::Array(t) => Type::Array(Box::from(norm_type(t, mapping, ctx))),
            Type::Rest(arg) => Type::Rest(Box::from(norm_type(arg, mapping, ctx))),
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
        .filter_map(|ty| match &ty {
            Type::Object(props) => Some(props),
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
                mutable: false,
                ty,
            }
        })
        .collect();
    props.sort_by_key(|prop| prop.name.clone()); // ensure a stable order

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|ty| !matches!(ty, Type::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    if !props.is_empty() {
        out_types.push(ctx.object(props));
    }
    // TODO: figure out a consistent way to sort types
    // out_types.sort_by_key(|ty| ty.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0].clone()
    } else {
        ctx.intersection(out_types)
    }
}

fn flatten_types(ty: &Type) -> Vec<Type> {
    match &ty {
        Type::Union(types) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![ty.to_owned()],
    }
}

pub fn union_types(t1: &Type, t2: &Type, ctx: &Context) -> Type {
    union_many_types(&[t1.to_owned(), t2.to_owned()], ctx)
}

pub fn union_many_types(ts: &[Type], ctx: &Context) -> Type {
    let types: Vec<_> = ts.iter().flat_map(flatten_types).collect();

    let types_set: HashSet<_> = types.iter().cloned().collect();

    let prim_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| matches!(ty, Type::Prim(_)))
        .collect();
    let lit_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| match &ty {
            // Primitive types subsume corresponding literal types
            Type::Lit(lit) => match lit {
                Lit::Num(_) => !prim_types.contains(&ctx.prim(Primitive::Num)),
                Lit::Bool(_) => !prim_types.contains(&ctx.prim(Primitive::Bool)),
                Lit::Str(_) => !prim_types.contains(&ctx.prim(Primitive::Str)),
                Lit::Null => !prim_types.contains(&ctx.prim(Primitive::Null)),
                Lit::Undefined => !prim_types.contains(&ctx.prim(Primitive::Undefined)),
            },
            _ => false,
        })
        .collect();
    let rest_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| !matches!(ty, Type::Prim(_) | Type::Lit(_)))
        .collect();

    let types: Vec<_> = prim_types
        .iter()
        .chain(lit_types.iter())
        .chain(rest_types.iter())
        .cloned()
        .collect();

    if types.len() > 1 {
        ctx.union(types)
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
fn compose_subs_with_context(s1: &Subst, s2: &Subst, ctx: &Context) -> Subst {
    let mut result: Subst = s2.iter().map(|(i, t)| (*i, t.apply(s1))).collect();
    for (i, t) in s1 {
        match result.get(i) {
            Some(t1) => {
                let t = union_types(t, t1, ctx);
                result.insert(*i, t)
            }
            None => result.insert(*i, t.to_owned()),
        };
    }
    result
}

pub fn compose_many_subs_with_context(subs: &[Subst], ctx: &Context) -> Subst {
    subs.iter().fold(Subst::new(), |accum, next| {
        compose_subs_with_context(&accum, next, ctx)
    })
}
