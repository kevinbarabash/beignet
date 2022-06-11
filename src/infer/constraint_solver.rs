use std::collections::HashSet;
use std::convert::identity;
use std::panic;

use crate::types::*;

use super::context::Context;
use super::substitutable::*;

#[derive(Clone, Debug)]
pub struct Constraint {
    pub types: (Type, Type),
}

impl From<(Type, Type)> for Constraint {
    fn from(types: (Type, Type)) -> Self {
        Constraint { types }
    }
}

type Unifier = (Subst, Vec<Constraint>);

pub fn run_solve(cs: &[Constraint], ctx: &Context) -> Result<Subst, String> {
    let empty_subst = Subst::new();

    solver((empty_subst, cs.to_vec()), ctx)
}

fn solver(u: Unifier, ctx: &Context) -> Result<Subst, String> {
    let (su, cs) = u;

    if cs.is_empty() {
        return Ok(su);
    }

    let rest = &cs[1..]; // const [_ , ...rest] = cs;

    // println!("-----------");
    // println!("constraints:");
    // for Constraint {
    //     types: (left, right),
    // } in cs.iter()
    // {
    //     println!("{left} = {right}")
    // }

    match cs.get(0) {
        Some(Constraint { types: (t1, t2) }) => {
            // su1 represents new substitutions from unifying the first constraint in cs.
            let su1 = unifies(t1, t2, ctx)?;
            // This is applied to the remaining constraints (rest).  compose_subs is used
            // to apply su1 to the HashMap of subsitutions, su.
            let unifier: Unifier = (compose_subs(&su1, &su)?, Vec::from(rest).apply(&su1));
            solver(unifier, ctx)
        }
        None => Ok(su),
    }
}

fn compose_subs(s1: &Subst, s2: &Subst) -> Result<Subst, String> {
    let mut result: Subst = s2.iter().map(|(id, tv)| (*id, tv.apply(s1))).collect();
    result.extend(s1.to_owned());
    Ok(result)
}

fn unifies(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, String> {
    match (&t1.variant, &t2.variant) {
        (Variant::Var, _) => bind(&t1.id, t2, ctx),
        (_, Variant::Var) => bind(&t2.id, t1, ctx),
        (Variant::Prim(p1), Variant::Prim(p2)) if p1 == p2 => Ok(Subst::new()),
        (Variant::Lit(l1), Variant::Lit(l2)) if l1 == l2 => Ok(Subst::new()),
        (Variant::Lam(lam1), Variant::Lam(lam2)) => unify_lams(lam1, lam2, ctx),
        // TODO: copy tests case from `compiler` project for this
        (Variant::Alias(alias1), Variant::Alias(alias2)) if alias1.name == alias2.name => {
            let cs: Result<Vec<_>, _> = match (&alias1.type_params, &alias2.type_params) {
                (Some(params1), Some(params2)) => {
                    if params1.len() != params2.len() {
                        return Err(String::from(
                            "alias params have different numbers of type params",
                        ));
                    }
                    let cs = params1
                        .iter()
                        .zip(params2)
                        .map(|(a, b)| Constraint {
                            types: (a.clone(), b.clone()),
                        })
                        .collect();
                    Ok(cs)
                }
                (None, None) => Ok(vec![]),
                _ => Err(String::from("mismatch in type params")),
            };
            unify_many(&cs?, ctx)
        }
        (Variant::Member(mem1), Variant::Member(mem2)) if mem1.prop == mem2.prop => {
            unifies(mem1.obj.as_ref(), mem2.obj.as_ref(), ctx)
        }
        (Variant::Object(props1), Variant::Object(props2)) => {
            let keys1: HashSet<_> = props1.iter().map(|p| p.name.to_owned()).collect();
            let keys2: HashSet<_> = props2.iter().map(|p| p.name.to_owned()).collect();
            if keys1 == keys2 {
                let mut cs: Vec<Constraint> = vec![];
                for p1 in props1 {
                    let p2 = props2.iter().find(|p2| p1.name == p2.name).unwrap();
                    cs.push(Constraint::from((p1.get_type(ctx), p2.get_type(ctx))));
                }
                unify_many(&cs, ctx)
            } else if t2.flag == Some(Flag::Pattern) {
                // NOTE: Patterns appear as the LHS of a let-binding or as the type of a function param.
                // As such, the pattern must be a super type of the initializer in the let-binding or the
                // argument passed to a function during a call/application.
                let mut cs: Vec<Constraint> = vec![];
                for p2 in props2 {
                    let p1 = match p2.optional {
                        true => match props1.iter().find(|p1| p1.name == p2.name) {
                            Some(prop) => prop,
                            None => continue,
                        },
                        false => props1.iter().find(|p1| p1.name == p2.name).unwrap(),
                    };
                    cs.push(Constraint::from((p1.get_type(ctx), p2.get_type(ctx))));
                }
                unify_many(&cs, ctx)
            } else {
                unify_mismatched_types(t1, t2, ctx)
            }
        }
        (_, Variant::Intersection(types)) => {
            for ty in types {
                let result = unifies(t1, ty, ctx);
                if result.is_ok() {
                    return result;
                }
            }
            Err(String::from("unification failed"))
        }
        (Variant::Alias(alias), _) => {
            let aliased_type = get_aliased_type(alias, ctx);
            unifies(&aliased_type, t2, ctx)
        }
        (_, Variant::Alias(alias)) => {
            let aliased_type = get_aliased_type(alias, ctx);
            unifies(t1, &aliased_type, ctx)
        }
        _ => unify_mismatched_types(t1, t2, ctx),
    }
}

fn get_aliased_type(alias: &AliasType, ctx: &Context) -> Type {
    let scheme = ctx.types.get(&alias.name).unwrap();
    let subs: Subst = match &alias.type_params {
        Some(params) => {
            let ids = scheme.qualifiers.iter().map(|id| id.to_owned());
            ids.zip(params.iter().cloned()).collect()
        }
        None => Subst::new(),
    };

    scheme.ty.apply(&subs)
}

fn unify_mismatched_types(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, String> {
    if is_subtype(t1, t2, ctx)? {
        match t2.flag {
            Some(Flag::Argument) => {
                return Ok(Subst::from([(t2.id, t1.to_owned())]));
            }
            Some(Flag::Parameter) | Some(Flag::Pattern) => {
                return Ok(Subst::from([(t1.id, t2.to_owned())]));
            }
            Some(Flag::MemberAccess) => (),
            None => {
                if !t1.frozen {
                    // TODO: we should be using an explicit flag so that we're only
                    // doing this when it makes sense to.  Right now the following test
                    // is using this: infer_fn_param_with_type_alias_with_param.
                    // We should create more tests that involving passing subtypes to
                    // to functions.
                    return Ok(Subst::new());
                }
            }
        }
    }

    if let (Variant::Object(props1), Variant::Object(props2)) = (&t1.variant, &t2.variant) {
        if t2.flag == Some(Flag::MemberAccess) {
            if t1.frozen {
                // TODO: propagate errors instead of unwrap()-ing
                let prop2 = props2.get(0).unwrap();
                let prop1 = props1.iter().find(|p| p.name == prop2.name).unwrap();
                return unifies(&prop1.ty.to_owned(), &prop2.ty.to_owned(), ctx);
                // TODO: check if either of the properties is optional and resolve
                // the mismatch accordingly using .get_type(ctx) to convert an optional
                // prop with type T to T | undefined.
            } else {
                let new_type = intersect_types(t1, t2, ctx);
                // We should never widen object types with the MemberAccess flag
                // because we may need to grab its only property in other cirucmstances.
                let result = Subst::from([(t1.id, new_type)]);
                return Ok(result);
            }
        }
    }

    if !t1.frozen && !t2.frozen {
        let new_type = union_types(t1, t2, ctx);

        let mut result = Subst::new();

        result.insert(t1.id, new_type.clone());
        result.insert(t2.id, new_type);

        return Ok(result);
    }

    println!("unifcation failed");

    Err(String::from("unification failed"))
}

fn unify_lams(t1: &LamType, t2: &LamType, ctx: &Context) -> Result<Subst, String> {
    // TODO:
    // - varargs
    // - subtyping

    // Partial application
    if t1.params.len() < t2.params.len() {
        let partial_args = t2.params[..t1.params.len()].to_vec();
        let partial_ret = ctx.lam(t2.params[t1.params.len()..].to_vec(), t2.ret.clone());
        let mut cs: Vec<_> = t1
            .params
            .iter()
            .zip(&partial_args)
            .map(|(a, b)| Constraint {
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {
            types: (*t1.ret.clone(), partial_ret),
        });
        unify_many(&cs, ctx)
    } else if t1.params.len() != t2.params.len() {
        Err(String::from("Unification mismatch: arity mismatch"))
    } else {
        let mut cs: Vec<_> = t1
            .params
            .iter()
            .zip(&t2.params)
            .map(|(a, b)| Constraint {
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {
            types: (*t1.ret.clone(), *t2.ret.clone()),
        });
        unify_many(&cs, ctx)
    }
}

fn unify_many(cs: &[Constraint], ctx: &Context) -> Result<Subst, String> {
    match cs {
        [Constraint { types: (t1, t2) }, tail @ ..] => {
            let su_1 = unifies(t1, t2, ctx)?;
            let su_2 = unify_many(&tail.to_vec().apply(&su_1), ctx)?;
            compose_subs(&su_2, &su_1)
        }
        _ => Ok(Subst::new()),
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
        (_, Variant::Alias(alias)) => {
            match ctx.types.get(&alias.name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    is_subtype(t1, &scheme.ty, ctx)
                }
                None => panic!("Can't find alias in context"),
            }
        }
        (Variant::Alias(alias), _) => {
            match ctx.types.get(&alias.name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    is_subtype(&scheme.ty, t2, ctx)
                }
                None => panic!("Can't find alias in context"),
            }
        }
        (t1, t2) => Ok(t1 == t2),
    }
}

fn bind(tv_id: &i32, ty: &Type, _: &Context) -> Result<Subst, String> {
    // TODO: Handle Variant::Mem once it's added
    // NOTE: This will require the use of the &Context

    match ty.variant {
        Variant::Var if &ty.id == tv_id => Ok(Subst::new()),
        _ if ty.ftv().contains(tv_id) => Err(String::from("type var appears in type")),
        _ => {
            let mut subst = Subst::new();
            subst.insert(tv_id.to_owned(), ty.clone());
            Ok(subst)
        }
    }
}

fn flatten_types(ty: &Type) -> Vec<Type> {
    match &ty.variant {
        Variant::Union(types) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![ty.to_owned()],
    }
}

fn union_types(t1: &Type, t2: &Type, ctx: &Context) -> Type {
    let mut types: Vec<Type> = vec![];
    types.extend(flatten_types(t1));
    types.extend(flatten_types(t2));

    let types_set: HashSet<_> = types.iter().cloned().collect();

    let prim_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| matches!(ty.variant, Variant::Prim(_)))
        .collect();
    let lit_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| match &ty.variant {
            // Primitive types subsume corresponding literal types
            Variant::Lit(lit) => match lit {
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
        .filter(|ty| !matches!(ty.variant, Variant::Prim(_) | Variant::Lit(_)))
        .collect();

    let mut types: Vec<_> = prim_types
        .iter()
        .chain(lit_types.iter())
        .chain(rest_types.iter())
        .cloned()
        .collect();
    types.sort_by_key(|k| k.id);

    if types.len() > 1 {
        ctx.union(types)
    } else {
        types[0].clone()
    }
}

fn intersect_properties(props1: &[TProp], props2: &[TProp], ctx: &Context) -> Vec<TProp> {
    // The resulting object type should combine all properties from each.
    let mut props: Vec<TProp> = vec![];
    let mut unique1: Vec<_> = props1
        .iter()
        .filter(|p1| !props2.iter().any(|p2| p1.name == p2.name))
        .cloned()
        .collect();
    let mut unique2: Vec<_> = props2
        .iter()
        .filter(|p2| !props1.iter().any(|p1| p1.name == p2.name))
        .cloned()
        .collect();
    props.append(&mut unique1);
    props.append(&mut unique2);

    // If there is a property that exists on both, then we need to take the
    // interesction of those properties' values.
    let mut duplicates: Vec<_> = props1
        .iter()
        .filter_map(|p1| {
            props2.iter().find(|p2| p1.name == p2.name).map(|p2| TProp {
                name: p1.name.to_owned(),
                optional: p1.optional && p2.optional,
                ty: intersect_types(&p1.get_type(ctx), &p2.get_type(ctx), ctx),
            })
        })
        .collect();
    props.append(&mut duplicates);

    props
}

fn intersect_types(t1: &Type, t2: &Type, ctx: &Context) -> Type {
    match (&t1.variant, &t2.variant) {
        (Variant::Object(props1), Variant::Object(props2)) => {
            let props = intersect_properties(props1, props2, ctx);
            ctx.object(props)
        }
        (_, _) => ctx.intersection(vec![t1.to_owned(), t2.to_owned()]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn union_of_number_primitives() {
        let ctx = Context::default();
        let t1 = ctx.prim(Primitive::Num);
        let t2 = ctx.prim(Primitive::Num);

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Num));
    }

    #[test]
    fn union_of_number_literals() {
        let ctx = Context::default();
        let t1 = ctx.lit_type(Lit::Num(String::from("5")));
        let t2 = ctx.lit_type(Lit::Num(String::from("10")));

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.union(vec![t1, t2]));
    }

    #[test]
    fn union_of_number_prim_and_lit() {
        let ctx = Context::default();
        let t1 = ctx.lit_type(Lit::Num(String::from("5")));
        let t2 = ctx.prim(Primitive::Num);

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Num));
    }

    #[test]
    fn union_of_string_prim_and_lit() {
        let ctx = Context::default();
        let t1 = ctx.lit_type(Lit::Str(String::from("hello")));
        let t2 = ctx.prim(Primitive::Str);

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Str));
    }

    #[test]
    fn union_of_boolean_prim_and_lit() {
        let ctx = Context::default();
        let t1 = ctx.lit_type(Lit::Bool(true));
        let t2 = ctx.prim(Primitive::Bool);

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Bool));
    }

    #[test]
    fn union_of_unions() {
        let ctx = Context::default();
        let t1 = ctx.union(vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Bool)]);
        let t2 = ctx.union(vec![ctx.prim(Primitive::Bool), ctx.prim(Primitive::Str)]);

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(
            result,
            ctx.union(vec![
                ctx.prim(Primitive::Num),
                ctx.prim(Primitive::Bool),
                ctx.prim(Primitive::Str),
            ])
        );
    }

    #[test]
    fn union_of_type_vars() {
        let ctx = Context::default();
        let t1 = ctx.fresh_var();
        let t2 = ctx.fresh_var();

        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.union(vec![t1, t2]));
    }

    #[test]
    fn union_of_num_lits_is_subtype_of_num_prim() {
        let ctx = Context::default();

        let t1 = ctx.lit_type(Lit::Num(String::from("5")));
        let t2 = ctx.lit_type(Lit::Num(String::from("10")));
        let union = ctx.union(vec![t1, t2]);

        assert_eq!(
            is_subtype(&union, &ctx.prim(Primitive::Num), &ctx),
            Ok(true)
        );
    }

    #[test]
    fn union_subtype() {
        let ctx = Context::default();

        let union1 = ctx.union(vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Str)]);
        let union2 = ctx.union(vec![
            ctx.prim(Primitive::Num),
            ctx.prim(Primitive::Str),
            ctx.prim(Primitive::Bool),
        ]);

        assert_eq!(is_subtype(&union1, &union2, &ctx), Ok(true));
    }

    #[test]
    fn union_not_subtype() {
        let ctx = Context::default();

        let union1 = ctx.union(vec![
            ctx.prim(Primitive::Num),
            ctx.prim(Primitive::Str),
            ctx.prim(Primitive::Bool),
        ]);
        let union2 = ctx.union(vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Str)]);

        assert_eq!(is_subtype(&union1, &union2, &ctx), Ok(false));
    }
}
