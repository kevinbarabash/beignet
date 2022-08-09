use crochet_ast::Primitive;

use super::context::{lookup_alias, Context};
use super::substitutable::{Subst, Substitutable};
use super::types::{self, Type, Variant};
use super::util::*;

// Returns Ok(substitions) if t2 admits all values from t1 and an Err() otherwise.
pub fn unify(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, String> {
    let result = match (&t1.variant, &t2.variant) {
        (Variant::Lit(lit), Variant::Prim(prim)) => {
            let b = matches!(
                (lit, prim),
                (types::Lit::Num(_), Primitive::Num)
                    | (types::Lit::Str(_), Primitive::Str)
                    | (types::Lit::Bool(_), Primitive::Bool)
            );
            if b {
                Ok(Subst::default())
            } else {
                Err(String::from("Unification failure"))
            }
        }
        (Variant::Lam(lam1), Variant::Lam(lam2)) => {
            let mut s = Subst::new();
            // If `lam1` is a function call then we treat it differently.  Instead
            // of checking if it's a subtype of `lam2`, we instead either:
            if lam1.is_call {
                let mut params_1: Vec<Type> = vec![];
                for param in &lam1.params {
                    match &param.variant {
                        Variant::Rest(rest) => {
                            match &rest.as_ref().variant {
                                Variant::Tuple(types) => params_1.extend(types.to_owned()),
                                _ => return Err(format!("spread of type {rest} not allowed")),
                            }
                        },
                        _ => params_1.push(param.to_owned())
                    }
                }
                let last_param_2 = lam2.params.last();
                let maybe_rest_param = if let Some(param) = last_param_2 {
                    match &param.variant {
                        Variant::Rest(rest) => Some(rest.as_ref()),
                        _ => None
                    }
                } else {
                    None
                };

                // TODO: add a `variadic` boolean to the Lambda type
                if let Some(rest_param) = maybe_rest_param {
                    let regular_param_count = lam2.params.len() - 1;
                    
                    let mut args = lam1.params.clone();
                    let regular_args: Vec<_> = args.drain(0..regular_param_count).collect();
                    let rest_arg = ctx.tuple(args);

                    let mut params = lam2.params.clone();
                    let regular_params: Vec<_> = params.drain(0..regular_param_count).collect();

                    // unify regular args and params
                    for (p1, p2) in regular_args.iter().zip(&regular_params) {
                        // Each argument must be a subtype of the corresponding param.
                        let arg = p1.apply(&s);
                        let param = p2.apply(&s);
                        let s1 = unify(&arg, &param, ctx)?;
                        s = compose_subs(&s, &s1);
                    }

                    // unify remaining args with the rest param
                    let s1 = unify(&rest_arg, rest_param, ctx)?;

                    // unify return types
                    let s2 = unify(&lam1.ret.apply(&s), &lam2.ret.apply(&s), ctx)?;

                    Ok(compose_subs(&s2, &s1))
                } else if params_1.len() < lam2.params.len() {
                    // Partially application.
                    // If there is fewer than expected by `lam2` we return an new
                    // lambda that accepts the remaining params and returns the
                    // original return type.
                    let partial_ret =
                        ctx.lam(lam2.params[params_1.len()..].to_vec(), lam2.ret.clone());
                    for (p1, p2) in params_1.iter().zip(&lam2.params) {
                        // Each argument must be a subtype of the corresponding param.
                        let arg = p1.apply(&s);
                        let param = p2.apply(&s);
                        let s1 = unify(&arg, &param, ctx)?;
                        s = compose_subs(&s, &s1);
                    }
                    let s1 = unify(&lam1.ret.apply(&s), &partial_ret, ctx)?;
                    Ok(compose_subs(&s, &s1))
                } else {
                    // Regular application.
                    // Any extra params (args) that `lam1` has are ignored.
                    for (p1, p2) in params_1.iter().zip(&lam2.params) {
                        // Each argument must be a subtype of the corresponding param.
                        let arg = p1.apply(&s);
                        let param = p2.apply(&s);
                        let s1 = unify(&arg, &param, ctx)?;
                        s = compose_subs(&s, &s1);
                    }
                    let s1 = unify(&lam1.ret.apply(&s), &lam2.ret.apply(&s), ctx)?;
                    Ok(compose_subs(&s, &s1))
                }
            } else if lam1.params.len() <= lam2.params.len() {
                // If `lam1` isn't being applied then it's okay if has fewer params
                // than `lam2`.  This is because functions can be passed extra params
                // meaning that any place `lam2` is used, `lam1` can be used as well.
                for (p1, p2) in lam1.params.iter().zip(&lam2.params) {
                    // NOTE: The order of params is reverse.  This allows a callback
                    // whose params can accept more values (are supertypes) than the
                    // function will pass to the callback.
                    let s1 = unify(&p2.apply(&s), &p1.apply(&s), ctx)?;
                    s = compose_subs(&s, &s1);
                }
                let s1 = unify(&lam1.ret.apply(&s), &lam2.ret.apply(&s), ctx)?;
                Ok(compose_subs(&s, &s1))
            } else {
                Err(String::from("Couldn't unify lambdas"))
            }
        }
        (Variant::Lam(_), Variant::Intersection(types)) => {
            for t in types {
                let result = unify(t1, t, ctx);
                if result.is_ok() {
                    return result;
                }
            }
            Err(String::from("Couldn't unify lambda with intersection"))
        }
        (Variant::Object(props1), Variant::Object(props2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            let result: Result<Vec<_>, String> = props2
                .iter()
                .map(|prop2| {
                    let mut b = false;
                    let mut ss = vec![];
                    for prop1 in props1.iter() {
                        if prop1.name == prop2.name {
                            if let Ok(s) = unify(&prop1.get_type(ctx), &prop2.get_type(ctx), ctx) {
                                b = true;
                                ss.push(s);
                            }
                        }
                    }

                    match b {
                        true => Ok(compose_many_subs(&ss)),
                        false => {
                            if prop2.optional {
                                Ok(Subst::default())
                            } else {
                                Err(String::from("Unification failure"))
                            }
                        }
                    }
                })
                .collect();

            let ss = result?;
            Ok(compose_many_subs(&ss))
        }
        (Variant::Tuple(types1), Variant::Tuple(types2)) => {
            let mut before2: Vec<Type> = vec![];
            let mut after2: Vec<Type> = vec![];
            let mut maybe_rest2: Option<Type> = None;

            for t in types2 {
                match &t.variant {
                    Variant::Rest(rest_type) => {
                        if maybe_rest2.is_some() {
                            return Err(String::from("Only one rest pattern is allowed in a tuple"));
                        }
                        maybe_rest2 = Some(rest_type.as_ref().to_owned());
                    }
                    _ => match maybe_rest2 {
                        Some(_) => after2.push(t.to_owned()),
                        None => before2.push(t.to_owned()),
                    },
                }
            }

            let min_len = before2.len() + after2.len();

            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            // If it doesn't, we return an error.
            if types1.len() < min_len {
                // TODO: include the types in the error message
                return Err(String::from("not enough elements to unpack"));
            }

            let mut types1 = types1.to_owned();
            let rest_len = types1.len() - min_len;

            let before1: Vec<_> = types1.drain(0..before2.len()).collect();

            let mut ss: Vec<Subst> = vec![];

            for (t1, t2) in before1.iter().zip(before2.iter()) {
                let s = unify(t1, t2, ctx)?;
                ss.push(s);
            }

            if let Some(rest2) = maybe_rest2 {
                let rest1: Vec<_> = types1.drain(0..rest_len).collect();
                let after1: Vec<_> = types1;

                let s = unify(&ctx.tuple(rest1), &rest2, ctx)?;
                ss.push(s);

                for (t1, t2) in after1.iter().zip(after2.iter()) {
                    let s = unify(t1, t2, ctx)?;
                    ss.push(s);
                }
            }

            Ok(compose_many_subs(&ss))
        }
        (Variant::Tuple(tuple_types), Variant::Array(array_type)) => {
            if tuple_types.is_empty() {
                Ok(Subst::default())
            } else {
                let mut ss = vec![];
                for t1 in tuple_types.iter() {
                    let s = unify(t1, array_type.as_ref(), ctx)?;
                    ss.push(s)
                }
                Ok(compose_many_subs(&ss))
            }
        }
        (Variant::Union(types), _) => {
            let result: Result<Vec<_>, _> = types.iter().map(|t1| unify(t1, t2, ctx)).collect();
            let ss = result?; // This is only okay if all calls to is_subtype are okay
            Ok(compose_many_subs_with_context(&ss, ctx))
        }
        (_, Variant::Union(types)) => {
            let mut b = false;
            let mut ss = vec![];
            for t2 in types.iter() {
                if let Ok(s) = unify(t1, t2, ctx) {
                    b = true;
                    ss.push(s);
                }
            }

            match b {
                true => Ok(compose_many_subs(&ss)),
                false => Err(String::from("Unification failure")),
            }
        }
        (Variant::Var, _) => bind(&t1.id, t2),
        (_, Variant::Var) => bind(&t2.id, t1),
        (Variant::Object(props), Variant::Intersection(types)) => {
            let obj_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t.variant, Variant::Object(_)))
                .cloned()
                .collect();
            let rest_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t.variant, Variant::Var))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(&obj_types, ctx);

            println!("rest_types.len() = {}", rest_types.len());

            match rest_types.len() {
                0 => unify(t1, &obj_type, ctx),
                1 => {
                    let all_obj_props = match &obj_type.variant {
                        Variant::Object(props) => props.to_owned(),
                        _ => vec![],
                    };

                    let (obj_props, rest_props): (Vec<_>, Vec<_>) = props
                        .iter()
                        .cloned()
                        .partition(|p| all_obj_props.iter().any(|op| op.name == p.name));

                    let s1 = unify(&ctx.object(obj_props), &obj_type, ctx)?;

                    let rest_type = rest_types.get(0).unwrap();
                    let s2 = unify(&ctx.object(rest_props), rest_type, ctx)?;

                    let s = compose_subs(&s2, &s1);
                    Ok(s)
                }
                _ => Err(String::from("Unification is undecidable")),
            }
        }
        (Variant::Intersection(types), Variant::Object(props)) => {
            let obj_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t.variant, Variant::Object(_)))
                .cloned()
                .collect();
            let rest_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t.variant, Variant::Var))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(&obj_types, ctx);

            match rest_types.len() {
                0 => unify(&obj_type, t2, ctx),
                1 => {
                    let all_obj_props = match &obj_type.variant {
                        Variant::Object(props) => props.to_owned(),
                        _ => vec![],
                    };

                    let (obj_props, rest_props): (Vec<_>, Vec<_>) = props
                        .iter()
                        .cloned()
                        .partition(|p| all_obj_props.iter().any(|op| op.name == p.name));

                    let s_obj = unify(&obj_type, &ctx.object(obj_props), ctx)?;

                    let rest_type = rest_types.get(0).unwrap();
                    let s_rest = unify(rest_type, &ctx.object(rest_props), ctx)?;

                    let s = compose_subs(&s_rest, &s_obj);
                    Ok(s)
                }
                _ => Err(String::from("Unification is undecidable")),
            }
        }
        (Variant::Alias(alias1), Variant::Alias(alias2)) => {
            if alias1.name == alias2.name {
                match (&alias1.type_params, &alias2.type_params) {
                    (Some(tp1), Some(tp2)) => {
                        let result: Result<Vec<_>, _> = tp1
                            .iter()
                            .zip(tp2.iter())
                            .map(|(t1, t2)| unify(t1, t2, ctx))
                            .collect();
                        let ss = result?; // This is only okay if all calls to is_subtype are okay
                        Ok(compose_many_subs_with_context(&ss, ctx))
                    }
                    (None, None) => Ok(Subst::default()),
                    _ => Err(String::from("Alias type mismatch")),
                }
            } else {
                todo!("unify(): handle aliases that point to another alias")
            }
        }
        (_, Variant::Alias(alias)) => {
            let alias_t = lookup_alias(ctx, alias)?;
            unify(t1, &alias_t, ctx)
        }
        (Variant::Alias(alias), _) => {
            let alias_t = lookup_alias(ctx, alias)?;
            unify(&alias_t, t2, ctx)
        }
        (v1, v2) => {
            if v1 == v2 {
                Ok(Subst::new())
            } else {
                Err(String::from("Unification failure"))
            }
        }
    };
    if result.is_err() {
        println!("Can't unify t1 = {t1} with t2 = {t2}");
    }
    result
}

fn bind(id: &i32, t: &Type) -> Result<Subst, String> {
    // | t == TVar a     = return nullSubst
    // | occursCheck a t = throwError $ InfiniteType a t
    // | otherwise       = return $ Map.singleton a t
    if &t.id == id {
        Ok(Subst::default())
    } else if occurs_check(id, t) {
        Err(String::from("InfiniteType"))
    } else {
        Ok(Subst::from([(id.to_owned(), t.to_owned())]))
    }
}

fn occurs_check(id: &i32, t: &Type) -> bool {
    t.ftv().contains(id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crochet_ast::Lit;

    fn num(val: &str) -> Lit {
        Lit::num(val.to_owned(), 0..0)
    }

    fn str(val: &str) -> Lit {
        Lit::str(val.to_owned(), 0..0)
    }

    fn bool(val: &bool) -> Lit {
        Lit::bool(val.to_owned(), 0..0)
    }

    #[test]
    fn literals_are_subtypes_of_corresponding_primitives() {
        let ctx = Context::default();

        let result = unify(&ctx.lit(num("5")), &ctx.prim(Primitive::Num), &ctx);
        assert_eq!(result, Ok(Subst::default()));

        let result = unify(&ctx.lit(str("hello")), &ctx.prim(Primitive::Str), &ctx);
        assert_eq!(result, Ok(Subst::default()));

        let result = unify(&ctx.lit(bool(&true)), &ctx.prim(Primitive::Bool), &ctx);
        assert_eq!(result, Ok(Subst::default()));
    }

    #[test]
    fn object_subtypes() {
        let ctx = Context::default();

        let t1 = ctx.object(vec![
            types::TProp {
                name: String::from("foo"),
                optional: false,
                mutable: false,
                ty: ctx.lit(num("5")),
            },
            types::TProp {
                name: String::from("bar"),
                optional: false,
                mutable: false,
                ty: ctx.lit(bool(&true)),
            },
            // Having extra properties is okay
            types::TProp {
                name: String::from("baz"),
                optional: false,
                mutable: false,
                ty: ctx.prim(Primitive::Str),
            },
        ]);

        let t2 = ctx.object(vec![
            types::TProp {
                name: String::from("foo"),
                optional: false,
                mutable: false,
                ty: ctx.prim(Primitive::Num),
            },
            types::TProp {
                name: String::from("bar"),
                optional: true,
                mutable: false,
                ty: ctx.prim(Primitive::Bool),
            },
            // It's okay for qux to not appear in the subtype since
            // it's an optional property.
            types::TProp {
                name: String::from("qux"),
                optional: true,
                mutable: false,
                ty: ctx.prim(Primitive::Str),
            },
        ]);

        let result = unify(&t1, &t2, &ctx);
        assert_eq!(result, Ok(Subst::default()));
    }

    // TODO: object subtype failure cases

    #[test]
    fn failure_case() {
        let ctx = Context::default();

        let result = unify(&ctx.prim(Primitive::Num), &ctx.lit(num("5")), &ctx);

        assert_eq!(result, Err(String::from("Unification failure")))
    }
}
