use std::cmp;
use std::collections::HashSet;

use crochet_types::{self as types, TObject, TPrim, Type};
use types::TObjElem;

use super::context::Context;
use super::substitutable::{Subst, Substitutable};
use super::util::*;

// Returns Ok(substitions) if t2 admits all values from t1 and an Err() otherwise.
pub fn unify(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, String> {
    let result = match (&t1, &t2) {
        // All binding must be done first
        (Type::Var(id), _) => bind(id, t2),
        (_, Type::Var(id)) => bind(id, t1),

        (Type::Lit(lit), Type::Prim(prim)) => {
            let b = matches!(
                (lit, prim),
                (types::TLit::Num(_), TPrim::Num)
                    | (types::TLit::Str(_), TPrim::Str)
                    | (types::TLit::Bool(_), TPrim::Bool)
            );
            if b {
                Ok(Subst::default())
            } else {
                Err(String::from("Unification failure"))
            }
        }
        (Type::App(app1), Type::App(app2)) => {
            let mut s = Subst::new();

            // NOTE: `app1` and `app2` currently must have the same number of args.
            // TODO: Once we have support for optional function params, update
            // this to support having different lengths of params.
            if app1.args.len() == app2.args.len() {
                for (p1, p2) in app1.args.iter().zip(&app2.args) {
                    let s1 = unify(&p1.apply(&s), &p2.apply(&s), ctx)?;
                    s = compose_subs(&s, &s1);
                }
                let s1 = unify(&app1.ret.apply(&s), &app2.ret.apply(&s), ctx)?;
                Ok(compose_subs(&s, &s1))
            } else {
                Err(String::from("Couldn't unify function calls"))
            }
        }
        (Type::Lam(lam1), Type::Lam(lam2)) => {
            let mut s = Subst::new();

            // It's okay if has fewer params than `lam2`.  This is because
            // functions can be passed extra params meaning that any place
            // `lam2` is used, `lam1` can be used as well.
            //
            // TODO: figure what this means for lambdas with rest params.
            if lam1.params.len() <= lam2.params.len() {
                for (p1, p2) in lam1.params.iter().zip(&lam2.params) {
                    // NOTE: The order of params is reversed.  This allows a callback
                    // whose params can accept more values (are supertypes) than the
                    // function will pass to the callback.
                    let s1 = unify(&p2.get_type().apply(&s), &p1.get_type().apply(&s), ctx)?;
                    s = compose_subs(&s, &s1);
                }
                let s1 = unify(&lam1.ret.apply(&s), &lam2.ret.apply(&s), ctx)?;
                Ok(compose_subs(&s, &s1))
            } else {
                Err(String::from("Couldn't unify lambdas"))
            }
        }
        // NOTE: this arm is only hit by the `infer_skk` test case
        (Type::Lam(_), Type::App(_)) => unify(t2, t1, ctx),
        (Type::App(_), Type::Object(obj)) => {
            let callables: Vec<_> = obj
                .elems
                .iter()
                .filter_map(|elem| match elem {
                    TObjElem::Call(lam) => Some(Type::Lam(lam.to_owned())),
                    TObjElem::Constructor(_) => None,
                    TObjElem::Index(_) => None,
                    TObjElem::Prop(_) => None,
                })
                .collect();

            if callables.is_empty() {
                Err(String::from("Couldn't application with object"))
            } else {
                for callable in callables {
                    let result = unify(t1, &callable, ctx);
                    if result.is_ok() {
                        return result;
                    }
                }
                Err(String::from("Couldn't application with object"))
            }
        }
        (Type::App(app), Type::Lam(lam)) => {
            let mut s = Subst::new();

            let last_param_2 = lam.params.last();
            let maybe_rest_param = if let Some(param) = last_param_2 {
                match &param.pat {
                    types::TPat::Rest(_) => Some(param.t.to_owned()),
                    _ => None,
                }
            } else {
                None
            };

            // TODO: work out how rest and spread should work together.
            //
            // args: (a1, a2, a_spread[0 to n]) -> 2 to 2 + n
            // params: (p1, p2, p3, p_rest[0 to n]) -> 3 to 3 + n
            // this means that a_spread must have a length of at least 1 in order
            // for the lower bounds to match.

            let optional_count = lam
                .params
                .iter()
                .fold(0, |accum, param| match param.optional {
                    true => accum + 1,
                    false => accum,
                });

            let param_count_low_bound = match maybe_rest_param {
                Some(_) => lam.params.len() - optional_count - 1,
                None => lam.params.len() - optional_count,
            };

            // NOTE: placeholder spreads must come last because we don't know they're
            // length.  This will also be true for spreading arrays, but in the case
            // of array spreads, they also need to come after the start of a rest param.

            let mut args: Vec<Type> = vec![];
            // TODO: disallow spreading an array if it isn't the last arg
            for arg in app.args.iter() {
                match &arg {
                    Type::Rest(spread) => match &spread.as_ref() {
                        Type::Tuple(types) => args.extend(types.to_owned()),
                        _ => return Err(format!("spread of type {spread} not allowed")),
                    },
                    _ => args.push(arg.to_owned()),
                }
            }

            if args.len() < param_count_low_bound {
                return Err(String::from("Not enough args provided"));
            }

            // TODO: Add a `variadic` boolean to the Lambda type as a convenience
            // so that we don't have to search through all the params for the rest
            // param.

            // TODO: Refactor this logic to be simpler, try to unify the rest and non-rest
            // cases if possible.
            if let Some(rest_param) = maybe_rest_param {
                let max_regular_arg_count = lam.params.len() - 1;
                let regular_arg_count = cmp::min(max_regular_arg_count, args.len());

                let mut args = app.args.clone();
                let regular_args: Vec<_> = args.drain(0..regular_arg_count).collect();
                let rest_arg = Type::Tuple(args);

                let mut params = lam.params.clone();
                let regular_params: Vec<_> = params.drain(0..regular_arg_count).collect();

                // Unify regular args and params
                for (p1, p2) in regular_args.iter().zip(&regular_params) {
                    // Each argument must be a subtype of the corresponding param.
                    let arg = p1.apply(&s);
                    let param = p2.apply(&s);
                    let s1 = unify(&arg, &param.get_type(), ctx)?;
                    s = compose_subs(&s, &s1);
                }

                // Unify remaining args with the rest param
                let s1 = unify(&rest_arg, &rest_param, ctx)?;

                // Unify return types
                let s2 = unify(&app.ret.apply(&s), &lam.ret.apply(&s), ctx)?;

                Ok(compose_subs(&s2, &s1))
            } else if args.len() >= param_count_low_bound {
                // NOTE: Any extra args are ignored.

                // Regular Application
                for (p1, p2) in args.iter().zip(&lam.params) {
                    // Each argument must be a subtype of the corresponding param.
                    let arg = p1.apply(&s);
                    let param = p2.get_type().apply(&s);
                    let s1 = unify(&arg, &param, ctx)?;
                    s = compose_subs(&s, &s1);
                }
                let s1 = unify(&app.ret.apply(&s), &lam.ret.apply(&s), ctx)?;
                Ok(compose_subs(&s, &s1))
            } else {
                Err(String::from("Not enough params provided"))
            }
        }
        (Type::App(_), Type::Intersection(types)) => {
            for t in types {
                let result = unify(t1, t, ctx);
                if result.is_ok() {
                    return result;
                }
            }
            Err(String::from("Couldn't unify lambda with intersection"))
        }
        (Type::Object(obj1), Type::Object(obj2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            let result: Result<Vec<_>, String> = obj2
                .elems
                .iter()
                .map(|e2| {
                    let mut b = false;
                    let mut ss = vec![];
                    for e1 in obj1.elems.iter() {
                        match (e1, e2) {
                            (TObjElem::Call(_), TObjElem::Call(_)) => {
                                // What to do about Call signatures?
                                todo!()
                            }
                            (TObjElem::Prop(prop1), TObjElem::Prop(prop2)) => {
                                if prop1.name == prop2.name {
                                    let t1 = get_property_type(prop1);
                                    let t2 = get_property_type(prop2);

                                    if let Ok(s) = unify(&t1, &t2, ctx) {
                                        b = true;
                                        ss.push(s);
                                    }
                                }
                            }
                            // skip pairs that aren't the same
                            _ => (),
                        }
                    }

                    match b {
                        true => Ok(compose_many_subs(&ss)),
                        false => match e2 {
                            TObjElem::Call(_) => Err(String::from("Unification failure")),
                            TObjElem::Constructor(_) => Err(String::from("Unification failure")),
                            TObjElem::Index(_) => Err(String::from("Unification failure")),
                            TObjElem::Prop(prop2) => {
                                // Will all optional properties to be missing
                                if prop2.optional {
                                    Ok(Subst::default())
                                } else {
                                    Err(String::from("Unification failure"))
                                }
                            }
                        },
                    }
                })
                .collect();

            let ss = result?;
            Ok(compose_many_subs(&ss))
        }
        (Type::Tuple(types1), Type::Tuple(types2)) => {
            let mut before2: Vec<Type> = vec![];
            let mut after2: Vec<Type> = vec![];
            let mut maybe_rest2: Option<Type> = None;

            for t in types2 {
                match &t {
                    Type::Rest(rest_type) => {
                        if maybe_rest2.is_some() {
                            return Err(String::from(
                                "Only one rest pattern is allowed in a tuple",
                            ));
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

                let s = unify(&Type::Tuple(rest1), &rest2, ctx)?;
                ss.push(s);

                for (t1, t2) in after1.iter().zip(after2.iter()) {
                    let s = unify(t1, t2, ctx)?;
                    ss.push(s);
                }
            }

            Ok(compose_many_subs(&ss))
        }
        (Type::Tuple(tuple_types), Type::Array(array_type)) => {
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
        (Type::Array(array_type_1), Type::Array(array_type_2)) => {
            unify(array_type_1, array_type_2, ctx)
        }
        (Type::Union(types), _) => {
            let result: Result<Vec<_>, _> = types.iter().map(|t1| unify(t1, t2, ctx)).collect();
            let ss = result?; // This is only okay if all calls to is_subtype are okay
            Ok(compose_many_subs_with_context(&ss))
        }
        (_, Type::Union(types)) => {
            let mut b = false;
            let mut ss = vec![];
            for t2 in types.iter() {
                // Should we stop after the first successful call to unify()?
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
        (Type::Object(obj), Type::Intersection(types)) => {
            let obj_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t, Type::Object(_)))
                .cloned()
                .collect();
            // NOTE: {a, ...x} is converted to {a} & tvar
            let rest_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t, Type::Var(_)))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(&obj_types);

            match rest_types.len() {
                0 => unify(t1, &obj_type, ctx),
                1 => {
                    let all_obj_elems = match &obj_type {
                        Type::Object(obj) => obj.elems.to_owned(),
                        _ => vec![],
                    };

                    let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                        obj.elems.iter().cloned().partition(|e| {
                            all_obj_elems.iter().any(|oe| match (oe, e) {
                                // What to do about Call signatures?
                                (TObjElem::Call(_), TObjElem::Call(_)) => todo!(),
                                (TObjElem::Prop(op), TObjElem::Prop(p)) => op.name == p.name,
                                _ => false,
                            })
                        });

                    let s1 = unify(
                        &Type::Object(TObject {
                            elems: obj_elems,
                            type_params: vec![],
                        }),
                        &obj_type,
                        ctx,
                    )?;

                    let rest_type = rest_types.get(0).unwrap();
                    let s2 = unify(
                        &Type::Object(TObject {
                            elems: rest_elems,
                            type_params: vec![],
                        }),
                        rest_type,
                        ctx,
                    )?;

                    let s = compose_subs(&s2, &s1);
                    Ok(s)
                }
                _ => Err(String::from("Unification is undecidable")),
            }
        }
        (Type::Intersection(types), Type::Object(obj)) => {
            let obj_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t, Type::Object(_)))
                .cloned()
                .collect();
            // NOTE: {a, ...x} is converted to {a} & tvar
            let rest_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(t, Type::Var(_)))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(&obj_types);

            match rest_types.len() {
                0 => unify(&obj_type, t2, ctx),
                1 => {
                    let all_obj_elems = match &obj_type {
                        Type::Object(obj) => obj.elems.to_owned(),
                        _ => vec![],
                    };

                    let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                        obj.elems.iter().cloned().partition(|e| {
                            all_obj_elems.iter().any(|oe| match (oe, e) {
                                // What to do about Call signatures?
                                (TObjElem::Call(_), TObjElem::Call(_)) => todo!(),
                                (TObjElem::Prop(op), TObjElem::Prop(p)) => op.name == p.name,
                                _ => false,
                            })
                        });

                    let s_obj = unify(
                        &obj_type,
                        &Type::Object(TObject {
                            elems: obj_elems,
                            type_params: vec![],
                        }),
                        ctx,
                    )?;

                    let rest_type = rest_types.get(0).unwrap();
                    let s_rest = unify(
                        rest_type,
                        &Type::Object(TObject {
                            elems: rest_elems,
                            type_params: vec![],
                        }),
                        ctx,
                    )?;

                    let s = compose_subs(&s_rest, &s_obj);
                    Ok(s)
                }
                _ => Err(String::from("Unification is undecidable")),
            }
        }
        (Type::Alias(alias1), Type::Alias(alias2)) => {
            if alias1.name == alias2.name {
                match (&alias1.type_args, &alias2.type_args) {
                    (Some(tp1), Some(tp2)) => {
                        let result: Result<Vec<_>, _> = tp1
                            .iter()
                            .zip(tp2.iter())
                            .map(|(t1, t2)| unify(t1, t2, ctx))
                            .collect();
                        let ss = result?; // This is only okay if all calls to is_subtype are okay
                        Ok(compose_many_subs_with_context(&ss))
                    }
                    (None, None) => Ok(Subst::default()),
                    _ => Err(String::from("Alias type mismatch")),
                }
            } else {
                todo!("unify(): handle aliases that point to another alias")
            }
        }
        (_, Type::Alias(alias)) => {
            let alias_t = ctx.lookup_alias(alias)?;
            unify(t1, &alias_t, ctx)
        }
        (Type::Alias(alias), _) => {
            let alias_t = ctx.lookup_alias(alias)?;
            unify(&alias_t, t2, ctx)
        }
        (Type::Array(array_arg), Type::Rest(rest_arg)) => {
            unify(array_arg.as_ref(), rest_arg.as_ref(), ctx)
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
        println!("Can't unify {t1} with {t2}");
    }
    result
}

fn bind(id: &i32, t: &Type) -> Result<Subst, String> {
    // | t == TVar a     = return nullSubst
    // | occursCheck a t = throwError $ InfiniteType a t
    // | otherwise       = return $ Map.singleton a t
    match t {
        Type::Var(other_id) if other_id == id => Ok(Subst::default()),
        _ => {
            if occurs_check(id, t) {
                // Union types are a special case since `t1` unifies trivially with `t1 | t2 | ... tn`
                if let Type::Union(elem_types) = &t {
                    let elem_types_without_id: Vec<Type> = elem_types
                        .iter()
                        .filter(|elem_type| match elem_type {
                            Type::Var(elem_id) => elem_id != id,
                            _ => true,
                        })
                        .cloned()
                        .collect();

                    if elem_types_without_id.len() < elem_types.len() {
                        // TODO: dedupe with `norm_type()` in substitutable.rs
                        // TODO: restrict this special case handling to recursive functions?
                        // Removes duplicates
                        let types: HashSet<Type> = elem_types_without_id.into_iter().collect();
                        // Converts set back to an array
                        let types: Vec<Type> = types.into_iter().collect();

                        let t = if types.len() == 1 {
                            types.get(0).unwrap().to_owned()
                        } else {
                            Type::Union(types)
                        };

                        return Ok(Subst::from([(id.to_owned(), t)]));
                    }
                }

                Err(String::from("InfiniteType"))
            } else {
                Ok(Subst::from([(id.to_owned(), t.to_owned())]))
            }
        }
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

        let result = unify(&Type::from(num("5")), &Type::Prim(TPrim::Num), &ctx);
        assert_eq!(result, Ok(Subst::default()));

        let result = unify(&Type::from(str("hello")), &Type::Prim(TPrim::Str), &ctx);
        assert_eq!(result, Ok(Subst::default()));

        let result = unify(&Type::from(bool(&true)), &Type::Prim(TPrim::Bool), &ctx);
        assert_eq!(result, Ok(Subst::default()));
    }

    #[test]
    fn object_subtypes() {
        let ctx = Context::default();

        let elems = vec![
            types::TObjElem::Prop(types::TProp {
                name: String::from("foo"),
                optional: false,
                mutable: false,
                t: Type::from(num("5")),
            }),
            types::TObjElem::Prop(types::TProp {
                name: String::from("bar"),
                optional: false,
                mutable: false,
                t: Type::from(bool(&true)),
            }),
            // Having extra properties is okay
            types::TObjElem::Prop(types::TProp {
                name: String::from("baz"),
                optional: false,
                mutable: false,
                t: Type::Prim(TPrim::Str),
            }),
        ];
        let t1 = Type::Object(TObject {
            elems,
            type_params: vec![],
        });

        let elems = vec![
            types::TObjElem::Prop(types::TProp {
                name: String::from("foo"),
                optional: false,
                mutable: false,
                t: Type::Prim(TPrim::Num),
            }),
            types::TObjElem::Prop(types::TProp {
                name: String::from("bar"),
                optional: true,
                mutable: false,
                t: Type::Prim(TPrim::Bool),
            }),
            // It's okay for qux to not appear in the subtype since
            // it's an optional property.
            types::TObjElem::Prop(types::TProp {
                name: String::from("qux"),
                optional: true,
                mutable: false,
                t: Type::Prim(TPrim::Str),
            }),
        ];
        let t2 = Type::Object(TObject {
            elems,
            type_params: vec![],
        });

        let result = unify(&t1, &t2, &ctx);
        assert_eq!(result, Ok(Subst::default()));
    }

    // TODO: object subtype failure cases

    #[test]
    fn failure_case() {
        let ctx = Context::default();

        let result = unify(&Type::Prim(TPrim::Num), &Type::from(num("5")), &ctx);

        assert_eq!(result, Err(String::from("Unification failure")))
    }
}
