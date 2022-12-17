use error_stack::{Report, Result};
use std::cmp;
use std::collections::BTreeSet;

use crochet_ast::types::{self as types, TGeneric, TLam, TObjElem, TObject, TVar, Type, TypeKind};
use crochet_ast::values::ExprKind;
use types::TKeyword;

use crate::context::Context;
use crate::expand_type::expand_type;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::unify_mut::unify_mut;
use crate::util::*;

// Returns Ok(substitions) if t2 admits all values from t1 and an Err() otherwise.
pub fn unify(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, TypeError> {
    // All binding must be done first
    match (&t1.kind, &t2.kind) {
        (TypeKind::Var(tv), _) => return bind(tv, t2, Relation::SubType, ctx),
        (_, TypeKind::Var(tv)) => return bind(tv, t1, Relation::SuperType, ctx),
        _ => (),
    };

    if t1.mutable && t2.mutable {
        return unify_mut(t1, t2, ctx);
    }
    if t2.mutable {
        // TODO: extract this into a `isLitExpr` helper that checks if the expression
        // is a literal, e.g. 1 + 2, but not something that calls a function or contains
        // variables.
        // NOTE: this is different from what a hypothetical `isConstExpr` would return
        // which is similar, but call also contain:
        // - binders that have been assigned const expressions
        // - pure function calls with args that are const expressions
        match &t1.provenance {
            Some(provenance) => match provenance.as_ref() {
                types::Provenance::Expr(e) => match e.kind {
                    ExprKind::Obj(_) => return Ok(Subst::new()),
                    ExprKind::Tuple(_) => return Ok(Subst::new()),
                    _ => (),
                },
                types::Provenance::Pattern(_) => (),
                types::Provenance::Type(_) => (),
                types::Provenance::TObjElem(_) => (),
                types::Provenance::TypeAnn(_) => (),
            },
            None => (),
        };

        // It's NOT okay to use an immutable in place of a mutable one
        return Err(Report::new(TypeError::UnexpectedImutableValue)
            .attach_printable("Cannot use immutable type where a mutable type was expected"));
    }
    // It's okay to use a mutable type in place of an immutable one so it's fine
    // to continue with the non-mutable unify() call if t1 is mutable as long as
    // t2 is not.

    let result = match (&t1.kind, &t2.kind) {
        (TypeKind::Lit(lit), TypeKind::Keyword(keyword)) => {
            let b = matches!(
                (lit, keyword),
                (types::TLit::Num(_), TKeyword::Number)
                    | (types::TLit::Str(_), TKeyword::String)
                    | (types::TLit::Bool(_), TKeyword::Boolean)
            );
            if b {
                Ok(Subst::default())
            } else {
                Err(Report::new(TypeError::UnificationError(
                    Box::from(t1.to_owned()),
                    Box::from(t2.to_owned()),
                ))
                .attach_printable("Unification failure"))
            }
        }
        (TypeKind::App(app1), TypeKind::App(app2)) => {
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
                Err(Report::new(TypeError::UnificationError(
                    Box::from(t1.to_owned()),
                    Box::from(t2.to_owned()),
                )))
            }
        }
        (TypeKind::Lam(lam1), TypeKind::Lam(lam2)) => {
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
                Err(Report::new(TypeError::UnificationError(
                    Box::from(t1.to_owned()),
                    Box::from(t2.to_owned()),
                ))
                .attach_printable("Couldn't unify lambdas"))
            }
        }
        // NOTE: this arm is only hit by the `infer_skk` test case
        (TypeKind::Lam(_), TypeKind::App(_)) => unify(t2, t1, ctx),
        (TypeKind::App(_), TypeKind::Object(obj)) => {
            let callables: Vec<_> = obj
                .elems
                .iter()
                .filter_map(|elem| match elem {
                    TObjElem::Call(call) => {
                        let lam = Type::from(TypeKind::Lam(TLam {
                            params: call.params.to_owned(),
                            ret: call.ret.to_owned(),
                        }));
                        let t = if call.type_params.is_empty() {
                            lam
                        } else {
                            Type::from(TypeKind::Generic(TGeneric {
                                t: Box::from(lam),
                                type_params: call.type_params.to_owned(),
                            }))
                        };
                        Some(t)
                    }
                    TObjElem::Constructor(_) => None,
                    TObjElem::Index(_) => None,
                    TObjElem::Prop(_) => None,
                })
                .collect();

            if callables.is_empty() {
                Err(Report::new(TypeError::ObjectIsNotCallable(Box::from(
                    t1.to_owned(),
                ))))
            } else {
                for callable in callables {
                    let result = unify(t1, &callable, ctx);
                    if result.is_ok() {
                        return result;
                    }
                }
                // TODO: include a report of which callable signatures were tried
                Err(Report::new(TypeError::NoValidCallable))
            }
        }
        (TypeKind::App(app), TypeKind::Lam(lam)) => {
            let mut s = Subst::new();

            let maybe_rest_param = if let Some(param) = lam.params.last() {
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

            // NOTE: placeholder spreads must come last because we don't know they're
            // length.  This will also be true for spreading arrays, but in the case
            // of array spreads, they also need to come after the start of a rest param.

            let mut args: Vec<Type> = vec![];
            // TODO: disallow spreading an array if it isn't the last arg
            for arg in app.args.iter() {
                match &arg.kind {
                    TypeKind::Rest(spread) => match &spread.as_ref().kind {
                        TypeKind::Tuple(types) => args.append(&mut types.to_owned()),
                        _ => return Err(Report::new(TypeError::InvalidSpread(spread.to_owned()))),
                    },
                    _ => args.push(arg.to_owned()),
                }
            }

            // TODO: Add a `variadic` boolean to the Lambda type as a convenience
            // so that we don't have to search through all the params for the rest
            // param.

            let (args, params) = match maybe_rest_param {
                Some(rest_param) => match &rest_param.kind {
                    TypeKind::Array(_) => {
                        let max_regular_arg_count = lam.params.len() - 1;

                        if args.len() < max_regular_arg_count - optional_count {
                            return Err(Report::new(TypeError::TooFewArguments(
                                Box::from(t1.to_owned()),
                                Box::from(t2.to_owned()),
                            ))
                            .attach_printable("Not enough args provided"));
                        }

                        let regular_arg_count = cmp::min(max_regular_arg_count, args.len());
                        let (args, rest_args) = app.args.split_at(regular_arg_count);
                        let mut args = args.to_vec();
                        // If there are any optional params for which there isn't
                        // an arg, we provide `undefined` as an arg.  This is done
                        // so that the number of args and params will line up with
                        // the last arg being a tuple type and the last param being
                        // an array type.
                        for _ in 0..max_regular_arg_count - regular_arg_count {
                            args.push(Type::from(TypeKind::Keyword(TKeyword::Undefined)));
                        }
                        args.push(Type::from(TypeKind::Tuple(rest_args.to_owned())));

                        let mut rest_param = rest_param.clone();
                        // NOTE: We intentionally change the mutability of the rest
                        // param to `false` so that the tuple (which is readonly)
                        // that's created from the arguments can be unified with
                        // the rest param array.
                        rest_param.mutable = false;

                        let mut params: Vec<_> = lam.params.iter().map(|p| p.get_type()).collect();
                        params.pop(); // Remove rest type
                        params.push(rest_param); // Add array type

                        // Given the function: let foo = (x: number, y?: number, ...z: number[]);
                        // `params` should be `[number, number | undefined, number[]]`
                        //
                        // Given the call foo(5);
                        // `args` should be `[5, undefined, []]`
                        //
                        // Given the call foo(5, 10);
                        // `args` should be `[5, 10, []]`
                        //
                        // Given the call foo(5, 10, 15, 20);
                        // `args` should be `[5, 10, [15, 20]]`
                        //
                        // All of these calls are valid and `args` and `params`
                        // should piece-wise unify successfully.

                        // NOTE: Any extra args are ignored.
                        (args, params)
                    }
                    TypeKind::Tuple(tuple) => {
                        let mut params: Vec<_> = lam.params.iter().map(|p| p.get_type()).collect();
                        params.pop(); // Remove rest type
                        params.extend(tuple.to_owned()); // Add each type from the tuple type

                        if args.len() < params.len() {
                            return Err(Report::new(TypeError::TooFewArguments(
                                Box::from(t1.to_owned()),
                                Box::from(t2.to_owned()),
                            ))
                            .attach_printable("Not enough args provided"));
                        }

                        // NOTE: Any extra args are ignored.
                        (args, params)
                    }
                    _ => panic!("{rest_param} cannot be used as a rest param"),
                },
                None => {
                    let params = lam.params.iter().map(|p| p.get_type()).collect();

                    if args.len() < lam.params.len() - optional_count {
                        return Err(Report::new(TypeError::TooFewArguments(
                            Box::from(t1.to_owned()),
                            Box::from(t2.to_owned()),
                        ))
                        .attach_printable("Not enough args provided"));
                    }

                    // NOTE: Any extra args are ignored.
                    (args, params)
                }
            };

            // Unify args with params
            let mut reports: Vec<Report<TypeError>> = vec![];
            for (p1, p2) in args.iter().zip(params) {
                let arg = p1.apply(&s);
                let param = p2.apply(&s);

                // Each argument must be a subtype of the corresponding param.
                match unify(&arg, &param, ctx) {
                    Ok(s1) => s = compose_subs(&s, &s1),
                    Err(report) => reports.push(report),
                }
            }

            if let Some(report) = merge_reports(reports) {
                return Err(report);
            }

            // Unify return types
            // Once #352 has been addressed we'll be able to also report
            // unification errors with return types.
            let s_ret = unify(&app.ret.apply(&s), &lam.ret.apply(&s), ctx)?;
            Ok(compose_subs(&s, &s_ret))
        }
        (TypeKind::App(_), TypeKind::Intersection(types)) => {
            for t in types {
                let result = unify(t1, t, ctx);
                if result.is_ok() {
                    return result;
                }
            }
            Err(Report::new(TypeError::NoValidOverload))
        }
        (TypeKind::Object(obj1), TypeKind::Object(obj2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            let result: Result<Vec<_>, TypeError> = obj2
                .elems
                .iter()
                .map(|e2| {
                    let mut has_matching_key = false;
                    let mut has_matching_value = false;
                    let mut ss = vec![];
                    for e1 in obj1.elems.iter() {
                        match (e1, e2) {
                            (TObjElem::Call(_), TObjElem::Call(_)) => {
                                // What to do about Call signatures?
                                // Treat them similarly to callbacks when dealing
                                // args in function calls / application.
                                todo!()
                            }
                            (TObjElem::Prop(prop1), TObjElem::Prop(prop2)) => {
                                if prop1.name == prop2.name {
                                    has_matching_key = true;

                                    let t1 = get_property_type(prop1);
                                    let t2 = get_property_type(prop2);

                                    if let Ok(s) = unify(&t1, &t2, ctx) {
                                        has_matching_value = true;
                                        ss.push(s);
                                    }
                                }
                            }
                            // skip pairs that aren't the same
                            _ => (),
                        }
                    }

                    if has_matching_value {
                        return Ok(compose_many_subs(&ss));
                    }

                    // If there's no matching key...
                    if !has_matching_key {
                        // ...but the element was optional...
                        if let TObjElem::Prop(prop) = e2 {
                            if prop.optional {
                                // ...that's also Ok.
                                return Ok(Subst::default());
                            }
                        }
                    }

                    // TODO: track which properties t1 is missing from t2 so that
                    // we can report a more specific error.
                    Err(Report::new(TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    ))
                    .attach_printable("Unification failure"))
                })
                .collect();

            let ss = result?;
            Ok(compose_many_subs(&ss))
        }
        (TypeKind::Tuple(types1), TypeKind::Tuple(types2)) => {
            let mut before2: Vec<Type> = vec![];
            let mut after2: Vec<Type> = vec![];
            let mut maybe_rest2: Option<Type> = None;

            for t in types2 {
                match &t.kind {
                    TypeKind::Rest(rest_type) => {
                        if maybe_rest2.is_some() {
                            return Err(Report::new(TypeError::MoreThanOneRestPattern));
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
                return Err(Report::new(TypeError::NotEnoughElementsToUnpack)
                    .attach_printable("not enough elements to unpack"));
            }

            let mut types1 = types1.to_owned();
            let rest_len = types1.len() - min_len;

            let before1: Vec<_> = types1.drain(0..before2.len()).collect();

            let mut ss: Vec<Subst> = vec![];

            let mut reports: Vec<_> = vec![];
            for (t1, t2) in before1.iter().zip(before2.iter()) {
                match unify(t1, t2, ctx) {
                    Ok(s) => ss.push(s),
                    Err(report) => reports.push(report),
                }
            }

            if let Some(rest2) = maybe_rest2 {
                let rest1: Vec<_> = types1.drain(0..rest_len).collect();
                let after1: Vec<_> = types1;

                let s = unify(&Type::from(TypeKind::Tuple(rest1)), &rest2, ctx)?;
                ss.push(s);

                for (t1, t2) in after1.iter().zip(after2.iter()) {
                    match unify(t1, t2, ctx) {
                        Ok(s) => ss.push(s),
                        Err(report) => reports.push(report),
                    }
                }
            }

            match merge_reports(reports) {
                Some(report) => Err(report),
                None => Ok(compose_many_subs(&ss)),
            }
        }
        (TypeKind::Tuple(tuple_types), TypeKind::Array(array_type)) => {
            if tuple_types.is_empty() {
                Ok(Subst::default())
            } else {
                // let mut ss = vec![];
                // TODO: take the union of all of the types in tuple_types
                // Right now if array_type is a type variable, we unify right
                // away and then the other types in tuple_types are ignored
                let s = unify(&union_many_types(tuple_types), array_type.as_ref(), ctx)?;
                // for t1 in tuple_types.iter() {
                //     println!("unifying {t1} with {array_type}");
                //     let s = unify(t1, array_type.as_ref(), ctx)?;
                //     ss.push(s)
                // }
                // Ok(compose_many_subs(&ss))
                Ok(s)
            }
        }
        (TypeKind::Array(array_type_1), TypeKind::Array(array_type_2)) => {
            unify(array_type_1, array_type_2, ctx)
        }
        (TypeKind::Union(types), _) => {
            let result: Result<Vec<_>, _> = types.iter().map(|t1| unify(t1, t2, ctx)).collect();
            let ss = result?; // This is only okay if all calls to is_subtype are okay
            Ok(compose_many_subs_with_context(&ss))
        }
        (_, TypeKind::Union(types)) => {
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
                false => Err(Report::new(TypeError::UnificationError(
                    Box::from(t1.to_owned()),
                    Box::from(t2.to_owned()),
                ))
                .attach_printable("Unification failure")),
            }
        }
        (TypeKind::Object(obj), TypeKind::Intersection(types)) => {
            let obj_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(&t.kind, TypeKind::Object(_)))
                .cloned()
                .collect();
            // NOTE: {a, ...x} is converted to {a} & tvar
            let rest_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(&t.kind, TypeKind::Var(_)))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(&obj_types);

            match rest_types.len() {
                0 => unify(t1, &obj_type, ctx),
                1 => {
                    let all_obj_elems = match &obj_type.kind {
                        TypeKind::Object(obj) => obj.elems.to_owned(),
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
                        &Type::from(TypeKind::Object(TObject { elems: obj_elems })),
                        &obj_type,
                        ctx,
                    )?;

                    let rest_type = rest_types.get(0).unwrap();
                    let s2 = unify(
                        &Type::from(TypeKind::Object(TObject { elems: rest_elems })),
                        rest_type,
                        ctx,
                    )?;

                    let s = compose_subs(&s2, &s1);
                    Ok(s)
                }
                _ => Err(Report::new(TypeError::UnificationIsUndecidable)
                    .attach_printable("Unification is undecidable")),
            }
        }
        (TypeKind::Intersection(types), TypeKind::Object(obj)) => {
            let obj_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(&t.kind, TypeKind::Object(_)))
                .cloned()
                .collect();
            // NOTE: {a, ...x} is converted to {a} & tvar
            let rest_types: Vec<_> = types
                .iter()
                .filter(|t| matches!(&t.kind, TypeKind::Var(_)))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(&obj_types);

            match rest_types.len() {
                0 => unify(&obj_type, t2, ctx),
                1 => {
                    let all_obj_elems = match &obj_type.kind {
                        TypeKind::Object(obj) => obj.elems.to_owned(),
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
                        &Type::from(TypeKind::Object(TObject { elems: obj_elems })),
                        ctx,
                    )?;

                    let rest_type = rest_types.get(0).unwrap();
                    let s_rest = unify(
                        rest_type,
                        &Type::from(TypeKind::Object(TObject { elems: rest_elems })),
                        ctx,
                    )?;

                    let s = compose_subs(&s_rest, &s_obj);
                    Ok(s)
                }
                _ => Err(Report::new(TypeError::UnificationIsUndecidable)
                    .attach_printable("Unification is undecidable")),
            }
        }
        (TypeKind::Ref(alias1), TypeKind::Ref(alias2)) => {
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
                    _ => Err(Report::new(TypeError::AliasTypeMismatch)),
                }
            } else {
                todo!("unify(): handle aliases that point to another alias")
            }
        }
        (_, TypeKind::Ref(_)) => unify(t1, &expand_type(t2, ctx)?, ctx),
        (TypeKind::Ref(_), _) => unify(&expand_type(t1, ctx)?, t2, ctx),
        (_, TypeKind::MappedType(_)) => unify(t1, &expand_type(t2, ctx)?, ctx),
        (TypeKind::MappedType(_), _) => unify(&expand_type(t1, ctx)?, t2, ctx),
        (_, TypeKind::IndexAccess(_)) => unify(t1, &expand_type(t2, ctx)?, ctx),
        (TypeKind::IndexAccess(_), _) => unify(&expand_type(t1, ctx)?, t2, ctx),

        // We instantiate any generic types that haven't already been instantiated
        // yet.  This handles cases like `[1, 2, 3].map((x) => x * x)` where the
        // `map` method is generic.
        // TODO: Consider instantiating properties when we look them up.
        (TypeKind::Generic(_), TypeKind::Generic(_)) => {
            unify(&ctx.instantiate(t1), &ctx.instantiate(t2), ctx)
        }
        (_, TypeKind::Generic(_)) => unify(t1, &ctx.instantiate(t2), ctx),
        (TypeKind::Generic(_), _) => unify(&ctx.instantiate(t1), t2, ctx),

        (TypeKind::Array(_), TypeKind::Rest(rest_arg)) => unify(t1, rest_arg.as_ref(), ctx),
        (TypeKind::Tuple(_), TypeKind::Rest(rest_arg)) => unify(t1, rest_arg.as_ref(), ctx),
        (_, TypeKind::KeyOf(_)) => unify(t1, &expand_type(t2, ctx)?, ctx),
        (TypeKind::Keyword(keyword1), TypeKind::Keyword(keyword2)) => match (keyword1, keyword2) {
            (TKeyword::Number, TKeyword::Number) => Ok(Subst::new()),
            (TKeyword::String, TKeyword::String) => Ok(Subst::new()),
            (TKeyword::Boolean, TKeyword::Boolean) => Ok(Subst::new()),
            (TKeyword::Null, TKeyword::Null) => Ok(Subst::new()),
            (TKeyword::Symbol, TKeyword::Symbol) => Ok(Subst::new()),
            (TKeyword::Undefined, TKeyword::Undefined) => Ok(Subst::new()),
            // Is 'never' a subtype of all types?
            (TKeyword::Never, TKeyword::Null) => Ok(Subst::new()),
            _ => Err(Report::new(TypeError::UnificationError(
                Box::from(t1.to_owned()),
                Box::from(t2.to_owned()),
            ))
            .attach_printable(format!("Can't unify {t1} with {t2}"))),
        },
        (v1, v2) => {
            if v1 == v2 {
                Ok(Subst::new())
            } else {
                Err(Report::new(TypeError::UnificationError(
                    Box::from(t1.to_owned()),
                    Box::from(t2.to_owned()),
                ))
                .attach_printable("Unification failure"))
            }
        }
    };
    if result.is_err() {
        println!("Can't unify {t1} with {t2}");
    }
    result
}

#[derive(PartialEq)]
enum Relation {
    SubType,
    SuperType,
}

fn bind(tv: &TVar, t: &Type, rel: Relation, ctx: &Context) -> Result<Subst, TypeError> {
    // | t == TVar a     = return nullSubst
    // | occursCheck a t = throwError $ InfiniteType a t
    // | otherwise       = return $ Map.singleton a t
    match &t.kind {
        TypeKind::Var(other_tv) if other_tv == tv => {
            println!("other_tv = {other_tv:#?}, tv = {tv:#?}");
            Ok(Subst::default())
        }
        _ => {
            if occurs_check(tv, t) {
                // Union types are a special case since `t1` unifies trivially with `t1 | t2 | ... tn`
                if let TypeKind::Union(elem_types) = &t.kind {
                    let elem_types_without_id: Vec<Type> = elem_types
                        .iter()
                        .filter(|elem_type| match &elem_type.kind {
                            TypeKind::Var(other_tv) => other_tv != tv,
                            _ => true,
                        })
                        .cloned()
                        .collect();

                    if elem_types_without_id.len() < elem_types.len() {
                        // TODO: dedupe with `norm_type()` in substitutable.rs
                        // TODO: restrict this special case handling to recursive functions?
                        // Removes duplicates
                        let types: BTreeSet<Type> = elem_types_without_id.into_iter().collect();
                        // Converts set back to an array
                        let types: Vec<Type> = types.into_iter().collect();

                        let t: Type = if types.len() == 1 {
                            types.get(0).unwrap().to_owned()
                        } else {
                            Type::from(TypeKind::Union(types))
                        };

                        return Ok(Subst::from([(tv.id.to_owned(), t)]));
                    }
                }

                Err(Report::new(TypeError::InfiniteType).attach_printable("InfiniteType"))
            } else {
                if let Some(c) = &tv.constraint {
                    // We only care whether the `unify()` call fails or not.  If it succeeds,
                    // that indicates that type `t` is a subtype of constraint `c`.
                    match rel {
                        Relation::SubType => unify(c, t, ctx)?,
                        Relation::SuperType => unify(t, c, ctx)?,
                    };

                    // If the `t` is a type variable, but has no constraints then return a
                    // substitution from the one that has no constraints to the one that does.
                    if let TypeKind::Var(TVar {
                        id,
                        constraint: None,
                    }) = t.kind
                    {
                        let s: Subst = Subst::from([(
                            id.to_owned(),
                            Type::from(TypeKind::Var(tv.to_owned())),
                        )]);
                        return Ok(s);
                    }

                    // TODO: handle the case where both type variables have constraints
                }

                Ok(Subst::from([(tv.id.to_owned(), t.to_owned())]))
            }
        }
    }
}

fn occurs_check(tv: &TVar, t: &Type) -> bool {
    t.ftv().contains(tv)
}

fn merge_reports(reports: Vec<Report<TypeError>>) -> Option<Report<TypeError>> {
    reports.into_iter().fold(None, |accum, report| match accum {
        Some(mut accum) => {
            accum.extend_one(report);
            Some(accum)
        }
        None => Some(report),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crochet_ast::types::TPropKey;
    use crochet_ast::values::Lit;

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
    fn literals_are_subtypes_of_corresponding_keywords() -> Result<(), TypeError> {
        let ctx = Context::default();

        let result = unify(
            &Type::from(num("5")),
            &Type::from(TypeKind::Keyword(TKeyword::Number)),
            &ctx,
        )?;
        assert_eq!(result, Subst::default());

        let result = unify(
            &Type::from(str("hello")),
            &Type::from(TypeKind::Keyword(TKeyword::String)),
            &ctx,
        )?;
        assert_eq!(result, Subst::default());

        let result = unify(
            &Type::from(bool(&true)),
            &Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            &ctx,
        )?;
        assert_eq!(result, Subst::default());

        Ok(())
    }

    #[test]
    fn object_subtypes() -> Result<(), TypeError> {
        let ctx = Context::default();

        let elems = vec![
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("foo")),
                optional: false,
                mutable: false,
                t: Type::from(num("5")),
            }),
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("bar")),
                optional: false,
                mutable: false,
                t: Type::from(bool(&true)),
            }),
            // Having extra properties is okay
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("baz")),
                optional: false,
                mutable: false,
                t: Type::from(TypeKind::Keyword(TKeyword::String)),
            }),
        ];
        let t1 = Type::from(TypeKind::Object(TObject { elems }));

        let elems = vec![
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("foo")),
                optional: false,
                mutable: false,
                t: Type::from(TypeKind::Keyword(TKeyword::Number)),
            }),
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("bar")),
                optional: true,
                mutable: false,
                t: Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            }),
            // It's okay for qux to not appear in the subtype since
            // it's an optional property.
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("qux")),
                optional: true,
                mutable: false,
                t: Type::from(TypeKind::Keyword(TKeyword::String)),
            }),
        ];
        let t2 = Type::from(TypeKind::Object(TObject { elems }));

        let result = unify(&t1, &t2, &ctx)?;
        assert_eq!(result, Subst::default());

        Ok(())
    }

    // TODO: object subtype failure cases

    // TODO: Update this test case to work with error-stack's Report type
    // #[test]
    // fn failure_case() {
    //     let ctx = Context::default();

    //     let result = unify(
    //         &Type::from(TypeKind::Keyword(TKeyword::Number)),
    //         &Type::from(num("5")),
    //         &ctx,
    //     );

    //     assert_eq!(result, Err(TypeError::from("Unification failure")))
    // }
}
