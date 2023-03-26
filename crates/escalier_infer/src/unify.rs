use std::cmp;
use std::collections::BTreeSet;

use escalier_ast::types::{
    self as types, Provenance, TFnParam, TLam, TObjElem, TObject, TVar, Type, TypeKind,
};
use escalier_ast::values::{ExprKind, TypeAnn, TypeAnnKind};
use types::TKeyword;

use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::{util::*, Diagnostic};

use crate::checker::Checker;

fn get_param_type(param: &TFnParam, checker: &'_ mut Checker) -> Type {
    match param.optional {
        true => {
            // TODO: map this back to the `?`
            let undefined = checker.from_type_kind(TypeKind::Keyword(TKeyword::Undefined));
            checker.from_type_kind(TypeKind::Union(vec![param.t.to_owned(), undefined]))
        }
        false => param.t.to_owned(),
    }
}

fn get_param_types(params: &[TFnParam], checker: &'_ mut Checker) -> Vec<Type> {
    params.iter().map(|p| get_param_type(p, checker)).collect()
}

impl Checker {
    // Returns Ok(substitions) if t2 admits all values from t1 and an Err() otherwise.
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Subst, Vec<TypeError>> {
        // All binding must be done first
        match (&t1.kind, &t2.kind) {
            // If both are type variables...
            (TypeKind::Var(tv1), TypeKind::Var(tv2)) => {
                // ...and one of them was inferred from a type reference, e.g. T,
                if let Some(provenance) = &t1.provenance {
                    if let Provenance::TypeAnn(type_ann) = provenance.as_ref() {
                        if let TypeAnn {
                            kind: TypeAnnKind::TypeRef(_),
                            ..
                        } = type_ann.as_ref()
                        {
                            // then we want to make sure that substitution we create
                            // has the other type variable pointing to the one that
                            // was inferred from the type reference.
                            return self.bind(tv2, t1, Relation::SuperType);
                        }
                    }
                }

                if let Some(provenance) = &t2.provenance {
                    if let Provenance::TypeAnn(type_ann) = provenance.as_ref() {
                        if let TypeAnn {
                            kind: TypeAnnKind::TypeRef(_),
                            ..
                        } = type_ann.as_ref()
                        {
                            return self.bind(tv1, t2, Relation::SubType);
                        }
                    }
                }

                return self.bind(tv1, t2, Relation::SubType);
            }
            (TypeKind::Var(tv), _) => {
                return self.bind(tv, t2, Relation::SubType);
            }
            (_, TypeKind::Var(tv)) => {
                return self.bind(tv, t1, Relation::SuperType);
            }
            _ => (),
        };

        if t1.mutable && t2.mutable {
            return self.unify_mut(t1, t2);
        }
        if t2.mutable {
            // TODO: extract this into a `isLitExpr` helper that checks if the expression
            // is a literal, e.g. 1 + 2, but not something that calls a function or contains
            // variables.
            // NOTE: this is different from what a hypothetical `isConstExpr` would return
            // which is similar, but can also contain:
            // - binders that have been assigned const expressions
            // - pure function calls with args that are const expressions
            match &t1.provenance {
                Some(provenance) => match provenance.as_ref() {
                    types::Provenance::Expr(e) => match e.kind {
                        ExprKind::Obj(_) => return Ok(Subst::new()),
                        ExprKind::Tuple(_) => return Ok(Subst::new()),
                        ExprKind::New(_) => return Ok(Subst::new()),
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
            return Err(vec![TypeError::UnexpectedImutableValue]);
        }

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
                    Err(vec![TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    )])
                }
            }
            (TypeKind::App(app1), TypeKind::App(app2)) => {
                let mut s = Subst::new();

                // NOTE: `app1` and `app2` currently must have the same number of args.
                // TODO: Once we have support for optional function params, update
                // this to support having different lengths of params.
                if app1.args.len() == app2.args.len() {
                    for (p1, p2) in app1.args.iter().zip(app2.args.iter()) {
                        let p1 = p1.apply(&s, self);
                        let p2 = p2.apply(&s, self);
                        let s1 = self.unify(&p1, &p2)?;
                        s = compose_subs(&s, &s1, self);
                    }
                    let app1_ret = app1.ret.apply(&s, self);
                    let app2_ret = app2.ret.apply(&s, self);
                    let s1 = self.unify(&app1_ret, &app2_ret)?;
                    Ok(compose_subs(&s, &s1, self))
                } else {
                    Err(vec![TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    )])
                }
            }
            (TypeKind::Lam(lam1), TypeKind::Lam(lam2)) => {
                // TODO: call instantiate_gen_lam on `lam1` and `lam2`

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
                        let pt2 = get_param_type(p2, self).apply(&s, self);
                        let pt1 = get_param_type(p1, self).apply(&s, self);
                        let s1 = self.unify(&pt2, &pt1)?;
                        s = compose_subs(&s, &s1, self);
                    }
                    let lam1_ret = lam1.ret.apply(&s, self);
                    let lam2_ret = lam2.ret.apply(&s, self);
                    let s1 = self.unify(&lam1_ret, &lam2_ret)?;
                    Ok(compose_subs(&s, &s1, self))
                } else {
                    Err(vec![TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    )])
                }
            }
            // NOTE: this arm is only hit by the `infer_skk` test case
            (TypeKind::Lam(_), TypeKind::App(_)) => self.unify(t2, t1),
            (TypeKind::App(app), TypeKind::Lam(lam)) => {
                let lam = self.instantiate_gen_lam(lam, app.type_args.as_ref());
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

                let optional_count =
                    lam.params
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
                            _ => return Err(vec![TypeError::InvalidSpread(spread.to_owned())]),
                        },
                        _ => args.push(arg.to_owned()),
                    }
                }

                // TODO: Add a `variadic` boolean to the Lambda type as a convenience
                // so that we don't have to search through all the params for the rest
                // param.

                let (mut args, mut params) = match maybe_rest_param {
                    Some(rest_param) => match &rest_param.kind {
                        TypeKind::Array(_) => {
                            let max_regular_arg_count = lam.params.len() - 1;

                            if args.len() < max_regular_arg_count - optional_count {
                                return Err(vec![TypeError::TooFewArguments(
                                    Box::from(t1.to_owned()),
                                    Box::from(t2.to_owned()),
                                )]);
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
                                args.push(
                                    self.from_type_kind(TypeKind::Keyword(TKeyword::Undefined)),
                                );
                            }
                            args.push(self.from_type_kind(TypeKind::Tuple(rest_args.to_owned())));

                            let mut rest_param = rest_param.clone();
                            // NOTE: We intentionally change the mutability of the rest
                            // param to `false` so that the tuple (which is readonly)
                            // that's created from the arguments can be unified with
                            // the rest param array.
                            rest_param.mutable = false;

                            let mut params = get_param_types(&lam.params, self);
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
                            let mut params = get_param_types(&lam.params, self);
                            params.pop(); // Remove rest type
                            params.extend(tuple.to_owned()); // Add each type from the tuple type

                            if args.len() < params.len() {
                                // TODO: include how many args were expected, right now
                                // we count a rest tuple as a single arg.
                                // TODO: figure out how to merge errors from a Result::Err()
                                // with the error reports in self.
                                return Err(vec![TypeError::TooFewArguments(
                                    Box::from(t1.to_owned()),
                                    Box::from(t2.to_owned()),
                                )]);
                            }

                            // NOTE: Any extra args are ignored.
                            (args, params)
                        }
                        _ => panic!("{rest_param} cannot be used as a rest param"),
                    },
                    None => {
                        let params = get_param_types(&lam.params, self);

                        if args.len() < lam.params.len() - optional_count {
                            return Err(vec![TypeError::TooFewArguments(
                                Box::from(t1.to_owned()),
                                Box::from(t2.to_owned()),
                            )]);
                        }

                        // NOTE: Any extra args are ignored.
                        (args, params)
                    }
                };

                // Unify args with params
                let mut reasons: Vec<TypeError> = vec![];
                for (p1, p2) in args.iter_mut().zip(params.iter_mut()) {
                    // Each argument must be a subtype of the corresponding param.
                    // TODO: collect unification errors as diagnostics
                    let p1 = p1.apply(&s, self);
                    let p2 = p2.apply(&s, self);
                    match self.unify(&p1, &p2) {
                        Ok(s1) => s = compose_subs(&s, &s1, self),
                        Err(mut report) => reasons.append(&mut report),
                    }
                }

                if !reasons.is_empty() {
                    self.current_report.push(Diagnostic {
                        code: 1,
                        message: "args don't match expected params".to_string(),
                        reasons,
                    });
                }

                // Unify return types
                // Once #352 has been addressed we'll be able to also report
                // unification errors with return types.
                let app_ret = app.ret.apply(&s, self);
                let lam_ret = lam.ret.apply(&s, self);
                let s_ret = self.unify(&app_ret, &lam_ret)?;
                Ok(compose_subs(&s, &s_ret, self))
            }
            (TypeKind::App(_), TypeKind::Object(obj)) => {
                let mut callables: Vec<_> = obj
                    .elems
                    .iter()
                    .filter_map(|elem| match elem {
                        TObjElem::Call(call) => {
                            eprintln!("handling callable");
                            let lam = self.from_type_kind(TypeKind::Lam(TLam {
                                params: call.params.to_owned(),
                                ret: call.ret.to_owned(),
                                type_params: None, // TODO
                            }));
                            let t = if call.type_params.is_some() {
                                lam
                            } else {
                                self.instantiate_callable(call)
                            };
                            eprintln!("callable instantiated as {t}");
                            Some(t)
                        }
                        TObjElem::Constructor(_) => None,
                        TObjElem::Index(_) => None,
                        TObjElem::Prop(_) => None,
                        TObjElem::Method(_) => todo!(),
                        TObjElem::Getter(_) => todo!(),
                        TObjElem::Setter(_) => todo!(),
                    })
                    .collect();

                if callables.is_empty() {
                    Err(vec![TypeError::ObjectIsNotCallable(Box::from(
                        t1.to_owned(),
                    ))])
                } else {
                    self.push_report();
                    for callable in callables.iter_mut() {
                        let result = self.unify(t1, callable);
                        if self.current_report.is_empty() {
                            self.pop_report();
                            return result;
                        }
                        self.current_report = vec![];
                    }
                    self.pop_report();
                    // TODO: include a report of which callable signatures were tried
                    Err(vec![TypeError::NoValidCallable])
                }
            }
            (TypeKind::App(_), TypeKind::Intersection(types)) => {
                self.push_report();
                for t in types {
                    let result = self.unify(t1, t);
                    if self.current_report.is_empty() {
                        self.pop_report();
                        return result;
                    }
                    self.current_report = vec![];
                }
                self.pop_report();
                // TODO: include reports for all call mismatches if we get here
                Err(vec![TypeError::NoValidOverload])
            }
            (TypeKind::Object(obj1), TypeKind::Object(obj2)) => {
                // It's okay if t1 has extra properties, but it has to have all of t2's properties.
                let result: Result<Vec<_>, Vec<TypeError>> = obj2
                    .elems
                    .iter()
                    .map(|e2| {
                        let mut has_matching_key = false;
                        let mut has_matching_value = false;
                        let mut ss = vec![];
                        for e1 in obj1.elems.iter() {
                            match (e1, e2.clone()) {
                                (TObjElem::Call(_), TObjElem::Call(_)) => {
                                    // What to do about Call signatures?
                                    // Treat them similarly to callbacks when dealing
                                    // args in function calls / application.
                                    todo!()
                                }
                                (TObjElem::Prop(prop1), TObjElem::Prop(prop2)) => {
                                    if prop1.name == prop2.name {
                                        has_matching_key = true;

                                        let t1 = get_property_type(prop1, self);
                                        let t2 = get_property_type(&prop2, self);

                                        if let Ok(s) = self.unify(&t1, &t2) {
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
                            return Ok(compose_many_subs(&ss, self));
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
                        Err(vec![TypeError::UnificationError(
                            Box::from(t1.to_owned()),
                            Box::from(t2.to_owned()),
                        )])
                    })
                    .collect();

                let ss = result?;
                Ok(compose_many_subs(&ss, self))
            }
            (TypeKind::Tuple(types1), TypeKind::Tuple(types2)) => {
                let mut before2: Vec<Type> = vec![];
                let mut after2: Vec<Type> = vec![];
                let mut maybe_rest2: Option<Type> = None;

                for t in types2 {
                    match &t.kind {
                        TypeKind::Rest(rest_type) => {
                            if maybe_rest2.is_some() {
                                return Err(vec![TypeError::MoreThanOneRestPattern]);
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
                    return Err(vec![TypeError::NotEnoughElementsToUnpack]);
                }

                let mut types1 = types1.to_owned();
                let rest_len = types1.len() - min_len;

                let mut before1: Vec<_> = types1.drain(0..before2.len()).collect();

                let mut ss: Vec<Subst> = vec![];

                let mut reports: Vec<TypeError> = vec![];
                for (t1, t2) in before1.iter_mut().zip(before2.iter_mut()) {
                    match self.unify(t1, t2) {
                        Ok(s) => ss.push(s),
                        Err(mut report) => reports.append(&mut report),
                    }
                }

                if let Some(rest2) = maybe_rest2 {
                    let rest1: Vec<_> = types1.drain(0..rest_len).collect();
                    let mut after1: Vec<_> = types1;

                    let new_rest_type = self.from_type_kind(TypeKind::Tuple(rest1));
                    let s = self.unify(&new_rest_type, &rest2)?;
                    ss.push(s);

                    for (t1, t2) in after1.iter_mut().zip(after2.iter_mut()) {
                        match self.unify(t1, t2) {
                            Ok(s) => ss.push(s),
                            Err(mut report) => reports.append(&mut report),
                        }
                    }
                }

                if reports.is_empty() {
                    Ok(compose_many_subs(&ss, self))
                } else {
                    Err(reports)
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
                    let union_t = union_many_types(tuple_types, self);
                    let s = self.unify(&union_t, array_type)?;
                    Ok(s)
                }
            }
            (TypeKind::Array(array_type_1), TypeKind::Array(array_type_2)) => {
                self.unify(array_type_1, array_type_2)
            }
            (TypeKind::Union(types), _) => {
                let result: Result<Vec<_>, _> = types.iter().map(|t1| self.unify(t1, t2)).collect();
                let ss = result?; // This is only okay if all calls to is_subtype are okay
                Ok(compose_many_subs_with_context(&ss, self))
            }
            (_, TypeKind::Union(types)) => {
                let mut b = false;
                let mut ss = vec![];
                for t2 in types.iter() {
                    // Should we stop after the first successful call to self.unify()?
                    if let Ok(s) = self.unify(t1, t2) {
                        b = true;
                        ss.push(s);
                    }
                }

                match b {
                    true => Ok(compose_many_subs(&ss, self)),
                    false => Err(vec![TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    )]),
                }
            }
            (TypeKind::Object(obj), TypeKind::Intersection(types)) => {
                let obj_types: Vec<_> = types
                    .iter()
                    .filter(|t| matches!(&t.kind, TypeKind::Object(_)))
                    .cloned()
                    .collect();
                // NOTE: {a, ...x} is converted to {a} & tvar
                let mut rest_types: Vec<_> = types
                    .iter()
                    .filter(|t| matches!(&t.kind, TypeKind::Var(_)))
                    .cloned()
                    .collect();
                // TODO: check for other variants, if there are we should error

                let obj_type = &mut simplify_intersection(&obj_types, self);

                match rest_types.len() {
                    0 => self.unify(t1, obj_type),
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

                        let new_obj_type = self.from_type_kind(TypeKind::Object(TObject {
                            elems: obj_elems,
                            is_interface: false,
                        }));
                        let s1 = self.unify(&new_obj_type, obj_type)?;

                        let rest_type = rest_types.get_mut(0).unwrap();
                        let new_rest_type = self.from_type_kind(TypeKind::Object(TObject {
                            elems: rest_elems,
                            is_interface: false,
                        }));
                        let s2 = self.unify(&new_rest_type, rest_type)?;

                        let s = compose_subs(&s2, &s1, self);
                        Ok(s)
                    }
                    _ => Err(vec![TypeError::UnificationIsUndecidable]),
                }
            }
            (TypeKind::Intersection(types), TypeKind::Object(obj)) => {
                let obj_types: Vec<_> = types
                    .iter()
                    .filter(|t| matches!(&t.kind, TypeKind::Object(_)))
                    .cloned()
                    .collect();
                // NOTE: {a, ...x} is converted to {a} & tvar
                let mut rest_types: Vec<_> = types
                    .iter()
                    .filter(|t| matches!(&t.kind, TypeKind::Var(_)))
                    .cloned()
                    .collect();
                // TODO: check for other variants, if there are we should error

                let obj_type = &mut simplify_intersection(&obj_types, self);

                match rest_types.len() {
                    0 => self.unify(obj_type, t2),
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

                        let new_obj_type = self.from_type_kind(TypeKind::Object(TObject {
                            elems: obj_elems,
                            is_interface: false,
                        }));
                        let s_obj = self.unify(obj_type, &new_obj_type)?;

                        let rest_type = rest_types.get_mut(0).unwrap();
                        let new_rest_type = self.from_type_kind(TypeKind::Object(TObject {
                            elems: rest_elems,
                            is_interface: false,
                        }));
                        let s_rest = self.unify(rest_type, &new_rest_type)?;

                        let s = compose_subs(&s_rest, &s_obj, self);
                        Ok(s)
                    }
                    _ => Err(vec![TypeError::UnificationIsUndecidable]),
                }
            }
            // NOTE: `expand_alias` doesn't expand interfaces so if we want to
            // destructure and object created from a class (which is modeled as an
            // interface) we need call `expand_alias_type` directly.
            (TypeKind::Object(_), TypeKind::Ref(alias)) => {
                let alias_t = self.expand_alias_type(alias)?;
                self.unify(t1, &alias_t)
            }
            (TypeKind::Ref(alias), TypeKind::Object(_)) => {
                let alias_t = self.expand_alias_type(alias)?;
                self.unify(&alias_t, t2)
            }
            (TypeKind::Ref(alias1), TypeKind::Ref(alias2)) => {
                if alias1.name == alias2.name {
                    eprintln!("unifying aliases {alias1} with {alias2}");
                    match (&alias1.type_args, &alias2.type_args) {
                        (Some(tp1), Some(tp2)) => {
                            let result: Result<Vec<_>, _> = tp1
                                .iter()
                                .zip(tp2.iter())
                                .map(|(t1, t2)| self.unify(t1, t2))
                                .collect();
                            let ss = result?; // This is only okay if all calls to is_subtype are okay
                            Ok(compose_many_subs_with_context(&ss, self))
                        }
                        (None, None) => Ok(Subst::default()),
                        _ => Err(vec![TypeError::AliasTypeMismatch]),
                    }
                } else {
                    todo!("self.unify(): handle aliases that point to another alias")
                }
            }
            // (TypeKind::Regex(_), TypeKind::Ref(alias)) if alias.name == "RegExp" => {
            //     Ok(Subst::default())
            // }
            // (TypeKind::Ref(alias), TypeKind::Regex(_)) if alias.name == "RegExp" => {
            //     Ok(Subst::default())
            // }
            (_, TypeKind::Ref(alias)) => {
                let alias_t = self.expand_alias_type(alias)?;
                self.unify(t1, &alias_t)
            }
            (TypeKind::Ref(alias), _) => {
                let alias_t = self.expand_alias_type(alias)?;
                self.unify(&alias_t, t2)
            }
            (_, TypeKind::MappedType(_)) => {
                let t2 = self.expand_type(t2)?;
                self.unify(t1, &t2)
            }
            (TypeKind::MappedType(_), _) => {
                let t1 = self.expand_type(t1)?;
                self.unify(&t1, t2)
            }
            (_, TypeKind::IndexAccess(_)) => {
                let t2 = self.expand_type(t2)?;
                self.unify(t1, &t2)
            }
            (TypeKind::IndexAccess(_), _) => {
                let t1 = self.expand_type(t1)?;
                self.unify(&t1, t2)
            }
            (_, TypeKind::InferType(_)) => {
                let t2 = self.expand_type(t2)?;
                self.unify(t1, &t2)
            }
            (TypeKind::InferType(_), _) => {
                let t1 = self.expand_type(t1)?;
                self.unify(&t1, t2)
            }
            (TypeKind::Array(_), TypeKind::Rest(rest_arg)) => self.unify(t1, rest_arg.as_ref()),
            (TypeKind::Tuple(_), TypeKind::Rest(rest_arg)) => self.unify(t1, rest_arg.as_ref()),
            (_, TypeKind::KeyOf(_)) => {
                let t2 = self.expand_type(t2)?;
                self.unify(t1, &t2)
            }
            (TypeKind::Keyword(keyword1), TypeKind::Keyword(keyword2)) => {
                match (keyword1, keyword2) {
                    (TKeyword::Number, TKeyword::Number) => Ok(Subst::new()),
                    (TKeyword::String, TKeyword::String) => Ok(Subst::new()),
                    (TKeyword::Boolean, TKeyword::Boolean) => Ok(Subst::new()),
                    (TKeyword::Null, TKeyword::Null) => Ok(Subst::new()),
                    (TKeyword::Symbol, TKeyword::Symbol) => Ok(Subst::new()),
                    (TKeyword::Undefined, TKeyword::Undefined) => Ok(Subst::new()),
                    // Is 'never' a subtype of all types?
                    (TKeyword::Never, TKeyword::Null) => Ok(Subst::new()),
                    _ => Err(vec![TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    )]),
                }
            }
            (v1, v2) => {
                if v1 == v2 {
                    Ok(Subst::new())
                } else {
                    // TODO: add this error to the current report
                    Err(vec![TypeError::UnificationError(
                        Box::from(t1.to_owned()),
                        Box::from(t2.to_owned()),
                    )])
                }
            }
        };
        if result.is_err() {
            eprintln!("Can't unify {t1} with {t2}");
        }
        result
    }

    fn unify_mut(&self, t1: &Type, t2: &Type) -> Result<Subst, Vec<TypeError>> {
        if t1 == t2 {
            eprintln!("unify_mut: {t1} == {t2}");
            Ok(Subst::new())
        } else {
            Err(vec![TypeError::UnificationError(
                Box::from(t1.to_owned()),
                Box::from(t2.to_owned()),
            )])
        }
    }

    fn bind(&mut self, tv: &TVar, t: &Type, rel: Relation) -> Result<Subst, Vec<TypeError>> {
        // | t == TVar a     = return nullSubst
        // | occursCheck a t = throwError $ InfiniteType a t
        // | otherwise       = return $ Map.singleton a t
        match &t.kind {
            TypeKind::Var(other_tv) if other_tv == tv => {
                eprintln!("other_tv = {other_tv:#?}, tv = {tv:#?}");
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
                                self.from_type_kind(TypeKind::Union(types))
                            };

                            return Ok(Subst::from(vec![(tv.id.to_owned(), t)]));
                        }
                    }

                    Err(vec![TypeError::InfiniteType])
                } else {
                    if let Some(constraint) = &tv.constraint {
                        let constraint = constraint.as_ref();
                        // We only care whether the `self.unify()` call fails or not.  If it succeeds,
                        // that indicates that type `t` is a subtype of constraint `c`.
                        match rel {
                            Relation::SubType => self.unify(constraint, t)?,
                            Relation::SuperType => self.unify(t, constraint)?,
                        };

                        // If the `t` is a type variable, but has no constraints then return a
                        // substitution from the one that has no constraints to the one that does.
                        if let TypeKind::Var(TVar {
                            id,
                            constraint: None,
                        }) = t.kind
                        {
                            let s: Subst = Subst::from(vec![(
                                id.to_owned(),
                                self.from_type_kind(TypeKind::Var(tv.to_owned())),
                            )]);
                            return Ok(s);
                        }

                        // TODO: handle the case where both type variables have constraints
                    }

                    Ok(Subst::from(vec![(tv.id.to_owned(), t.to_owned())]))
                }
            }
        }
    }
}

#[derive(PartialEq, Debug)]
enum Relation {
    SubType,
    SuperType,
}

fn occurs_check(tv: &TVar, t: &Type) -> bool {
    t.ftv().contains(tv)
}

#[cfg(test)]
mod tests {
    use super::*;
    use escalier_ast::types::TPropKey;
    use escalier_ast::values::{Lit, DUMMY_LOC};

    fn num(val: &str) -> Lit {
        Lit::num(val.to_owned(), 0..0, DUMMY_LOC)
    }

    fn str(val: &str) -> Lit {
        Lit::str(val.to_owned(), 0..0, DUMMY_LOC)
    }

    fn bool(val: &bool) -> Lit {
        Lit::bool(val.to_owned(), 0..0, DUMMY_LOC)
    }

    #[test]
    fn literals_are_subtypes_of_corresponding_keywords() -> Result<(), Vec<TypeError>> {
        let mut checker = Checker::default();
        let t1 = checker.from_lit(num("5"));
        let t2 = checker.from_type_kind(TypeKind::Keyword(TKeyword::Number));
        let result = checker.unify(&t1, &t2)?;
        assert_eq!(result, Subst::default());

        let t1 = checker.from_lit(str("hello"));
        let t2 = checker.from_type_kind(TypeKind::Keyword(TKeyword::String));
        let result = checker.unify(&t1, &t2)?;
        assert_eq!(result, Subst::default());

        let t1 = checker.from_lit(bool(&true));
        let t2 = checker.from_type_kind(TypeKind::Keyword(TKeyword::Boolean));
        let result = checker.unify(&t1, &t2)?;
        assert_eq!(result, Subst::default());

        Ok(())
    }

    #[test]
    fn object_subtypes() -> Result<(), Vec<TypeError>> {
        let mut checker = Checker::default();

        let elems = vec![
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("foo")),
                optional: false,
                mutable: false,
                t: checker.from_lit(num("5")),
            }),
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("bar")),
                optional: false,
                mutable: false,
                t: checker.from_lit(bool(&true)),
            }),
            // Having extra properties is okay
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("baz")),
                optional: false,
                mutable: false,
                t: checker.from_type_kind(TypeKind::Keyword(TKeyword::String)),
            }),
        ];
        let t1 = checker.from_type_kind(TypeKind::Object(TObject {
            elems,
            is_interface: false,
        }));

        let elems = vec![
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("foo")),
                optional: false,
                mutable: false,
                t: checker.from_type_kind(TypeKind::Keyword(TKeyword::Number)),
            }),
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("bar")),
                optional: true,
                mutable: false,
                t: checker.from_type_kind(TypeKind::Keyword(TKeyword::Boolean)),
            }),
            // It's okay for qux to not appear in the subtype since
            // it's an optional property.
            types::TObjElem::Prop(types::TProp {
                name: TPropKey::StringKey(String::from("qux")),
                optional: true,
                mutable: false,
                t: checker.from_type_kind(TypeKind::Keyword(TKeyword::String)),
            }),
        ];
        let t2 = checker.from_type_kind(TypeKind::Object(TObject {
            elems,
            is_interface: false,
        }));

        let result = checker.unify(&t1, &t2)?;
        assert_eq!(result, Subst::default());

        Ok(())
    }

    // TODO: object subtype failure cases

    // TODO: Update this test case to work with error-stack's Report type
    // #[test]
    // fn failure_case() {
    //     let ctx = Context::default();

    //     let result = self.unify(
    //         &self.from_type_kind(TypeKind::Keyword(TKeyword::Number)),
    //         &self.from_type_kind(num("5")),
    //         &ctx,
    //     );

    //     assert_eq!(result, Err(TypeError::from("Unification failure")))
    // }
}
