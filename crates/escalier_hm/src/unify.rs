use defaultmap::*;
use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};

use escalier_ast::{BindingIdent, Expr, Literal as Lit, Span};

use crate::context::*;
use crate::errors::*;
use crate::infer::{check_mutability, infer_expression};
use crate::types::*;
use crate::util::*;

/// Unify the two types t1 and t2.
///
/// Makes the types t1 and t2 the same.
///
/// Args:
///     t1: The first type to be made equivalent (subtype)
///     t2: The second type to be be equivalent (supertype)
///
/// Returns:
///     None
///
/// Raises:
///     InferenceError: Raised if the types cannot be unified.
pub fn unify(arena: &mut Arena<Type>, ctx: &Context, t1: Index, t2: Index) -> Result<(), Errors> {
    let a = prune(arena, t1);
    let b = prune(arena, t2);

    // TODO: only expand if unification fails since it's expensive
    let a = expand(arena, ctx, a)?;
    let b = expand(arena, ctx, b)?;

    let a_t = arena[a].clone();
    let b_t = arena[b].clone();

    match (&a_t.kind, &b_t.kind) {
        (TypeKind::Variable(_), _) => bind(arena, ctx, a, b),
        (_, TypeKind::Variable(_)) => bind(arena, ctx, b, a),

        (TypeKind::Keyword(kw1), TypeKind::Keyword(kw2)) => {
            if kw1 == kw2 {
                Ok(())
            } else {
                Err(Errors::InferenceError(format!(
                    "type mismatch: {} != {}",
                    a_t.as_string(arena),
                    b_t.as_string(arena)
                )))
            }
        }

        (_, TypeKind::Keyword(Keyword::Unknown)) => {
            // All types are assignable to `unknown`
            Ok(())
        }

        (TypeKind::Union(union), _) => {
            // All types in the union must be subtypes of t2
            for t in union.types.iter() {
                unify(arena, ctx, *t, b)?;
            }
            Ok(())
        }
        (_, TypeKind::Union(union)) => {
            // If t1 is a subtype of any of the types in the union, then it is a
            // subtype of the union.
            for t2 in union.types.iter() {
                if unify(arena, ctx, a, *t2).is_ok() {
                    return Ok(());
                }
            }

            Err(Errors::InferenceError(format!(
                "type mismatch: unify({}, {}) failed",
                a_t.as_string(arena),
                b_t.as_string(arena)
            )))
        }
        (TypeKind::Tuple(tuple1), TypeKind::Tuple(tuple2)) => {
            'outer: {
                if tuple1.types.len() < tuple2.types.len() {
                    // If there's a rest pattern in tuple1, then it can unify
                    // with the reamining elements of tuple2.
                    if let Some(last) = tuple1.types.last() {
                        if let TypeKind::Rest(_) = arena[*last].kind {
                            break 'outer;
                        }
                    }

                    return Err(Errors::InferenceError(format!(
                        "Expected tuple of length {}, got tuple of length {}",
                        tuple2.types.len(),
                        tuple1.types.len()
                    )));
                }
            }

            for (i, (p, q)) in tuple1.types.iter().zip(tuple2.types.iter()).enumerate() {
                // let q_t = arena[*q];
                match (&arena[*p].kind, &arena[*q].kind) {
                    (TypeKind::Rest(_), TypeKind::Rest(_)) => {
                        return Err(Errors::InferenceError(
                            "Can't unify two rest elements".to_string(),
                        ))
                    }
                    (TypeKind::Rest(_), _) => {
                        let rest_q = new_tuple_type(arena, &tuple2.types[i..]);
                        unify(arena, ctx, *p, rest_q)?;
                    }
                    (_, TypeKind::Rest(_)) => {
                        let rest_p = new_tuple_type(arena, &tuple1.types[i..]);
                        unify(arena, ctx, rest_p, *q)?;
                    }
                    (_, _) => unify(arena, ctx, *p, *q)?,
                }
            }
            Ok(())
        }
        (TypeKind::Tuple(tuple), TypeKind::Constructor(array)) if array.name == "Array" => {
            let q = array.types[0];
            for p in &tuple.types {
                match &arena[*p].kind {
                    TypeKind::Constructor(Constructor { name, types }) if name == "Array" => {
                        unify(arena, ctx, types[0], q)?;
                    }
                    TypeKind::Rest(_) => unify(arena, ctx, *p, b)?,
                    _ => unify(arena, ctx, *p, q)?,
                }
            }
            Ok(())
        }
        (TypeKind::Constructor(array), TypeKind::Tuple(tuple)) if array.name == "Array" => {
            let p = array.types[0];
            for q in &tuple.types {
                let undefined = new_keyword(arena, Keyword::Undefined);
                let p_or_undefined = new_union_type(arena, &[p, undefined]);

                match &arena[*q].kind {
                    TypeKind::Rest(_) => unify(arena, ctx, a, *q)?,
                    _ => unify(arena, ctx, p_or_undefined, *q)?,
                }
            }
            Ok(())
        }
        (TypeKind::Rest(rest), TypeKind::Constructor(array)) if (array.name == "Array") => {
            unify(arena, ctx, rest.arg, b)
        }
        (TypeKind::Rest(rest), TypeKind::Tuple(_)) => unify(arena, ctx, rest.arg, b),
        (TypeKind::Constructor(array), TypeKind::Rest(rest)) if (array.name == "Array") => {
            unify(arena, ctx, a, rest.arg)
        }
        (TypeKind::Tuple(_), TypeKind::Rest(rest)) => unify(arena, ctx, a, rest.arg),
        (TypeKind::Constructor(con_a), TypeKind::Constructor(con_b)) => {
            // TODO: support type constructors with optional and default type params
            if con_a.name != con_b.name || con_a.types.len() != con_b.types.len() {
                return Err(Errors::InferenceError(format!(
                    "type mismatch: {} != {}",
                    a_t.as_string(arena),
                    b_t.as_string(arena),
                )));
            }
            for (p, q) in con_a.types.iter().zip(con_b.types.iter()) {
                unify(arena, ctx, *p, *q)?;
            }
            Ok(())
        }
        (TypeKind::Function(func_a), TypeKind::Function(func_b)) => {
            // Is this the right place to instantiate the function types?
            let func_a = instantiate_func(arena, func_a, None)?;
            let func_b = instantiate_func(arena, func_b, None)?;

            let mut rest_a = None;
            let mut rest_b = None;

            for param in &func_a.params {
                if let TPat::Rest(rest) = &param.pattern {
                    if rest_a.is_some() {
                        return Err(Errors::InferenceError(
                            "multiple rest params in function".to_string(),
                        ));
                    }
                    rest_a = Some((rest, param.t));
                }
            }

            for param in &func_b.params {
                if let TPat::Rest(rest) = &param.pattern {
                    if rest_b.is_some() {
                        return Err(Errors::InferenceError(
                            "multiple rest params in function".to_string(),
                        ));
                    }
                    rest_b = Some((rest, param.t));
                }
            }

            let min_params_a = func_a.params.len() - rest_a.is_some() as usize;
            let min_params_b = func_b.params.len() - rest_b.is_some() as usize;

            if min_params_a > min_params_b {
                if let Some(rest_b) = rest_b {
                    for i in 0..min_params_b {
                        let p = &func_a.params[i];
                        let q = &func_b.params[i];
                        // NOTE: We reverse the order of the params here because func_a
                        // should be able to accept any params that func_b can accept,
                        // its params may be more lenient.
                        unify(arena, ctx, q.t, p.t)?;
                    }

                    let mut remaining_args_a = vec![];

                    for p in &func_a.params[min_params_b..] {
                        let arg = match &p.pattern {
                            TPat::Rest(_) => match &arena[p.t].kind {
                                TypeKind::Tuple(tuple) => {
                                    for t in &tuple.types {
                                        remaining_args_a.push(*t);
                                    }
                                    continue;
                                }
                                TypeKind::Constructor(array) if array.name == "Array" => {
                                    new_rest_type(arena, p.t)
                                }
                                TypeKind::Constructor(_) => todo!(),
                                _ => {
                                    return Err(Errors::InferenceError(format!(
                                        "rest param must be an array or tuple, got {}",
                                        arena[p.t].as_string(arena)
                                    )))
                                }
                            },
                            _ => p.t,
                        };

                        remaining_args_a.push(arg);
                    }

                    let remaining_args_a = new_tuple_type(arena, &remaining_args_a);

                    // NOTE: We reverse the order of the params here because func_a
                    // should be able to accept any params that func_b can accept,
                    // its params may be more lenient.
                    unify(arena, ctx, rest_b.1, remaining_args_a)?;

                    unify(arena, ctx, func_a.ret, func_b.ret)?;

                    return Ok(());
                }

                return Err(Errors::InferenceError(format!(
                    "{} is not a subtype of {} since it requires more params",
                    a_t.as_string(arena),
                    b_t.as_string(arena),
                )));
            }

            for i in 0..min_params_a {
                let p = &func_a.params[i];
                let q = &func_b.params[i];
                // NOTE: We reverse the order of the params here because func_a
                // should be able to accept any params that func_b can accept,
                // its params may be more lenient.
                unify(arena, ctx, q.t, p.t)?;
            }

            if let Some(rest_a) = rest_a {
                for i in min_params_a..min_params_b {
                    let q = &func_b.params[i];
                    // NOTE: We reverse the order of the params here because func_a
                    // should be able to accept any params that func_b can accept,
                    // its params may be more lenient.
                    unify(arena, ctx, q.t, rest_a.1)?;
                }

                if let Some(rest_b) = rest_b {
                    // NOTE: We reverse the order of the params here because func_a
                    // should be able to accept any params that func_b can accept,
                    // its params may be more lenient.
                    unify(arena, ctx, rest_b.1, rest_a.1)?;
                }
            }

            unify(arena, ctx, func_a.ret, func_b.ret)?;

            Ok(())
        }
        (TypeKind::Literal(lit1), TypeKind::Literal(lit2)) => {
            let equal = match (&lit1, &lit2) {
                (Lit::Boolean(value1), Lit::Boolean(value2)) => value1 == value2,
                (Lit::Number(value1), Lit::Number(value2)) => value1 == value2,
                (Lit::String(value1), Lit::String(value2)) => value1 == value2,
                _ => false,
            };
            if !equal {
                return Err(Errors::InferenceError(format!(
                    "type mismatch: {} != {}",
                    a_t.as_string(arena),
                    b_t.as_string(arena),
                )));
            }
            Ok(())
        }
        (TypeKind::Literal(Lit::Number(_)), TypeKind::Primitive(Primitive::Number)) => Ok(()),
        (TypeKind::Literal(Lit::String(_)), TypeKind::Primitive(Primitive::String)) => Ok(()),
        (TypeKind::Literal(Lit::Boolean(_)), TypeKind::Primitive(Primitive::Boolean)) => Ok(()),
        (TypeKind::Primitive(prim1), TypeKind::Primitive(prim2)) => match (prim1, prim2) {
            (Primitive::Number, Primitive::Number) => Ok(()),
            (Primitive::String, Primitive::String) => Ok(()),
            (Primitive::Boolean, Primitive::Boolean) => Ok(()),
            (Primitive::Symbol, Primitive::Symbol) => Ok(()),
            _ => Err(Errors::InferenceError(format!(
                "type mismatch: {} != {}",
                a_t.as_string(arena),
                b_t.as_string(arena),
            ))),
        },
        (TypeKind::Object(object1), TypeKind::Object(object2)) => {
            // object1 must have atleast as the same properties as object2
            // This is pretty inefficient... we should have some way of hashing
            // each object element so that we can look them.  The problem comes
            // in with functions where different signatures can expand to be the
            // same.  Do these kinds of checks is going to be really slow.
            // We could also try bucketing the different object element types
            // to reduce the size of the n^2.

            // NOTES:
            // - we don't bother unifying setters because they aren't available
            //   on immutable objects (setters need to be unified inside of
            //   unify_mut() below)
            // - we unify all of the other named elements all at once because
            //   a property could be a function and we want that to unify with
            //   a method of the same name
            // - we should also unify indexers with other named values since
            //   they can be accessed by name as well but are optional

            let mut calls_1: Vec<&TCallable> = vec![];
            let mut constructors_1: Vec<&TCallable> = vec![];
            let mut indexes_1: Vec<&TIndex> = vec![];

            let mut calls_2: Vec<&TCallable> = vec![];
            let mut constructors_2: Vec<&TCallable> = vec![];
            let mut indexes_2: Vec<&TIndex> = vec![];

            enum NamedElem<'a> {
                Getter(&'a TGetter),
                Method(&'a TMethod),
                Prop(&'a TProp),
            }

            let named_elems_1: HashMap<_, _> = object1
                .elems
                .iter()
                .filter_map(|elem| match elem {
                    TObjElem::Call(_) => None,
                    TObjElem::Constructor(_) => None,
                    TObjElem::Method(method) => {
                        Some((method.name.to_string(), NamedElem::Method(method)))
                    }
                    TObjElem::Getter(getter) => {
                        Some((getter.name.to_string(), NamedElem::Getter(getter)))
                    }
                    TObjElem::Setter(_) => None,
                    TObjElem::Index(_) => None,
                    TObjElem::Prop(prop) => Some((prop.name.to_string(), NamedElem::Prop(prop))),
                })
                .collect();

            let named_elems_2: HashMap<_, _> = object2
                .elems
                .iter()
                .filter_map(|elem| match elem {
                    TObjElem::Call(_) => None,
                    TObjElem::Constructor(_) => None,
                    TObjElem::Method(method) => {
                        Some((method.name.to_string(), NamedElem::Method(method)))
                    }
                    TObjElem::Getter(getter) => {
                        Some((getter.name.to_string(), NamedElem::Getter(getter)))
                    }
                    TObjElem::Setter(_) => None,
                    TObjElem::Index(_) => None,
                    TObjElem::Prop(prop) => Some((prop.name.to_string(), NamedElem::Prop(prop))),
                })
                .collect();

            // object1 must have at least as the same named elements as object2
            // TODO: handle the case where object1 has an indexer that covers
            // some of the named elements of object2
            for (name, elem_2) in &named_elems_2 {
                match named_elems_1.get(name) {
                    Some(elem_1) => {
                        let t1 = match elem_1 {
                            NamedElem::Getter(getter) => getter.ret,
                            NamedElem::Method(TMethod {
                                params,
                                ret,
                                type_params,
                                ..
                            }) => new_func_type(arena, &params[1..], *ret, type_params),
                            NamedElem::Prop(prop) => match prop.optional {
                                true => {
                                    let undefined = new_keyword(arena, Keyword::Undefined);
                                    new_union_type(arena, &[prop.t, undefined])
                                }
                                false => prop.t,
                            },
                        };
                        let t2 = match elem_2 {
                            NamedElem::Getter(getter) => getter.ret,
                            NamedElem::Method(TMethod {
                                params,
                                ret,
                                type_params,
                                ..
                            }) => new_func_type(arena, &params[1..], *ret, type_params),
                            NamedElem::Prop(prop) => match prop.optional {
                                true => {
                                    let undefined = new_keyword(arena, Keyword::Undefined);
                                    new_union_type(arena, &[prop.t, undefined])
                                }
                                false => prop.t,
                            },
                        };

                        unify(arena, ctx, t1, t2)?;
                    }
                    None => {
                        return Err(Errors::InferenceError(format!(
                            "'{}' is missing in {}",
                            name,
                            a_t.as_string(arena),
                        )));
                    }
                }
            }

            for prop1 in &object1.elems {
                match prop1 {
                    TObjElem::Call(call) => calls_1.push(call),
                    TObjElem::Constructor(constructor) => constructors_1.push(constructor),
                    TObjElem::Index(indexer) => indexes_1.push(indexer),
                    _ => (),
                }
            }

            for prop2 in &object2.elems {
                match prop2 {
                    TObjElem::Call(call) => calls_2.push(call),
                    TObjElem::Constructor(constructor) => constructors_2.push(constructor),
                    TObjElem::Index(indexer) => indexes_2.push(indexer),
                    _ => (),
                }
            }

            match indexes_2.len() {
                0 => (),
                1 => {
                    match indexes_1.len() {
                        0 => {
                            for (_, elem_1) in named_elems_1 {
                                let undefined = new_keyword(arena, Keyword::Undefined);

                                let t1 = match elem_1 {
                                    NamedElem::Getter(getter) => getter.ret,
                                    NamedElem::Method(TMethod {
                                        params,
                                        ret,
                                        type_params,
                                        ..
                                    }) => new_func_type(arena, &params[1..], *ret, type_params),
                                    NamedElem::Prop(prop) => match prop.optional {
                                        true => new_union_type(arena, &[prop.t, undefined]),
                                        false => prop.t,
                                    },
                                };

                                let t2 = new_union_type(arena, &[indexes_2[0].t, undefined]);

                                unify(arena, ctx, t1, t2)?;
                            }
                        }
                        1 => {
                            unify(arena, ctx, indexes_1[0].t, indexes_2[0].t)?;
                            // NOTE: the order is reverse here because object1
                            // has to have at least the same keys as object2,
                            // but it can have more.
                            unify(arena, ctx, indexes_2[0].key.t, indexes_1[0].key.t)?;
                        }
                        _ => {
                            return Err(Errors::InferenceError(format!(
                                "{} has multiple indexers",
                                a_t.as_string(arena)
                            )))
                        }
                    }
                }
                _ => {
                    return Err(Errors::InferenceError(format!(
                        "{} has multiple indexers",
                        b_t.as_string(arena)
                    )))
                }
            }

            // TODO:
            // - call (all calls in object1 must cover the calls in object2)
            // - constructor (all constructors in object1 must cover the
            //   constructors in object2)
            Ok(())
        }
        (TypeKind::Object(object1), TypeKind::Intersection(intersection)) => {
            let obj_types: Vec<_> = intersection
                .types
                .iter()
                .filter(|t| matches!(arena[**t].kind, TypeKind::Object(_)))
                .cloned()
                .collect();
            let rest_types: Vec<_> = intersection
                .types
                .iter()
                .filter(|t| matches!(arena[**t].kind, TypeKind::Variable(_)))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(arena, &obj_types);

            match rest_types.len() {
                0 => unify(arena, ctx, t1, obj_type),
                1 => {
                    let all_obj_elems = match &arena[obj_type].kind {
                        TypeKind::Object(obj) => obj.elems.to_owned(),
                        _ => vec![],
                    };

                    let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                        object1.elems.iter().cloned().partition(|e| {
                            all_obj_elems.iter().any(|oe| match (oe, e) {
                                // What to do about Call signatures?
                                // (TObjElem::Call(_), TObjElem::Call(_)) => todo!(),
                                (TObjElem::Prop(op), TObjElem::Prop(p)) => op.name == p.name,
                                _ => false,
                            })
                        });

                    let new_obj_type = new_object_type(arena, &obj_elems);
                    unify(arena, ctx, new_obj_type, obj_type)?;

                    let new_rest_type = new_object_type(arena, &rest_elems);
                    unify(arena, ctx, new_rest_type, rest_types[0])?;

                    Ok(())
                }
                _ => Err(Errors::InferenceError(
                    "Inference is undecidable".to_string(),
                )),
            }
        }
        (TypeKind::Intersection(intersection), TypeKind::Object(object2)) => {
            let obj_types: Vec<_> = intersection
                .types
                .iter()
                .filter(|t| matches!(arena[**t].kind, TypeKind::Object(_)))
                .cloned()
                .collect();
            let rest_types: Vec<_> = intersection
                .types
                .iter()
                .filter(|t| matches!(arena[**t].kind, TypeKind::Variable(_)))
                .cloned()
                .collect();

            let obj_type = simplify_intersection(arena, &obj_types);

            match rest_types.len() {
                0 => unify(arena, ctx, t1, obj_type),
                1 => {
                    let all_obj_elems = match &arena[obj_type].kind {
                        TypeKind::Object(obj) => obj.elems.to_owned(),
                        _ => vec![],
                    };

                    let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                        object2.elems.iter().cloned().partition(|e| {
                            all_obj_elems.iter().any(|oe| match (oe, e) {
                                // What to do about Call signatures?
                                // (TObjElem::Call(_), TObjElem::Call(_)) => todo!(),
                                (TObjElem::Prop(op), TObjElem::Prop(p)) => op.name == p.name,
                                _ => false,
                            })
                        });

                    let new_obj_type = new_object_type(arena, &obj_elems);
                    unify(arena, ctx, obj_type, new_obj_type)?;

                    let new_rest_type = new_object_type(arena, &rest_elems);
                    unify(arena, ctx, rest_types[0], new_rest_type)?;

                    Ok(())
                }
                _ => Err(Errors::InferenceError(
                    "Inference is undecidable".to_string(),
                )),
            }
        }
        _ => Err(Errors::InferenceError(format!(
            "type mismatch: unify({}, {}) failed",
            a_t.as_string(arena),
            b_t.as_string(arena)
        ))),
    }
}

pub fn unify_mut(
    arena: &mut Arena<Type>,
    ctx: &Context,
    t1: Index,
    t2: Index,
) -> Result<(), Errors> {
    let t1 = prune(arena, t1);
    let t2 = prune(arena, t2);

    // TODO: only expand if unification fails since it's expensive
    let t1 = expand(arena, ctx, t1)?;
    let t2 = expand(arena, ctx, t2)?;

    let t1 = arena.get(t1).unwrap();
    let t2 = arena.get(t2).unwrap();

    if t1.equals(t2, arena) {
        Ok(())
    } else {
        Err(Errors::InferenceError(format!(
            "unify_mut: {} != {}",
            t1.as_string(arena),
            t2.as_string(arena)
        )))
    }
}

// This function unifies and infers the return type of a function call.
pub fn unify_call(
    arena: &mut Arena<Type>,
    ctx: &mut Context,
    args: &mut [Expr],
    type_args: Option<&[Index]>,
    t2: Index,
) -> Result<Index, Errors> {
    let ret_type = new_var_type(arena, None);

    let b = prune(arena, t2);
    let b_t = arena.get(b).unwrap().clone();

    match b_t.kind {
        TypeKind::Variable(_) => {
            let arg_types: Vec<FuncParam> = args
                .iter_mut()
                .enumerate()
                .map(|(i, arg)| {
                    let t = infer_expression(arena, arg, ctx)?;
                    let param = FuncParam {
                        pattern: TPat::Ident(BindingIdent {
                            name: format!("arg{i}"),
                            mutable: false,
                            // loc: DUMMY_LOC,
                            span: Span { start: 0, end: 0 },
                        }),
                        // name: format!("arg{i}"),
                        t,
                        optional: false,
                    };
                    Ok(param)
                })
                .collect::<Result<Vec<_>, _>>()?;
            let call_type = new_func_type(arena, &arg_types, ret_type, &None);
            bind(arena, ctx, b, call_type)?
        }
        TypeKind::Union(Union { types }) => {
            let mut ret_types = vec![];
            for t in types.iter() {
                let ret_type = unify_call(arena, ctx, args, type_args, *t)?;
                ret_types.push(ret_type);
            }

            return Ok(new_union_type(
                arena,
                &ret_types.into_iter().unique().collect_vec(),
            ));
        }
        TypeKind::Intersection(Intersection { types }) => {
            for t in types.iter() {
                // TODO: if there are multiple overloads that unify, pick the
                // best one.
                let result = unify_call(arena, ctx, args, type_args, *t);
                match result {
                    Ok(ret_type) => return Ok(ret_type),
                    Err(_) => continue,
                }
            }
            return Err(Errors::InferenceError(
                "no valid overload for args".to_string(),
            ));
        }
        TypeKind::Tuple(_) => {
            return Err(Errors::InferenceError("tuple is not callable".to_string()))
        }
        TypeKind::Constructor(Constructor {
            name,
            types: type_args,
        }) => match ctx.schemes.get(&name) {
            Some(scheme) => {
                let mut mapping: HashMap<String, Index> = HashMap::new();
                if let Some(type_params) = &scheme.type_params {
                    for (param, arg) in type_params.iter().zip(type_args.iter()) {
                        mapping.insert(param.name.clone(), arg.to_owned());
                    }
                }

                let t = instantiate_scheme(arena, scheme.t, &mapping);
                let type_args = if type_args.is_empty() {
                    None
                } else {
                    Some(type_args.as_slice())
                };

                return unify_call(arena, ctx, args, type_args, t);
            }
            None => {
                panic!("Couldn't find scheme for {name:#?}");
            }
        },
        TypeKind::Literal(lit) => {
            return Err(Errors::InferenceError(format!(
                "literal {lit:#?} is not callable"
            )));
        }
        TypeKind::Primitive(primitive) => {
            return Err(Errors::InferenceError(format!(
                "Primitive {primitive:#?} is not callable"
            )));
        }
        TypeKind::Keyword(keyword) => {
            return Err(Errors::InferenceError(format!("{keyword} is not callable")))
        }
        TypeKind::Object(_) => {
            // TODO: check if the object has a callbale signature
            return Err(Errors::InferenceError("object is not callable".to_string()));
        }
        TypeKind::Rest(_) => {
            return Err(Errors::InferenceError("rest is not callable".to_string()));
        }
        TypeKind::Function(func) => {
            let func = if func.type_params.is_some() {
                instantiate_func(arena, &func, type_args)?
            } else {
                func
            };

            if args.len() < func.params.len() {
                return Err(Errors::InferenceError(format!(
                    "too few arguments to function: expected {}, got {}",
                    func.params.len(),
                    args.len()
                )));
            }

            let arg_types = args
                .iter_mut()
                .map(|arg| {
                    // TODO: handle spreads
                    let t = infer_expression(arena, arg, ctx)?;
                    Ok((arg, t))
                })
                .collect::<Result<Vec<_>, _>>()?;

            for ((arg, p), param) in arg_types.iter().zip(func.params.iter()) {
                match check_mutability(ctx, &param.pattern, arg)? {
                    true => unify_mut(arena, ctx, *p, param.t)?,
                    false => unify(arena, ctx, *p, param.t)?,
                };
            }

            unify(arena, ctx, ret_type, func.ret)?;
        }
        TypeKind::KeyOf(KeyOf { t }) => {
            return Err(Errors::InferenceError(format!(
                "keyof {} is not callable",
                arena[t].as_string(arena)
            )));
        }
        TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
            let t = get_prop(arena, ctx, obj, index)?;
            unify_call(arena, ctx, args, type_args, t)?;
        }
        TypeKind::Conditional(Conditional {
            check,
            extends,
            true_type,
            false_type,
        }) => {
            match unify(arena, ctx, check, extends) {
                Ok(_) => unify_call(arena, ctx, args, type_args, true_type)?,
                Err(_) => unify_call(arena, ctx, args, type_args, false_type)?,
            };
        }
        TypeKind::Infer(Infer { name }) => {
            return Err(Errors::InferenceError(format!(
                "infer {name} is not callable",
            )));
        }
    }

    // We need to prune the return type, because it might be a type variable.
    let result = prune(arena, ret_type);

    eprintln!("unify_call result = {:#?}", arena[result].as_string(arena));

    Ok(result)
}

fn bind(arena: &mut Arena<Type>, ctx: &Context, a: Index, b: Index) -> Result<(), Errors> {
    // eprint!("bind(");
    // eprint!("{:#?}", arena[a].as_string(arena));
    // if let Some(provenance) = &arena[a].provenance {
    //     eprint!(" : {:#?}", provenance);
    // }
    // eprint!(", {:#?}", arena[b].as_string(arena));
    // if let Some(provenance) = &arena[b].provenance {
    //     eprint!(" : {:#?}", provenance);
    // }
    // eprintln!(")");

    if a != b {
        if occurs_in_type(arena, a, b) {
            return Err(Errors::InferenceError("recursive unification".to_string()));
        }

        match arena.get_mut(a) {
            Some(t) => match &mut t.kind {
                TypeKind::Variable(avar) => {
                    avar.instance = Some(b);
                    if let Some(constraint) = avar.constraint {
                        unify(arena, ctx, b, constraint)?;
                    }
                }
                _ => {
                    unimplemented!("bind not implemented for {:#?}", t.kind);
                }
            },
            None => todo!(),
        }
    }
    Ok(())
}

// TODO: handle optional properties correctly
// Maybe we can have a function that will canonicalize objects by converting
// `x: T | undefined` to `x?: T`
pub fn simplify_intersection(arena: &mut Arena<Type>, in_types: &[Index]) -> Index {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|t| match &arena[*t].kind {
            TypeKind::Object(elems) => Some(elems),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, BTreeSet<Index>> = defaulthashmap!();
    for obj in obj_types {
        for elem in &obj.elems {
            match elem {
                // What do we do with Call and Index signatures
                TObjElem::Call(_) => todo!(),
                TObjElem::Constructor(_) => todo!(),
                TObjElem::Method(_) => todo!(),
                TObjElem::Getter(_) => todo!(),
                TObjElem::Setter(_) => todo!(),
                TObjElem::Index(_) => todo!(),
                TObjElem::Prop(prop) => {
                    let key = match &prop.name {
                        TPropKey::StringKey(key) => key.to_owned(),
                        TPropKey::NumberKey(key) => key.to_owned(),
                    };
                    props_map[key].insert(prop.t);
                }
            }
        }
    }

    let mut elems: Vec<TObjElem> = props_map
        .iter()
        .map(|(name, types)| {
            let types: Vec<_> = types.iter().cloned().collect();
            let t: Index = if types.len() == 1 {
                types[0]
            } else {
                new_intersection_type(arena, &types)
                // checker.from_type_kind(TypeKind::Intersection(types))
            };
            TObjElem::Prop(TProp {
                name: TPropKey::StringKey(name.to_owned()),
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
        TObjElem::Method(_) => todo!(),
        TObjElem::Getter(_) => todo!(),
        TObjElem::Setter(_) => todo!(),
        TObjElem::Index(_) => todo!(),
        TObjElem::Prop(prop) => prop.name.clone(),
    }); // ensure a stable order

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|t| !matches!(&arena[**t].kind, TypeKind::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    if !elems.is_empty() {
        out_types.push(new_object_type(arena, &elems));
    }
    // TODO: figure out a consistent way to sort types
    // out_types.sort_by_key(|t| t.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0]
    } else {
        new_intersection_type(arena, &out_types)
    }
}

fn expand(arena: &mut Arena<Type>, ctx: &Context, a: Index) -> Result<Index, Errors> {
    let a_t = arena[a].clone();

    match &a_t.kind {
        TypeKind::Constructor(Constructor { name, .. }) if name == "Array" => Ok(a),
        TypeKind::Constructor(Constructor { name, .. }) if name == "Promise" => Ok(a),
        _ => expand_type(arena, ctx, a),
    }
}
