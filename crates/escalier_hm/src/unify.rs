use defaultmap::*;
use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};

use escalier_ast::{BindingIdent, Expr, ExprKind, Literal as Lit, Span};

use crate::context::*;
use crate::errors::*;
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

    let a = expand(arena, ctx, a)?;
    let b = expand(arena, ctx, b)?;

    let a_t = arena[a].clone();
    let b_t = arena[b].clone();

    match (&a_t.kind, &b_t.kind) {
        (TypeKind::Binding(binding1), TypeKind::Binding(binding2)) => {
            match (&binding1.is_mut, &binding1.is_mut) {
                (true, true) => unify_mut(arena, ctx, binding1.t, binding2.t),
                // It's okay to use a mutable reference as if it were immutable
                (true, false) => unify(arena, ctx, binding1.t, binding2.t),
                // It's not okay to use an immutable reference as if it were mutable
                (false, true) => Err(Errors::InferenceError(format!(
                    "type mismatch: unify({}, {}) failed",
                    a_t.as_string(arena),
                    b_t.as_string(arena)
                ))),
                (false, false) => unify(arena, ctx, binding1.t, binding2.t),
            }
        }

        (TypeKind::Variable(_), _) => bind(arena, ctx, a, b),
        (_, TypeKind::Variable(_)) => bind(arena, ctx, b, a),

        // binding types can be unified with non-binding types in the usual way
        // by unwrapped the binding type
        (TypeKind::Binding(binding), _) => unify(arena, ctx, binding.t, b),
        (_, TypeKind::Binding(binding)) => unify(arena, ctx, a, binding.t),

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
            if func_a.params.len() > func_b.params.len() {
                return Err(Errors::InferenceError(format!(
                    "{} is not a subtype of {} since it requires more params",
                    a_t.as_string(arena),
                    b_t.as_string(arena),
                )));
            }

            for (p, q) in func_a.params.iter().zip(func_b.params.iter()) {
                // NOTE: We reverse the order of the params here because func_a
                // should be able to accept any params that func_b can accept,
                // its params may be more lenient.  Thus q is a subtype of p.
                unify(arena, ctx, q.t, p.t)?;
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
        (TypeKind::Literal(Lit::Number(_)), TypeKind::Keyword(Keyword::Number)) => Ok(()),
        (TypeKind::Literal(Lit::String(_)), TypeKind::Keyword(Keyword::String)) => Ok(()),
        (TypeKind::Literal(Lit::Boolean(_)), TypeKind::Keyword(Keyword::Boolean)) => Ok(()),
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

fn unify_mut(arena: &mut Arena<Type>, ctx: &Context, t1: Index, t2: Index) -> Result<(), Errors> {
    let t1 = prune(arena, t1);
    let t2 = prune(arena, t2);

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
    ctx: &Context,
    arg_types: &[(&Expr, Index)],
    type_args: Option<&[Index]>,
    t2: Index,
) -> Result<Index, Errors> {
    let ret_type = new_var_type(arena, None);

    let b = prune(arena, t2);
    let b_t = arena.get(b).unwrap().clone();

    match b_t.kind {
        TypeKind::Variable(_) => {
            let arg_types: Vec<FuncParam> = arg_types
                .iter()
                .enumerate()
                .map(|(i, (_, t))| FuncParam {
                    pattern: TPat::Ident(BindingIdent {
                        name: format!("arg{i}"),
                        mutable: false,
                        // loc: DUMMY_LOC,
                        span: Span { start: 0, end: 0 },
                    }),
                    // name: format!("arg{i}"),
                    t: *t,
                    optional: false,
                })
                .collect();
            let call_type = new_func_type(arena, &arg_types, ret_type, &None);
            bind(arena, ctx, b, call_type)?
        }
        TypeKind::Union(Union { types }) => {
            let mut ret_types = vec![];
            for t in types.iter() {
                let ret_type = unify_call(arena, ctx, arg_types, type_args, *t)?;
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
                let result = unify_call(arena, ctx, arg_types, type_args, *t);
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
        TypeKind::Constructor(Constructor { name, types: _ }) => {
            // TODO: lookup name in scope, and see if it has any callable signatures
            todo!("check if {name} has any callable signatures");
        }
        TypeKind::Literal(lit) => {
            return Err(Errors::InferenceError(format!(
                "literal {lit:#?} is not callable"
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

            if arg_types.len() < func.params.len() {
                return Err(Errors::InferenceError(format!(
                    "too few arguments to function: expected {}, got {}",
                    func.params.len(),
                    arg_types.len()
                )));
            }

            for ((expr, p), q) in arg_types.iter().zip(func.params.iter()) {
                unify(arena, ctx, *p, q.t)?;
            }
            unify(arena, ctx, ret_type, func.ret)?;
        }
        TypeKind::Binding(TypeBinding { t, is_mut: _ }) => {
            unify_call(arena, ctx, arg_types, type_args, t)?;
        }
        TypeKind::KeyOf(KeyOf { t }) => {
            return Err(Errors::InferenceError(format!(
                "keyof {} is not callable",
                arena[t].as_string(arena)
            )));
        }
        TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
            let t = get_prop(arena, ctx, obj, index)?;
            unify_call(arena, ctx, arg_types, type_args, t)?;
        }
        TypeKind::Conditional(Conditional {
            check,
            extends,
            true_type,
            false_type,
        }) => {
            match unify(arena, ctx, check, extends) {
                Ok(_) => unify_call(arena, ctx, arg_types, type_args, true_type)?,
                Err(_) => unify_call(arena, ctx, arg_types, type_args, false_type)?,
            };
        }
    }

    // We need to prune the return type, because it might be a type variable.
    Ok(prune(arena, ret_type))
}

fn bind(arena: &mut Arena<Type>, ctx: &Context, a: Index, b: Index) -> Result<(), Errors> {
    eprintln!(
        "bind({:#?}, {:#?})",
        arena[a].as_string(arena),
        arena[b].as_string(arena)
    );
    if a != b {
        if occurs_in_type(arena, a, b) {
            return Err(Errors::InferenceError("recursive unification".to_string()));
        }
        let t = &arena[a];

        match &t.kind {
            TypeKind::Variable(avar) => {
                if let Some(constraint) = avar.constraint {
                    unify(arena, ctx, b, constraint)?;
                }

                let t = &mut arena[a];
                t.set_instance(b);
            }
            _ => {
                unimplemented!("bind not implemented for {:#?}", t.kind);
            }
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

    let a = match &a_t.kind {
        TypeKind::Constructor(Constructor {
            name,
            types: type_args,
        }) if !["Promise", "Array"].contains(&name.as_str()) => match ctx.schemes.get(name) {
            Some(scheme) => expand_alias(arena, name, scheme, type_args),
            None => Err(Errors::InferenceError(format!("Unbound type name: {name}"))),
        },
        _ => Ok(a),
    }?;

    expand_type(arena, ctx, a)
}
