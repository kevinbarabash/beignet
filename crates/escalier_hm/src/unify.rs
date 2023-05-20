use defaultmap::*;
use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::BTreeSet;

use crate::ast::{Bool, Lit, Num, Str};
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
    // Why do we clone here?
    let a_t = arena.get(a).unwrap().clone();
    let b_t = arena.get(b).unwrap().clone();

    eprintln!("unify({}, {})", a_t.as_string(arena), b_t.as_string(arena));
    // eprintln!("unify({a_t:#?}, {b_t:#?})");

    match (&a_t.kind, &b_t.kind) {
        (TypeKind::Variable(_), _) => bind(arena, ctx, a, b),
        (_, TypeKind::Variable(_)) => bind(arena, ctx, b, a),

        (_, TypeKind::Constructor(unknown)) if unknown.name == "unknown" => {
            // All types are subtypes of unknown
            Ok(())
        }

        (TypeKind::Constructor(union), _) if union.name == "@@union" => {
            // All types in the union must be subtypes of t2
            for t in union.types.iter() {
                unify(arena, ctx, *t, b)?;
            }
            Ok(())
        }
        (_, TypeKind::Constructor(union)) if union.name == "@@union" => {
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
        (TypeKind::Constructor(tuple1), TypeKind::Constructor(tuple2))
            if tuple1.name == "@@tuple" && tuple2.name == "@@tuple" =>
        {
            if tuple1.types.len() < tuple2.types.len() {
                return Err(Errors::InferenceError(format!(
                    "Expected tuple of length {}, got tuple of length {}",
                    tuple2.types.len(),
                    tuple1.types.len()
                )));
            }

            for (i, (p, q)) in tuple1.types.iter().zip(tuple2.types.iter()).enumerate() {
                let q_t = arena[*q].clone();
                match q_t.kind {
                    TypeKind::Rest(_) => {
                        let rest_p = new_tuple_type(arena, &tuple1.types[i..]);
                        unify(arena, ctx, rest_p, *q)?;
                    }
                    _ => unify(arena, ctx, *p, *q)?,
                }
            }
            Ok(())
        }
        (TypeKind::Constructor(tuple), TypeKind::Constructor(array))
            if tuple.name == "@@tuple" && array.name == "Array" =>
        {
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
        (TypeKind::Constructor(array), TypeKind::Constructor(tuple))
            if array.name == "Array" && tuple.name == "@@tuple" =>
        {
            let p = array.types[0];
            for q in &tuple.types {
                let undefined = new_constructor(arena, "undefined", &[]);
                let p_or_undefined = new_union_type(arena, &[p, undefined]);

                match &arena[*q].kind {
                    TypeKind::Rest(_) => unify(arena, ctx, a, *q)?,
                    _ => unify(arena, ctx, p_or_undefined, *q)?,
                }
            }
            Ok(())
        }
        (TypeKind::Rest(rest), TypeKind::Constructor(array))
            if (array.name == "Array" || array.name == "@@tuple") =>
        {
            unify(arena, ctx, rest.arg, b)
        }
        (TypeKind::Constructor(array), TypeKind::Rest(rest))
            if (array.name == "Array" || array.name == "@@tuple") =>
        {
            unify(arena, ctx, a, rest.arg)
        }
        (TypeKind::Constructor(con_a), TypeKind::Constructor(con_b)) => {
            // TODO: support type constructors with optional and default type params

            // If the names are the same, maybe one is an alias
            if con_a.name != con_b.name || con_a.types.len() != con_b.types.len() {
                if let Some(scheme_a) = ctx.schemes.get(&con_a.name) {
                    let t1 = expand_alias(arena, &con_a.name, scheme_a, &con_a.types)?;
                    return unify(arena, ctx, t1, t2);
                }
                eprintln!("con_b.name = {}", con_b.name);
                eprintln!(
                    "ctx.schemes.get(&con_b.name) = {:?}",
                    ctx.schemes.get(&con_b.name)
                );

                if let Some(scheme_b) = ctx.schemes.get(&con_b.name) {
                    let t2 = expand_alias(arena, &con_b.name, scheme_b, &con_b.types)?;
                    return unify(arena, ctx, t1, t2);
                }

                // panic!(
                //     "type mismatch: {} != {}",
                //     a_t.as_string(arena),
                //     b_t.as_string(arena)
                // );
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
                unify(arena, ctx, *q, *p)?;
            }
            unify(arena, ctx, func_a.ret, func_b.ret)?;
            Ok(())
        }
        (TypeKind::Literal(lit1), TypeKind::Literal(lit2)) => {
            let equal = match (&lit1, &lit2) {
                (Lit::Bool(Bool { value: value1, .. }), Lit::Bool(Bool { value: value2, .. })) => {
                    value1 == value2
                }
                (Lit::Num(Num { value: value1, .. }), Lit::Num(Num { value: value2, .. })) => {
                    value1 == value2
                }
                (Lit::Str(Str { value: value1, .. }), Lit::Str(Str { value: value2, .. })) => {
                    value1 == value2
                }
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
        (TypeKind::Literal(Lit::Num(_)), TypeKind::Constructor(Constructor { name, .. }))
            if name == "number" =>
        {
            Ok(())
        }
        (TypeKind::Literal(Lit::Str(_)), TypeKind::Constructor(Constructor { name, .. }))
            if name == "string" =>
        {
            Ok(())
        }
        (TypeKind::Literal(Lit::Bool(_)), TypeKind::Constructor(Constructor { name, .. }))
            if name == "boolean" =>
        {
            Ok(())
        }
        (TypeKind::Object(object1), TypeKind::Object(object2)) => {
            // object1 must have atleast as the same properties as object2
            'outer: for prop2 in &object2.props {
                for prop1 in &object1.props {
                    match (prop1, prop2) {
                        (TObjElem::Prop(prop1), TObjElem::Prop(prop2))
                            if prop1.name == prop2.name =>
                        {
                            let p1_t = match prop1.optional {
                                true => {
                                    let undefined = new_constructor(arena, "undefined", &[]);
                                    new_union_type(arena, &[prop1.t, undefined])
                                }
                                false => prop1.t,
                            };
                            let p2_t = match prop2.optional {
                                true => {
                                    let undefined = new_constructor(arena, "undefined", &[]);
                                    new_union_type(arena, &[prop2.t, undefined])
                                }
                                false => prop2.t,
                            };
                            unify(arena, ctx, p1_t, p2_t)?;
                            continue 'outer;
                        }
                        _ => (),
                    }
                }

                // If we haven't found a matching property, then we report an
                // appropriate type error.
                match prop2 {
                    TObjElem::Method(method) => {
                        let name = match &method.name {
                            TPropKey::NumberKey(name) => name.to_string(),
                            TPropKey::StringKey(name) => name.to_string(),
                        };
                        return Err(Errors::InferenceError(format!(
                            "'{name}' is missing in {}",
                            a_t.as_string(arena),
                        )));
                    }
                    TObjElem::Index(_) => todo!(),
                    TObjElem::Prop(prop) => {
                        let name = match &prop.name {
                            TPropKey::NumberKey(name) => name.to_string(),
                            TPropKey::StringKey(name) => name.to_string(),
                        };
                        return Err(Errors::InferenceError(format!(
                            "'{name}' is missing in {}",
                            a_t.as_string(arena),
                        )));
                    }
                };
            }
            Ok(())
        }
        (TypeKind::Object(object1), TypeKind::Constructor(intersection))
            if intersection.name == "@@intersection" =>
        {
            let obj_types: Vec<_> = intersection
                .types
                .iter()
                .filter(|t| matches!(&arena[**t].kind, TypeKind::Object(_)))
                .cloned()
                .collect();
            let rest_types: Vec<_> = intersection
                .types
                .iter()
                .filter(|t| matches!(&&arena[**t].kind, TypeKind::Variable(_)))
                .cloned()
                .collect();
            // TODO: check for other variants, if there are we should error

            let obj_type = simplify_intersection(arena, &obj_types);

            match rest_types.len() {
                0 => unify(arena, ctx, t1, obj_type),
                1 => {
                    let all_obj_elems = match &arena[obj_type].kind {
                        TypeKind::Object(obj) => obj.props.to_owned(),
                        _ => vec![],
                    };

                    let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                        object1.props.iter().cloned().partition(|e| {
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
        (
            _,
            TypeKind::Constructor(Constructor {
                name,
                types: type_args,
            }),
        ) if !name.starts_with("@@") => match ctx.schemes.get(name) {
            Some(scheme) => {
                let t = expand_alias(arena, name, scheme, type_args)?;
                unify(arena, ctx, t1, t)
            }
            None => {
                eprintln!("Can't find type alias for {name}");
                Err(Errors::InferenceError(format!(
                    "Can't find type alias for {name}"
                )))
            }
        },
        (
            TypeKind::Constructor(Constructor {
                name,
                types: type_args,
            }),
            TypeKind::Object(_),
        ) if !name.starts_with("@@") => match ctx.schemes.get(name) {
            Some(scheme) => {
                let t = expand_alias(arena, name, scheme, type_args)?;
                unify(arena, ctx, t, t2)
            }
            None => Err(Errors::InferenceError(format!(
                "Can't find type alias for {name}"
            ))),
        },
        _ => {
            panic!(
                "type mismatch: unify({}, {}) failed",
                a_t.as_string(arena),
                b_t.as_string(arena)
            );
            // Err(Errors::InferenceError(format!(
            //     "type mismatch: unify({}, {}) failed",
            //     a_t.as_string(arena),
            //     b_t.as_string(arena)
            // )))
        }
    }
}

// This function unifies and infers the return type of a function call.
pub fn unify_call(
    arena: &mut Arena<Type>,
    ctx: &Context,
    arg_types: &[Index],
    type_args: Option<&[Index]>,
    t2: Index,
) -> Result<Index, Errors> {
    let ret_type = new_var_type(arena, None);
    let call_type = new_func_type(arena, arg_types, ret_type, None);

    let b = prune(arena, t2);
    let b_t = arena.get(b).unwrap().clone();

    match b_t.kind {
        TypeKind::Variable(_) => bind(arena, ctx, b, call_type)?,
        TypeKind::Constructor(Constructor { name, types }) => {
            match name.as_str() {
                "@@tuple" => {
                    return Err(Errors::InferenceError("tuple is not callable".to_string()));
                }
                "@@union" => {
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
                "@@intersection" => {
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
                _ => {
                    // TODO: lookup name in scope, and see if it has any callable signatures
                    todo!("check if {name} has any callable signatures");
                }
            }
        }
        TypeKind::Literal(lit) => {
            return Err(Errors::InferenceError(format!(
                "literal {lit} is not callable"
            )));
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

            for (p, q) in arg_types.iter().zip(func.params.iter()) {
                unify(arena, ctx, *p, *q)?;
            }
            unify(arena, ctx, ret_type, func.ret)?;
        }
    }

    // We need to prune the return type, because it might be a type variable.
    Ok(prune(arena, ret_type))
}

fn bind(arena: &mut Arena<Type>, ctx: &Context, a: Index, b: Index) -> Result<(), Errors> {
    if a != b {
        if occurs_in_type(arena, a, b) {
            return Err(Errors::InferenceError("recursive unification".to_string()));
        }
        let t = &arena[a];

        match &t.kind {
            TypeKind::Variable(avar) => {
                if let Some(constraint) = avar.constraint {
                    // QUESTION: Why isn't `b` after `constraint`?
                    unify(arena, ctx, b, constraint)?;
                }

                let t = &mut arena[a];
                t.set_instance(b);
            }
            _ => unimplemented!(),
        }
    }
    Ok(())
}

// TODO: make this recursive
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
        for elem in &obj.props {
            match elem {
                // What do we do with Call and Index signatures
                // TObjElem::Call(_) => todo!(),
                // TObjElem::Constructor(_) => todo!(),
                TObjElem::Method(_) => todo!(),
                // TObjElem::Getter(_) => todo!(),
                // TObjElem::Setter(_) => todo!(),
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
        // TObjElem::Call(_) => todo!(),
        // TObjElem::Constructor(_) => todo!(),
        TObjElem::Method(_) => todo!(),
        // TObjElem::Getter(_) => todo!(),
        // TObjElem::Setter(_) => todo!(),
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
