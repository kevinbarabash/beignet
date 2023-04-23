use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::HashMap;

use crate::ast::Lit;
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
pub fn unify(arena: &mut Arena<Type>, t1: Index, t2: Index) -> Result<(), Errors> {
    let a = prune(arena, t1);
    let b = prune(arena, t2);
    // Why do we clone here?
    let a_t = arena.get(a).unwrap().clone();
    let b_t = arena.get(b).unwrap().clone();
    match (&a_t.kind, &b_t.kind) {
        (TypeKind::Variable(_), _) => bind(arena, a, b),
        (_, TypeKind::Variable(_)) => bind(arena, b, a),

        (TypeKind::Constructor(union), _) if union.name == "@@union" => {
            // All types in the union must be subtypes of t2
            for t in union.types.iter() {
                unify(arena, *t, b)?;
            }
            Ok(())
        }
        (_, TypeKind::Constructor(union)) if union.name == "@@union" => {
            // If t1 is a subtype of any of the types in the union, then it is a
            // subtype of the union.
            for t2 in union.types.iter() {
                if unify(arena, a, *t2).is_ok() {
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

            for (p, q) in tuple1.types.iter().zip(tuple2.types.iter()) {
                unify(arena, *p, *q)?;
            }
            Ok(())
        }
        (TypeKind::Constructor(tuple), TypeKind::Constructor(array))
            if tuple.name == "@@tuple" && array.name == "Array" =>
        {
            let q = array.types[0];
            for p in &tuple.types {
                unify(arena, *p, q)?;
            }
            Ok(())
        }

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
                unify(arena, *p, *q)?;
            }
            Ok(())
        }
        (TypeKind::Function(func_a), TypeKind::Function(func_b)) => {
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
                unify(arena, *q, *p)?;
            }
            unify(arena, func_a.ret, func_b.ret)?;
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
            for (name, t) in &object2.props {
                if let Some(prop) = object1.props.iter().find(|props| &props.0 == name) {
                    unify(arena, prop.1, *t)?;
                } else {
                    return Err(Errors::InferenceError(format!(
                        "'{name}' is missing in {}",
                        a_t.as_string(arena),
                    )));
                }
            }
            Ok(())
        }
        _ => Err(Errors::InferenceError(format!(
            "type mismatch: unify({}, {}) failed",
            a_t.as_string(arena),
            b_t.as_string(arena)
        ))),
    }
}

// This function unifies and infers the return type of a function call.
pub fn unify_call(
    arena: &mut Arena<Type>,
    arg_types: &[Index],
    t2: Index,
) -> Result<Index, Errors> {
    let ret_type = new_var_type(arena);
    let call_type = new_func_type(arena, arg_types, ret_type, None);

    let b = prune(arena, t2);
    let b_t = arena.get(b).unwrap().clone();

    match b_t.kind {
        TypeKind::Variable(_) => bind(arena, b, call_type)?,
        TypeKind::Constructor(Constructor { name, types }) => {
            match name.as_str() {
                "@@tuple" => {
                    return Err(Errors::InferenceError("tuple is not callable".to_string()));
                }
                "@@union" => {
                    let mut ret_types = vec![];
                    for t in types.iter() {
                        let ret_type = unify_call(arena, arg_types, *t)?;
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
                        let result = unify_call(arena, arg_types, *t);
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
                    // lookup definition of type constructor, and see if its instances
                    // have any callable signatures
                    todo!("constructor");
                }
            }
        }
        TypeKind::Ref(Ref { name }) => {
            // TODO: lookup name in scope, and see if it has any callable signatures
            todo!("check if {name} has any callable signatures");
        }
        TypeKind::Literal(lit) => {
            return Err(Errors::InferenceError(format!(
                "literal {lit} is not callable"
            )));
        }
        TypeKind::Object(_) => {
            return Err(Errors::InferenceError("object is not callable".to_string()));
        }
        TypeKind::Function(func) => {
            let func = if func.type_params.is_some() {
                instantiate_func(arena, &func)
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
                unify(arena, *p, *q)?;
            }
            unify(arena, ret_type, func.ret)?;
        }
    }

    // We need to prune the return type, because it might be a type variable.
    Ok(prune(arena, ret_type))
}

fn bind(arena: &mut Arena<Type>, a: Index, b: Index) -> Result<(), Errors> {
    if a != b {
        if occurs_in_type(arena, a, b) {
            // raise InferenceError("recursive unification")
            return Err(Errors::InferenceError("recursive unification".to_string()));
        }
        arena.get_mut(a).unwrap().set_instance(b);
    }
    Ok(())
}

fn instantiate_func(arena: &mut Arena<Type>, func: &Function) -> Function {
    // A mapping of TypeVariables to TypeVariables
    let mut mappings: HashMap<String, Index> = HashMap::default();

    if let Some(type_params) = &func.type_params {
        for tp in type_params {
            mappings.insert(tp.name.to_owned(), new_var_type(arena));
        }
    }

    // TODO: dedupe with freshrec and generalize_rec
    fn instrec(arena: &mut Arena<Type>, tp: Index, mappings: &HashMap<String, Index>) -> Index {
        let p = prune(arena, tp);
        match &arena.get(p).unwrap().clone().kind {
            TypeKind::Variable(_) => {
                p
                // TODO: Try to figure out a test case where we'd need to do this as well.
                // if is_generic(arena, p, ctx) {
                //     mappings
                //         .entry(p)
                //         .or_insert_with(|| new_var_type(arena))
                //         .to_owned()
                // } else {
                //     p
                // }
            }
            TypeKind::Ref(Ref { name }) => match mappings.get(name) {
                Some(tp) => *tp,
                None => new_type_ref(arena, name),
            },
            TypeKind::Literal(lit) => new_lit_type(arena, lit),
            TypeKind::Object(object) => {
                let props: Vec<_> = object
                    .props
                    .iter()
                    .map(|(name, tp)| (name.clone(), instrec(arena, *tp, mappings)))
                    .collect();
                if props != object.props {
                    new_object_type(arena, &props)
                } else {
                    p
                }
            }
            TypeKind::Function(func) => {
                let params = instrec_many(arena, &func.params, mappings);
                let ret = instrec(arena, func.ret, mappings);
                let type_params = func.type_params.clone();
                // TODO: copy the type params
                if params != func.params || ret != func.ret {
                    new_func_type(arena, &params, ret, type_params)
                } else {
                    p
                }
            }
            TypeKind::Constructor(con) => {
                let types = instrec_many(arena, &con.types, mappings);
                if types != con.types {
                    new_constructor(arena, &con.name, &types)
                } else {
                    p
                }
            }
        }
    }

    pub fn instrec_many(
        a: &mut Arena<Type>,
        types: &[Index],
        mappings: &HashMap<String, Index>,
    ) -> Vec<Index> {
        types.iter().map(|x| instrec(a, *x, mappings)).collect()
    }

    let params = instrec_many(arena, &func.params, &mappings);
    let ret = instrec(arena, func.ret, &mappings);

    Function {
        params: params.to_vec(),
        ret,
        type_params: None,
    }
}
