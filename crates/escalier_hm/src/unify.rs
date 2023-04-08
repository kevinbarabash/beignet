use itertools::Itertools;

use crate::errors::*;
use crate::literal::*;
use crate::types::*;
use crate::util::*;

/// Unify the two types t1 and t2.
///
/// Makes the types t1 and t2 the same.
///
/// Args:
///     t1: The first type to be made equivalent
///     t2: The second type to be be equivalent
///
/// Returns:
///     None
///
/// Raises:
///     InferenceError: Raised if the types cannot be unified.
pub fn unify(alloc: &mut Vec<Type>, t1: ArenaType, t2: ArenaType) -> Result<(), Errors> {
    let a = prune(alloc, t1);
    let b = prune(alloc, t2);
    // Why do we clone here?
    let a_t = alloc.get(a).unwrap().clone();
    let b_t = alloc.get(b).unwrap().clone();
    match (&a_t.kind, &b_t.kind) {
        (TypeKind::Variable(_), _) => bind(alloc, a, b),
        (_, TypeKind::Variable(_)) => bind(alloc, b, a),
        (TypeKind::Constructor(con_a), TypeKind::Constructor(con_b)) => {
            // TODO: support type constructors with optional and default type params
            if con_a.name != con_b.name || con_a.types.len() != con_b.types.len() {
                return Err(Errors::InferenceError(format!("type mismatch: {a} != {b}")));
            }
            for (p, q) in con_a.types.iter().zip(con_b.types.iter()) {
                unify(alloc, *p, *q)?;
            }
            Ok(())
        }
        (TypeKind::Function(func_a), TypeKind::Function(func_b)) => {
            if func_a.params.len() > func_b.params.len() {
                return Err(Errors::InferenceError(format!(
                    "{a_t:?} is not a subtype of {b_t:?} since it requires more params",
                )));
            }

            for (p, q) in func_a.params.iter().zip(func_b.params.iter()) {
                // NOTE: We reverse the order of the params here because func_a
                // should be able to accept any params that func_b can accept,
                // its params may be more lenient.  Thus q is a subtype of p.
                unify(alloc, *q, *p)?;
            }
            unify(alloc, func_a.ret, func_b.ret)?;
            Ok(())
        }
        (
            TypeKind::Literal(Literal::Number(_)),
            TypeKind::Constructor(Constructor { name, .. }),
        ) if name == "number" => Ok(()),
        (
            TypeKind::Literal(Literal::String(_)),
            TypeKind::Constructor(Constructor { name, .. }),
        ) if name == "string" => Ok(()),
        (
            TypeKind::Literal(Literal::Boolean(_)),
            TypeKind::Constructor(Constructor { name, .. }),
        ) if name == "boolean" => Ok(()),
        (TypeKind::Union(Union { types }), _) => {
            // All types in the union must be subtypes of t2
            for t in types.iter() {
                unify(alloc, *t, b)?;
            }
            Ok(())
        }
        (_, TypeKind::Union(Union { types })) => {
            for t2 in types.iter() {
                if unify(alloc, a, *t2).is_ok() {
                    return Ok(());
                }
            }

            Err(Errors::InferenceError(format!(
                "type mismatch: unify({a_t:?}, {b_t:?}) failed"
            )))
        }
        _ => Err(Errors::InferenceError(format!(
            "type mismatch: unify({a_t:?}, {b_t:?}) failed"
        ))),
    }
}

// This function unifies and infers the return type of a function call.
pub fn unify_call(
    alloc: &mut Vec<Type>,
    arg_types: &[ArenaType],
    t2: ArenaType,
) -> Result<ArenaType, Errors> {
    let ret_type = new_var_type(alloc);
    let call_type = new_func_type(alloc, arg_types, ret_type);

    let b = prune(alloc, t2);
    let b_t = alloc.get(b).unwrap().clone();

    match b_t.kind {
        TypeKind::Variable(_) => bind(alloc, b, call_type)?,
        TypeKind::Constructor(_) => {
            // lookup definition of type constructor, and see if its instances
            // have any callable signatures
            todo!("constructor");
        }
        TypeKind::Literal(lit) => {
            return Err(Errors::InferenceError(format!(
                "literal {lit} is not callable"
            )));
        }
        TypeKind::Function(func) => {
            if arg_types.len() < func.params.len() {
                return Err(Errors::InferenceError(format!(
                    "too few arguments to function: expected {}, got {}",
                    func.params.len(),
                    arg_types.len()
                )));
            }

            for (p, q) in arg_types.iter().zip(func.params.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, ret_type, func.ret)?;
        }
        TypeKind::Union(union) => {
            let mut ret_types = vec![];
            for t in union.types.iter() {
                let ret_type = unify_call(alloc, arg_types, *t)?;
                ret_types.push(ret_type);
            }

            return Ok(new_union_type(
                alloc,
                &ret_types.into_iter().unique().collect_vec(),
            ));
        }
    }

    Ok(ret_type)
}

fn bind(alloc: &mut Vec<Type>, a: usize, b: usize) -> Result<(), Errors> {
    if a != b {
        if occurs_in_type(alloc, a, b) {
            // raise InferenceError("recursive unification")
            return Err(Errors::InferenceError("recursive unification".to_string()));
        }
        alloc.get_mut(a).unwrap().set_instance(b);
    }
    Ok(())
}
