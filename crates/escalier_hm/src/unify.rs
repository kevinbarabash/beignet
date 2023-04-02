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
            for (p, q) in func_a.params.iter().zip(func_b.params.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, func_a.ret, func_b.ret)?;
            Ok(())
        }
        (TypeKind::Call(call), TypeKind::Function(func)) => {
            for (p, q) in call.args.iter().zip(func.params.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, call.ret, func.ret)?;
            Ok(())
        }
        (TypeKind::Call(call_a), TypeKind::Call(call_b)) => {
            for (p, q) in call_a.args.iter().zip(call_b.args.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, call_a.ret, call_b.ret)?;
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
