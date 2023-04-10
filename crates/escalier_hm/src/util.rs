use crate::types::*;

/// Checks whether a type variable occurs in a type expression.
///
/// Note: Must be called with v pre-pruned
///
/// Args:
///     v:  The TypeVariable to be tested for
///     type2: The type in which to search
///
/// Returns:
///     True if v occurs in type2, otherwise False
pub fn occurs_in_type(a: &mut Vec<Type>, v: ArenaType, type2: ArenaType) -> bool {
    let pruned_type2 = prune(a, type2);
    if pruned_type2 == v {
        return true;
    }
    // We clone here because we can't move out of a shared reference.
    // TODO: Consider using Rc<RefCell<Type>> to avoid unnecessary cloning.
    match a.get(pruned_type2).unwrap().clone().kind {
        TypeKind::Variable(_) => false, // leaf node
        TypeKind::Literal(_) => false,  // leaf node
        TypeKind::Tuple(Tuple { types }) => occurs_in(a, v, &types),
        TypeKind::Object(Object { props }) => {
            let types = props.iter().map(|(_, v)| *v).collect::<Vec<_>>();
            occurs_in(a, v, &types)
        }
        TypeKind::Constructor(Constructor { types, .. }) => occurs_in(a, v, &types),
        TypeKind::Ref(_) => false, // TODO: add support for type args
        TypeKind::Function(Function { params, ret, .. }) => {
            occurs_in(a, v, &params) || occurs_in_type(a, v, ret)
        }
        TypeKind::Union(Union { types }) => occurs_in(a, v, &types),
    }
}

/// Checks whether a types variable occurs in any other types.
///
/// Args:
///     t:  The TypeVariable to be tested for
///     types: The sequence of types in which to search
///
/// Returns:
///     True if t occurs in any of types, otherwise False
///
pub fn occurs_in(a: &mut Vec<Type>, t: ArenaType, types: &[ArenaType]) -> bool {
    for t2 in types.iter() {
        if occurs_in_type(a, t, *t2) {
            return true;
        }
    }
    false
}

/// Checks whether a given variable occurs in a list of non-generic variables
///
/// Note that a variables in such a list may be instantiated to a type term,
/// in which case the variables contained in the type term are considered
/// non-generic.
///
/// Note: Must be called with v pre-pruned
///
/// Args:
///     v: The TypeVariable to be tested for genericity
///     non_generic: A set of non-generic TypeVariables
///
/// Returns:
///     True if v is a generic variable, otherwise False
pub fn is_generic(a: &mut Vec<Type>, v: ArenaType, non_generic: &[ArenaType]) -> bool {
    !occurs_in(a, v, non_generic)
}

/// Returns the currently defining instance of t.
///
/// As a side effect, collapses the list of type instances. The function Prune
/// is used whenever a type expression has to be inspected: it will always
/// return a type expression which is either an uninstantiated type variable or
/// a type operator; i.e. it will skip instantiated variables, and will
/// actually prune them from expressions to remove long chains of instantiated
/// variables.
///
/// Args:
///     t: The type to be pruned
///
/// Returns:
///     An uninstantiated TypeVariable or a TypeOperator
pub fn prune(a: &mut Vec<Type>, t: ArenaType) -> ArenaType {
    let v2 = match a.get(t).unwrap().kind {
        // TODO: handle .unwrap() panicing
        TypeKind::Variable(Variable {
            instance: Some(value),
        }) => value,
        _ => {
            return t;
        }
    };

    let value = prune(a, v2);
    match &mut a.get_mut(t).unwrap().kind {
        // TODO: handle .unwrap() panicing
        TypeKind::Variable(Variable {
            ref mut instance, ..
        }) => {
            *instance = Some(value);
        }
        _ => {
            return t;
        }
    }
    value
}
