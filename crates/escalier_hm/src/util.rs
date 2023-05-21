use generational_arena::{Arena, Index};
use std::collections::HashMap;

use crate::context::*;
use crate::errors::*;
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
pub fn occurs_in_type(arena: &mut Arena<Type>, v: Index, type2: Index) -> bool {
    let pruned_type2 = prune(arena, type2);
    if pruned_type2 == v {
        return true;
    }
    // We clone here because we can't move out of a shared reference.
    // TODO: Consider using Rc<RefCell<Type>> to avoid unnecessary cloning.
    match arena.get(pruned_type2).unwrap().clone().kind {
        TypeKind::Variable(_) => false, // leaf node
        TypeKind::Literal(_) => false,  // leaf node
        TypeKind::Object(Object { props }) => props.iter().any(|prop| match prop {
            TObjElem::Method(method) => {
                // TODO: check constraints and default on type_params
                let param_types: Vec<_> = method.params.iter().map(|param| param.t).collect();
                occurs_in(arena, v, &param_types) || occurs_in_type(arena, v, method.ret)
            }
            TObjElem::Index(index) => occurs_in_type(arena, v, index.t),
            TObjElem::Prop(prop) => occurs_in_type(arena, v, prop.t),
        }),
        TypeKind::Rest(Rest { arg }) => occurs_in_type(arena, v, arg),
        TypeKind::Function(Function {
            params,
            ret,
            type_params: _,
        }) => {
            // TODO: check constraints and default on type_params
            let param_types: Vec<_> = params.iter().map(|param| param.t).collect();
            occurs_in(arena, v, &param_types) || occurs_in_type(arena, v, ret)
        }
        TypeKind::Constructor(Constructor { types, .. }) => occurs_in(arena, v, &types),
        TypeKind::Utility(Utility { types, .. }) => occurs_in(arena, v, &types),
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
pub fn occurs_in<'a, I>(a: &mut Arena<Type>, t: Index, types: I) -> bool
where
    I: IntoIterator<Item = &'a Index>,
{
    for t2 in types.into_iter() {
        if occurs_in_type(a, t, *t2) {
            return true;
        }
    }
    false
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
pub fn prune(a: &mut Arena<Type>, t: Index) -> Index {
    let v2 = match a.get(t).unwrap().kind {
        // TODO: handle .unwrap() panicing
        TypeKind::Variable(Variable {
            instance: Some(value),
            ..
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

pub fn expand_alias(
    arena: &mut Arena<Type>,
    name: &str,
    scheme: &Scheme,
    type_args: &[Index],
) -> Result<Index, Errors> {
    match &scheme.type_params {
        Some(type_params) => {
            if type_params.len() != type_args.len() {
                Err(Errors::InferenceError(format!(
                    "{name} expects {} type args, but was passed {}",
                    type_params.len(),
                    type_args.len()
                )))
            } else {
                let mut mapping: HashMap<String, Index> = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    mapping.insert(param.name.clone(), arg.to_owned());
                }

                let t = instantiate_scheme(arena, scheme.t, &mapping);

                Ok(t)
            }
        }
        None => {
            if type_args.is_empty() {
                Ok(scheme.t)
            } else {
                Err(Errors::InferenceError(format!(
                    "{name} doesn't require any type args"
                )))
            }
        }
    }
}
