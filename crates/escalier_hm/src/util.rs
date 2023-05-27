use generational_arena::{Arena, Index};
use std::collections::HashMap;

use crate::ast::{Lit, Num, SourceLocation, Str};
use crate::context::*;
use crate::errors::*;
use crate::types::*;
use crate::unify::unify;

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

pub fn expand_type(arena: &mut Arena<Type>, ctx: &Context, t: Index) -> Result<Index, Errors> {
    let t = prune(arena, t);
    // It's okay to clone here because we aren't mutating the type
    match &arena[t].clone().kind {
        TypeKind::Variable(_) => Ok(t),
        TypeKind::Literal(_) => Ok(t),
        TypeKind::Object(_) => Ok(t),
        TypeKind::Rest(_) => Ok(t),
        TypeKind::Function(_) => Ok(t),
        TypeKind::Constructor(Constructor { name, types }) => match ctx.schemes.get(name) {
            Some(scheme) => expand_alias(arena, name, scheme, types),
            None => {
                if ["number", "string", "boolean", "symbol", "null", "undefined"]
                    .contains(&name.as_str())
                {
                    Ok(t)
                } else {
                    Err(Errors::InferenceError(format!(
                        "Can't find type alias for {name}"
                    )))
                }
            }
        },
        TypeKind::Utility(Utility { name, types }) => match name.as_str() {
            "@@index" => expand_index_access(arena, ctx, types[0], types[1]),
            "@@keyof" => expand_keyof(arena, ctx, types[0]),
            _ => Err(Errors::InferenceError(format!(
                "Can't find utility type for {name}"
            ))),
        },
    }
}

// TODO: check that the index access is valid where it's inferred
pub fn expand_index_access(
    arena: &mut Arena<Type>,
    ctx: &Context,
    obj: Index,
    index: Index,
) -> Result<Index, Errors> {
    let obj = expand_type(arena, ctx, obj)?;
    let index = expand_type(arena, ctx, index)?;

    eprintln!("obj: {}", arena[obj].as_string(arena));
    eprintln!("index: {}", arena[index].as_string(arena));

    match (&arena[obj].kind.clone(), &arena[index].kind.clone()) {
        (TypeKind::Object(Object { props }), TypeKind::Literal(Lit::Str(Str { value, .. }))) => {
            let mut indexer: Option<TIndex> = None;
            for prop in props {
                if let TObjElem::Prop(TProp {
                    name,
                    t,
                    optional,
                    mutable: _,
                }) = prop
                {
                    let name = match name {
                        TPropKey::StringKey(value) => value,
                        TPropKey::NumberKey(value) => value,
                    };
                    if name == value {
                        return match optional {
                            true => {
                                let undefined = new_constructor(arena, "undefined", &[]);
                                Ok(new_union_type(arena, &[*t, undefined]))
                            }
                            false => Ok(*t),
                        };
                    }
                }

                if let TObjElem::Index(index) = prop {
                    indexer = Some(index.clone());
                }
            }

            if let Some(indexer) = indexer {
                match unify(arena, ctx, index, indexer.key.t) {
                    Ok(_) => {
                        let undefined = new_constructor(arena, "undefined", &[]);
                        Ok(new_union_type(arena, &[indexer.t, undefined]))
                    }
                    Err(_) => Err(Errors::InferenceError(format!(
                        "Couldn't find property {} in object {}",
                        value,
                        arena[obj].as_string(arena)
                    ))),
                }
            } else {
                Err(Errors::InferenceError(format!(
                    "Couldn't find property {} in object {}",
                    value,
                    arena[obj].as_string(arena)
                )))
            }
        }
        (TypeKind::Object(Object { props }), TypeKind::Literal(Lit::Num(Num { value, .. }))) => {
            let mut indexer: Option<TIndex> = None;
            for prop in props {
                if let TObjElem::Prop(TProp {
                    name,
                    t,
                    optional,
                    mutable: _,
                }) = prop
                {
                    let name = match name {
                        TPropKey::StringKey(value) => value,
                        TPropKey::NumberKey(value) => value,
                    };
                    if name == value {
                        return match optional {
                            true => {
                                let undefined = new_constructor(arena, "undefined", &[]);
                                Ok(new_union_type(arena, &[*t, undefined]))
                            }
                            false => Ok(*t),
                        };
                    }
                }

                if let TObjElem::Index(index) = prop {
                    indexer = Some(index.clone());
                }
            }

            if let Some(indexer) = indexer {
                match unify(arena, ctx, index, indexer.key.t) {
                    Ok(_) => {
                        let undefined = new_constructor(arena, "undefined", &[]);
                        Ok(new_union_type(arena, &[indexer.t, undefined]))
                    }
                    Err(_) => Err(Errors::InferenceError(format!(
                        "Couldn't find property {} in object {}",
                        value,
                        arena[obj].as_string(arena)
                    ))),
                }
            } else {
                Err(Errors::InferenceError(format!(
                    "Couldn't find property {} in object {}",
                    value,
                    arena[obj].as_string(arena)
                )))
            }
        }
        (TypeKind::Constructor(tuple), TypeKind::Literal(Lit::Num(Num { value, .. })))
            if tuple.name == "@@tuple" =>
        {
            let index: usize = str::parse(value)
                .map_err(|_| Errors::InferenceError(format!("{} isn't a valid index", value)))?;

            if index >= tuple.types.len() {
                return Err(Errors::InferenceError(format!(
                    "Index {} out of bounds for tuple {}",
                    index,
                    arena[obj].as_string(arena)
                )));
            }

            Ok(tuple.types[index])
        }
        (TypeKind::Object(_), TypeKind::Constructor(number)) if number.name == "string" => {
            todo!("number index access on objects where the 'string' type is the key")
        }
        (TypeKind::Object(_), TypeKind::Constructor(number)) if number.name == "number" => {
            todo!("number index access on object where the 'number' type is the key")
        }
        (TypeKind::Constructor(tuple), TypeKind::Constructor(number))
            if tuple.name == "@@tuple" && number.name == "number" =>
        {
            let mut types = tuple.types.clone();
            types.push(new_constructor(arena, "undefined", &[]));

            Ok(new_union_type(arena, &types))
        }
        _ => Err(Errors::InferenceError(format!(
            "{} can't be used as a key on {}",
            arena[index].as_string(arena),
            arena[obj].as_string(arena),
        ))),
    }
}

pub fn expand_keyof(arena: &mut Arena<Type>, ctx: &Context, t: Index) -> Result<Index, Errors> {
    let obj = expand_type(arena, ctx, t)?;

    match &arena[obj].kind.clone() {
        TypeKind::Object(Object { props }) => {
            let mut keys = Vec::new();
            for prop in props {
                // TODO: include indexers as well
                if let TObjElem::Prop(TProp { name, .. }) = prop {
                    let name = match name {
                        TPropKey::StringKey(value) => value,
                        TPropKey::NumberKey(value) => value,
                    };
                    keys.push(new_lit_type(
                        arena,
                        &Lit::Str(Str {
                            value: name.to_owned(),
                            loc: SourceLocation::default(),
                            span: 0..0,
                        }),
                    ));
                }
            }

            match keys.len() {
                0 => Ok(new_constructor(arena, "never", &[])),
                1 => Ok(keys[0].to_owned()),
                _ => Ok(new_union_type(arena, &keys)),
            }
        }
        _ => Err(Errors::InferenceError(format!(
            "{} isn't an object",
            arena[t].as_string(arena),
        ))),
    }
}
