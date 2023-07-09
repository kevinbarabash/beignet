use generational_arena::{Arena, Index};
use std::collections::HashMap;

use escalier_ast::Literal as Lit;

use crate::context::*;
use crate::errors::*;
use crate::types::*;
use crate::unify::*;

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
// TODO: reimplement as a fold operation
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
        TypeKind::Keyword(_) => false,  // leaf node
        TypeKind::Object(Object { elems }) => elems.iter().any(|elem| match elem {
            TObjElem::Constructor(constructor) => {
                // TODO: check constraints and default on type_params
                let param_types: Vec<_> = constructor.params.iter().map(|param| param.t).collect();
                occurs_in(arena, v, &param_types) || occurs_in_type(arena, v, constructor.ret)
            }
            TObjElem::Call(call) => {
                // TODO: check constraints and default on type_params
                let param_types: Vec<_> = call.params.iter().map(|param| param.t).collect();
                occurs_in(arena, v, &param_types) || occurs_in_type(arena, v, call.ret)
            }
            TObjElem::Method(method) => {
                // TODO: check constraints and default on type_params
                let param_types: Vec<_> = method.params.iter().map(|param| param.t).collect();
                occurs_in(arena, v, &param_types) || occurs_in_type(arena, v, method.ret)
            }
            TObjElem::Getter(getter) => occurs_in_type(arena, v, getter.ret),
            TObjElem::Setter(setter) => occurs_in_type(arena, v, setter.param.t),
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
        TypeKind::Union(Union { types }) => occurs_in(arena, v, &types),
        TypeKind::Intersection(Intersection { types }) => occurs_in(arena, v, &types),
        TypeKind::Tuple(Tuple { types }) => occurs_in(arena, v, &types),
        TypeKind::Constructor(Constructor { types, .. }) => occurs_in(arena, v, &types),
        TypeKind::Utility(Utility { types, .. }) => occurs_in(arena, v, &types),
        TypeKind::Mutable(Mutable { t }) => occurs_in_type(arena, v, t),
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
        TypeKind::Constructor(Constructor { name, types }) => match ctx.schemes.get(name) {
            Some(scheme) => expand_alias(arena, name, scheme, types),
            None => Err(Errors::InferenceError(format!(
                "Can't find type alias for {name}"
            ))),
        },
        TypeKind::Utility(Utility { kind, types }) => match kind {
            UtilityKind::Index => get_computed_member(arena, ctx, types[0], types[1]),
            UtilityKind::KeyOf => expand_keyof(arena, ctx, types[0]),
            _ => Err(Errors::InferenceError(format!(
                "Can't find utility type for {kind:#?}"
            ))),
        },
        _ => Ok(t),
    }
}

pub fn expand_keyof(arena: &mut Arena<Type>, ctx: &Context, t: Index) -> Result<Index, Errors> {
    let obj = expand_type(arena, ctx, t)?;

    match &arena[obj].kind.clone() {
        TypeKind::Object(Object { elems }) => {
            let mut keys = Vec::new();
            for elem in elems {
                // TODO: include indexers as well
                if let TObjElem::Prop(TProp { name, .. }) = elem {
                    keys.push(new_lit_type(arena, &Lit::String(name.to_string())));
                }
            }

            match keys.len() {
                0 => Ok(new_keyword(arena, Keyword::Never)),
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

pub fn get_computed_member(
    arena: &mut Arena<Type>,
    ctx: &Context,
    obj_idx: Index,
    key_idx: Index,
) -> Result<Index, Errors> {
    // NOTE: cloning is fine here because we aren't mutating `obj_type` or
    // `prop_type`.
    let obj_type = arena[obj_idx].clone();
    let key_type = arena[key_idx].clone();

    match &obj_type.kind {
        TypeKind::Object(_) => get_prop(arena, ctx, obj_idx, key_idx),
        // let tuple = [5, "hello", true]
        // tuple[1]; // "hello"
        TypeKind::Tuple(tuple) => {
            match &key_type.kind {
                TypeKind::Literal(Lit::Number(value)) => {
                    let index: usize = str::parse(value).map_err(|_| {
                        Errors::InferenceError(format!("{} isn't a valid index", value))
                    })?;
                    if index < tuple.types.len() {
                        // TODO: update AST with the inferred type
                        return Ok(tuple.types[index]);
                    }
                    Err(Errors::InferenceError(format!(
                        "{index} was outside the bounds 0..{} of the tuple",
                        tuple.types.len()
                    )))
                }
                TypeKind::Literal(Lit::String(_)) => {
                    // TODO: look up methods on the `Array` interface
                    // we need to instantiate the scheme such that `T` is equal
                    // to the union of all types in the tuple
                    get_prop(arena, ctx, obj_idx, key_idx)
                }
                TypeKind::Keyword(Keyword::Number) => {
                    let mut types = tuple.types.clone();
                    types.push(new_keyword(arena, Keyword::Undefined));
                    Ok(new_union_type(arena, &types))
                }
                _ => Err(Errors::InferenceError(
                    "Can only access tuple properties with a number".to_string(),
                )),
            }
        }
        // declare let tuple: [number, number] | [string, string]
        // tuple[1]; // number | string
        TypeKind::Union(union) => {
            let mut result_types = vec![];
            let mut undefined_count = 0;
            for idx in &union.types {
                match get_computed_member(arena, ctx, *idx, key_idx) {
                    Ok(t) => result_types.push(t),
                    Err(_) => {
                        // TODO: check what the error is, we may want to propagate
                        // certain errors
                        if undefined_count == 0 {
                            let undefined = new_keyword(arena, Keyword::Undefined);
                            result_types.push(undefined);
                        }
                        undefined_count += 1;
                    }
                }
            }
            if undefined_count == union.types.len() {
                // TODO: include name of property in error message
                Err(Errors::InferenceError(
                    "Couldn't find property on object".to_string(),
                ))
            } else {
                Ok(new_union_type(arena, &result_types))
            }
        }
        TypeKind::Constructor(Constructor {
            name: alias_name,
            types,
            ..
        }) => match ctx.schemes.get(alias_name) {
            Some(scheme) => {
                let obj_idx = expand_alias(arena, alias_name, scheme, types)?;
                get_computed_member(arena, ctx, obj_idx, key_idx)
            }
            None => Err(Errors::InferenceError(format!(
                "Can't find type alias for {alias_name}"
            ))),
        },
        TypeKind::Mutable(Mutable { t, .. }) => {
            let idx = get_computed_member(arena, ctx, *t, key_idx)?;
            Ok(new_mutable_type(arena, idx))
        }
        _ => {
            // TODO: provide a more specific error message for type variables
            Err(Errors::InferenceError(
                "Can only access properties on objects/tuples".to_string(),
            ))
        }
    }
}

// TODO(#624) - to behave differently when used to look up an lvalue vs a rvalue
pub fn get_prop(
    arena: &mut Arena<Type>,
    ctx: &Context,
    obj_idx: Index,
    key_idx: Index,
) -> Result<Index, Errors> {
    let undefined = new_keyword(arena, Keyword::Undefined);
    // It's fine to clone here because we aren't mutating
    let obj_type = arena[obj_idx].clone();
    let key_type = arena[key_idx].clone();

    if let TypeKind::Object(object) = &obj_type.kind {
        match &key_type.kind {
            TypeKind::Keyword(keyword)
                if keyword == &Keyword::Number
                    || keyword == &Keyword::String
                    || keyword == &Keyword::Symbol =>
            {
                let mut maybe_index: Option<&TIndex> = None;
                let mut values: Vec<Index> = vec![];

                for elem in &object.elems {
                    match elem {
                        // Callable signatures have no name so we ignore them.
                        TObjElem::Constructor(constructor) => continue,
                        TObjElem::Call(call) => continue,
                        TObjElem::Method(method) => {
                            // This only makes sense when we're getting the property
                            // as rvalue
                            match &method.name {
                                TPropKey::StringKey(_) if keyword == &Keyword::String => (),
                                TPropKey::NumberKey(_) if keyword == &Keyword::Number => (),
                                _ => continue,
                            };

                            values.push(arena.insert(Type::from(TypeKind::Function(Function {
                                params: method.params.clone(),
                                ret: method.ret,
                                type_params: method.type_params.clone(),
                            }))));
                        }
                        TObjElem::Getter(getter) => {
                            // This only makes sense when we're getting the property
                            // as rvalue
                            todo!()
                        }
                        TObjElem::Setter(setter) => {
                            // This only makes sense when we're getting the property
                            // as lvalue
                            todo!()
                        }
                        TObjElem::Index(index) => {
                            if maybe_index.is_some() {
                                return Err(Errors::InferenceError(
                                    "Object types can only have a single indexer".to_string(),
                                ));
                            }
                            maybe_index = Some(index);
                        }
                        TObjElem::Prop(prop) => {
                            match &prop.name {
                                TPropKey::StringKey(_) if keyword == &Keyword::String => (),
                                TPropKey::NumberKey(_) if keyword == &Keyword::Number => (),
                                _ => continue,
                            };

                            let prop_t = match prop.optional {
                                true => new_union_type(arena, &[prop.t, undefined]),
                                false => prop.t,
                            };
                            values.push(prop_t);
                        }
                    }
                }

                // TODO: handle combinations of indexers and non-indexer elements
                if let Some(indexer) = maybe_index {
                    match unify(arena, ctx, key_idx, indexer.key.t) {
                        Ok(_) => {
                            let undefined = new_keyword(arena, Keyword::Undefined);
                            Ok(new_union_type(arena, &[indexer.t, undefined]))
                        }
                        Err(_) => Err(Errors::InferenceError(format!(
                            "{} is not a valid indexer for {}",
                            key_type.as_string(arena),
                            obj_type.as_string(arena)
                        ))),
                    }
                } else if !values.is_empty() {
                    values.push(undefined);
                    Ok(new_union_type(arena, &values))
                } else {
                    Err(Errors::InferenceError(format!(
                        "{} has no indexer",
                        obj_type.as_string(arena)
                    )))
                }
            }
            TypeKind::Literal(Lit::String(name)) => {
                let mut maybe_index: Option<&TIndex> = None;
                for elem in &object.elems {
                    match elem {
                        // Callable signatures have no name so we ignore them.
                        TObjElem::Constructor(_) => continue,
                        TObjElem::Call(_) => continue,
                        TObjElem::Method(method) => {
                            let key = match &method.name {
                                TPropKey::StringKey(key) => key,
                                TPropKey::NumberKey(key) => key,
                            };
                            if key == name {
                                return Ok(arena.insert(Type::from(TypeKind::Function(
                                    Function {
                                        params: method.params.clone(),
                                        ret: method.ret,
                                        type_params: method.type_params.clone(),
                                    },
                                ))));
                            }
                        }
                        TObjElem::Getter(getter) => {
                            let key = match &getter.name {
                                TPropKey::StringKey(key) => key,
                                TPropKey::NumberKey(key) => key,
                            };
                            if key == name {
                                return Ok(arena.insert(Type::from(TypeKind::Function(
                                    Function {
                                        params: vec![],
                                        ret: getter.ret,
                                        type_params: None,
                                    },
                                ))));
                            }
                        }
                        TObjElem::Setter(setter) => {
                            let key = match &setter.name {
                                TPropKey::StringKey(key) => key,
                                TPropKey::NumberKey(key) => key,
                            };
                            if key == name {
                                return Ok(arena.insert(Type::from(TypeKind::Function(
                                    Function {
                                        params: vec![setter.param.to_owned()],
                                        ret: undefined,
                                        type_params: None,
                                    },
                                ))));
                            }
                        }
                        TObjElem::Index(index) => {
                            if maybe_index.is_some() {
                                return Err(Errors::InferenceError(
                                    "Object types can only have a single indexer".to_string(),
                                ));
                            }
                            maybe_index = Some(index);
                        }
                        TObjElem::Prop(prop) => {
                            let key = match &prop.name {
                                TPropKey::StringKey(key) => key,
                                TPropKey::NumberKey(key) => key,
                            };
                            if key == name {
                                let prop_t = match prop.optional {
                                    true => new_union_type(arena, &[prop.t, undefined]),
                                    false => prop.t,
                                };
                                return Ok(prop_t);
                            }
                        }
                    }
                }

                if let Some(indexer) = maybe_index {
                    match unify(arena, ctx, key_idx, indexer.key.t) {
                        Ok(_) => {
                            let undefined = new_keyword(arena, Keyword::Undefined);
                            Ok(new_union_type(arena, &[indexer.t, undefined]))
                        }
                        Err(_) => Err(Errors::InferenceError(format!(
                            "Couldn't find property {} in object",
                            name,
                        ))),
                    }
                } else {
                    Err(Errors::InferenceError(format!(
                        "Couldn't find property '{name}' on object",
                    )))
                }
            }
            TypeKind::Literal(Lit::Number(name)) => {
                let mut maybe_index: Option<&TIndex> = None;
                for elem in &object.elems {
                    if let TObjElem::Index(index) = elem {
                        if maybe_index.is_some() {
                            return Err(Errors::InferenceError(
                                "Object types can only have a single indexer".to_string(),
                            ));
                        }
                        maybe_index = Some(index);
                    }
                    // QUESTION: Do we care about allowing numbers as keys for
                    // methods and props?
                }

                if let Some(indexer) = maybe_index {
                    match unify(arena, ctx, key_idx, indexer.key.t) {
                        Ok(_) => {
                            let undefined = new_keyword(arena, Keyword::Undefined);
                            Ok(new_union_type(arena, &[indexer.t, undefined]))
                        }
                        Err(_) => Err(Errors::InferenceError(format!(
                            "Couldn't find property {} in object",
                            name,
                        ))),
                    }
                } else {
                    Err(Errors::InferenceError(format!(
                        "Couldn't find property '{name}' on object",
                    )))
                }
            }
            TypeKind::Utility(_) => {
                // expand the utilty type and then check again
                todo!()
            }
            _ => Err(Errors::InferenceError(format!(
                "{} is not a valid key",
                arena[key_idx].as_string(arena)
            ))),
        }
    } else {
        Err(Errors::InferenceError(
            "Can't access property on non-object type".to_string(),
        ))
    }
}
