use generational_arena::{Arena, Index};
use std::collections::{BTreeMap, HashMap};

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
        TypeKind::KeyOf(KeyOf { t }) => occurs_in_type(arena, v, t),
        TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
            occurs_in_type(arena, v, obj) || occurs_in_type(arena, v, index)
        }
        TypeKind::Conditional(Conditional {
            check,
            extends,
            true_type,
            false_type,
        }) => {
            occurs_in_type(arena, v, check)
                || occurs_in_type(arena, v, extends)
                || occurs_in_type(arena, v, true_type)
                || occurs_in_type(arena, v, false_type)
        }
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
pub fn prune(arena: &mut Arena<Type>, t: Index) -> Index {
    let v2 = match arena.get(t).unwrap().kind {
        // TODO: handle .unwrap() panicing
        TypeKind::Variable(Variable {
            instance: Some(value),
            ..
        }) => value,
        _ => {
            return t;
        }
    };

    let value = prune(arena, v2);
    match &mut arena.get_mut(t).unwrap().kind {
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

pub fn expand_constructor(
    arena: &mut Arena<Type>,
    ctx: &Context,
    constructor: &Constructor,
) -> Result<Index, Errors> {
    match ctx.schemes.get(&constructor.name) {
        Some(scheme) => expand_alias(arena, &constructor.name, scheme, &constructor.types),
        None => Err(Errors::InferenceError(format!(
            "{} isn't defined",
            constructor.name
        ))),
    }
}

pub fn expand_alias_by_name(
    arena: &mut Arena<Type>,
    ctx: &Context,
    name: &str,
    type_args: &[Index],
) -> Result<Index, Errors> {
    match ctx.schemes.get(name) {
        Some(scheme) => expand_alias(arena, name, scheme, type_args),
        None => Err(Errors::InferenceError(format!("{} isn't defined", name))),
    }
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
        TypeKind::KeyOf(KeyOf { t }) => expand_keyof(arena, ctx, *t),
        TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
            get_computed_member(arena, ctx, *obj, *index)
        }
        TypeKind::Conditional(_) => {
            todo!()
        }
        _ => Ok(t),
    }
}

// Expands `keyof` types into one of the followwing:
// - string or number literals
// - string, number, or symbol type
// - never
// - union of any of those types
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
            Ok(new_union_type(arena, &keys))
        }
        // NOTE: The behavior of `keyof` with arrays and tuples differs from
        // TypeScript.  TypeScript includes the names of all the properties from
        // Array.prototype as well.
        // TODO(#637): Have interop layer translate between Escalier's and
        // TypeScript's `keyof` utility type.
        TypeKind::Constructor(array) if array.name == "Array" => {
            Ok(new_keyword(arena, Keyword::Number))
        }
        TypeKind::Tuple(tuple) => {
            let keys: Vec<Index> = tuple
                .types
                .iter()
                .enumerate()
                .map(|(k, _)| new_lit_type(arena, &Lit::Number(k.to_string())))
                .collect();
            Ok(new_union_type(arena, &keys))
        }
        TypeKind::Constructor(constructor) => {
            let idx = expand_constructor(arena, ctx, constructor)?;
            expand_keyof(arena, ctx, idx)
        }
        TypeKind::Intersection(Intersection { types }) => {
            let mut string_keys = BTreeMap::new();
            let mut number_keys = BTreeMap::new();
            let mut maybe_string = None;
            let mut maybe_number = None;
            let mut maybe_symbol = None;

            for t in types {
                let keys = expand_keyof(arena, ctx, *t)?;

                match &arena[keys].kind {
                    TypeKind::Union(Union { types }) => {
                        for t in types {
                            match &arena[*t].kind {
                                TypeKind::Literal(Lit::Number(num)) => {
                                    number_keys.insert(num.to_string(), *t);
                                }
                                TypeKind::Literal(Lit::String(str)) => {
                                    string_keys.insert(str.to_string(), *t);
                                }
                                TypeKind::Keyword(Keyword::Number) => {
                                    maybe_number = Some(*t);
                                }
                                TypeKind::Keyword(Keyword::String) => {
                                    maybe_string = Some(*t);
                                }
                                TypeKind::Keyword(Keyword::Symbol) => {
                                    maybe_symbol = Some(*t);
                                }
                                _ => (),
                            }
                        }
                    }
                    TypeKind::Literal(Lit::Number(num)) => {
                        number_keys.insert(num.to_string(), keys);
                    }
                    TypeKind::Literal(Lit::String(str)) => {
                        string_keys.insert(str.to_string(), keys);
                    }
                    TypeKind::Keyword(Keyword::Number) => {
                        maybe_number = Some(keys);
                    }
                    TypeKind::Keyword(Keyword::String) => {
                        maybe_string = Some(keys);
                    }
                    TypeKind::Keyword(Keyword::Symbol) => {
                        maybe_symbol = Some(*t);
                    }
                    _ => (),
                }
            }

            let mut all_keys: Vec<Index> = vec![];

            match maybe_number {
                Some(number) => all_keys.push(number),
                None => all_keys.append(&mut number_keys.values().cloned().collect::<Vec<Index>>()),
            }

            match maybe_string {
                Some(string) => all_keys.push(string),
                None => all_keys.append(&mut string_keys.values().cloned().collect::<Vec<Index>>()),
            }

            if let Some(symbol) = maybe_symbol {
                all_keys.push(symbol);
            }

            Ok(new_union_type(arena, &all_keys))
        }
        TypeKind::Union(_) => Ok(new_keyword(arena, Keyword::Never)),
        TypeKind::Keyword(keyword) => match keyword {
            // TODO: update get_property to handle these cases as well
            // TODO: split these out in TypeKind::Primitive
            Keyword::Number => {
                let idx = expand_alias_by_name(arena, ctx, "Number", &[])?;
                expand_keyof(arena, ctx, idx)
            }
            Keyword::Boolean => {
                let idx = expand_alias_by_name(arena, ctx, "Boolean", &[])?;
                expand_keyof(arena, ctx, idx)
            }
            Keyword::String => {
                let idx = expand_alias_by_name(arena, ctx, "String", &[])?;
                expand_keyof(arena, ctx, idx)
            }
            Keyword::Symbol => {
                let idx = expand_alias_by_name(arena, ctx, "Symbol", &[])?;
                expand_keyof(arena, ctx, idx)
            }
            Keyword::Null => Ok(new_keyword(arena, Keyword::Never)),
            Keyword::Undefined => Ok(new_keyword(arena, Keyword::Never)),
            Keyword::Unknown => Ok(new_keyword(arena, Keyword::Never)),
            Keyword::Never => {
                let string = new_keyword(arena, Keyword::String);
                let number = new_keyword(arena, Keyword::Number);
                let symbol = new_keyword(arena, Keyword::Symbol);
                Ok(new_union_type(arena, &[string, number, symbol]))
            }
        },
        TypeKind::Literal(_) => {
            // TODO: handle literals in a similar way to primitives
            todo!()
        }
        _ => {
            let expanded_t = expand_type(arena, ctx, t)?;
            if expanded_t != t {
                expand_keyof(arena, ctx, expanded_t)
            } else {
                Ok(new_keyword(arena, Keyword::Never))
            }
        }
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
