use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

use escalier_ast::Literal;

use crate::checker::Checker;
use crate::context::*;
use crate::errors::*;
use crate::folder::walk_index;
use crate::folder::Folder;
use crate::key_value_store::KeyValueStore;
use crate::types::*;
use crate::visitor::{self, Visitor};

impl Checker {
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
    pub fn occurs_in_type(&mut self, v: Index, type2: Index) -> bool {
        let pruned_type2 = self.prune(type2);
        if pruned_type2 == v {
            return true;
        }
        // We clone here because we can't move out of a shared reference.
        // TODO: Consider using Rc<RefCell<Type>> to avoid unnecessary cloning.
        match self.arena.get(pruned_type2).unwrap().clone().kind {
            TypeKind::Variable(_) => false,  // leaf node
            TypeKind::Literal(_) => false,   // leaf node
            TypeKind::Primitive(_) => false, // leaf node
            TypeKind::Keyword(_) => false,   // leaf node
            TypeKind::Infer(_) => false,     // leaf node
            TypeKind::Wildcard => false,     // leaf node
            TypeKind::Object(Object { elems }) => elems.iter().any(|elem| match elem {
                TObjElem::Constructor(constructor) => {
                    // TODO: check constraints and default on type_params
                    let param_types: Vec<_> =
                        constructor.params.iter().map(|param| param.t).collect();
                    self.occurs_in(v, &param_types) || self.occurs_in_type(v, constructor.ret)
                }
                TObjElem::Call(call) => {
                    // TODO: check constraints and default on type_params
                    let param_types: Vec<_> = call.params.iter().map(|param| param.t).collect();
                    self.occurs_in(v, &param_types) || self.occurs_in_type(v, call.ret)
                }
                TObjElem::Mapped(mapped) => {
                    self.occurs_in_type(v, mapped.key)
                        || self.occurs_in_type(v, mapped.value)
                        || self.occurs_in_type(v, mapped.source)
                    // TODO: handle .check and .extends
                }
                TObjElem::Prop(prop) => self.occurs_in_type(v, prop.t),
            }),
            TypeKind::Rest(Rest { arg }) => self.occurs_in_type(v, arg),
            TypeKind::Function(Function {
                params,
                ret,
                type_params: _, // TODO
                throws: _,      // TODO
            }) => {
                // TODO: check constraints and default on type_params
                let param_types: Vec<_> = params.iter().map(|param| param.t).collect();
                self.occurs_in(v, &param_types) || self.occurs_in_type(v, ret)
            }
            TypeKind::Union(Union { types }) => self.occurs_in(v, &types),
            TypeKind::Intersection(Intersection { types }) => self.occurs_in(v, &types),
            TypeKind::Tuple(Tuple { types }) => self.occurs_in(v, &types),
            TypeKind::Constructor(Constructor { types, .. }) => self.occurs_in(v, &types),
            TypeKind::KeyOf(KeyOf { t }) => self.occurs_in_type(v, t),
            TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
                self.occurs_in_type(v, obj) || self.occurs_in_type(v, index)
            }
            TypeKind::Conditional(Conditional {
                check,
                extends,
                true_type,
                false_type,
            }) => {
                self.occurs_in_type(v, check)
                    || self.occurs_in_type(v, extends)
                    || self.occurs_in_type(v, true_type)
                    || self.occurs_in_type(v, false_type)
            }
            TypeKind::Binary(BinaryT { op: _, left, right }) => {
                self.occurs_in_type(v, left) || self.occurs_in_type(v, right)
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
    pub fn occurs_in<'b, I>(&mut self, t: Index, types: I) -> bool
    where
        I: IntoIterator<Item = &'b Index>,
    {
        for t2 in types.into_iter() {
            if self.occurs_in_type(t, *t2) {
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
    pub fn prune(&mut self, t: Index) -> Index {
        let v2 = match self.arena.get(t).unwrap().kind {
            // TODO: handle .unwrap() panicing
            TypeKind::Variable(Variable {
                instance: Some(value),
                ..
            }) => value,
            _ => {
                return t;
            }
        };

        let value = self.prune(v2);
        match &mut self.arena.get_mut(t).unwrap().kind {
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

    pub fn expand_alias_by_name(
        &mut self,
        ctx: &Context,
        name: &str,
        type_args: &[Index],
    ) -> Result<Index, Errors> {
        match ctx.schemes.get(name) {
            Some(scheme) => self.expand_alias(ctx, name, scheme, type_args),
            None => {
                panic!("{} isn't defined", name);
                // Err(Errors::InferenceError(format!("{} isn't defined", name)))
            }
        }
    }

    pub fn expand_alias(
        &mut self,
        ctx: &Context,
        name: &str,
        scheme: &Scheme,
        type_args: &[Index],
    ) -> Result<Index, Errors> {
        match &scheme.type_params {
            Some(type_params) => {
                if type_params.len() != type_args.len() {
                    return Err(Errors::InferenceError(format!(
                        "{name} expects {} type args, but was passed {}",
                        type_params.len(),
                        type_args.len()
                    )));
                }

                if let TypeKind::Conditional(Conditional { check, .. }) = self.arena[scheme.t].kind
                {
                    if let TypeKind::Constructor(tref) = &self.arena[check].kind.clone() {
                        if let Some((index_of_check_type, _)) = type_params
                            .iter()
                            .find_position(|type_param| type_param.name == tref.name)
                        {
                            let type_arg = self.expand_type(ctx, type_args[index_of_check_type])?;

                            if let TypeKind::Union(Union { types: union_types }) =
                                &self.arena[type_arg].kind.clone()
                            {
                                let mut types = vec![];

                                for t in union_types.iter() {
                                    let mut mapping: HashMap<String, Index> = HashMap::new();

                                    for (type_param, type_arg) in
                                        type_params.iter().zip(type_args.iter())
                                    {
                                        if type_param.name == tref.name {
                                            mapping.insert(type_param.name.to_owned(), *t);
                                        } else {
                                            mapping.insert(type_param.name.to_owned(), *type_arg);
                                        }
                                    }

                                    let t = instantiate_scheme(&mut self.arena, scheme.t, &mapping);
                                    types.push(self.expand_type(ctx, t)?);
                                }

                                let filtered_types = types
                                    .into_iter()
                                    .filter(|t| {
                                        if let TypeKind::Keyword(kw) = &self.arena[*t].kind {
                                            kw != &Keyword::Never
                                        } else {
                                            true
                                        }
                                    })
                                    .collect::<Vec<_>>();

                                let t = new_union_type(&mut self.arena, &filtered_types);
                                return self.expand_type(ctx, t);
                            }
                        }
                    }
                }

                let mut mapping: HashMap<String, Index> = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    if let Some(constraint) = param.constraint {
                        self.unify(ctx, *arg, constraint)?;
                    }
                    mapping.insert(param.name.clone(), arg.to_owned());
                }

                let t = instantiate_scheme(&mut self.arena, scheme.t, &mapping);
                self.expand_type(ctx, t)
            }
            None => {
                if type_args.is_empty() {
                    let t = self.expand_type(ctx, scheme.t)?;
                    Ok(t)
                } else {
                    Err(Errors::InferenceError(format!(
                        "{name} doesn't require any type args"
                    )))
                }
            }
        }
    }

    pub fn expand_type(&mut self, ctx: &Context, t: Index) -> Result<Index, Errors> {
        let t = self.prune(t);

        // It's okay to clone here because we aren't mutating the type
        let t = match &self.arena[t].clone().kind {
            TypeKind::KeyOf(KeyOf { t }) => self.expand_keyof(ctx, *t)?,
            TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
                self.get_computed_member(ctx, *obj, *index)?
            }
            TypeKind::Conditional(conditional) => self.expand_conditional(ctx, conditional)?,
            TypeKind::Constructor(Constructor { name, types, .. }) => {
                self.expand_alias_by_name(ctx, name, types)?
            }
            TypeKind::Binary(binary) => self.expand_binary(ctx, binary)?,
            TypeKind::Object(object) => return self.expand_object(ctx, object),
            _ => return Ok(t), // Early return to avoid infinite loop
        };

        self.expand_type(ctx, t)
    }

    // Expands `keyof` types into one of the followwing:
    // - string or number literals
    // - string, number, or symbol type
    // - never
    // - union of any of those types
    pub fn expand_keyof(&mut self, ctx: &Context, t: Index) -> Result<Index, Errors> {
        let obj = match &self.arena[t].kind {
            // Don't expand Array's since they have a special `keyof` behavior.  We
            // don't want to expand them because we don't want to include array methods
            // in the list of keys.  This is similar to how objects are treated.
            TypeKind::Constructor(Constructor { name, .. }) if name == "Array" => t,
            _ => self.expand_type(ctx, t)?,
        };

        match &self.arena[obj].kind.clone() {
            TypeKind::Object(Object { elems }) => {
                let mut string_keys: Vec<Index> = Vec::new();
                let mut number_keys: Vec<Index> = Vec::new();
                let mut maybe_string: Option<Index> = None;
                let mut maybe_number: Option<Index> = None;
                let mut maybe_symbol: Option<Index> = None;

                for elem in elems {
                    match elem {
                        TObjElem::Call(_) => (),
                        TObjElem::Constructor(_) => (),
                        TObjElem::Mapped(mapped) => {
                            let mapped_key = get_mapped_key(&mut self.arena, mapped);

                            match &self.arena[mapped_key].kind {
                                TypeKind::Primitive(Primitive::String) => {
                                    maybe_string = Some(mapped_key);
                                }
                                TypeKind::Primitive(Primitive::Number) => {
                                    maybe_number = Some(mapped_key);
                                }
                                TypeKind::Primitive(Primitive::Symbol) => {
                                    maybe_symbol = Some(mapped_key);
                                }
                                _ => todo!(),
                            }
                        }
                        TObjElem::Prop(TProp { name, .. }) => match name {
                            TPropKey::StringKey(name) => {
                                string_keys.push(new_lit_type(
                                    &mut self.arena,
                                    &Literal::String(name.to_owned()),
                                ));
                            }
                            TPropKey::NumberKey(name) => {
                                number_keys.push(new_lit_type(
                                    &mut self.arena,
                                    &Literal::Number(name.to_owned()),
                                ));
                            }
                        },
                    }
                }

                let mut all_keys: Vec<Index> = vec![];

                match maybe_number {
                    Some(number) => all_keys.push(number),
                    None => all_keys.append(&mut number_keys),
                }

                match maybe_string {
                    Some(string) => all_keys.push(string),
                    None => all_keys.append(&mut string_keys),
                }

                if let Some(symbol) = maybe_symbol {
                    all_keys.push(symbol);
                }

                Ok(new_union_type(&mut self.arena, &all_keys))
            }
            // NOTE: The behavior of `keyof` with arrays and tuples differs from
            // TypeScript.  TypeScript includes the names of all the properties from
            // Array.prototype as well.
            // TODO(#637): Have interop layer translate between Escalier's and
            // TypeScript's `keyof` utility type.
            TypeKind::Constructor(array) if array.name == "Array" => {
                Ok(new_primitive(&mut self.arena, Primitive::Number))
            }
            TypeKind::Tuple(tuple) => {
                let keys: Vec<Index> = tuple
                    .types
                    .iter()
                    .enumerate()
                    .map(|(k, _)| new_lit_type(&mut self.arena, &Literal::Number(k.to_string())))
                    .collect();
                Ok(new_union_type(&mut self.arena, &keys))
            }
            TypeKind::Constructor(constructor) => {
                let idx = self.expand_alias_by_name(ctx, &constructor.name, &constructor.types)?;
                self.expand_keyof(ctx, idx)
            }
            TypeKind::Intersection(Intersection { types }) => {
                let mut string_keys = BTreeMap::new();
                let mut number_keys = BTreeMap::new();
                let mut maybe_string = None;
                let mut maybe_number = None;
                let mut maybe_symbol = None;

                for t in types {
                    let keys = self.expand_keyof(ctx, *t)?;

                    match &self.arena[keys].kind {
                        TypeKind::Union(Union { types }) => {
                            for t in types {
                                match &self.arena[*t].kind {
                                    TypeKind::Literal(Literal::Number(num)) => {
                                        number_keys.insert(num.to_string(), *t);
                                    }
                                    TypeKind::Literal(Literal::String(str)) => {
                                        string_keys.insert(str.to_string(), *t);
                                    }
                                    TypeKind::Primitive(Primitive::Number) => {
                                        maybe_number = Some(*t);
                                    }
                                    TypeKind::Primitive(Primitive::String) => {
                                        maybe_string = Some(*t);
                                    }
                                    TypeKind::Primitive(Primitive::Symbol) => {
                                        maybe_symbol = Some(*t);
                                    }
                                    _ => (),
                                }
                            }
                        }
                        TypeKind::Literal(Literal::Number(num)) => {
                            number_keys.insert(num.to_string(), keys);
                        }
                        TypeKind::Literal(Literal::String(str)) => {
                            string_keys.insert(str.to_string(), keys);
                        }
                        TypeKind::Primitive(Primitive::Number) => {
                            maybe_number = Some(keys);
                        }
                        TypeKind::Primitive(Primitive::String) => {
                            maybe_string = Some(keys);
                        }
                        TypeKind::Primitive(Primitive::Symbol) => {
                            maybe_symbol = Some(*t);
                        }
                        _ => (),
                    }
                }

                let mut all_keys: Vec<Index> = vec![];

                match maybe_number {
                    Some(number) => all_keys.push(number),
                    None => {
                        all_keys.append(&mut number_keys.values().cloned().collect::<Vec<Index>>())
                    }
                }

                match maybe_string {
                    Some(string) => all_keys.push(string),
                    None => {
                        all_keys.append(&mut string_keys.values().cloned().collect::<Vec<Index>>())
                    }
                }

                if let Some(symbol) = maybe_symbol {
                    all_keys.push(symbol);
                }

                Ok(new_union_type(&mut self.arena, &all_keys))
            }
            TypeKind::Union(_) => Ok(new_keyword(&mut self.arena, Keyword::Never)),
            TypeKind::Keyword(keyword) => match keyword {
                // TODO: update get_property to handle these cases as well
                Keyword::Null => Ok(new_keyword(&mut self.arena, Keyword::Never)),
                Keyword::Undefined => Ok(new_keyword(&mut self.arena, Keyword::Never)),
                Keyword::Unknown => Ok(new_keyword(&mut self.arena, Keyword::Never)),
                Keyword::Never => {
                    let string = new_primitive(&mut self.arena, Primitive::String);
                    let number = new_primitive(&mut self.arena, Primitive::Number);
                    let symbol = new_primitive(&mut self.arena, Primitive::Symbol);
                    Ok(new_union_type(&mut self.arena, &[string, number, symbol]))
                }
            },
            TypeKind::Primitive(primitive) => {
                let scheme_name = primitive.get_scheme_name();
                let idx = self.expand_alias_by_name(ctx, scheme_name, &[])?;
                self.expand_keyof(ctx, idx)
            }
            TypeKind::Literal(literal) => match literal.get_scheme_name() {
                Some(name) => {
                    let idx = self.expand_alias_by_name(ctx, name, &[])?;
                    self.expand_keyof(ctx, idx)
                }
                None => todo!(),
            },
            _ => {
                let expanded_t = self.expand_type(ctx, t)?;
                if expanded_t != t {
                    self.expand_keyof(ctx, expanded_t)
                } else {
                    Ok(new_keyword(&mut self.arena, Keyword::Never))
                }
            }
        }
    }

    // TODO: have a separate version of this for expanding conditional types that
    // are the definition of a type alias.  In that situation, if the `check` is
    // a type reference and the arg passed to the type alias is a union, then we
    // have distribute the union.
    pub fn expand_conditional(
        &mut self,
        ctx: &Context,
        conditional: &Conditional,
    ) -> Result<Index, Errors> {
        let Conditional {
            check,
            extends,
            true_type,
            false_type,
        } = conditional;

        let infer_types = find_infer_types(&mut self.arena, extends);

        let mut type_param_map: HashMap<String, Index> = HashMap::new();
        for infer_t in infer_types {
            type_param_map.insert(infer_t.name.to_owned(), new_var_type(&mut self.arena, None));
        }

        let extends = replace_infer_types(&mut self.arena, extends, &type_param_map);

        match self.unify(ctx, *check, extends) {
            Ok(_) => {
                let true_type = instantiate_scheme(&mut self.arena, *true_type, &type_param_map);
                Ok(true_type)
            }
            Err(_) => Ok(*false_type),
        }
    }

    pub fn expand_binary(&mut self, _ctx: &Context, binary: &BinaryT) -> Result<Index, Errors> {
        let t = match (
            &self.arena[binary.left].kind,
            &self.arena[binary.right].kind,
        ) {
            (
                TypeKind::Literal(Literal::Number(left)),
                TypeKind::Literal(Literal::Number(right)),
            ) => {
                let left = left.parse::<f64>().unwrap();
                let right = right.parse::<f64>().unwrap();

                let result = match binary.op {
                    TBinaryOp::Add => left + right,
                    TBinaryOp::Sub => left - right,
                    TBinaryOp::Mul => left * right,
                    TBinaryOp::Div => left / right,
                    TBinaryOp::Mod => left % right,
                };

                new_lit_type(&mut self.arena, &Literal::Number(result.to_string()))
            }
            (TypeKind::Literal(Literal::Number(_)), TypeKind::Primitive(Primitive::Number)) => {
                new_primitive(&mut self.arena, Primitive::Number)
            }
            (TypeKind::Primitive(Primitive::Number), TypeKind::Literal(Literal::Number(_))) => {
                new_primitive(&mut self.arena, Primitive::Number)
            }
            (TypeKind::Primitive(Primitive::Number), TypeKind::Primitive(Primitive::Number)) => {
                new_primitive(&mut self.arena, Primitive::Number)
            }
            (_, _) => {
                return Err(Errors::InferenceError(format!(
                    "Cannot perform binary operation on types: {:?} and {:?}",
                    self.print_type(&binary.left),
                    self.print_type(&binary.right),
                )));
            }
        };

        Ok(t)
    }

    pub fn expand_object(&mut self, ctx: &Context, object: &Object) -> Result<Index, Errors> {
        let mut new_elems = vec![];

        for elem in &object.elems {
            match elem {
                // TODO: Handle `check` and `extends`
                TObjElem::Mapped(mapped) => {
                    let source = self.expand_type(ctx, mapped.source)?;

                    match &self.arena[source].kind.clone() {
                        TypeKind::Union(Union { types }) => {
                            let mut non_literal_keys = vec![];

                            for t in types {
                                let mut mapping: HashMap<String, Index> = HashMap::new();
                                mapping.insert(mapped.target.to_owned(), *t);
                                let key = instantiate_scheme(&mut self.arena, mapped.key, &mapping);
                                let value =
                                    instantiate_scheme(&mut self.arena, mapped.value, &mapping);

                                let name = match &self.arena[key].kind {
                                    TypeKind::Literal(Literal::String(name)) => {
                                        TPropKey::StringKey(name.to_owned())
                                    }
                                    TypeKind::Literal(Literal::Number(name)) => {
                                        TPropKey::NumberKey(name.to_owned())
                                    }
                                    _ => {
                                        non_literal_keys.push(key);
                                        continue;
                                    }
                                };

                                new_elems.push(TObjElem::Prop(TProp {
                                    name,
                                    modifier: None,
                                    optional: false, // TODO
                                    mutable: false,  // TODO
                                    t: self.expand_type(ctx, value)?,
                                }));

                                if !non_literal_keys.is_empty() {
                                    let union = new_union_type(&mut self.arena, &non_literal_keys);
                                    new_elems.push(TObjElem::Mapped(MappedType {
                                        target: mapped.target.to_owned(),
                                        key: union,
                                        value: mapped.value,
                                        source: mapped.source,
                                        check: mapped.check,
                                        extends: mapped.extends,
                                    }));
                                }
                            }
                        }
                        _ => {
                            new_elems.push(TObjElem::Mapped(mapped.to_owned()));
                        }
                    }
                }
                _ => new_elems.push(elem.to_owned()),
            }
        }

        Ok(self.arena.insert(Type {
            kind: TypeKind::Object(Object { elems: new_elems }),
            provenance: None, // TODO
        }))
    }

    pub fn get_computed_member(
        &mut self,
        ctx: &Context,
        obj_idx: Index,
        key_idx: Index,
    ) -> Result<Index, Errors> {
        // NOTE: cloning is fine here because we aren't mutating `obj_type` or
        // `prop_type`.
        let obj_type = self.arena[obj_idx].clone();
        let key_type = self.arena[key_idx].clone();

        match &obj_type.kind {
            TypeKind::Object(_) => self.get_prop(ctx, obj_idx, key_idx),
            // let tuple = [5, "hello", true]
            // tuple[1]; // "hello"
            TypeKind::Tuple(tuple) => {
                match &key_type.kind {
                    TypeKind::Literal(Literal::Number(value)) => {
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
                    TypeKind::Literal(Literal::String(_)) => {
                        // TODO: look up methods on the `Array` interface
                        // we need to instantiate the scheme such that `T` is equal
                        // to the union of all types in the tuple
                        self.get_prop(ctx, obj_idx, key_idx)
                    }
                    TypeKind::Primitive(Primitive::Number) => {
                        let mut types = tuple.types.clone();
                        types.push(new_keyword(&mut self.arena, Keyword::Undefined));
                        Ok(new_union_type(&mut self.arena, &types))
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
                    match self.get_computed_member(ctx, *idx, key_idx) {
                        Ok(t) => result_types.push(t),
                        Err(_) => {
                            // TODO: check what the error is, we may want to propagate
                            // certain errors
                            if undefined_count == 0 {
                                let undefined = new_keyword(&mut self.arena, Keyword::Undefined);
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
                    Ok(new_union_type(&mut self.arena, &result_types))
                }
            }
            TypeKind::Constructor(Constructor { name, types, .. }) => {
                let idx = self.expand_alias_by_name(ctx, name, types)?;
                self.get_computed_member(ctx, idx, key_idx)
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
        &mut self,
        ctx: &Context,
        obj_idx: Index,
        key_idx: Index,
    ) -> Result<Index, Errors> {
        let undefined = new_keyword(&mut self.arena, Keyword::Undefined);
        // It's fine to clone here because we aren't mutating
        let obj_type = self.arena[obj_idx].clone();
        let key_type = self.arena[key_idx].clone();

        if let TypeKind::Object(object) = &obj_type.kind {
            match &key_type.kind {
                TypeKind::Primitive(primitive)
                    if primitive == &Primitive::Number
                        || primitive == &Primitive::String
                        || primitive == &Primitive::Symbol =>
                {
                    let mut maybe_mapped: Option<&MappedType> = None;
                    let mut values: Vec<Index> = vec![];

                    for elem in &object.elems {
                        match elem {
                            // Callable signatures have no name so we ignore them.
                            TObjElem::Constructor(_) => continue,
                            TObjElem::Call(_) => continue,
                            TObjElem::Mapped(mapped) => {
                                if maybe_mapped.is_some() {
                                    return Err(Errors::InferenceError(
                                        "Object types can only have a single mapped signature"
                                            .to_string(),
                                    ));
                                }
                                maybe_mapped = Some(mapped);
                            }
                            TObjElem::Prop(prop) => {
                                match &prop.name {
                                    TPropKey::StringKey(_) if primitive == &Primitive::String => (),
                                    TPropKey::NumberKey(_) if primitive == &Primitive::Number => (),
                                    _ => continue,
                                };

                                let prop_t = match prop.optional {
                                    true => new_union_type(&mut self.arena, &[prop.t, undefined]),
                                    false => prop.t,
                                };
                                values.push(prop_t);
                            }
                        }
                    }

                    // TODO: handle combinations of indexers and non-indexer elements
                    if let Some(mapped) = maybe_mapped {
                        let mapped_key = get_mapped_key(&mut self.arena, mapped);

                        match self.unify(ctx, key_idx, mapped_key) {
                            Ok(_) => {
                                let undefined = new_keyword(&mut self.arena, Keyword::Undefined);
                                Ok(new_union_type(&mut self.arena, &[mapped.value, undefined]))
                            }
                            Err(_) => Err(Errors::InferenceError(format!(
                                "{} is not a valid indexer for {}",
                                key_type.as_string(&self.arena),
                                obj_type.as_string(&self.arena)
                            ))),
                        }
                    } else if !values.is_empty() {
                        values.push(undefined);
                        Ok(new_union_type(&mut self.arena, &values))
                    } else {
                        Err(Errors::InferenceError(format!(
                            "{} has no indexer",
                            obj_type.as_string(&self.arena)
                        )))
                    }
                }
                TypeKind::Literal(Literal::String(name)) => {
                    let mut maybe_mapped: Option<&MappedType> = None;
                    for elem in &object.elems {
                        match elem {
                            // Callable signatures have no name so we ignore them.
                            TObjElem::Constructor(_) => continue,
                            TObjElem::Call(_) => continue,
                            TObjElem::Mapped(mapped) => {
                                if maybe_mapped.is_some() {
                                    return Err(Errors::InferenceError(
                                        "Object types can only have a single mapped signature"
                                            .to_string(),
                                    ));
                                }
                                maybe_mapped = Some(mapped);
                            }
                            TObjElem::Prop(prop) => {
                                let key = match &prop.name {
                                    TPropKey::StringKey(key) => key,
                                    TPropKey::NumberKey(key) => key,
                                };
                                if key == name {
                                    let prop_t = match prop.optional {
                                        true => {
                                            new_union_type(&mut self.arena, &[prop.t, undefined])
                                        }
                                        false => prop.t,
                                    };
                                    return Ok(prop_t);
                                }
                            }
                        }
                    }

                    if let Some(mapped) = maybe_mapped {
                        let mapped_key = get_mapped_key(&mut self.arena, mapped);

                        match self.unify(ctx, key_idx, mapped_key) {
                            Ok(_) => {
                                let undefined = new_keyword(&mut self.arena, Keyword::Undefined);
                                Ok(new_union_type(&mut self.arena, &[mapped.value, undefined]))
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
                TypeKind::Literal(Literal::Number(name)) => {
                    let mut maybe_mapped: Option<&MappedType> = None;
                    for elem in &object.elems {
                        if let TObjElem::Mapped(mapped) = elem {
                            if maybe_mapped.is_some() {
                                return Err(Errors::InferenceError(
                                    "Object types can only have a single mapped signature"
                                        .to_string(),
                                ));
                            }
                            maybe_mapped = Some(mapped);
                        }
                        // QUESTION: Do we care about allowing numbers as keys for
                        // methods and props?
                    }

                    if let Some(mapped) = maybe_mapped {
                        let mapped_key = get_mapped_key(&mut self.arena, mapped);
                        match self.unify(ctx, key_idx, mapped_key) {
                            Ok(_) => {
                                let undefined = new_keyword(&mut self.arena, Keyword::Undefined);
                                Ok(new_union_type(&mut self.arena, &[mapped.value, undefined]))
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
                    self.print_type(&key_idx)
                ))),
            }
        } else {
            Err(Errors::InferenceError(
                "Can't access property on non-object type".to_string(),
            ))
        }
    }
}

pub fn filter_nullables(arena: &Arena<Type>, types: &[Index]) -> Vec<Index> {
    types
        .iter()
        .filter(|t| {
            !matches!(&arena[**t].kind, TypeKind::Keyword(Keyword::Undefined))
                && !matches!(&arena[**t].kind, TypeKind::Keyword(Keyword::Null))
        })
        .cloned()
        .collect()
}

fn get_mapped_key(arena: &mut Arena<Type>, mapped: &MappedType) -> Index {
    let mut mapping: HashMap<String, Index> = HashMap::new();
    mapping.insert(mapped.target.to_owned(), mapped.source);
    instantiate_scheme(arena, mapped.key, &mapping)
}

pub struct FindInferVisitor<'a> {
    pub arena: &'a mut Arena<Type>,
    pub infer_types: Vec<Infer>,
}

impl<'a> KeyValueStore<Index, Type> for FindInferVisitor<'a> {
    // NOTE: The reason we return both an Index and a Type is that
    // this method calls `prune` which maybe return a different Index
    // from the one passed to it. We need to ensure this method returns
    // an Index that corresponds to the returned Type.
    fn get_type(&mut self, idx: &Index) -> Type {
        self.arena[*idx].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Visitor for FindInferVisitor<'a> {
    fn visit_index(&mut self, index: &Index) {
        let t = self.get_type(index);
        match &t.kind {
            TypeKind::Infer(infer) => {
                self.infer_types.push(infer.to_owned());
            }
            _ => {
                visitor::walk_index(self, index);
            }
        }
    }
}

pub fn find_infer_types(arena: &mut Arena<Type>, t: &Index) -> Vec<Infer> {
    let mut replace_visitor = FindInferVisitor {
        arena,
        infer_types: vec![],
    };

    replace_visitor.visit_index(t);

    replace_visitor.infer_types
}

pub struct ReplaceVisitor<'a> {
    pub arena: &'a mut Arena<Type>,
    pub mapping: &'a std::collections::HashMap<String, Index>,
}

impl<'a> KeyValueStore<Index, Type> for ReplaceVisitor<'a> {
    fn get_type(&mut self, idx: &Index) -> Type {
        self.arena[*idx].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Folder for ReplaceVisitor<'a> {
    fn fold_index(&mut self, index: &Index) -> Index {
        let t = self.get_type(index);

        match &t.kind {
            TypeKind::Infer(infer) => match self.mapping.get(&infer.name) {
                Some(index) => *index,
                None => *index,
            },
            _ => walk_index(self, index),
        }
    }
}

pub fn replace_infer_types(
    arena: &mut Arena<Type>,
    t: &Index,
    mapping: &std::collections::HashMap<String, Index>,
) -> Index {
    let mut replace_visitor = ReplaceVisitor { arena, mapping };

    replace_visitor.fold_index(t)
}
