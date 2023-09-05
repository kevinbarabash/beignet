use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};
use std::mem::transmute;

use escalier_ast::Literal;

use crate::checker::Checker;
use crate::context::*;
use crate::folder::walk_index;
use crate::folder::Folder;
use crate::key_value_store::KeyValueStore;
use crate::type_error::TypeError;
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
            TypeKind::TypeVar(_) => false,   // leaf node
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
            TypeKind::Array(Array { t }) => self.occurs_in_type(v, t),
            TypeKind::TypeRef(TypeRef { types, .. }) => self.occurs_in(v, &types),
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
            TypeKind::TypeVar(TypeVar {
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
            TypeKind::TypeVar(TypeVar {
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
        &mut self,
        ctx: &Context,
        name: &str,
        type_args: &[Index],
    ) -> Result<Index, TypeError> {
        let scheme = ctx.get_scheme(name)?;

        match &scheme.type_params {
            Some(type_params) => {
                if type_params.len() != type_args.len() {
                    return Err(TypeError {
                        message: format!(
                            "{name} expects {} type args, but was passed {}",
                            type_params.len(),
                            type_args.len()
                        ),
                    });
                }

                if let TypeKind::Conditional(Conditional { check, .. }) = self.arena[scheme.t].kind
                {
                    // We're not mutating `kind` so this should be safe.
                    let check_kind: &TypeKind = unsafe { transmute(&self.arena[check].kind) };
                    if let TypeKind::TypeRef(tref) = check_kind {
                        if let Some((index_of_check_type, _)) = type_params
                            .iter()
                            .find_position(|type_param| type_param.name == tref.name)
                        {
                            let type_arg = self.expand_type(ctx, type_args[index_of_check_type])?;
                            // We're not mutating `kind` so this should be safe.
                            let type_arg_kind: &TypeKind =
                                unsafe { transmute(&self.arena[type_arg].kind) };
                            if let TypeKind::Union(Union { types: union_types }) = type_arg_kind {
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

                                    let t = self.instantiate_type(&scheme.t, &mapping);
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

                                let t = self.new_union_type(&filtered_types);
                                return self.expand_type(ctx, t);
                            }
                        }
                    }
                }

                // Contraints can reference other type params so we need make
                // sure that definitions for each type param are in scope where
                // each type param is defined to be the corresponding type arg.
                let mut sig_ctx = ctx.clone();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    sig_ctx.schemes.insert(
                        param.name.clone(),
                        Scheme {
                            type_params: None,
                            t: *arg,
                        },
                    );
                }

                let mut mapping: HashMap<String, Index> = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    if let Some(constraint) = param.constraint {
                        self.unify(&sig_ctx, *arg, constraint)?;
                    }
                    mapping.insert(param.name.clone(), arg.to_owned());
                }

                let t = self.instantiate_type(&scheme.t, &mapping);
                self.expand_type(&sig_ctx, t)
            }
            None => {
                if type_args.is_empty() {
                    let t = self.expand_type(ctx, scheme.t)?;
                    Ok(t)
                } else {
                    Err(TypeError {
                        message: format!("{name} doesn't require any type args"),
                    })
                }
            }
        }
    }

    pub fn expand_type(&mut self, ctx: &Context, t: Index) -> Result<Index, TypeError> {
        let t = self.prune(t);

        // It's okay to clone here because we aren't mutating the type
        let t = match &self.arena[t].clone().kind {
            TypeKind::KeyOf(KeyOf { t }) => self.expand_keyof(ctx, *t)?,
            // TODO: Readonly<T> should remove mutating methods
            // TODO: IndexedAccess["key"] should remove the `self` param from methods
            TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
                let is_mut = true;
                self.get_computed_member(ctx, *obj, *index, is_mut)?
            }
            TypeKind::Conditional(conditional) => self.expand_conditional(ctx, conditional)?,
            TypeKind::TypeRef(TypeRef { name, types, .. }) => {
                self.expand_alias(ctx, name, types)?
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
    pub fn expand_keyof(&mut self, ctx: &Context, t: Index) -> Result<Index, TypeError> {
        let obj = self.expand_type(ctx, t)?;
        // We're not mutating `kind` so this should be safe.
        let obj_kind: &TypeKind = unsafe { transmute(&self.arena[obj].kind) };
        match obj_kind {
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
                            let mapped_key = get_mapped_key(self, mapped);

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
                                string_keys
                                    .push(self.new_lit_type(&Literal::String(name.to_owned())));
                            }
                            TPropKey::NumberKey(name) => {
                                number_keys
                                    .push(self.new_lit_type(&Literal::Number(name.to_owned())));
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

                Ok(self.new_union_type(&all_keys))
            }
            // NOTE: The behavior of `keyof` with arrays and tuples differs from
            // TypeScript.  TypeScript includes the names of all the properties from
            // Array.prototype as well.
            // TODO(#637): Have interop layer translate between Escalier's and
            // TypeScript's `keyof` utility type.
            TypeKind::Array(_) => Ok(self.new_primitive(Primitive::Number)),
            TypeKind::Tuple(tuple) => {
                let keys: Vec<Index> = tuple
                    .types
                    .iter()
                    .enumerate()
                    .map(|(k, _)| self.new_lit_type(&Literal::Number(k.to_string())))
                    .collect();
                Ok(self.new_union_type(&keys))
            }
            TypeKind::TypeRef(constructor) => {
                let idx = self.expand_alias(ctx, &constructor.name, &constructor.types)?;
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

                Ok(self.new_union_type(&all_keys))
            }
            TypeKind::Union(_) => Ok(self.new_keyword(Keyword::Never)),
            TypeKind::Keyword(keyword) => match keyword {
                // TODO: update get_property to handle these cases as well
                Keyword::Null => Ok(self.new_keyword(Keyword::Never)),
                Keyword::Never => {
                    let string = self.new_primitive(Primitive::String);
                    let number = self.new_primitive(Primitive::Number);
                    let symbol = self.new_primitive(Primitive::Symbol);
                    Ok(self.new_union_type(&[string, number, symbol]))
                }
                Keyword::Object => Ok(self.new_keyword(Keyword::Object)),
                Keyword::Undefined => Ok(self.new_keyword(Keyword::Never)),
                Keyword::Unknown => Ok(self.new_keyword(Keyword::Never)),
            },
            TypeKind::Primitive(primitive) => {
                let name = primitive.get_scheme_name();
                let idx = self.expand_alias(ctx, name, &[])?;
                self.expand_keyof(ctx, idx)
            }
            TypeKind::Literal(literal) => match literal.get_scheme_name() {
                Some(name) => {
                    let idx = self.expand_alias(ctx, name, &[])?;
                    self.expand_keyof(ctx, idx)
                }
                None => todo!(),
            },
            _ => {
                let expanded_t = self.expand_type(ctx, t)?;
                if expanded_t != t {
                    self.expand_keyof(ctx, expanded_t)
                } else {
                    Ok(self.new_keyword(Keyword::Never))
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
    ) -> Result<Index, TypeError> {
        let Conditional {
            check,
            extends,
            true_type,
            false_type,
        } = conditional;

        let infer_types = find_infer_types(&mut self.arena, extends);

        let mut type_param_map: HashMap<String, Index> = HashMap::new();
        for infer_t in infer_types {
            type_param_map.insert(infer_t.name.to_owned(), self.new_type_var(None));
        }

        let extends = replace_infer_types(&mut self.arena, extends, &type_param_map);

        match self.unify(ctx, *check, extends) {
            Ok(_) => {
                let true_type = self.instantiate_type(true_type, &type_param_map);
                Ok(true_type)
            }
            Err(_) => Ok(*false_type),
        }
    }

    pub fn expand_binary(&mut self, _ctx: &Context, binary: &BinaryT) -> Result<Index, TypeError> {
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

                self.new_lit_type(&Literal::Number(result.to_string()))
            }
            (TypeKind::Literal(Literal::Number(_)), TypeKind::Primitive(Primitive::Number)) => {
                self.new_primitive(Primitive::Number)
            }
            (TypeKind::Primitive(Primitive::Number), TypeKind::Literal(Literal::Number(_))) => {
                self.new_primitive(Primitive::Number)
            }
            (TypeKind::Primitive(Primitive::Number), TypeKind::Primitive(Primitive::Number)) => {
                self.new_primitive(Primitive::Number)
            }
            (_, _) => {
                return Err(TypeError {
                    message: format!(
                        "Cannot perform binary operation on types: {:?} and {:?}",
                        self.print_type(&binary.left),
                        self.print_type(&binary.right),
                    ),
                });
            }
        };

        Ok(t)
    }

    pub fn expand_object(&mut self, ctx: &Context, object: &Object) -> Result<Index, TypeError> {
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
                                let key = self.instantiate_type(&mapped.key, &mapping);

                                let mut value = self.instantiate_type(&mapped.value, &mapping);

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

                                let mut optional = false;

                                // The mapped type's `value` is looks like T[P]
                                // and `P` is the key type we need to copy the
                                // optionality and readonlyness from from the
                                // object type.
                                // TODO: check for `T[P]` anywhere within mapped.value
                                if let TypeKind::IndexedAccess(IndexedAccess { obj, index }) =
                                    &self.arena[mapped.value].kind
                                {
                                    if !self.equals(index, &mapped.key) {
                                        continue;
                                    }

                                    let obj = self.expand_type(ctx, *obj)?;

                                    if let TypeKind::Object(Object { elems }) =
                                        &self.arena[obj].kind
                                    {
                                        for elem in elems {
                                            if let TObjElem::Prop(prop) = elem {
                                                if prop.name == name {
                                                    optional = prop.optional;
                                                    // TODO: use mapped.optional to
                                                    // mimic TypeScript's behavior
                                                    // where optional fields are
                                                    // given type `T | undefined`
                                                    value = prop.t;
                                                    eprintln!("prop = {prop:#?}");
                                                }
                                            }
                                        }
                                    }

                                    // TODO: we really need to rethink where we
                                    // actually need to track mutability.  For
                                    // example when we call `typeof foo` we should
                                    // get back different types depending on whether
                                    // or not `foo` is mutable.  This also means
                                    // that Readonly<Instance> should return a type
                                    // with all of the mutating methods removed.
                                    // Or maybe we do the reverse where `Instance`
                                    // has the mutating methods filtered and then
                                    // `Mutable<Instance>` has all of the methods.
                                }

                                if let Some(mode) = &mapped.optional {
                                    match mode {
                                        MappedModifier::Add => optional = true,
                                        MappedModifier::Remove => optional = false,
                                    }
                                }

                                new_elems.push(TObjElem::Prop(TProp {
                                    name,
                                    modifier: None,
                                    optional,
                                    mutable: false, // TODO
                                    t: self.expand_type(ctx, value)?,
                                }));
                            }

                            if !non_literal_keys.is_empty() {
                                let union = self.new_union_type(&non_literal_keys);
                                new_elems.push(TObjElem::Mapped(MappedType {
                                    target: mapped.target.to_owned(),
                                    key: union,
                                    value: mapped.value,
                                    source: mapped.source,
                                    optional: mapped.optional.to_owned(),
                                    check: mapped.check,
                                    extends: mapped.extends,
                                }));
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
        is_mut: bool,
    ) -> Result<Index, TypeError> {
        // NOTE: cloning is fine here because we aren't mutating `obj_type` or
        // `prop_type`.
        let obj_type = self.arena[obj_idx].clone();
        let key_type = self.arena[key_idx].clone();

        match &obj_type.kind {
            TypeKind::Object(_) => self.get_prop_value(ctx, obj_idx, key_idx, is_mut),
            TypeKind::Array(array) => {
                match &key_type.kind {
                    TypeKind::Literal(Literal::Number(_)) => {
                        // TODO: update AST with the inferred type
                        let types = vec![array.t, self.new_keyword(Keyword::Undefined)];
                        Ok(self.new_union_type(&types))
                    }
                    TypeKind::Literal(Literal::String(_)) => {
                        // TODO: look up methods on the `Array` interface
                        // we need to instantiate the scheme such that `T` is equal
                        // to the union of all types in the tuple
                        self.get_prop_value(ctx, obj_idx, key_idx, is_mut)
                    }
                    TypeKind::Primitive(Primitive::Number) => {
                        let types = vec![array.t, self.new_keyword(Keyword::Undefined)];
                        Ok(self.new_union_type(&types))
                    }
                    _ => Err(TypeError {
                        message: "Can only access tuple properties with a number".to_string(),
                    }),
                }
            }
            TypeKind::Tuple(tuple) => {
                match &key_type.kind {
                    TypeKind::Literal(Literal::Number(value)) => {
                        let index: usize = str::parse(value).map_err(|_| TypeError {
                            message: format!("{} isn't a valid index", value),
                        })?;
                        if index < tuple.types.len() {
                            // TODO: update AST with the inferred type
                            return Ok(tuple.types[index]);
                        }
                        Err(TypeError {
                            message: format!(
                                "{index} was outside the bounds 0..{} of the tuple",
                                tuple.types.len()
                            ),
                        })
                    }
                    TypeKind::Literal(Literal::String(_)) => {
                        // TODO: look up methods on the `Array` interface
                        // we need to instantiate the scheme such that `T` is equal
                        // to the union of all types in the tuple
                        self.get_prop_value(ctx, obj_idx, key_idx, is_mut)
                    }
                    TypeKind::Primitive(Primitive::Number) => {
                        let mut types = tuple.types.clone();
                        types.push(self.new_keyword(Keyword::Undefined));
                        Ok(self.new_union_type(&types))
                    }
                    _ => Err(TypeError {
                        message: "Can only access tuple properties with a number".to_string(),
                    }),
                }
            }
            // declare let tuple: [number, number] | [string, string]
            // tuple[1]; // number | string
            TypeKind::Union(union) => {
                let mut result_types = vec![];
                let mut undefined_count = 0;
                for idx in &union.types {
                    match self.get_computed_member(ctx, *idx, key_idx, is_mut) {
                        Ok(t) => result_types.push(t),
                        Err(_) => {
                            // TODO: check what the error is, we may want to propagate
                            // certain errors
                            if undefined_count == 0 {
                                let undefined = self.new_keyword(Keyword::Undefined);
                                result_types.push(undefined);
                            }
                            undefined_count += 1;
                        }
                    }
                }
                if undefined_count == union.types.len() {
                    // TODO: include name of property in error message
                    Err(TypeError {
                        message: "Couldn't find property on object".to_string(),
                    })
                } else {
                    Ok(self.new_union_type(&result_types))
                }
            }
            TypeKind::TypeRef(TypeRef { name, types, .. }) => {
                let idx = self.expand_alias(ctx, name, types)?;
                self.get_computed_member(ctx, idx, key_idx, is_mut)
            }
            _ => {
                // TODO: provide a more specific error message for type variables
                Err(TypeError {
                    message: "Can only access properties on objects/tuples".to_string(),
                })
            }
        }
    }

    // TODO(#624) - to behave differently when used to look up an lvalue vs a rvalue
    pub fn get_prop_value(
        &mut self,
        ctx: &Context,
        obj_idx: Index,
        key_idx: Index,
        is_mut: bool,
    ) -> Result<Index, TypeError> {
        let undefined = self.new_keyword(Keyword::Undefined);
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
                                    return Err(TypeError {
                                        message:
                                            "Object types can only have a single mapped signature"
                                                .to_string(),
                                    });
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
                                    true => self.new_union_type(&[prop.t, undefined]),
                                    false => prop.t,
                                };
                                values.push(prop_t);
                            }
                        }
                    }

                    // TODO: handle combinations of indexers and non-indexer elements
                    if let Some(mapped) = maybe_mapped {
                        let mapped_key = get_mapped_key(self, mapped);

                        match self.unify(ctx, key_idx, mapped_key) {
                            Ok(_) => {
                                let undefined = self.new_keyword(Keyword::Undefined);
                                Ok(self.new_union_type(&[mapped.value, undefined]))
                            }
                            Err(_) => Err(TypeError {
                                message: format!(
                                    "{} is not a valid indexer for {}",
                                    self.print_type(&key_idx),
                                    self.print_type(&obj_idx),
                                ),
                            }),
                        }
                    } else if !values.is_empty() {
                        values.push(undefined);
                        Ok(self.new_union_type(&values))
                    } else {
                        Err(TypeError {
                            message: format!("{} has no indexer", self.print_type(&obj_idx),),
                        })
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
                                    return Err(TypeError {
                                        message:
                                            "Object types can only have a single mapped signature"
                                                .to_string(),
                                    });
                                }
                                maybe_mapped = Some(mapped);
                            }
                            TObjElem::Prop(prop) => {
                                let key = match &prop.name {
                                    TPropKey::StringKey(key) => key,
                                    TPropKey::NumberKey(key) => key,
                                };
                                if key == name {
                                    if let TypeKind::Function(Function { params, .. }) =
                                        &self.arena[prop.t].kind
                                    {
                                        if let Some(param) = params.first() {
                                            if param.is_mut_self() && !is_mut {
                                                return Err(TypeError {
                                                    message: format!(
                                                        "Cannot call mutating method {} on a non-mutable object",
                                                        name,
                                                    ),
                                                });
                                            }
                                        }
                                    }

                                    let prop_t = match prop.optional {
                                        true => self.new_union_type(&[prop.t, undefined]),
                                        false => prop.t,
                                    };
                                    return Ok(prop_t);
                                }
                            }
                        }
                    }

                    if let Some(mapped) = maybe_mapped {
                        let mapped_key = get_mapped_key(self, mapped);

                        match self.unify(ctx, key_idx, mapped_key) {
                            Ok(_) => {
                                let undefined = self.new_keyword(Keyword::Undefined);
                                Ok(self.new_union_type(&[mapped.value, undefined]))
                            }
                            Err(_) => Err(TypeError {
                                message: format!("Couldn't find property {} in object", name,),
                            }),
                        }
                    } else {
                        Err(TypeError {
                            message: format!("Couldn't find property '{name}' on object",),
                        })
                    }
                }
                TypeKind::Literal(Literal::Number(name)) => {
                    let mut maybe_mapped: Option<&MappedType> = None;
                    for elem in &object.elems {
                        if let TObjElem::Mapped(mapped) = elem {
                            if maybe_mapped.is_some() {
                                return Err(TypeError {
                                    message: "Object types can only have a single mapped signature"
                                        .to_string(),
                                });
                            }
                            maybe_mapped = Some(mapped);
                        }
                        // QUESTION: Do we care about allowing numbers as keys for
                        // methods and props?
                    }

                    if let Some(mapped) = maybe_mapped {
                        let mapped_key = get_mapped_key(self, mapped);
                        match self.unify(ctx, key_idx, mapped_key) {
                            Ok(_) => {
                                let undefined = self.new_keyword(Keyword::Undefined);
                                Ok(self.new_union_type(&[mapped.value, undefined]))
                            }
                            Err(_) => Err(TypeError {
                                message: format!("Couldn't find property {} in object", name,),
                            }),
                        }
                    } else {
                        Err(TypeError {
                            message: format!("Couldn't find property '{name}' on object",),
                        })
                    }
                }
                _ => Err(TypeError {
                    message: format!("{} is not a valid key", self.print_type(&key_idx)),
                }),
            }
        } else {
            Err(TypeError {
                message: "Can't access property on non-object type".to_string(),
            })
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

fn get_mapped_key(checker: &mut Checker, mapped: &MappedType) -> Index {
    let mut mapping: HashMap<String, Index> = HashMap::new();
    mapping.insert(mapped.target.to_owned(), mapped.source);
    checker.instantiate_type(&mapped.key, &mapping)
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
