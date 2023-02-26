use escalier_ast::types::*;
use im::hashmap::HashMap;
use std::cell::Cell;

use crate::assump::Assump;
use crate::scheme::{generalize, instantiate, Scheme};
use crate::type_error::TypeError;
use crate::util::immutable_obj_type;

pub type Env = HashMap<String, Scheme>;

#[derive(Clone, Debug)]
pub struct Binding {
    pub mutable: bool,
    pub t: Type,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub values: HashMap<String, Binding>,
    pub types: Env,
    pub is_async: bool,
    pub count: Cell<i32>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            values: HashMap::default(),
            types: HashMap::default(),
            is_async: false,
            count: Cell::from(0),
        }
    }
}

impl Context {
    pub fn is_async(&self) -> bool {
        self.is_async
    }

    pub fn get_all_types(&self) -> Env {
        self.types.to_owned()
    }

    pub fn insert_value(&mut self, name: String, t: Type) {
        self.values.insert(name, Binding { mutable: false, t });
    }

    pub fn insert_binding(&mut self, name: String, b: Binding) {
        self.values.insert(name, b);
    }

    pub fn insert_bindings(&mut self, a: &Assump) {
        for (name, binding) in a {
            self.insert_binding(name.to_owned(), binding.to_owned());
        }
    }

    // TODO: determine if we need to generalize the inserted type everywhere
    // this is being called.
    pub fn insert_type(&mut self, name: String, t: Type) {
        let scheme = generalize(&self.types, &t);
        self.types.insert(name, scheme);
    }

    pub fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        self.types.insert(name, scheme);
    }

    // TODO: update to instantiate Lam
    pub fn lookup_value_and_instantiate(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        if let Some(binding) = self.values.get(name) {
            return Ok(binding.t.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    // TODO: dedupe this and lookup_value_and_instantiate since they do the same thing
    pub fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        if let Some(b) = self.values.get(name) {
            return Ok(b.t.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>> {
        if let Some(b) = self.values.get(name) {
            return Ok(b.to_owned());
        }
        eprintln!("lookup_binding failed");
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        if let Some(scheme) = self.types.get(name) {
            return Ok(scheme.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_type(&self, name: &str, mutable: bool) -> Result<Type, Vec<TypeError>> {
        let mut t = self._lookup_type(name)?;

        if !mutable {
            if let TypeKind::Object(obj) = &t.kind {
                if let Some(obj) = immutable_obj_type(obj) {
                    t = Type {
                        kind: TypeKind::Object(obj),
                        mutable: false,
                        provenance: t.provenance,
                    }
                }
            }
        }
        // TODO: convert mutable type to immutable type of `mutable` is true

        Ok(t)
    }

    pub fn _lookup_type(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        if let Some(scheme) = self.types.get(name) {
            return Ok(instantiate(self, scheme));
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn fresh_id(&self) -> i32 {
        let id = self.count.get() + 1;
        self.count.set(id);
        id
    }

    pub fn fresh_var(&self) -> Type {
        Type::from(TypeKind::Var(TVar {
            id: self.fresh_id(),
            constraint: None,
        }))
    }
}
