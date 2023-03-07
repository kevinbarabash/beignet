use escalier_ast::types::*;
use im::hashmap::HashMap;

use crate::assump::Assump;
use crate::scheme::{generalize, Scheme};
use crate::type_error::TypeError;
use crate::util::immutable_obj_type;

use crate::checker::Checker;

pub type Env = HashMap<String, Scheme>;

#[derive(Clone, Debug)]
pub struct Binding {
    pub mutable: bool,
    pub t: Type,
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub values: HashMap<String, Binding>,
    pub types: Env,
    pub is_async: bool,
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
}

impl Checker {
    pub fn lookup_type(&mut self, name: &str, mutable: bool) -> Result<Type, Vec<TypeError>> {
        let mut t = if let Some(scheme) = self.current_scope.types.get(name) {
            self.instantiate(&scheme.clone())
        } else {
            return Err(vec![TypeError::CantFindIdent(name.to_owned())]);
        };

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

    pub fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        self.current_scope.lookup_value(name)
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        self.current_scope.lookup_scheme(name)
    }
}
