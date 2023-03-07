use escalier_ast::types::*;
use im::hashmap::HashMap;

use crate::scheme::{generalize, Scheme};
use crate::type_error::TypeError;

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
    pub fn insert_binding(&mut self, name: String, b: Binding) {
        self.values.insert(name, b);
    }

    pub fn insert_value(&mut self, name: String, t: Type) {
        self.insert_binding(name, Binding { mutable: false, t });
    }

    pub fn insert_type(&mut self, name: String, t: Type) {
        let scheme = generalize(&self.types, &t);
        self.insert_scheme(name, scheme);
    }

    pub fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        self.types.insert(name, scheme);
    }

    pub fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>> {
        if let Some(b) = self.values.get(name) {
            return Ok(b.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        let b = self.lookup_binding(name)?;
        Ok(b.t)
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        if let Some(scheme) = self.types.get(name) {
            return Ok(scheme.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }
}

impl Checker {
    pub fn lookup_type(&mut self, name: &str) -> Result<Type, Vec<TypeError>> {
        let scheme = self.lookup_scheme(name)?;
        let t = self.instantiate(&scheme);
        Ok(t)
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        self.current_scope.lookup_value(name)
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        self.current_scope.lookup_scheme(name)
    }
}
