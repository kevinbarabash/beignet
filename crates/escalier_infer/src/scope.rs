use escalier_old_ast::types::*;
use im::hashmap::HashMap;

use crate::binding::Binding;
use crate::checker::Checker;
use crate::context::Context;
use crate::scheme::{generalize, Scheme};
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;

pub type Env = HashMap<String, Scheme>;

#[derive(Clone, Debug, Default)]
pub struct Scope {
    pub values: HashMap<String, Binding>,
    pub types: Env,
    pub is_async: bool,
}

// TODO: create `Context` trait which both `Scope` and `Checker` can implement.
impl Context for Scope {
    fn insert_binding(&mut self, name: String, b: Binding) {
        self.values.insert(name, b);
    }

    fn insert_value(&mut self, name: String, t: Type) {
        self.insert_binding(name, Binding { mutable: false, t });
    }

    fn insert_type(&mut self, name: String, t: Type, checker: &'_ mut Checker) {
        let scheme = generalize(&t, checker);
        self.insert_scheme(name, scheme);
    }

    fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        self.types.insert(name, scheme);
    }

    fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>> {
        if let Some(b) = self.values.get(name) {
            return Ok(b.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        let b = self.lookup_binding(name)?;
        Ok(b.t)
    }

    fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        if let Some(scheme) = self.types.get(name) {
            return Ok(scheme.to_owned());
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    fn apply(&mut self, s: &Subst, checker: &mut Checker) {
        // QUESTION: Do we need to update self.types as well?
        self.values = self.values.apply(s, checker);
    }
}
