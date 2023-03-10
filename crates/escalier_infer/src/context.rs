use escalier_ast::types::*;

use crate::binding::Binding;
use crate::scheme::Scheme;
use crate::substitutable::Subst;
use crate::type_error::TypeError;

pub trait Context {
    fn apply(&mut self, s: &Subst);

    fn insert_binding(&mut self, name: String, b: Binding);
    fn insert_value(&mut self, name: String, t: Type);
    fn insert_type(&mut self, name: String, t: Type);
    fn insert_scheme(&mut self, name: String, scheme: Scheme);

    fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>>;
    fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>>;
    fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>>;
}
