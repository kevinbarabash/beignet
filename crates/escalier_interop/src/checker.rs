use escalier_ast::types::{TVar, Type, TypeKind};
use escalier_infer::{self, Scheme, Scope, TypeError};

#[derive(Default, Debug, Clone)]
pub struct Checker {
    pub current_scope: Scope,
    pub next_id: u32,
}

impl Checker {
    pub fn fresh_var(&mut self, constraint: Option<Box<Type>>) -> Type {
        let id = self.next_id;
        self.next_id = id + 1;
        Type::from(TypeKind::Var(TVar { id, constraint }))
    }

    pub fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        self.current_scope.insert_scheme(name, scheme);
    }

    pub fn insert_value(&mut self, name: String, t: Type) {
        self.current_scope.insert_value(name, t);
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        self.current_scope.lookup_scheme(name)
    }
}
