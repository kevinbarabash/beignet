use escalier_ast::types::{TVar, Type, TypeKind};

use crate::binding::Binding;
use crate::context::Context;
use crate::diagnostic::Diagnostic;
use crate::scheme::Scheme;
use crate::scope::Scope;
use crate::substitutable::Subst;
use crate::type_error::TypeError;

pub struct Checker {
    pub next_id: u32,
    pub current_scope: Scope,
    pub parent_scopes: Vec<Scope>,
    pub diagnostics: Vec<Diagnostic>,
}

impl From<Scope> for Checker {
    fn from(scope: Scope) -> Self {
        Checker {
            next_id: 1,
            current_scope: scope,
            parent_scopes: vec![],
            diagnostics: vec![],
        }
    }
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            next_id: 1,
            current_scope: Scope::default(),
            parent_scopes: vec![],
            diagnostics: vec![],
        }
    }
}

impl Context for Checker {
    fn insert_binding(&mut self, name: String, b: Binding) {
        self.current_scope.insert_binding(name, b);
    }

    fn insert_value(&mut self, name: String, t: Type) {
        self.current_scope.insert_value(name, t);
    }

    fn insert_type(&mut self, name: String, t: Type) {
        self.current_scope.insert_type(name, t);
    }

    fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        self.current_scope.insert_scheme(name, scheme);
    }

    fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>> {
        self.current_scope.lookup_binding(name)
    }

    fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        self.current_scope.lookup_value(name)
    }

    fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        self.current_scope.lookup_scheme(name)
    }

    fn apply(&mut self, s: &Subst) {
        self.current_scope.apply(s);
    }
}

pub enum ScopeKind {
    Inherit,
    Async,
    Sync,
}

impl From<bool> for ScopeKind {
    fn from(is_async: bool) -> Self {
        match is_async {
            true => ScopeKind::Async,
            false => ScopeKind::Sync,
        }
    }
}

impl Checker {
    pub fn push_scope(&mut self, scope_kind: ScopeKind) {
        self.parent_scopes.push(self.current_scope.to_owned());
        self.current_scope = self.current_scope.clone();
        match scope_kind {
            ScopeKind::Inherit => (),
            ScopeKind::Async => self.current_scope.is_async = true,
            ScopeKind::Sync => self.current_scope.is_async = false,
        }
    }

    pub fn pop_scope(&mut self) {
        let parent_scope = self.parent_scopes.pop().unwrap();
        self.current_scope = parent_scope;
    }

    pub fn fresh_var(&mut self, constraint: Option<Box<Type>>) -> Type {
        let id = self.next_id;
        self.next_id = id + 1;
        Type::from(TypeKind::Var(TVar { id, constraint }))
    }
}
