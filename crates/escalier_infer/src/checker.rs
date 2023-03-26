use escalier_ast::types::{TVar, Type, TypeKind};

use crate::binding::Binding;
use crate::context::Context;
use crate::diagnostic::Diagnostic;
use crate::scheme::{generalize, Scheme};
use crate::scope::Scope;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;

pub type Report = Vec<Diagnostic>;

#[derive(Debug, Clone)]
pub struct Checker {
    pub next_id: u32,
    pub current_scope: Scope,
    pub parent_scopes: Vec<Scope>,
    pub current_report: Report,
    pub parent_reports: Vec<Report>,
}

impl From<Scope> for Checker {
    fn from(scope: Scope) -> Self {
        Checker {
            current_scope: scope,
            ..Checker::default()
        }
    }
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            next_id: 1,
            current_scope: Scope::default(),
            parent_scopes: vec![],
            current_report: vec![],
            parent_reports: vec![],
        }
    }
}

impl Checker {
    pub fn insert_binding(&mut self, name: String, b: Binding) {
        self.current_scope.insert_binding(name, b);
    }

    pub fn insert_value(&mut self, name: String, t: Type) {
        self.current_scope.insert_value(name, t);
    }

    pub fn insert_type(&mut self, name: String, t: Type) {
        // self.current_scope.insert_type(name, t, self);
        let scheme = generalize(&t, self);
        self.insert_scheme(name, scheme);
    }

    pub fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        self.current_scope.insert_scheme(name, scheme);
    }

    pub fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>> {
        self.current_scope.lookup_binding(name)
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        self.current_scope.lookup_value(name)
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        self.current_scope.lookup_scheme(name)
    }

    pub fn apply(&'_ mut self, s: &Subst) {
        let values = self.current_scope.values.clone();
        self.current_scope.values = values
            .iter()
            .map(|(k, v)| (k.to_owned(), v.apply(s, self)))
            .collect();
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
        let mut scope = self.current_scope.clone();
        std::mem::swap(&mut scope, &mut self.current_scope);
        self.parent_scopes.push(scope);
        match scope_kind {
            ScopeKind::Inherit => (),
            ScopeKind::Async => self.current_scope.is_async = true,
            ScopeKind::Sync => self.current_scope.is_async = false,
        }
    }

    pub fn pop_scope(&mut self) {
        self.current_scope = self.parent_scopes.pop().unwrap();
    }

    pub fn push_report(&mut self) {
        let mut report: Report = vec![];
        std::mem::swap(&mut report, &mut self.current_report);
        self.parent_reports.push(report);
    }

    // TODO: merge any diagnostics into the parent report when popping
    pub fn pop_report(&mut self) {
        let mut report = self.current_report.clone();
        self.current_report = self.parent_reports.pop().unwrap();
        self.current_report.append(&mut report);
    }

    pub fn fresh_var(&mut self, constraint: Option<Box<Type>>) -> Type {
        let id = self.next_id;
        self.next_id = id + 1;
        Type::from(TypeKind::Var(TVar { id, constraint }))
    }
}
