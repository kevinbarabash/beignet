use escalier_ast::types::{TKeyword, TLit, TVar, Type, TypeKind};
use escalier_ast::values::{Keyword, Lit};
use im::HashMap;

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
    pub types: HashMap<u32, Type>,
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
            types: HashMap::default(),
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
        let id = self.fresh_id();
        // self.from_type_kind(TypeKind::Var(TVar { id, constraint }))
        let t = Type {
            id,
            kind: TypeKind::Var(TVar { id, constraint }),
            provenance: None,
            mutable: false,
        };
        self.types.insert(id, t.to_owned());
        t
    }

    pub fn fresh_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id = id + 1;
        id
    }

    pub fn from_type_kind(&mut self, kind: TypeKind) -> Type {
        let id = self.fresh_id();
        let t = Type {
            id,
            kind,
            provenance: None,
            mutable: false,
        };
        self.types.insert(id, t.to_owned());
        t
    }

    pub fn from_type_lit(&mut self, lit: TLit) -> Type {
        self.from_type_kind(TypeKind::Lit(lit))
    }

    pub fn from_keyword(&mut self, keyword: Keyword) -> Type {
        self.from_type_kind(TypeKind::Keyword(match keyword {
            Keyword::Number => TKeyword::Number,
            Keyword::String => TKeyword::String,
            Keyword::Boolean => TKeyword::Boolean,
            Keyword::Null => TKeyword::Null,
            Keyword::Symbol => TKeyword::Symbol,
            Keyword::Undefined => TKeyword::Undefined,
            Keyword::Self_ => TKeyword::Self_,
            Keyword::Never => TKeyword::Never,
        }))
    }

    pub fn from_lit(&mut self, lit: Lit) -> Type {
        self.from_type_kind(TypeKind::Lit(match lit {
            Lit::Num(n) => TLit::Num(n.value),
            Lit::Bool(b) => TLit::Bool(b.value),
            Lit::Str(s) => TLit::Str(s.value),
        }))
    }
}
