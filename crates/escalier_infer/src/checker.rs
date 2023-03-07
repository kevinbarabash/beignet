use escalier_ast::types::{TVar, Type, TypeKind};

use crate::context::Context;

pub struct Checker {
    pub current_scope: Context,
    pub next_id: u32,
    pub parent_scopes: Vec<Context>,
}

impl From<Context> for Checker {
    fn from(ctx: Context) -> Self {
        Checker {
            current_scope: ctx,
            parent_scopes: vec![],
            next_id: 1,
        }
    }
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            current_scope: Context::default(),
            parent_scopes: vec![],
            next_id: 1,
        }
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
