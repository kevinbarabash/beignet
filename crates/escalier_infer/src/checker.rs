use crate::context::Context;

#[derive(Default)]
pub struct Checker {
    pub current_scope: Context,
    parent_scopes: Vec<Context>,
}

impl From<Context> for Checker {
    fn from(ctx: Context) -> Self {
        Checker {
            current_scope: ctx,
            parent_scopes: vec![],
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
        let mut parent_scope = self.parent_scopes.pop().unwrap();
        parent_scope.count = self.current_scope.count.clone();
        self.current_scope = parent_scope;
    }
}
