use crate::context::Context;

#[derive(Default)]
pub struct Checker {
    pub current_scope: Context,
    pub parent_scopes: Vec<Context>,
}

impl Checker {
    pub fn push_scope(&mut self) {
        self.parent_scopes.push(self.current_scope.to_owned());
        self.current_scope = self.current_scope.clone();
    }

    pub fn pop_scope(&mut self) {
        let mut parent_scope = self.parent_scopes.pop().unwrap();
        parent_scope.count = self.current_scope.count.clone();
        self.current_scope = parent_scope;
    }
}
