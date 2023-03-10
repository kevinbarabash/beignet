use escalier_ast::types::*;

#[derive(Clone, Debug)]
pub struct Binding {
    pub mutable: bool,
    pub t: Type,
}
