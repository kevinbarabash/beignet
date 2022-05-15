use crate::ast::ident::Ident;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(Ident),
    // TODO: add more patterns later
}
