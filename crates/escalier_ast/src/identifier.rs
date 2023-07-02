use crate::span::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BindingIdent {
    pub name: String,
    pub span: Span,
    pub mutable: bool,
}
