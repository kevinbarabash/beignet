use crate::ast::span::Span;
use crate::ast::ident::Ident;
use crate::ast::types::TypeAnn;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(BindingIdent),
    Rest(RestPat),

    // Array(ArrayPat),
    // Object(ObjectPat),
    // Assign(AssignPat),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Ident(ident) => ident.span.to_owned(),
            Pattern::Rest(rest) => rest.span.to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingIdent {
    pub span: Span,
    pub id: Ident,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestPat {
    pub span: Span,
    pub arg: Box<Pattern>,
    pub type_ann: Option<TypeAnn>,
}
