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
