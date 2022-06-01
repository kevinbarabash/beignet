use crate::ast::span::Span;
use crate::ast::ident::Ident;
use crate::ast::types::TypeAnn;
use crate::ast::expr::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(BindingIdent),
    Rest(RestPat),
    Object(ObjectPat),
    Array(ArrayPat),
    // This can't be used at the top level similar to rest
    // Assign(AssignPat),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Ident(ident) => ident.span.to_owned(),
            Pattern::Rest(rest) => rest.span.to_owned(),
            Pattern::Object(obj) => obj.span.to_owned(),
            Pattern::Array(array) => array.span.to_owned(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayPat {
    pub span: Span,
    pub elems: Vec<Option<Pattern>>,
    pub optional: bool,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectPat {
    pub span: Span,
    pub props: Vec<ObjectPatProp>,
    pub optional: bool,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectPatProp {
    KeyValue(KeyValuePatProp),
    Rest(RestPat),
    Assign(AssignPatProp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValuePatProp {
    pub key: Ident,
    pub value: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignPatProp {
    pub span: Span,
    pub key: Ident,
    pub value: Option<Box<Expr>>,
}
