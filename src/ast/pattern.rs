use crate::ast::expr::Expr;
use crate::ast::ident::Ident;
use crate::ast::span::Span;
use crate::ast::types::TypeAnn;
use crate::ast::Lit;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(BindingIdent),
    Rest(RestPat),
    Object(ObjectPat),
    Array(ArrayPat),
    Lit(LitPat),
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
            Pattern::Lit(lit) => lit.span.to_owned(),
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
pub struct LitPat {
    pub span: Span,
    pub lit: Lit,
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

pub fn is_refutable(pat: &Pattern) -> bool {
    match pat {
        Pattern::Ident(_) => false,
        Pattern::Rest(_) => false,
        Pattern::Object(ObjectPat { props, .. }) => props.iter().any(|prop| match prop {
            ObjectPatProp::KeyValue(KeyValuePatProp { value, .. }) => is_refutable(value),
            ObjectPatProp::Rest(RestPat { arg, ..}) => is_refutable(arg),
            ObjectPatProp::Assign(_) => false, // corresponds to {x = 5}
        }),
        Pattern::Array(ArrayPat { elems, .. }) => {
            elems.iter().any(|elem| {
                match elem {
                    Some(elem) => is_refutable(elem),
                    // FixMe: this should probably be true since it's equivalent
                    // to having an element with the value `undefined`
                    None => false,
                }
            })
        }
        Pattern::Lit(_) => true,
    }
}
