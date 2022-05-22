use crate::ast::span::Span;
use crate::ast::literal::Lit;
use crate::types::{Primitive};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LamType {
    pub span: Span,
    pub args: Vec<TypeAnn>,
    pub ret: Box<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimType {
    pub span: Span,
    pub prim: Primitive,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LitType {
    pub span: Span,
    pub lit: Lit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeRef {
    pub span: Span,
    pub name: String,
    pub type_params: Option<Vec<TypeAnn>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnn {
    TypeRef(TypeRef),
    Lam(LamType),
    Prim(PrimType),
    Lit(LitType),
    // Union(UnionType),
    // Object(ObjectType),
}
