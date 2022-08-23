use crate::expr::EFnParamPat;
use crate::ident::Ident;
use crate::keyword::Keyword;
use crate::lit::Lit;
use crate::pattern::{BindingIdent, RestPat};
use crate::prim::Primitive;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAnnFnParam {
    pub pat: EFnParamPat,
    pub type_ann: TypeAnn,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LamType {
    pub span: Span,
    pub params: Vec<TypeAnnFnParam>,
    pub ret: Box<TypeAnn>,
    pub type_params: Option<Vec<TypeParam>>,
}

// TODO: replace this with FnParam from expr
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FnParam {
    Ident(BindingIdent),
    Rest(RestPat),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimType {
    pub span: Span,
    pub prim: Primitive,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeywordType {
    pub span: Span,
    pub keyword: Keyword,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeRef {
    pub span: Span,
    pub name: String,
    pub type_params: Option<Vec<TypeAnn>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectType {
    pub span: Span,
    pub props: Vec<TProp>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TProp {
    pub span: Span,
    pub name: String,
    pub optional: bool,
    pub mutable: bool,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnionType {
    pub span: Span,
    pub types: Vec<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntersectionType {
    pub span: Span,
    pub types: Vec<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TupleType {
    pub span: Span,
    pub types: Vec<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayType {
    pub span: Span,
    pub elem_type: Box<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnn {
    Lam(LamType),
    Lit(Lit),
    Keyword(KeywordType),
    Prim(PrimType),
    Object(ObjectType),
    TypeRef(TypeRef),
    Union(UnionType),
    Intersection(IntersectionType),
    Tuple(TupleType),
    Array(ArrayType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub constraint: Option<Box<TypeAnn>>,
    pub default: Option<Box<TypeAnn>>,
}
