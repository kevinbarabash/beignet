use crate::span::Span;
use crate::type_ann::TypeAnn;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParam {
    pub span: Span,
    pub t: TypeAnn,
    pub bound: Option<TypeAnn>,
    pub default: Option<TypeAnn>,
}
