use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::span::Span;
use crate::type_ann::TypeAnn;
use crate::type_param::TypeParam;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDecl {
    pub is_declare: bool,
    pub is_var: bool,
    pub pattern: Pattern,
    pub expr: Option<Expr>,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeDecl {
    pub name: String,
    pub type_ann: TypeAnn,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DeclKind {
    TypeDecl(TypeDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}
