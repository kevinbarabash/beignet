use generational_arena::Index;

use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::span::Span;
use crate::type_ann::TypeAnn;
use crate::TypeParam;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnStmt {
    pub arg: Option<Expr>,
}

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
pub enum StmtKind {
    Expr(ExprStmt),
    Return(ReturnStmt),

    VarDecl(VarDecl),
    TypeDecl(TypeDecl),
    // TODO:
    // - explicit type annotations
    // - function decls: `fn foo() {}` desugars to `let foo = fn () {}`
    // - class decls: `class Foo {}` desugars to `let Foo = class {}`
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub inferred_type: Option<Index>,
}
