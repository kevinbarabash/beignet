use generational_arena::Index;

use crate::ast::block::Block;
use crate::ast::class::Class;
use crate::ast::common::{SourceLocation, Span};
use crate::ast::expr::Expr;
use crate::ast::ident::Ident;
use crate::ast::pattern::Pattern;
use crate::ast::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassDecl {
    pub ident: Ident, // Why do have `ident` here an in `Class`?
    pub class: Box<Class>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub pattern: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub init: Option<Box<Expr>>,
    pub declare: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDecl {
    pub declare: bool,
    pub id: Ident,
    pub type_ann: TypeAnn,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForStmt {
    pub pattern: Box<Pattern>,
    pub expr: Box<Expr>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnStmt {
    pub arg: Option<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StmtKind {
    // Declarations
    ClassDecl(ClassDecl),
    VarDecl(VarDecl),
    TypeDecl(TypeDecl),

    // Statements
    ExprStmt(Expr),
    ForStmt(ForStmt),
    ReturnStmt(ReturnStmt),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    pub loc: SourceLocation,
    pub span: Span,
    pub kind: StmtKind,
    pub inferred_type: Option<Index>,
}
