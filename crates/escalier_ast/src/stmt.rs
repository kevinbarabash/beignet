use generational_arena::Index;

use crate::block::Block;
use crate::decl::*;
use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ForStmt {
    pub left: Box<Pattern>,
    pub right: Box<Expr>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnStmt {
    pub arg: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Expr(ExprStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Decl(Decl),
    // VarDecl(VarDecl),
    // TypeDecl(TypeDecl),
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
