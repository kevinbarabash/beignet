use crate::span::Span;
use crate::stmt::Stmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}
