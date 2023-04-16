use crate::ast::common::Span;
use crate::ast::stmt::Statement;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub span: Span,
    // #[drive(skip)]
    // pub loc: SourceLocation,
    pub stmts: Vec<Statement>,
}
