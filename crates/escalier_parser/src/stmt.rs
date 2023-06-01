use crate::expr::Expr;
use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Expr { expr: Expr },
    Let { name: String, expr: Expr },
    Return { arg: Option<Expr> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: SourceLocation,
}
