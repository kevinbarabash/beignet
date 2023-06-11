use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Expr { expr: Expr },
    // TODO: add support for explicit type annotations
    Let { pattern: Pattern, expr: Expr },
    Return { arg: Option<Expr> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: SourceLocation,
}
