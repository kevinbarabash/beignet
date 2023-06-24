use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::span::Span;
use crate::type_ann::TypeAnn;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Expr {
        expr: Expr,
    },
    // TODO: add support for explicit type annotations
    Let {
        pattern: Pattern,
        expr: Expr,
        type_ann: Option<TypeAnn>,
    },
    Return {
        arg: Option<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
