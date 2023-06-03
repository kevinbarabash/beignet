use crate::source_location::SourceLocation;
use crate::stmt::Stmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Identifier(String),
    Number(String),
    String(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
    Index {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Function {
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    Call {
        args: Vec<Expr>,
        callee: Box<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Or,
    And,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: SourceLocation,
}
