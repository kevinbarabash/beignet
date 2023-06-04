use crate::source_location::SourceLocation;
use crate::stmt::Stmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Number(String),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Identifier(String),
    Literal(Literal),
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
    Lambda {
        params: Vec<String>,
        expr: Box<Expr>,
    },
    Call {
        args: Vec<Expr>,
        callee: Box<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: Box<Expr>,
    },
    IfElse {
        cond: Box<Expr>,
        consequent: Vec<Stmt>,
        alternate: Option<Vec<Stmt>>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
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
