use crate::literal::Literal;
use crate::source_location::SourceLocation;
use crate::stmt::Stmt;

// TODO: track source location
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ObjectKey {
    Identifier(String), // TODO: use Identifier
    String(String),
    Number(String),
    Computed(Box<Expr>),
}

// TODO: track source location
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Prop {
    Shorthand { key: String }, // TODO: use Identifier
    Property { key: ObjectKey, value: Expr },
    // TODO:
    // - method
    // - getter
    // - setter
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PropOrSpread {
    Prop(Prop),
    Spread(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprOrSpread {
    Expr(Expr),
    Spread(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Identifier(String),
    Literal(Literal),
    TemplateLiteral {
        parts: Vec<Literal>,
        exprs: Vec<Expr>,
    },
    Object {
        properties: Vec<PropOrSpread>,
    },
    Tuple {
        elements: Vec<ExprOrSpread>,
    },
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
