use super::literal::Literal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Decl {
        pattern: Pattern,
        value: Expression,
    },
    Expression {
        expr: Expression,
    },
    Return {
        arg: Expression,   
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Function {
        params: Vec<Param>,
        body: Vec<Statement>,
    },
    Ident {
        name: String,
    },
    Literal {literal: Literal},
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        arg: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident { name: String },
    // TODO: add more patterns later
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Param {
    Ident { name: String },
    Rest { name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}
