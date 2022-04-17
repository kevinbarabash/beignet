use super::literal::Literal;

pub type Span = std::ops::Range<usize>;

pub type WithSpan<T> = (T, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<WithSpan<Statement>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Decl {
        pattern: WithSpan<Pattern>,
        value: WithSpan<Expr>,
    },
    Expr(WithSpan<Expr>), // NOTE: does not include Expr::Let
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    App {
        lam: Box<WithSpan<Expr>>,
        args: Vec<WithSpan<Expr>>,
    },
    Ident {
        name: String,
    },
    Lam {
        args: Vec<WithSpan<BindingIdent>>,
        body: Box<WithSpan<Expr>>,
        is_async: bool,
    },
    Let {
        pattern: WithSpan<Pattern>,
        value: Box<WithSpan<Expr>>,
        body: Box<WithSpan<Expr>>,
    },
    Lit {
        literal: Literal,
    },
    Op {
        op: BinOp,
        left: Box<WithSpan<Expr>>,
        right: Box<WithSpan<Expr>>,
    },
}

// TODO: rename this to something else since we can't use it for let bindings
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingIdent {
    Ident { name: String },
    Rest { name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident { name: String },
    // TODO: add more patterns later
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}
