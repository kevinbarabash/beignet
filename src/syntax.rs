use super::literal::Literal;

// TODO: rename this to something else since we can't use it
// let bindings
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingIdent {
    Ident(String),
    Rest(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    App {
        lam: Box<Expr>,
        args: Vec<Expr>,
    },
    Ident {
        name: String,
    },
    Lam {
        args: Vec<BindingIdent>,
        body: Box<Expr>,
        is_async: bool,
    },
    Let {
        pattern: Pattern,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Lit {
        literal: Literal,
    },
    Op {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(String),
    // TODO: add more patterns later
}
