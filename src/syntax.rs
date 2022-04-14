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
    App(Box<Expr>, Vec<Expr>),
    Ident(String),
    Lam(Vec<BindingIdent>, Box<Expr>, bool),
    Let(Pattern, Box<Expr>, Box<Expr>),
    Lit(Literal),
    Op(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(String),
    // TODO: add more patterns later
}
