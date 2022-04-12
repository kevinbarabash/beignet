use super::literal::Literal;

#[derive(Debug, PartialEq, Eq)]
pub enum BindingIdent {
    Ident(String),
    Rest(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    App(Box<Expr>, Vec<Expr>),
    Ident(String),
    Lam(Vec<BindingIdent>, Box<Expr>, bool),
    Lit(Literal),
}
