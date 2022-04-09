use super::literal::Literal;

#[derive(Debug)]
pub enum BindingIdent {
    Ident(String),
    Rest(String),
}

#[derive(Debug)]
pub enum Expr {
    App(Box<Expr>, Vec<Expr>),
    Ident(String),
    Lam(Vec<BindingIdent>, Box<Expr>, bool),
    Lit(Literal),
}
