use super::literal::Literal;

pub type Span = std::ops::Range<usize>;

// TODO: rename this to something else since we can't use it
// let bindings
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingIdent {
    Ident { name: String },
    Rest { name: String },
}

pub type BindingIdentWithSpan = (BindingIdent, Span);

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
        lam: Box<ExprWithSpan>,
        args: Vec<ExprWithSpan>,
    },
    Ident {
        name: String,
    },
    Lam {
        args: Vec<BindingIdentWithSpan>,
        body: Box<ExprWithSpan>,
        is_async: bool,
    },
    Let {
        pattern: PatternWithSpan,
        value: Box<ExprWithSpan>,
        body: Box<ExprWithSpan>,
    },
    Lit {
        literal: Literal,
    },
    Op {
        op: BinOp,
        left: Box<ExprWithSpan>,
        right: Box<ExprWithSpan>,
    },
}

pub type ExprWithSpan = (Expr, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident { name: String },
    // TODO: add more patterns later
}

pub type PatternWithSpan = (Pattern, Span);
