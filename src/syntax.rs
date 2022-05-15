use super::literal::Literal;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Decl {
        span: Span,
        pattern: Pattern,
        value: Expr,
    },
    Expr {
        span: Span,
        expr: Expr,
    }, // NOTE: does not include Expr::Let
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXText {
    span: Span,
    value: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXExprContainer {
    span: Span,
    expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXElement {
    // Other ASTs make have JSXOpeningElement and JSXClosingElement
    pub name: String,
    pub children: Vec<JSXElementChild>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JSXElementChild {
    JSXText(JSXText),
    JSXExprContainer(JSXExprContainer),
    // JSXSpreadChild(JSXSpreadChild),
    JSXElement(Box<JSXElement>),
    // JSXFragment(JSXFragment),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    App {
        span: Span,
        lam: Box<Expr>,
        args: Vec<Expr>,
    },
    Fix {
        span: Span,
        expr: Box<Expr>,
    },
    Ident {
        span: Span,
        name: String,
    },
    If {
        span: Span,
        cond: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Box<Expr>,
    },
    JSXElement(JSXElement),
    Lam {
        span: Span,
        args: Vec<BindingIdent>,
        body: Box<Expr>,
        is_async: bool,
    },
    Let {
        span: Span,
        pattern: Pattern,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Lit {
        span: Span,
        literal: Literal,
    },
    Op {
        span: Span,
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Obj {
        span: Span,
        properties: Vec<Property>,
    },
    Await {
        span: Span,
        expr: Box<Expr>,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match &self {
            Expr::App { span, .. } => span.to_owned(),
            Expr::Fix { span, .. } => span.to_owned(),
            Expr::Ident { span, .. } => span.to_owned(),
            Expr::If { span, .. } => span.to_owned(),
            Expr::JSXElement(_) => todo!(),
            Expr::Lam { span, .. } => span.to_owned(),
            Expr::Let { span, .. } => span.to_owned(),
            Expr::Lit { span, .. } => span.to_owned(),
            Expr::Op { span, .. } => span.to_owned(),
            Expr::Obj { span, .. } => span.to_owned(),
            Expr::Await { span, .. } => span.to_owned(),
        }
    }
}

// TODO: rename this to something else since we can't use it for let bindings
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingIdent {
    Ident { 
        span: Span,
        name: String,
    },
    Rest { 
        span: Span,
        name: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident { 
        span: Span,
        name: String,
    },
    // TODO: add more patterns later
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub span: Span,
    pub name: String,
    pub value: Expr,
}
