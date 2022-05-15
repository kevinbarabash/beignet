use super::literal::Lit;

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
pub struct App {
    pub span: Span,
    pub lam: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fix {
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElse {
    pub span: Span,
    pub cond: Box<Expr>,
    pub consequent: Box<Expr>,
    pub alternate: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub span: Span,
    pub args: Vec<BindingIdent>,
    pub body: Box<Expr>,
    pub is_async: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub span: Span,
    pub pattern: Pattern,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Op {
    pub span: Span,
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obj {
    pub span: Span,
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Await {
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    App(App),
    Fix(Fix),
    Ident(Ident),
    IfElse(IfElse),
    JSXElement(JSXElement),
    Lambda(Lambda),
    Let(Let),
    Lit(Lit),
    Op(Op),
    Obj(Obj),
    Await(Await),
}

impl Expr {
    pub fn span(&self) -> Span {
        match &self {
            Expr::App(App { span, .. }) => span.to_owned(),
            Expr::Fix(Fix { span, .. }) => span.to_owned(),
            Expr::Ident(Ident { span, .. }) => span.to_owned(),
            Expr::IfElse(IfElse { span, .. }) => span.to_owned(),
            Expr::JSXElement(_) => todo!(),
            Expr::Lambda(Lambda{ span, .. }) => span.to_owned(),
            Expr::Let(Let { span, .. }) => span.to_owned(),
            Expr::Lit(lit) => lit.span(),
            Expr::Op(Op { span, .. }) => span.to_owned(),
            Expr::Obj(Obj { span, .. }) => span.to_owned(),
            Expr::Await(Await { span, .. }) => span.to_owned(),
        }
    }
}

// TODO: rename this to something else since we can't use it for let bindings
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingIdent {
    Ident(Ident),
    Rest { 
        span: Span,
        name: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(Ident),
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
