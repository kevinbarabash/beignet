use crate::values::expr::Expr;
use crate::values::ident::Ident;
use crate::values::lit::Lit;
use crate::values::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXText {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXExprContainer {
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXElement {
    pub span: Span,
    // Other ASTs make have JSXOpeningElement and JSXClosingElement
    pub name: String,
    pub attrs: Vec<JSXAttr>,
    pub children: Vec<JSXElementChild>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JSXElementChild {
    JSXText(JSXText),
    JSXExprContainer(JSXExprContainer),
    JSXElement(Box<JSXElement>),
    // JSXSpreadChild(JSXSpreadChild),
    // JSXFragment(JSXFragment),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXAttr {
    pub span: Span,
    pub ident: Ident,
    pub value: JSXAttrValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JSXAttrValue {
    Lit(Lit),
    JSXExprContainer(JSXExprContainer),
}
