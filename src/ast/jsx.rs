use crate::ast::expr::Expr;
use crate::ast::ident::Ident;
use crate::ast::literal::Lit;
use crate::ast::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXText {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JSXExprContainer {
    pub span: Span,
    pub expr: Expr,
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