use crate::ast::common::{SourceLocation, Span};
use crate::ast::expr::Expr;
use crate::ast::ident::Ident;
use crate::ast::lit::Lit;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JSXText {
    pub loc: SourceLocation,
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JSXExprContainer {
    pub loc: SourceLocation,
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JSXElement {
    pub loc: SourceLocation,
    pub span: Span,
    // Other ASTs make have JSXOpeningElement and JSXClosingElement
    pub name: String,
    pub attrs: Vec<JSXAttr>,
    pub children: Vec<JSXElementChild>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JSXElementChild {
    JSXText(JSXText),
    JSXExprContainer(JSXExprContainer),
    JSXElement(Box<JSXElement>),
    // JSXSpreadChild(JSXSpreadChild),
    // JSXFragment(JSXFragment),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JSXAttr {
    pub loc: SourceLocation,
    pub span: Span,
    pub ident: Ident,
    pub value: JSXAttrValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JSXAttrValue {
    Lit(Lit),
    JSXExprContainer(JSXExprContainer),
}
