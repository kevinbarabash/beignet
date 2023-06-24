use crate::expr::Expr;
use crate::identifier::Ident;
use crate::span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum JSXElementName {
    Ident(Ident),
    JSXMemberExpr(JSXMemberExpr),
    // JSXNamespacedName(JSXNamespacedName),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXMemberExpr {
    pub obj: JSXObject,
    pub prop: Ident,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum JSXObject {
    JSXMemberExpr(Box<JSXMemberExpr>),
    Ident(Ident),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXAttr {
    pub name: String,
    pub value: Option<JSXAttrValue>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum JSXAttrValue {
    Str(String),
    ExprContainer(JSXExprContainer),
    // JSX supports using JSXElements and JSXFragments as attribute values without
    // wrapping them in braces.  The spec supports this, but I feel like it's
    // harder to read.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXExprContainer {
    // pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXOpeningElement {
    pub name: JSXElementName,
    pub attrs: Vec<JSXAttr>, // TODO: support spread
    pub self_closing: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXClosingElement {
    pub name: JSXElementName,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXElement {
    pub span: Span,
    pub opening: JSXOpeningElement,
    pub children: Vec<JSXElementChild>,
    pub closing: Option<JSXClosingElement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXFragment {
    pub span: Span,
    pub opening: JSXOpeningFragment,
    pub children: Vec<JSXElementChild>,
    pub closing: JSXClosingFragment,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXOpeningFragment {
    // pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXClosingFragment {
    // pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum JSXElementChild {
    Text(JSXText),
    ExprContainer(JSXExprContainer),
    SpreadChild(JSXSpreadChild),
    Element(Box<JSXElement>),
    Fragment(Box<JSXFragment>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXSpreadChild {
    // pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JSXText {
    pub span: Span,
    pub value: String,
    // pub raw: String,
}
