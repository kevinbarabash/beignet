use derive_visitor::{Drive, DriveMut};

use crate::values::common::{SourceLocation, Span};
use crate::values::expr::Expr;
use crate::values::ident::Ident;
use crate::values::lit::Lit;

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct JSXText {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub value: String,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct JSXExprContainer {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct JSXElement {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    // Other ASTs make have JSXOpeningElement and JSXClosingElement
    #[drive(skip)]
    pub name: String,
    pub attrs: Vec<JSXAttr>,
    pub children: Vec<JSXElementChild>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum JSXElementChild {
    JSXText(JSXText),
    JSXExprContainer(JSXExprContainer),
    JSXElement(Box<JSXElement>),
    // JSXSpreadChild(JSXSpreadChild),
    // JSXFragment(JSXFragment),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct JSXAttr {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub ident: Ident,
    pub value: JSXAttrValue,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum JSXAttrValue {
    Lit(Lit),
    JSXExprContainer(JSXExprContainer),
}
