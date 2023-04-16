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
pub struct JSXExprContainer<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct JSXElement<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    // Other ASTs make have JSXOpeningElement and JSXClosingElement
    #[drive(skip)]
    pub name: String,
    pub attrs: Vec<JSXAttr<T>>,
    pub children: Vec<JSXElementChild<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum JSXElementChild<T: 'static>
where
    T: Drive + DriveMut,
{
    JSXText(JSXText),
    JSXExprContainer(JSXExprContainer<T>),
    JSXElement(Box<JSXElement<T>>),
    // JSXSpreadChild(JSXSpreadChild),
    // JSXFragment(JSXFragment),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct JSXAttr<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub ident: Ident,
    pub value: JSXAttrValue<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum JSXAttrValue<T: 'static>
where
    T: Drive + DriveMut,
{
    Lit(Lit),
    JSXExprContainer(JSXExprContainer<T>),
}
