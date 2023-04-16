use derive_visitor::{Drive, DriveMut};

use crate::types::{TObjElem, Type};
use crate::values::{Expr, Pattern, Span, TypeAnn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provenance<T: 'static>
where
    T: Drive + DriveMut,
{
    Expr(Box<Expr<T>>),
    Pattern(Box<Pattern<T>>),
    Type(Box<T>),
    TObjElem(Box<TObjElem>),
    TypeAnn(Box<TypeAnn<T>>),
}

pub trait GetProvenance<T>
where
    T: Drive + DriveMut,
{
    fn get_provenance(&self) -> Option<Provenance<T>>;
}

impl<T> Provenance<T>
where
    T: Drive + DriveMut + Clone + GetProvenance<T>,
{
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Provenance::Expr(expr) => match &expr.inferred_type {
                // Some(t) => todo!(),
                Some(t) => match &t.get_provenance() {
                    Some(prov) => prov.get_span(),
                    // Fallback to `expr`'s span, if the `inferred_type` has no
                    // provenance.
                    None => Some(expr.span.to_owned()),
                },
                None => Some(expr.span.to_owned()),
            },
            Provenance::Pattern(pattern) => Some(pattern.span.to_owned()),
            Provenance::Type(t) => match &t.get_provenance() {
                Some(prov) => prov.get_span(),
                None => None,
            },
            Provenance::TObjElem(_) => None, // TODO: add provenance to TObjElem
            Provenance::TypeAnn(type_ann) => Some(type_ann.span.to_owned()),
        }
    }

    pub fn get_expr(&self) -> Option<Box<Expr<T>>> {
        match self {
            Provenance::Expr(expr) => match &expr.inferred_type {
                Some(t) => match &t.get_provenance() {
                    Some(prov) => prov.get_expr(),
                    // Fallback to `expr`'s span, if the `inferred_type` has no
                    // provenance.
                    None => Some(expr.to_owned()),
                },
                None => Some(expr.to_owned()),
            },
            Provenance::Pattern(_) => None,
            Provenance::Type(t) => match &t.get_provenance() {
                Some(prov) => prov.get_expr(),
                None => None,
            },
            Provenance::TObjElem(_) => None,
            Provenance::TypeAnn(_) => None,
        }
    }
}

impl<T> From<Expr<T>> for Provenance<T>
where
    T: Drive + DriveMut,
{
    fn from(expr: Expr<T>) -> Self {
        Provenance::Expr(Box::from(expr))
    }
}

impl<T> From<&Expr<T>> for Provenance<T>
where
    T: Drive + DriveMut + Clone + GetProvenance<T>,
{
    fn from(expr: &Expr<T>) -> Self {
        Provenance::Expr(Box::from(expr.to_owned()))
    }
}

impl<T> From<&mut Expr<T>> for Provenance<T>
where
    T: Drive + DriveMut + Clone + GetProvenance<T>,
{
    fn from(expr: &mut Expr<T>) -> Self {
        Provenance::Expr(Box::from(expr.to_owned()))
    }
}

impl From<Type> for Provenance<Type> {
    fn from(t: Type) -> Self {
        Provenance::Type(Box::from(t))
    }
}

impl From<&Type> for Provenance<Type> {
    fn from(t: &Type) -> Self {
        Provenance::Type(Box::from(t.to_owned()))
    }
}

impl From<&mut Type> for Provenance<Type> {
    fn from(t: &mut Type) -> Self {
        Provenance::Type(Box::from(t.to_owned()))
    }
}
