use crate::types::{TObjElem, Type};
use crate::values::{Expr, Pattern, Span, TypeAnn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provenance {
    Expr(Box<Expr>),
    Pattern(Box<Pattern>),
    Type(Box<Type>),
    TObjElem(Box<TObjElem>),
    TypeAnn(Box<TypeAnn>),
}

impl Provenance {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Provenance::Expr(expr) => match &expr.inferred_type {
                Some(t) => match &t.provenance {
                    Some(prov) => prov.get_span(),
                    // Fallback to `expr`'s span, if the `inferred_type` has no
                    // provenance.
                    None => Some(expr.span.to_owned()),
                },
                None => Some(expr.span.to_owned()),
            },
            Provenance::Pattern(pattern) => Some(pattern.span.to_owned()),
            Provenance::Type(t) => match &t.provenance {
                Some(prov) => prov.get_span(),
                None => None,
            },
            Provenance::TObjElem(_) => None, // TODO: add provenance to TObjElem
            Provenance::TypeAnn(type_ann) => Some(type_ann.span.to_owned()),
        }
    }

    pub fn get_expr(&self) -> Option<Box<Expr>> {
        match self {
            Provenance::Expr(expr) => match &expr.inferred_type {
                Some(t) => match &t.provenance {
                    Some(prov) => prov.get_expr(),
                    // Fallback to `expr`'s span, if the `inferred_type` has no
                    // provenance.
                    None => Some(expr.to_owned()),
                },
                None => Some(expr.to_owned()),
            },
            Provenance::Pattern(_) => None,
            Provenance::Type(t) => match &t.provenance {
                Some(prov) => prov.get_expr(),
                None => None,
            },
            Provenance::TObjElem(_) => None,
            Provenance::TypeAnn(_) => None,
        }
    }
}

impl From<Expr> for Provenance {
    fn from(expr: Expr) -> Self {
        Provenance::Expr(Box::from(expr))
    }
}

impl From<&Expr> for Provenance {
    fn from(expr: &Expr) -> Self {
        Provenance::Expr(Box::from(expr.to_owned()))
    }
}

impl From<&mut Expr> for Provenance {
    fn from(expr: &mut Expr) -> Self {
        Provenance::Expr(Box::from(expr.to_owned()))
    }
}

impl From<Type> for Provenance {
    fn from(t: Type) -> Self {
        Provenance::Type(Box::from(t))
    }
}

impl From<&Type> for Provenance {
    fn from(t: &Type) -> Self {
        Provenance::Type(Box::from(t.to_owned()))
    }
}

impl From<&mut Type> for Provenance {
    fn from(t: &mut Type) -> Self {
        Provenance::Type(Box::from(t.to_owned()))
    }
}
