use crate::types::Type;
use crate::values::{Expr, Pattern, Span, TypeAnn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provenance {
    Expr(Box<Expr>),
    Pattern(Box<Pattern>),
    Type(Box<Type>),
    TypeAnn(Box<TypeAnn>),
}

impl Provenance {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Provenance::Expr(expr) => Some(expr.span.to_owned()),
            Provenance::Pattern(pattern) => Some(pattern.span.to_owned()),
            Provenance::Type(t) => match &t.provenance {
                Some(prov) => prov.get_span(),
                None => None,
            },
            Provenance::TypeAnn(type_ann) => Some(type_ann.span.to_owned()),
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
