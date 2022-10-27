use crate::types::Type;
use crate::values::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provenance {
    Expr(Box<Expr>),
    Type(Box<Type>),
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
