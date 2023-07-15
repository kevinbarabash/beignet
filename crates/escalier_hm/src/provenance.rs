use escalier_ast::{Expr, TypeAnn};

use crate::types::Type;

#[derive(Debug, Clone)]
pub enum Provenance {
    // from AST
    Expr(Box<Expr>),
    TypeAnn(Box<TypeAnn>),

    // from other types
    Type(Box<Type>),
}
