use escalier_ast::{Expr, Span, TypeAnn};

use crate::types::Type;

#[derive(Debug, Clone)]
pub enum Provenance {
    // from AST
    Expr(Box<Expr>),
    TypeAnn(Box<TypeAnn>),

    // from other types
    Type(Box<Type>),
}

impl Provenance {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Provenance::Expr(expr) => {
                Some(expr.span.to_owned())
                // TODO: add .provenance to Type
                // match &expr.inferred_type {
                //     Some(t) => match &t.provenance {
                //         Some(prov) => prov.get_span(),
                //         // Fallback to `expr`'s span, if the `inferred_type` has no
                //         // provenance.
                //         None => Some(expr.span.to_owned()),
                //     },
                //     None => Some(expr.span.to_owned()),
                // }
            }
            // Provenance::Pattern(pattern) => Some(pattern.span.to_owned()),
            Provenance::Type(t) => match &t.provenance {
                Some(prov) => prov.get_span(),
                None => None,
            },
            // Provenance::TObjElem(_) => None, // TODO: add provenance to TObjElem
            Provenance::TypeAnn(type_ann) => Some(type_ann.span.to_owned()),
        }
    }

    pub fn get_expr(&self) -> Option<Box<Expr>> {
        match self {
            Provenance::Expr(expr) => {
                Some(expr.to_owned())
                // TODO: add .provenance to Type
                // match &expr.inferred_type {
                //     Some(t) => match &t.provenance {
                //         Some(prov) => prov.get_expr(),
                //         // Fallback to `expr`'s span, if the `inferred_type` has no
                //         // provenance.
                //         None => Some(expr.to_owned()),
                //     },
                //     None => Some(expr.to_owned()),
                // }
            }
            // Provenance::Pattern(_) => None,
            Provenance::Type(t) => match &t.provenance {
                Some(prov) => prov.get_expr(),
                None => None,
            },
            // Provenance::TObjElem(_) => None,
            Provenance::TypeAnn(_) => None,
        }
    }
}
