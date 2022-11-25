use std::fmt;

use swc_common::DUMMY_SP;

use crate::types::{TKeyword, Type, TypeKind};
use crate::values::span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Null {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Undefined {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Number,
    Boolean,
    String,
    Null,
    Symbol,
    Undefined,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::Number => write!(f, "number"),
            Keyword::Boolean => write!(f, "boolean"),
            Keyword::String => write!(f, "string"),
            Keyword::Null => write!(f, "null"),
            Keyword::Symbol => write!(f, "symbol"),
            Keyword::Undefined => write!(f, "undefined"),
        }
    }
}

impl From<Keyword> for Type {
    fn from(keyword: Keyword) -> Self {
        Type {
            kind: TypeKind::Keyword(match keyword {
                Keyword::Number => TKeyword::Number,
                Keyword::String => TKeyword::String,
                Keyword::Boolean => TKeyword::Boolean,
                Keyword::Null => TKeyword::Null,
                Keyword::Symbol => TKeyword::Symbol,
                Keyword::Undefined => TKeyword::Undefined,
            }),
            provenance: None,
            mutable: false,
        }
    }
}

impl From<&Keyword> for swc_ecma_ast::Expr {
    fn from(keyword: &Keyword) -> Self {
        match keyword {
            Keyword::Number => todo!(),
            Keyword::Boolean => todo!(),
            Keyword::String => todo!(),
            Keyword::Null => {
                let lit = swc_ecma_ast::Lit::Null(swc_ecma_ast::Null { span: DUMMY_SP });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Keyword::Symbol => todo!(),
            Keyword::Undefined => {
                // NOTE: `undefined` is actually an identifier in JavaScript.
                let ident = swc_ecma_ast::Ident {
                    span: DUMMY_SP,
                    sym: swc_atoms::JsWord::from(String::from("undefined")),
                    optional: false,
                };
                swc_ecma_ast::Expr::Ident(ident)
            }
        }
    }
}
