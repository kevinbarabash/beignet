use std::fmt;
use swc_atoms::JsWord;
use swc_common::source_map::DUMMY_SP;

use crochet_types::{TKeyword, Type};

use crate::span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Null {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Undefined {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Null(Null),
    Undefined(Undefined),
}

impl Keyword {
    pub fn span(&self) -> Span {
        match &self {
            Keyword::Null(n) => n.span.to_owned(),
            Keyword::Undefined(u) => u.span.to_owned(),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::Null(_) => write!(f, "null"),
            Keyword::Undefined(_) => write!(f, "undefined"),
        }
    }
}

impl From<Keyword> for Type {
    fn from(keyword: Keyword) -> Self {
        Type::Keyword(match keyword {
            Keyword::Null(_) => TKeyword::Null,
            Keyword::Undefined(_) => TKeyword::Undefined,
        })
    }
}

impl From<&Keyword> for swc_ecma_ast::Expr {
    fn from(keyword: &Keyword) -> Self {
        match keyword {
            Keyword::Null(_) => {
                let lit = swc_ecma_ast::Lit::Null(swc_ecma_ast::Null { span: DUMMY_SP });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Keyword::Undefined(_) => {
                let ident = swc_ecma_ast::Ident {
                    span: DUMMY_SP,
                    optional: false,
                    sym: JsWord::from("undefined".to_owned()),
                };
                swc_ecma_ast::Expr::Ident(ident)
            }
        }
    }
}
