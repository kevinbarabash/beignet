use std::fmt;

use crochet_types::{TKeyword, Type};

use crate::span::Span;

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
    Null,
    Symbol,
    Undefined,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::Null => write!(f, "null"),
            Keyword::Symbol => write!(f, "symbol"),
            Keyword::Undefined => write!(f, "undefined"),
        }
    }
}

impl From<Keyword> for Type {
    fn from(keyword: Keyword) -> Self {
        Type::Keyword(match keyword {
            Keyword::Null => TKeyword::Null,
            Keyword::Symbol => TKeyword::Symbol,
            Keyword::Undefined => TKeyword::Undefined,
        })
    }
}
