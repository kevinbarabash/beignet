use std::fmt;

use crate::types::{TKeyword, Type, TypeKind};

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
