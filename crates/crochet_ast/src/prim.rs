use std::fmt;
use std::hash::Hash;

use crochet_types::{TPrim, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Primitive {
    Num,
    Bool,
    Str,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Num => write!(f, "number",),
            Primitive::Bool => write!(f, "boolean"),
            Primitive::Str => write!(f, "string"),
        }
    }
}

impl From<Primitive> for Type {
    fn from(prim: Primitive) -> Self {
        Type::Prim(match prim {
            Primitive::Num => TPrim::Num,
            Primitive::Bool => TPrim::Bool,
            Primitive::Str => TPrim::Str,
        })
    }
}
