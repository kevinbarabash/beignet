use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TPrim {
    Num,
    Bool,
    Str,
}

impl fmt::Display for TPrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TPrim::Num => write!(f, "number",),
            TPrim::Bool => write!(f, "boolean"),
            TPrim::Str => write!(f, "string"),
        }
    }
}
