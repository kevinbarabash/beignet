use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TKeyword {
    Number,
    Boolean,
    String,
    Null,
    Symbol,
    Undefined,
    Never,
}

impl fmt::Display for TKeyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TKeyword::Number => write!(f, "number"),
            TKeyword::Boolean => write!(f, "boolean"),
            TKeyword::String => write!(f, "string"),
            TKeyword::Null => write!(f, "null"),
            TKeyword::Symbol => write!(f, "symbol"),
            TKeyword::Undefined => write!(f, "undefined"),
            TKeyword::Never => write!(f, "never"),
        }
    }
}
