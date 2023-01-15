use std::fmt;
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TKeyword {
    Number,
    Boolean,
    String,
    Null,
    Self_,
    Symbol,
    Undefined,
    Never,
    Object,
}

impl fmt::Display for TKeyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TKeyword::Number => write!(f, "number"),
            TKeyword::Boolean => write!(f, "boolean"),
            TKeyword::String => write!(f, "string"),
            TKeyword::Null => write!(f, "null"),
            TKeyword::Self_ => write!(f, "self"),
            TKeyword::Symbol => write!(f, "symbol"),
            TKeyword::Undefined => write!(f, "undefined"),
            TKeyword::Never => write!(f, "never"),
            TKeyword::Object => write!(f, "object"),
        }
    }
}
