use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TKeyword {
    Null,
    Symbol,
    Undefined,
    Never,
}

impl fmt::Display for TKeyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TKeyword::Null => write!(f, "null"),
            TKeyword::Symbol => write!(f, "symbol"),
            TKeyword::Undefined => write!(f, "undefined"),
            TKeyword::Never => write!(f, "never"),
        }
    }
}
