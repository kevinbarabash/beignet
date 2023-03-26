use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Number,
    Boolean,
    String,
    Null,
    Self_, // self is a replacement for this
    Symbol,
    Undefined,
    Never,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::Number => write!(f, "number"),
            Keyword::Boolean => write!(f, "boolean"),
            Keyword::String => write!(f, "string"),
            Keyword::Null => write!(f, "null"),
            Keyword::Self_ => write!(f, "self"),
            Keyword::Symbol => write!(f, "symbol"),
            Keyword::Undefined => write!(f, "undefined"),
            Keyword::Never => write!(f, "never"),
        }
    }
}
