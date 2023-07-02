use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Literal {
    Number(String),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Null => write!(f, "null"),
            Literal::Undefined => write!(f, "undefined"),
        }
    }
}
