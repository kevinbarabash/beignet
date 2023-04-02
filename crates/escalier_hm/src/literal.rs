use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Number(String),
    String(String),
    Boolean(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Literal::Number(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "\"{}\"", value),
            Literal::Boolean(value) => write!(f, "{}", value),
        }
    }
}
