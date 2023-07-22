use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Literal {
    Number(String),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl Literal {
    pub fn get_scheme_name(&self) -> Option<&'static str> {
        match self {
            Literal::Number(_) => Some("Number"),
            Literal::String(_) => Some("String"),
            Literal::Boolean(_) => Some("Boolean"),
            Literal::Null => None,
            Literal::Undefined => None,
        }
    }
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
