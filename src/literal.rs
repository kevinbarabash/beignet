use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(String),
    Bool(String),
    Str(String),
    Null,
    Undefined,
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        Literal::Str(s.to_owned())
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::Str(s)
    }
}

impl From<&bool> for Literal {
    fn from(b: &bool) -> Self {
        Literal::Bool(b.to_string())
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        Literal::Bool(b.to_string())
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Num(val) => write!(f, "{}", val),
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::Str(val) => write!(f, "\"{}\"", val),
            Literal::Null => write!(f, "null"),
            Literal::Undefined => write!(f, "undefined"),
        }
    }
}
