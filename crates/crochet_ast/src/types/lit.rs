use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TLit {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(String),
    Bool(bool),
    Str(String),
}

impl fmt::Display for TLit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TLit::Num(n) => write!(f, "{}", n),
            TLit::Bool(b) => write!(f, "{}", b),
            TLit::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}
