use derive_visitor::{Drive, DriveMut};
use std::fmt;

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub enum Literal {
    #[drive(skip)]
    Number(String),
    #[drive(skip)]
    String(String),
    #[drive(skip)]
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
