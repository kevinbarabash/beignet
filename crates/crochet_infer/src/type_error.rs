use error_stack::Context;
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct TypeError;

impl fmt::Display for TypeError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "TypeError")
    }
}

impl Context for TypeError {}
