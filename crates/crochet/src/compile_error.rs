use error_stack::Context;
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct CompileError;

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "CompileError")
    }
}

impl Context for CompileError {}
