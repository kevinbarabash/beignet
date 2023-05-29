use std::fmt;

use escalier_infer::{Diagnostic, TypeError};
use escalier_old_parser::ParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum CompileError {
    TypeError(Vec<TypeError>),
    Diagnostic(Vec<Diagnostic>),
    ParseError(ParseError),
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "CompileError")
    }
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        CompileError::ParseError(error)
    }
}

impl From<Vec<TypeError>> for CompileError {
    fn from(errors: Vec<TypeError>) -> Self {
        CompileError::TypeError(errors)
    }
}
