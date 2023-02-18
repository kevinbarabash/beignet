use std::fmt;

use escalier_infer::TypeError;
use escalier_parser::ParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum CompileError {
    TypeError(Vec<TypeError>),
    ParseError(ParseError),
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "CompileError")
    }
}
