use std::fmt;

use crochet_infer::TypeError;
use crochet_parser::ParseError;

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
