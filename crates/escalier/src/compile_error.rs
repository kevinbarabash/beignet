use std::fmt;

use escalier_hm::diagnostic::Diagnostic;
use escalier_hm::type_error::TypeError;
use escalier_parser::ParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum CompileError {
    TypeError(TypeError),
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

// impl From<Vec<TypeError>> for CompileError {
//     fn from(errors: Vec<TypeError>) -> Self {
//         CompileError::TypeError(errors)
//     }
// }

impl From<TypeError> for CompileError {
    fn from(error: TypeError) -> Self {
        CompileError::TypeError(error)
    }
}
