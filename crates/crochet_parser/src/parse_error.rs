use std::fmt;

// TODO: turn this into an enum so that we can report different kinds of parse
// errors
#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { message } = self;
        write!(fmt, "ParseError: {message}")
    }
}

impl From<&str> for ParseError {
    fn from(message: &str) -> Self {
        ParseError {
            message: message.to_owned(),
        }
    }
}

impl From<String> for ParseError {
    fn from(message: String) -> Self {
        ParseError { message }
    }
}
