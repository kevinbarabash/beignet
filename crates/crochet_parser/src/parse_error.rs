#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub msg: String,
}

impl From<String> for ParseError {
    fn from(msg: String) -> Self {
        ParseError { msg }
    }
}

impl From<&str> for ParseError {
    fn from(msg: &str) -> Self {
        ParseError {
            msg: msg.to_owned(),
        }
    }
}
