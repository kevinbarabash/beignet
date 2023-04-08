#[derive(Debug, PartialEq, Eq)]
pub enum Errors {
    InferenceError(String),
    ParseError(String),
}
