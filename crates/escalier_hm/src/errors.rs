#[derive(Debug)]
pub enum Errors {
    InferenceError(String),
    ParseError(String),
}
