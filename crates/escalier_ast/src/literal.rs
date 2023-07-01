#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Number(String),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}
