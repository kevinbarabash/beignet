#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Number(String),
    Identifier(String), // [a-zA-Z_][a-zA-Z0-9_]*
    Assign,
    Plus,
    Minus,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Question,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}
