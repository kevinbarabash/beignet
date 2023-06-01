use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Number(String),
    Identifier(String), // [a-zA-Z_][a-zA-Z0-9_]*

    // Keywords
    Let,
    Return,

    // Operators
    Assign,
    Plus,
    Minus,
    Times,
    Divide,
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
    Or,
    And,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}
