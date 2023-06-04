use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Identifier(String), // [a-zA-Z_][a-zA-Z0-9_]*

    // Literals
    Number(String),
    True,
    False,
    Null,
    Undefined,

    // Keywords
    Let,
    Return,
    Fn,
    Await,
    Async,
    If,
    Else,

    // Operators
    Assign,
    Plus,
    Minus,
    Times,
    Divide,
    Dot,
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

    // punctuation
    Semicolon,
    Comma,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}
