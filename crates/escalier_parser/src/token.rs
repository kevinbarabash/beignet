use crate::expr::Expr;
use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Identifier(String), // [a-zA-Z_][a-zA-Z0-9_]*

    // Literals
    BoolLit(bool),
    NumLit(String),
    StrLit(String),
    StrTemplateLit {
        parts: Vec<Token>, // This should only contain StrLit tokens
        exprs: Vec<Expr>,
    },
    Null,
    Undefined,

    // Types
    Number,
    Boolean,
    String,
    Symbol,

    // Keywords
    Let,
    Return,
    Fn,
    Await,
    Async,
    If,
    Else,
    Mut, // denotes a binding to a mutable reference
    Var, // denotes a re-assignable binding
    Match,
    Try,
    Catch,
    Finally,
    Do,

    // Operators
    Assign,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Dot,
    QuestionDot, // used for optional chaining
    DotDot,      // used for ranges
    DotDotDot,   // used for rest/spread
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
    Arrow,
    Underscore,

    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}
