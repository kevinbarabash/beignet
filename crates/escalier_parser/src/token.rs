use crate::expr::Expr;
use crate::jsx::{JSXElement, JSXFragment};
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
    JSXElement(JSXElement),
    JSXFragment(JSXFragment),

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

    // Binary Operators
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Or,
    And,

    // Assignment Operators
    Assign,
    PlusAssign,
    MinusAssign,
    TimesAssign,
    DivideAssign,
    ModuloAssign,

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
    Question,
    QuestionDot, // used for optional chaining
    Dot,
    DotDot,    // used for ranges
    DotDotDot, // used for rest/spread
    Colon,

    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}
