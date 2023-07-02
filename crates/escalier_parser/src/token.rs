use escalier_ast::*;

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
    Declare,
    Let,
    Return,
    Fn,
    Get,
    Set,
    Async,
    Await,
    Gen,
    Yield,
    If,
    Else,
    Mut, // denotes a binding to a mutable reference
    Var, // denotes a re-assignable binding
    Match,
    Try,
    Catch,
    Finally,
    Do,
    Class,
    Extends,
    Type,
    TypeOf,
    KeyOf,

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
    Colon,
    Comma,
    Semicolon,
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
    Pipe,
    Ampersand,

    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub const EOF: Token = Token {
    kind: TokenKind::Eof,
    span: DUMMY_SPAN,
};
