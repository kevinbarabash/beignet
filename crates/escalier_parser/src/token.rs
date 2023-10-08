use escalier_ast::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Identifier(String), // [a-zA-Z_][a-zA-Z0-9_]*
    Comment(String),

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
    Unknown,
    Never,

    // Keywords
    Import,
    Export,
    From,
    As,
    Declare,
    Let,
    Mut, // denotes a binding to a mutable reference
    Var, // denotes a re-assignable binding
    Fn,
    Return,
    Throws,
    Get,
    Set,
    Pub,
    Private,
    Static,
    Async,
    Await,
    Gen,
    Yield,
    If,
    Else,
    Match,
    Is,
    Try,
    Catch,
    Finally,
    Throw,
    Do,
    For,
    In,
    Class,
    Extends,
    Type,
    TypeOf,
    KeyOf,
    Infer,
    New,

    // Arithmetic Operators
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,

    // Comparison Operators
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // Logical Operators
    Or,
    And,
    Not,

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
    DoubleArrow,
    SingleArrow,
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
