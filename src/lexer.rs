use chumsky::prelude::*;
use std::fmt;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Token {
    Ident(String),

    // keywords
    Else,
    If,
    In,
    Let,
    Rec,

    // literals
    Num(String), // We use string instead of f64 b/c f64 isn't hashable
    Str(String),
    True,
    False,

    // operators
    Minus,
    Plus,
    Times,
    Div,
    Eq,

    // punct
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Semi,
    Colon,
    FatArrow,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Num(num) => write!(f, "{}", num),
            Token::Str(num) => write!(f, "\"{}\"", num),
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Times => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Else => write!(f, "else"),
            Token::If => write!(f, "if"),
            Token::Let => write!(f, "let"),
            Token::Rec => write!(f, "rec"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::Eq => write!(f, "="),
            Token::Comma => write!(f, ","),
            Token::Semi => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::FatArrow => write!(f, "=>"),
            Token::In => write!(f, "in"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

pub type Span = std::ops::Range<usize>;

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let int = text::int::<char, Simple<char>>(10).map(|s: String| Token::Num(s));

    let r#str = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|s| Token::Str(s));

    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map(|s: String| Token::Num(s));

    let op = choice((
        just("+").to(Token::Plus),
        just("-").to(Token::Minus),
        just("*").to(Token::Times),
        just("/").to(Token::Div),
    ));

    let punct = choice((
        just(",").to(Token::Comma),
        just("=>").to(Token::FatArrow), // must appear before '='
        just("=").to(Token::Eq),
        just("(").to(Token::OpenParen),
        just(")").to(Token::CloseParen),
        just("{").to(Token::OpenBrace),
        just("}").to(Token::CloseBrace),
        just(";").to(Token::Semi),
        just(":").to(Token::Colon),
    ));

    let ident = text::ident::<char, Simple<char>>();

    let word = ident.map(|s: String| match s.as_str() {
        "let" => Token::Let,
        "in" => Token::In,
        "if" => Token::If,
        "else" => Token::Else,
        "rec" => Token::Rec,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(s.clone()),
    });

    let token = choice((word, real, int, r#str, op, punct));

    token
        .map_with_span(move |token, span| (token, span))
        .padded()
        .repeated()
        .then_ignore(end())
}

// TODO: prevent the parsing of keywords as identifiers

// TODO: implement a parser that generates tokens from chars
// pub fn parser() -> impl Parser<char, Token

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::lexer::lexer;

    #[test]
    fn fn_with_multiple_params() {
        let tokens = lexer().parse("(a, b) => c").unwrap();
        insta::assert_debug_snapshot!(tokens, @r###"
        [
            (
                OpenParen,
                0..1,
            ),
            (
                Ident(
                    "a",
                ),
                1..2,
            ),
            (
                Comma,
                2..3,
            ),
            (
                Ident(
                    "b",
                ),
                4..5,
            ),
            (
                CloseParen,
                5..6,
            ),
            (
                FatArrow,
                7..9,
            ),
            (
                Ident(
                    "c",
                ),
                10..11,
            ),
        ]
        "###);
    }

    #[test]
    fn lex_string_literal() {
        let tokens = lexer().parse("\"hello\"").unwrap();
        insta::assert_debug_snapshot!(tokens, @r###"
        [
            (
                Str(
                    "hello",
                ),
                0..7,
            ),
        ]
        "###);
    }

    #[test]
    fn lex_read_number_literal() {
        let tokens = lexer().parse("1.23").unwrap();
        insta::assert_debug_snapshot!(tokens, @r###"
        [
            (
                Num(
                    "1.23",
                ),
                0..4,
            ),
        ]
        "###);
    }

    #[test]
    fn lex_math_expr() {
        let tokens = lexer().parse("1 + 2 - 3").unwrap();
        insta::assert_debug_snapshot!(tokens, @r###"
        [
            (
                Num(
                    "1",
                ),
                0..1,
            ),
            (
                Plus,
                2..3,
            ),
            (
                Num(
                    "2",
                ),
                4..5,
            ),
            (
                Minus,
                6..7,
            ),
            (
                Num(
                    "3",
                ),
                8..9,
            ),
        ]
        "###);
    }

    #[test]
    fn lex_math_with_identifiers() {
        let tokens = lexer().parse("x * y / z").unwrap();
        insta::assert_debug_snapshot!(tokens, @r###"
        [
            (
                Ident(
                    "x",
                ),
                0..1,
            ),
            (
                Times,
                2..3,
            ),
            (
                Ident(
                    "y",
                ),
                4..5,
            ),
            (
                Div,
                6..7,
            ),
            (
                Ident(
                    "z",
                ),
                8..9,
            ),
        ]
        "###);
    }
}
