use core::panic;
use std::iter::Iterator;

use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::scanner::Scanner;
use crate::token::*;

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    pub scanner: Scanner<'a>,
    pub brace_counts: Vec<usize>,
    pub peeked: Option<Token>,
}

impl<'a> Iterator for Parser<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match &self.peeked {
            Some(token) => Some(token.to_owned()),
            None => self.take(),
        };
        self.peeked = None;
        result
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            scanner: Scanner::new(input),
            brace_counts: vec![0], // we need separate brace counts for each mode
            peeked: None,
        }
    }

    pub fn restore(&mut self, backup: Parser<'a>) {
        self.scanner = backup.scanner;
        self.brace_counts = backup.brace_counts;
        self.peeked = backup.peeked;
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.take();
        }
        match &self.peeked {
            Some(value) => Some(value),
            _ => None,
        }
    }

    fn take(&mut self) -> Option<Token> {
        if !self.scanner.is_done() {
            let mut character = match self.scanner.peek(0) {
                Some(c) => c,
                None => return None,
            };

            let start = self.scanner.cursor();

            // skip whitespace
            while character == ' ' || character == '\n' || character == '\t' {
                self.scanner.pop();
                match self.scanner.peek(0) {
                    Some(c) => character = c,
                    None => return None,
                }
            }

            let kind = match character {
                'a'..='z' | 'A'..='Z' | '_' => {
                    // avoids an extra scanner.pop() call after the match
                    return Some(self.lex_ident_or_keyword());
                }
                '0'..='9' => {
                    // avoids an extra scanner.pop() call after the match
                    return Some(self.lex_number());
                }
                '"' => {
                    // avoids an extra scanner.pop() call after the match
                    return Some(self.lex_string());
                }
                '`' => {
                    // avoids an extra scanner.pop() call after the match
                    return match self.lex_template_string(start) {
                        Ok(token) => Some(token),
                        Err(ParseError { message }) => panic!("{}", message),
                    };
                }
                '=' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::Equals
                    }
                    Some('>') => {
                        self.scanner.pop();
                        TokenKind::Arrow
                    }
                    _ => TokenKind::Assign,
                },
                '+' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::PlusAssign
                    }
                    _ => TokenKind::Plus,
                },
                '-' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::MinusAssign
                    }
                    _ => TokenKind::Minus,
                },
                '*' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::TimesAssign
                    }
                    _ => TokenKind::Times,
                },
                '/' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::DivideAssign
                    }
                    _ => TokenKind::Divide,
                },
                '%' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::ModuloAssign
                    }
                    _ => TokenKind::Modulo,
                },
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '{' => {
                    let brace_count = self.brace_counts.last_mut().unwrap();
                    *brace_count += 1;
                    TokenKind::LeftBrace
                }
                '}' => {
                    let brace_count = self.brace_counts.last_mut().unwrap();
                    // We've matched already matched all of the braces but have
                    // encountered an extra closing brace.  We could be in a
                    // template string or a JSX expression so we return and let
                    // the caller handle it.  The caller is responsible for
                    // consuming this token.
                    if brace_count == &0 {
                        return None;
                    }
                    *brace_count -= 1;
                    TokenKind::RightBrace
                }
                '[' => TokenKind::LeftBracket,
                ']' => TokenKind::RightBracket,
                ',' => TokenKind::Comma,
                '.' => {
                    if self.scanner.peek(1) == Some('.') {
                        if self.scanner.peek(2) == Some('.') {
                            self.scanner.pop();
                            self.scanner.pop();
                            TokenKind::DotDotDot
                        } else {
                            self.scanner.pop();
                            TokenKind::DotDot
                        }
                    } else {
                        TokenKind::Dot
                    }
                }
                ';' => TokenKind::Semicolon,
                ':' => TokenKind::Colon,
                '?' => match self.scanner.peek(1) {
                    Some('.') => {
                        self.scanner.pop();
                        TokenKind::QuestionDot
                    }
                    _ => TokenKind::Question,
                },
                '<' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::LessThanOrEqual
                    }
                    _ => TokenKind::LessThan,
                },
                '>' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::GreaterThanOrEqual
                    }
                    _ => TokenKind::GreaterThan,
                },
                '!' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::NotEquals
                    }
                    _ => panic!("Unexpected character: '{}'", character),
                },
                '&' => match self.scanner.peek(1) {
                    Some('&') => {
                        self.scanner.pop();
                        TokenKind::And
                    }
                    _ => panic!("Unexpected character: '{}'", character),
                },
                '|' => match self.scanner.peek(1) {
                    Some('|') => {
                        self.scanner.pop();
                        TokenKind::Or
                    }
                    _ => panic!("Unexpected character: '{}'", character),
                },
                _ => panic!("Unexpected character: '{}'", character),
            };
            self.scanner.pop();

            Some(Token {
                kind,
                span: Span {
                    start,
                    end: self.scanner.cursor(),
                },
            })
        } else {
            None
        }
    }

    pub fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.scanner.cursor();
        let mut ident = String::new();
        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();
            match character {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    ident.push(character);
                    self.scanner.pop();
                }
                _ => {
                    break;
                }
            }
        }
        let kind = match ident.as_ref() {
            "fn" => TokenKind::Fn,
            "get" => TokenKind::Get,
            "set" => TokenKind::Set,
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            "gen" => TokenKind::Gen,
            "yield" => TokenKind::Yield,
            "let" => TokenKind::Let,
            "var" => TokenKind::Var,
            "mut" => TokenKind::Mut,
            "match" => TokenKind::Match,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "finally" => TokenKind::Finally,
            "do" => TokenKind::Do,
            "class" => TokenKind::Class,
            "extends" => TokenKind::Extends,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::BoolLit(true),
            "false" => TokenKind::BoolLit(false),
            "null" => TokenKind::Null,
            "undefined" => TokenKind::Undefined,
            "number" => TokenKind::Number,
            "string" => TokenKind::String,
            "boolean" => TokenKind::Boolean,
            "symbol" => TokenKind::Symbol,
            "_" => TokenKind::Underscore,
            _ => TokenKind::Identifier(ident),
        };
        Token {
            kind,
            span: Span {
                start,
                end: self.scanner.cursor(),
            },
        }
    }

    pub fn lex_number(&mut self) -> Token {
        let start = self.scanner.cursor();

        let mut number = String::new();
        let mut decimal = false;

        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();
            match character {
                '0'..='9' => {
                    number.push(character);
                    self.scanner.pop();
                }
                '.' => {
                    if decimal {
                        panic!("Unexpected character: '{}'", character);
                    }
                    number.push(character);
                    self.scanner.pop();
                    decimal = true;
                }
                _ => {
                    break;
                }
            }
        }
        Token {
            kind: TokenKind::NumLit(number),
            span: Span {
                start,
                end: self.scanner.cursor(),
            },
        }
    }

    pub fn lex_string(&mut self) -> Token {
        let start = self.scanner.cursor();

        let mut string = String::new();
        self.scanner.pop();

        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '"' => {
                    self.scanner.pop();
                    break;
                }
                '\\' => {
                    self.scanner.pop();
                    let escaped = self.scanner.pop().unwrap();
                    match escaped {
                        '"' => string.push('"'),
                        '\\' => string.push('\\'),
                        '/' => string.push('/'),
                        'b' => string.push('\u{0008}'),
                        'f' => string.push('\u{000c}'),
                        'n' => string.push('\n'),
                        'r' => string.push('\r'),
                        't' => string.push('\t'),
                        'u' => {
                            let mut code = String::new();
                            for _ in 0..4 {
                                code.push(self.scanner.peek(0).unwrap());
                                self.scanner.pop();
                            }
                            let code = u32::from_str_radix(&code, 16).unwrap();
                            string.push(char::from_u32(code).unwrap());
                        }
                        // NOTE: This doesn't match JS behavior
                        character => panic!("Unexpected character: '{}'", character),
                    }
                }
                character => {
                    string.push(character);
                    self.scanner.pop();
                }
            }
        }
        Token {
            kind: TokenKind::StrLit(string),
            span: Span {
                start,
                end: self.scanner.cursor(),
            },
        }
    }

    pub fn lex_template_string(&mut self, start: usize) -> Result<Token, ParseError> {
        let mut string = String::new();
        let mut parts: Vec<Token> = vec![];
        let mut exprs: Vec<Expr> = vec![];
        let mut string_start = start;
        self.scanner.pop();
        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '`' => {
                    self.scanner.pop();
                    break;
                }
                '\\' => {
                    self.scanner.pop();
                    let escaped = self.scanner.pop().unwrap();
                    match escaped {
                        '`' => string.push('`'),
                        '/' => string.push('/'),
                        'b' => string.push('\u{0008}'),
                        'f' => string.push('\u{000c}'),
                        'n' => string.push('\n'),
                        'r' => string.push('\r'),
                        't' => string.push('\t'),
                        'u' => {
                            let mut code = String::new();
                            for _ in 0..4 {
                                code.push(self.scanner.peek(0).unwrap());
                                self.scanner.pop();
                            }
                            let code = u32::from_str_radix(&code, 16).unwrap();
                            string.push(char::from_u32(code).unwrap());
                        }
                        // NOTE: This doesn't match JS behavior
                        character => panic!("Unexpected character: '{}'", character),
                    }
                }
                '$' => {
                    let string_end = self.scanner.cursor();
                    self.scanner.pop();
                    if self.scanner.peek(0).unwrap() == '{' {
                        parts.push(Token {
                            kind: TokenKind::StrLit(string),
                            span: Span {
                                start: string_start,
                                end: string_end,
                            },
                        });
                        self.scanner.pop(); // consumes '{'

                        self.brace_counts.push(0);
                        exprs.push(self.parse_expr()?);
                        self.brace_counts.pop();

                        self.scanner.pop(); // consumes '}'

                        string = String::new();
                        string_start = self.scanner.cursor();
                    } else {
                        string.push('$');
                    }
                }
                character => {
                    string.push(character);
                    self.scanner.pop();
                }
            }
        }

        parts.push(Token {
            kind: TokenKind::StrLit(string),
            span: Span {
                start: string_start,
                end: self.scanner.cursor(),
            },
        });

        Ok(Token {
            kind: TokenKind::StrTemplateLit { parts, exprs },
            span: Span {
                start,
                end: self.scanner.cursor(),
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_identifiers() {
        let parser = Parser::new("abc _a0 123");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::Identifier("abc".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            crate::token::TokenKind::Identifier("_a0".to_string())
        );
        assert_eq!(
            tokens[2].kind,
            crate::token::TokenKind::NumLit("123".to_string())
        );
    }

    #[test]
    fn lex_numbers() {
        let parser = Parser::new("123 1.23");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::NumLit("123".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            crate::token::TokenKind::NumLit("1.23".to_string())
        );
    }

    #[test]
    #[should_panic = "Unexpected character: '.'"]
    fn lex_number_multiple_decimals_error() {
        let parser = Parser::new("1.2.3");

        let _ = parser.collect::<Vec<_>>();
    }

    #[test]
    fn lex_comparison_ops() {
        let parser = Parser::new("> >= < <= == !=");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::GreaterThan);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::GreaterThanOrEqual);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::LessThan);
        assert_eq!(tokens[3].kind, crate::token::TokenKind::LessThanOrEqual);
        assert_eq!(tokens[4].kind, crate::token::TokenKind::Equals);
        assert_eq!(tokens[5].kind, crate::token::TokenKind::NotEquals);
    }

    #[test]
    #[should_panic = "Unexpected character: '!'"]
    fn lex_unexpected_exclamation() {
        let parser = Parser::new("1 ! 2");

        let _ = parser.collect::<Vec<_>>();
    }

    #[test]
    fn lex_string() {
        let parser = Parser::new("\"abc\"");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::StrLit("abc".to_string())
        );
    }

    #[test]
    fn lex_string_escapes() {
        let parser = Parser::new(r#""\"\\\/\b\f\n\r\t\u221E""#);

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::StrLit("\"\\/\u{8}\u{c}\n\r\t∞".to_string())
        );
    }

    #[test]
    fn lex_template_string() {
        let parser = Parser::new("`abc`");

        let tokens = parser.collect::<Vec<_>>();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_template_string_escapes() {
        let parser = Parser::new(r#"`\`\/\b\f\n\r\t\u221E`"#);

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(
            tokens[0].kind,
            TokenKind::StrTemplateLit {
                parts: vec![Token {
                    kind: TokenKind::StrLit("`/\u{8}\u{c}\n\r\t∞".to_string()),
                    span: Span { start: 0, end: 22 },
                }],
                exprs: vec![]
            }
        );
    }

    #[test]
    fn lex_template_string_with_exprs() {
        let parser = Parser::new("`abc${x}`");

        let tokens = parser.collect::<Vec<_>>();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_nested_template_strings() {
        let parser = Parser::new(r#"`a${`b${c}`}`"#);

        let tokens = parser.collect::<Vec<_>>();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_nested_template_strings_complex() {
        let parser = Parser::new(r#"`ids = ${ids.map(fn (id) => `x${id}`).join(", ")}`"#);

        let tokens = parser.collect::<Vec<_>>();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_dots() {
        let parser = Parser::new(". .. ...");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::Dot);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::DotDot);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::DotDotDot);
    }

    #[test]
    fn lex_dots_reverse() {
        let parser = Parser::new("... .. .");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::DotDotDot);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::DotDot);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::Dot);
    }

    #[test]
    fn lex_assignment() {
        let parser = Parser::new("= += -= *= /= %=");

        let tokens = parser.collect::<Vec<_>>();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::Assign);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::PlusAssign);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::MinusAssign);
        assert_eq!(tokens[3].kind, crate::token::TokenKind::TimesAssign);
        assert_eq!(tokens[4].kind, crate::token::TokenKind::DivideAssign);
        assert_eq!(tokens[5].kind, crate::token::TokenKind::ModuloAssign);
    }
}
