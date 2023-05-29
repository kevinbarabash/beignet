use core::panic;

use crate::scanner::Scanner;
use crate::source_location::*;
use crate::token::*;

pub struct Lexer {
    scanner: Scanner,
}

impl Lexer {
    pub fn new(string: &str) -> Self {
        Self {
            scanner: Scanner::new(string),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();
            let start = self.scanner.position();
            let kind = match character {
                'a'..='z' | 'A'..='Z' | '_' => {
                    tokens.push(self.lex_ident(start));
                    continue;
                }
                '0'..='9' => {
                    tokens.push(self.lex_number(start));
                    continue;
                }
                '=' => match self.scanner.peek(1) {
                    Some('=') => {
                        self.scanner.pop();
                        TokenKind::Equals
                    }
                    _ => TokenKind::Assign,
                },
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '{' => TokenKind::LeftBrace,
                '}' => TokenKind::RightBrace,
                '[' => TokenKind::LeftBracket,
                ']' => TokenKind::RightBracket,
                ',' => TokenKind::Comma,
                '.' => TokenKind::Dot,
                ';' => TokenKind::Semicolon,
                ':' => TokenKind::Colon,
                '?' => TokenKind::Question,
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
                _ => {
                    // TODO: Error handling
                    self.scanner.pop();
                    continue;
                }
            };
            self.scanner.pop();
            tokens.push(Token {
                kind,
                loc: SourceLocation {
                    start,
                    end: self.scanner.position(),
                },
            });
        }
        tokens
    }

    pub fn lex_ident(&mut self, start: Position) -> Token {
        let mut ident = String::new();
        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();
            match character {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    ident.push(*character);
                    self.scanner.pop();
                }
                _ => {
                    break;
                }
            }
        }
        Token {
            kind: TokenKind::Identifier(ident),
            loc: SourceLocation {
                start,
                end: self.scanner.position(),
            },
        }
    }

    pub fn lex_number(&mut self, start: Position) -> Token {
        let mut number = String::new();
        let mut decimal = false;
        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();
            match character {
                '0'..='9' => {
                    number.push(*character);
                    self.scanner.pop();
                }
                '.' => {
                    if decimal {
                        panic!("Unexpected character: '{}'", character);
                    }
                    number.push(*character);
                    self.scanner.pop();
                    decimal = true;
                }
                _ => {
                    break;
                }
            }
        }
        Token {
            kind: TokenKind::Number(number),
            loc: SourceLocation {
                start,
                end: self.scanner.position(),
            },
        }
    }
}
