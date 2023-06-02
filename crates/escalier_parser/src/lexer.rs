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
                    tokens.push(self.lex_ident_or_keyword(start));
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
                '*' => TokenKind::Times,
                '/' => TokenKind::Divide,
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
                _ => {
                    // It's okay fo
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

    pub fn lex_ident_or_keyword(&mut self, start: Position) -> Token {
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
        let kind = match ident.as_ref() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            _ => TokenKind::Identifier(ident),
        };
        Token {
            kind,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_identifiers() {
        let mut lexer = Lexer::new("abc _a0 123");

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::Identifier("abc".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            crate::token::TokenKind::Identifier("_a0".to_string())
        );
    }

    #[test]
    fn lex_numbers() {
        let mut lexer = Lexer::new("123 1.23");

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::Number("123".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            crate::token::TokenKind::Number("1.23".to_string())
        );
    }

    #[test]
    #[should_panic = "Unexpected character: '.'"]
    fn lex_number_multiple_decimals_error() {
        let mut lexer = Lexer::new("1.2.3");

        lexer.lex();
    }

    #[test]
    fn lex_comparison_ops() {
        let mut lexer = Lexer::new("> >= < <= == !=");

        let tokens = lexer.lex();

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
        let mut lexer = Lexer::new("1 ! 2");

        lexer.lex();
    }
}
