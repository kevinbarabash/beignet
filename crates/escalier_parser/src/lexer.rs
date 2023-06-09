use core::panic;

use crate::expr::Expr;
use crate::expr_parser::parse_expr;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::source_location::*;
use crate::token::*;

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a str) -> Self {
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
                '"' => {
                    tokens.push(self.lex_string(start));
                    continue;
                }
                '`' => {
                    tokens.push(self.lex_template_string(start));
                    continue;
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
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Times,
                '/' => TokenKind::Divide,
                '%' => TokenKind::Modulo,
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

    // TODO: we need to match braces, we can't just stop at the first '}' we see.
    // Instead of using vectors, consider user iterables.  That way the expression
    // parser can decide when to stop.
    pub fn lex_to(&mut self, c: char) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();
            if c == character {
                self.scanner.pop();
                return tokens;
            }
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
                '"' => {
                    tokens.push(self.lex_string(start));
                    continue;
                }
                '`' => {
                    tokens.push(self.lex_template_string(start));
                    continue;
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
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Times,
                '/' => TokenKind::Divide,
                '%' => TokenKind::Modulo,
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
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            "let" => TokenKind::Let,
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
            loc: SourceLocation {
                start,
                end: self.scanner.position(),
            },
        }
    }

    pub fn lex_string(&mut self, start: Position) -> Token {
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
                    let escaped = self.scanner.peek(0).unwrap();
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
                            self.scanner.pop();
                            let mut code = String::new();
                            for _ in 0..4 {
                                code.push(self.scanner.peek(0).unwrap());
                                self.scanner.pop();
                            }
                            let code = u32::from_str_radix(&code, 16).unwrap();
                            string.push(char::from_u32(code).unwrap());
                        }
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
            loc: SourceLocation {
                start,
                end: self.scanner.position(),
            },
        }
    }

    pub fn lex_template_string(&mut self, start: Position) -> Token {
        let mut string = String::new();
        let mut parts: Vec<Token> = vec![];
        let mut exprs: Vec<Expr> = vec![];
        let mut string_start = start.clone();
        self.scanner.pop();
        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '`' => {
                    self.scanner.pop();
                    break;
                }
                '\\' => {
                    self.scanner.pop();
                    let escaped = self.scanner.peek(0).unwrap();
                    match escaped {
                        '`' => string.push('`'),
                        '"' => string.push('"'),
                        '\\' => string.push('\\'),
                        '/' => string.push('/'),
                        'b' => string.push('\u{0008}'),
                        'f' => string.push('\u{000c}'),
                        'n' => string.push('\n'),
                        'r' => string.push('\r'),
                        't' => string.push('\t'),
                        'u' => {
                            self.scanner.pop();
                            let mut code = String::new();
                            for _ in 0..4 {
                                code.push(self.scanner.peek(0).unwrap());
                                self.scanner.pop();
                            }
                            let code = u32::from_str_radix(&code, 16).unwrap();
                            string.push(char::from_u32(code).unwrap());
                        }
                        character => panic!("Unexpected character: '{}'", character),
                    }
                }
                '$' => {
                    let string_end = self.scanner.position();
                    self.scanner.pop();
                    if self.scanner.peek(0).unwrap() == '{' {
                        parts.push(Token {
                            kind: TokenKind::StrLit(string),
                            loc: SourceLocation {
                                start: string_start,
                                end: string_end,
                            },
                        });
                        self.scanner.pop();
                        let tokens = self.lex_to('}');

                        let mut parser = Parser::new(tokens);
                        exprs.push(parse_expr(&mut parser));

                        string = String::new();
                        string_start = self.scanner.position();
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
            loc: SourceLocation {
                start: string_start,
                end: self.scanner.position(),
            },
        });

        Token {
            kind: TokenKind::StrTemplateLit { parts, exprs },
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

    #[test]
    fn lex_string() {
        let mut lexer = Lexer::new("\"abc\"");

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::StrLit("abc".to_string())
        );
    }

    #[test]
    fn lex_template_string() {
        let mut lexer = Lexer::new("`abc`");

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_template_string_with_exprs() {
        let mut lexer = Lexer::new("`abc${x}`");

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_nested_template_strings() {
        let mut lexer = Lexer::new(r#"`a${b`c${d}`}`"#);

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_nested_template_strings_complex() {
        let mut lexer = Lexer::new(r#"`ids = ${ids.map(fn (id) => id)).join(", ")}`"#);

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }
}
