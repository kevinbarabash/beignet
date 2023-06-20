use core::panic;
use std::iter::Iterator;

use crate::expr::Expr;
use crate::expr_parser::parse_expr;
use crate::identifier::Ident;
use crate::jsx::*;
use crate::scanner::Scanner;
use crate::source_location::*;
use crate::token::*;

#[derive(Clone)]
pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    brace_count: usize,
    end_delims: Vec<char>,
    peeked: Option<Token>,
}

impl<'a> Iterator for Lexer<'a> {
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

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            scanner: Scanner::new(input),
            brace_count: 0,
            end_delims: vec![],
            peeked: None,
        }
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

            // TODO: handle closing brace in string interpolations
            match self.end_delims.last() {
                Some(c) if character == *c && self.brace_count == 0 => {
                    self.scanner.pop();
                    return None;
                }
                _ => (),
            }

            let start = self.scanner.position();

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
                    return Some(self.lex_template_string(start));
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
                    self.brace_count += 1;
                    TokenKind::LeftBrace
                }
                '}' => {
                    self.brace_count -= 1;
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
                loc: SourceLocation {
                    start,
                    end: self.scanner.position(),
                },
            })
        } else {
            None
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        self.lex_to(None)
    }

    pub fn lex_to(&mut self, delim: Option<char>) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut brace_count = 0;
        // let mut bracket_count = 0;
        // let mut paren_count = 0;
        while !self.scanner.is_done() {
            let character = self.scanner.peek(0).unwrap();

            match delim {
                Some(c) if character == c && brace_count == 0 => {
                    self.scanner.pop();
                    return tokens;
                }
                _ => (),
            }

            let start = self.scanner.position();
            let kind = match character {
                'a'..='z' | 'A'..='Z' | '_' => {
                    tokens.push(self.lex_ident_or_keyword());
                    continue;
                }
                '0'..='9' => {
                    tokens.push(self.lex_number());
                    continue;
                }
                '"' => {
                    tokens.push(self.lex_string());
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
                    brace_count += 1;
                    TokenKind::LeftBrace
                }
                '}' => {
                    brace_count -= 1;
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

    pub fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.scanner.position();
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
            "var" => TokenKind::Var,
            "mut" => TokenKind::Mut,
            "match" => TokenKind::Match,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "finally" => TokenKind::Finally,
            "do" => TokenKind::Do,
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
            loc: SourceLocation {
                start,
                end: self.scanner.position(),
            },
        }
    }

    pub fn lex_number(&mut self) -> Token {
        let start = self.scanner.position();

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

    pub fn lex_string(&mut self) -> Token {
        let start = self.scanner.position();

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

                        // let mut lexer = self.clone();
                        // TODO: end_delim really needs to be a stack
                        self.end_delims.push('}');

                        exprs.push(parse_expr(self));

                        self.end_delims.pop();

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

    pub fn lex_jsx_element(&mut self) -> JSXElement {
        assert_eq!(self.scanner.pop(), Some('<'));
        let name_token = self.lex_ident_or_keyword();
        let name = match name_token.kind {
            TokenKind::Identifier(name) => JSXElementName::Ident(Ident {
                name,
                loc: SourceLocation {
                    start: name_token.loc.start,
                    end: name_token.loc.end,
                },
            }),
            _ => panic!("Expected identifier or keyword"),
        };

        let mut attrs = vec![];
        let mut self_closing = false;

        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '/' => {
                    self.scanner.pop();
                    assert_eq!(self.scanner.pop(), Some('>'));
                    self_closing = true;
                    break;
                }
                '>' => {
                    self.scanner.pop();
                    break;
                }
                ' ' => {
                    self.scanner.pop();
                }
                _ => {
                    attrs.push(self.lex_jsx_attribute());
                }
            }
        }

        let opening = JSXOpeningElement {
            name,
            attrs,
            self_closing,
        };

        let children = vec![];

        let closing = if self_closing {
            None
        } else {
            // TODO: parse children
            while self.scanner.peek(0) != Some('<') {
                self.scanner.pop();
            }

            assert_eq!(self.scanner.pop(), Some('<'));
            assert_eq!(self.scanner.pop(), Some('/'));
            let end_name = self.lex_ident_or_keyword();
            assert_eq!(self.scanner.pop(), Some('>'));

            Some(JSXClosingElement {
                name: match end_name.kind {
                    TokenKind::Identifier(name) => JSXElementName::Ident(Ident {
                        name,
                        loc: SourceLocation {
                            start: end_name.loc.start,
                            end: end_name.loc.end,
                        },
                    }),
                    _ => panic!("Expected identifier or keyword"),
                },
            })
        };

        JSXElement {
            opening,
            children,
            closing,
        }
    }

    pub fn lex_jsx_attribute(&mut self) -> JSXAttr {
        let name = self.lex_ident_or_keyword();

        let name = match name.kind {
            TokenKind::Identifier(name) => name,
            _ => panic!("Unexpected token"),
        };

        if let Some('=') = self.scanner.peek(0) {
            self.scanner.pop();
        } else {
            return JSXAttr { name, value: None };
        }

        match self.scanner.peek(0).unwrap() {
            '"' => {
                let value = self.lex_string();
                let value = match value.kind {
                    TokenKind::StrLit(value) => value,
                    _ => panic!("Unexpected token"),
                };

                JSXAttr {
                    name,
                    value: Some(JSXAttrValue::Str(value)),
                }
            }
            '{' => {
                self.scanner.pop();

                let mut lexer = self.clone();
                lexer.end_delims.push('}');

                let expr = parse_expr(&mut lexer);

                JSXAttr {
                    name,
                    value: Some(JSXAttrValue::ExprContainer(JSXExprContainer {
                        expr: Box::new(expr),
                    })),
                }
            }
            _ => panic!("Unexpected character"),
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
        assert_eq!(
            tokens[2].kind,
            crate::token::TokenKind::NumLit("123".to_string())
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
    fn lex_string_escapes() {
        let mut lexer = Lexer::new(r#""\"\\\/\b\f\n\r\t\u221E""#);

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::StrLit("\"\\/\u{8}\u{c}\n\r\t∞".to_string())
        );
    }

    #[test]
    fn lex_template_string() {
        let mut lexer = Lexer::new("`abc`");

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_template_string_escapes() {
        let mut lexer = Lexer::new(r#"`\`\/\b\f\n\r\t\u221E`"#);

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            TokenKind::StrTemplateLit {
                parts: vec![Token {
                    kind: TokenKind::StrLit("`/\u{8}\u{c}\n\r\t∞".to_string()),
                    loc: SourceLocation {
                        start: Position { line: 1, column: 1 },
                        end: Position {
                            line: 1,
                            column: 23
                        }
                    }
                }],
                exprs: vec![]
            }
        );
    }

    #[test]
    fn lex_template_string_with_exprs() {
        let mut lexer = Lexer::new("`abc${x}`");

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_nested_template_strings() {
        let mut lexer = Lexer::new(r#"`a${`b${c}`}`"#);

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_nested_template_strings_complex() {
        let mut lexer = Lexer::new(r#"`ids = ${ids.map(fn (id) => `x${id}`).join(", ")}`"#);

        let tokens = lexer.lex();

        insta::assert_debug_snapshot!(tokens[0].kind);
    }

    #[test]
    fn lex_dots() {
        let mut lexer = Lexer::new(". .. ...");

        let tokens = lexer.lex();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::Dot);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::DotDot);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::DotDotDot);
    }

    #[test]
    fn lex_dots_reverse() {
        let mut lexer = Lexer::new("... .. .");

        let tokens = lexer.lex();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::DotDotDot);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::DotDot);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::Dot);
    }

    #[test]
    fn lex_assignment() {
        let mut lexer = Lexer::new("= += -= *= /= %=");

        let tokens = lexer.lex();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::Assign);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::PlusAssign);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::MinusAssign);
        assert_eq!(tokens[3].kind, crate::token::TokenKind::TimesAssign);
        assert_eq!(tokens[4].kind, crate::token::TokenKind::DivideAssign);
        assert_eq!(tokens[5].kind, crate::token::TokenKind::ModuloAssign);
    }

    #[test]
    fn lex_jsx_element() {
        let mut lexer = Lexer::new(r#"<Foo bar="baz" qux></Foo>"#);

        let jsx_elem = lexer.lex_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn lex_self_closing_jsx_element() {
        let mut lexer = Lexer::new(r#"<Foo bar="baz" qux />"#);

        let jsx_elem = lexer.lex_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }
}
