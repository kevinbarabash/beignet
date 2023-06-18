use std::iter::{Iterator, Peekable};

use crate::source_location::*;
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Peekable<Box<dyn Iterator<Item = Token>>>,
}

impl Parser {
    pub fn new(tokens: Box<dyn Iterator<Item = Token>>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        })
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.peek().cloned().unwrap_or(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        })
    }
}
