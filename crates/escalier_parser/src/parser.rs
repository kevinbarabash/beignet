use std::iter::{Iterator, Peekable};

use crate::lexer::Lexer;
use crate::source_location::*;
use crate::token::{Token, TokenKind};

pub struct Parser<'a> {
    // tokens: Peekable<Box<dyn Iterator<Item = Token>>>,
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    pub fn next(&mut self) -> Token {
        self.lexer.next().unwrap_or(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        })
    }

    pub fn peek(&mut self) -> Token {
        self.lexer.peek().cloned().unwrap_or(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        })
    }
}
