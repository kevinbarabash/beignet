use crate::source_location::*;
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    // TODO: change this to an iterator
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn next(&mut self) -> Token {
        let result = self.peek();
        self.cursor += 1;
        result
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.get(self.cursor).cloned().unwrap_or(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        })
    }
}
