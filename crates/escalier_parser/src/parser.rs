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
        let result = self.peek(0);
        self.cursor += 1;
        result
    }

    pub fn peek(&mut self, offset: usize) -> Token {
        self.tokens
            .get(self.cursor + offset)
            .cloned()
            .unwrap_or(Token {
                kind: TokenKind::Eof,
                loc: SourceLocation {
                    start: Position { line: 0, column: 0 },
                    end: Position { line: 0, column: 0 },
                },
            })
    }

    pub fn replace(&mut self, offset: usize, token: Token) {
        self.tokens[self.cursor + offset] = token;
    }
}
