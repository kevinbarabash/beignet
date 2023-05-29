use crate::token::Position;

pub struct Scanner {
    cursor: usize,
    column: usize,
    line: usize,
    characters: Vec<char>,
}

impl Scanner {
    pub fn new(string: &str) -> Self {
        Self {
            cursor: 0,
            column: 1,
            line: 1,
            characters: string.chars().collect(),
        }
    }

    /// Returns the current cursor. Useful for reporting errors.
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    /// Returns the next character without advancing the cursor.
    /// AKA "lookahead"
    pub fn peek(&self, lookahead: usize) -> Option<&char> {
        self.characters.get(self.cursor + lookahead)
    }

    /// Returns true if further progress is not possible.
    pub fn is_done(&self) -> bool {
        self.cursor == self.characters.len()
    }

    /// Returns the next character (if available) and advances the cursor.
    pub fn pop(&mut self) -> Option<&char> {
        match self.characters.get(self.cursor) {
            Some(character) => {
                self.cursor += 1;
                if *character == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }

                Some(character)
            }
            None => None,
        }
    }
}
