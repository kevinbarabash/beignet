use crate::source_location::Position;

#[derive(Clone)]
pub struct Scanner<'a> {
    cursor: usize,
    column: usize,
    line: usize,
    input: &'a str,
    // characters: Vec<char>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: 0,
            column: 1,
            line: 1,
            input,
            // characters: string.chars().collect(),
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
    pub fn peek(&self, lookahead: usize) -> Option<char> {
        let start = self.cursor + lookahead;
        let end = start + 1;
        self.input
            .get(start..end)
            .map(|sub_str| sub_str.chars().next().unwrap())
    }

    /// Returns true if further progress is not possible.
    pub fn is_done(&self) -> bool {
        self.cursor == self.input.len()
    }

    /// Returns the next character (if available) and advances the cursor.
    pub fn pop(&mut self) -> Option<char> {
        let start = self.cursor;
        let end = start + 1;
        match self.input.get(start..end) {
            Some(str) => {
                self.cursor += 1;
                if str == "\n" {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }

                str.get(0..1).map(|sub_str| sub_str.chars().next().unwrap())
            }
            None => None,
        }
    }

    // pub fn backup(&mut self) ->
}
