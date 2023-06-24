use std::cmp::{max, min};
use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

pub type Span = Range<usize>;

pub const DUMMY_SPAN: Span = Span { start: 0, end: 0 };

pub fn merge_spans(left: &Span, right: &Span) -> Span {
    Span {
        start: min(left.start, right.start),
        end: max(left.end, right.end),
    }
}
