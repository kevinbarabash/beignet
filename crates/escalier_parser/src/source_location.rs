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

pub fn merge_locations(left: &SourceLocation, right: &SourceLocation) -> SourceLocation {
    SourceLocation {
        start: left.start.clone(),
        end: right.end.clone(),
    }
}
