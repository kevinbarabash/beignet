use tree_sitter::Node;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

impl<'a> From<&Node<'a>> for SourceLocation {
    fn from(node: &Node) -> Self {
        let range = node.range();
        SourceLocation {
            start: Position {
                line: range.start_point.row as u32,
                column: range.start_point.column as u32,
            },
            end: Position {
                line: range.end_point.row as u32,
                column: range.end_point.column as u32,
            },
        }
    }
}

pub static DUMMY_LOC: SourceLocation = SourceLocation {
    start: Position { line: 0, column: 0 },
    end: Position { line: 0, column: 0 },
};
