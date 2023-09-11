use swc_common::source_map::{BytePos, SourceFile};

use lsp_types::Position;

#[derive(Clone)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

pub fn get_location(file: &SourceFile, offset: u32) -> Option<Position> {
    let byte_pos = BytePos(offset);
    let line = file.lookup_line(byte_pos)?;
    let start_pos = file.line_begin_pos(byte_pos);
    let column = byte_pos - start_pos;

    Some(Position {
        line: line as u32 + 1,
        character: column.0 + 1,
    })
}

pub fn get_byte_pos(file: &SourceFile, pos: &Position) -> Option<BytePos> {
    let (start, _) = file.line_bounds(pos.line as usize - 1);
    let byte_pos = start + BytePos(pos.character - 1);

    Some(byte_pos)
}
