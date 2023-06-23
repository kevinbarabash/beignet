use crate::jsx::{JSXElement, JSXFragment};
use crate::parser::*;
use crate::source_location::*;
use crate::token::{Token, TokenKind};

const EOF: Token = Token {
    kind: TokenKind::Eof,
    loc: SourceLocation {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    },
};

pub fn parse_jsx_element(lexer: &mut Parser) -> JSXElement {
    todo!();
}

pub fn parse_jsx_fragment(lexer: &mut Parser) -> JSXFragment {
    assert_eq!(
        lexer.next().unwrap_or(EOF.clone()).kind,
        TokenKind::LessThan
    );
    assert_eq!(
        lexer.next().unwrap_or(EOF.clone()).kind,
        TokenKind::GreaterThan
    );

    todo!();
}
