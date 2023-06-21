use crate::lexer::*;
use crate::pattern::Pattern;
use crate::pattern_parser::parse_pattern;
use crate::source_location::*;
use crate::token::{Token, TokenKind};
use crate::type_ann::TypeAnn;
use crate::type_ann_parser::parse_type_ann;

const EOF: Token = Token {
    kind: TokenKind::Eof,
    loc: SourceLocation {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    },
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub pattern: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub optional: bool,
}

pub fn parse_params(lexer: &mut Lexer) -> Vec<FuncParam> {
    assert_eq!(
        lexer.next().unwrap_or(EOF.clone()).kind,
        TokenKind::LeftParen
    );

    let mut params: Vec<FuncParam> = Vec::new();
    while lexer.peek().unwrap_or(&EOF).kind != TokenKind::RightParen {
        let pattern = parse_pattern(lexer);

        let optional = if let TokenKind::Question = lexer.peek().unwrap_or(&EOF).kind {
            lexer.next().unwrap_or(EOF.clone());
            true
        } else {
            false
        };

        if let TokenKind::Colon = lexer.peek().unwrap_or(&EOF).kind {
            lexer.next().unwrap_or(EOF.clone());
            params.push(FuncParam {
                pattern,
                type_ann: Some(parse_type_ann(lexer)),
                optional,
            });
        } else {
            params.push(FuncParam {
                pattern,
                type_ann: None,
                optional: false, // Should `?` be supported when there's not type param?
            });
        }

        // TODO: param defaults

        match lexer.peek().unwrap_or(&EOF).kind {
            TokenKind::RightParen => break,
            TokenKind::Comma => {
                lexer.next().unwrap_or(EOF.clone());
            }
            _ => panic!(
                "Expected comma or right paren, got {:?}",
                lexer.peek().unwrap_or(&EOF)
            ),
        }
    }

    assert_eq!(
        lexer.next().unwrap_or(EOF.clone()).kind,
        TokenKind::RightParen
    );

    params
}
