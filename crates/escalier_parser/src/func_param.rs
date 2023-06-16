use crate::parser::Parser;
use crate::pattern::Pattern;
use crate::pattern_parser::parse_pattern;
use crate::token::TokenKind;
use crate::type_ann::TypeAnn;
use crate::type_ann_parser::parse_type_ann;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub pattern: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub optional: bool,
}

pub fn parse_params(parser: &mut Parser) -> Vec<FuncParam> {
    assert_eq!(parser.next().kind, TokenKind::LeftParen);

    let mut params: Vec<FuncParam> = Vec::new();
    while parser.peek(0).kind != TokenKind::RightParen {
        let pattern = parse_pattern(parser);

        let optional = if let TokenKind::Question = parser.peek(0).kind {
            parser.next();
            true
        } else {
            false
        };

        if let TokenKind::Colon = parser.peek(0).kind {
            parser.next();
            params.push(FuncParam {
                pattern,
                type_ann: Some(parse_type_ann(parser)),
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

        match parser.peek(0).kind {
            TokenKind::RightParen => break,
            TokenKind::Comma => {
                parser.next();
            }
            _ => panic!("Expected comma or right paren, got {:?}", parser.peek(0)),
        }
    }

    assert_eq!(parser.next().kind, TokenKind::RightParen);

    params
}
