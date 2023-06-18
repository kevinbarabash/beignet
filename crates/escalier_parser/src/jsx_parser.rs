use crate::identifier::{BindingIdent, Ident};
use crate::jsx::{JSXElement, JSXFragment};
use crate::literal::Literal;
use crate::parser::Parser;
use crate::pattern::*;
use crate::source_location::merge_locations;
use crate::token::TokenKind;

pub fn parse_jsx_element(parser: &mut Parser) -> JSXElement {
    todo!();
}

pub fn parse_jsx_fragment(parser: &mut Parser) -> JSXFragment {
    assert_eq!(parser.next().kind, TokenKind::LessThan);
    assert_eq!(parser.next().kind, TokenKind::GreaterThan);

    todo!();
}
