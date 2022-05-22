use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::types::*;
use crate::parser::util::just_with_padding;

pub fn pattern_parser() -> impl Parser<char, Pattern, Error = Simple<char>> {
    let type_ann = type_parser();

    text::ident()
        .then(just_with_padding(":").ignore_then(type_ann).or_not())
        .map_with_span(|(name, type_ann), span: Span| {
            Pattern::Ident(BindingIdent {
                span: span.clone(),
                id: Ident {
                    name,
                    span: span.clone(),
                },
                type_ann,
            })
        })
        .padded()
}
