use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use crate::parser::types::*;
use crate::ast::*;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

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
