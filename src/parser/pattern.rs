use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::types::*;
use crate::parser::util::just_with_padding;

// TODO: handle destructuring of objects and arrays
pub fn pattern_parser() -> BoxedParser<'static, char, Pattern, Simple<char>> {
    let type_ann = type_parser();

    let parser = text::ident()
        .map_with_span(|name, span| Ident { name, span })
        .then(just_with_padding(":").ignore_then(type_ann).or_not())
        .map_with_span(|(id, type_ann), span: Span| {
            Pattern::Ident(BindingIdent { span, id, type_ann })
        })
        .padded();

    parser.boxed()
}
