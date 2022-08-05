use chumsky::prelude::*;
use crochet_ast::*;
use unescape::unescape;

use crate::util::just_with_padding;

pub fn boolean_parser() -> BoxedParser<'static, char, Lit, Simple<char>> {
    // NOTE: It's important to have padding around all tokens in order
    // for comments to work because comments themselvse have no padding.
    let r#true = just_with_padding("true").map_with_span(|_, span| Lit::bool(true, span));
    let r#false = just_with_padding("false").map_with_span(|_, span| Lit::bool(false, span));

    let comment = just("//")
        .then(take_until(just('\n')))
        .padded()
        .labelled("comment");

    choice((r#true, r#false))
        .padded()
        .padded_by(comment.repeated())
        .boxed()
}

pub fn number_parser() -> BoxedParser<'static, char, Lit, Simple<char>> {
    let int = text::int::<char, Simple<char>>(10).map_with_span(Lit::num);

    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map_with_span(Lit::num);

    let comment = just("//")
        .then(take_until(just('\n')))
        .padded()
        .labelled("comment");

    // NOTE: It's important to have padding around all tokens in order
    // for comments to work because comments themselvse have no padding.
    // TODO: move the `.then_ignore(comment.repeated())` to come after
    // `atom` in expr.js otherwise we'll have to add it to the other literals.
    choice((real, int))
        .padded()
        .padded_by(comment.repeated())
        .boxed()
}

// Based on https://github.com/zesterer/chumsky/blob/master/examples/json.rs.
pub fn string_parser() -> BoxedParser<'static, char, Lit, Simple<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .or(just('u').ignore_then(
                filter(|c: &char| c.is_ascii_hexdigit())
                    .repeated()
                    .exactly(4)
                    .collect::<String>()
                    .validate(|digits, span, emit| {
                        char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(
                            || {
                                emit(Simple::custom(span, "invalid unicode character"));
                                '\u{FFFD}' // unicode replacement character
                            },
                        )
                    }),
            )),
    );

    let string = just("\"")
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just("\""))
        .collect::<String>()
        .map_with_span(|raw, span| {
            // unescape needs to know whether the string is contained in single quotes
            // or double quotes so that it can unescape quote characters correctly.
            let cooked = unescape(&raw).unwrap();
            Lit::str(cooked, span)
        });

    let comment = just("//")
        .then(take_until(just('\n')))
        .padded()
        .labelled("comment");

    // NOTE: It's important to have padding around all tokens in order
    // for comments to work because comments themselvse have no padding.
    string.padded().padded_by(comment.repeated()).boxed()
}
