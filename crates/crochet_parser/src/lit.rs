use chumsky::prelude::*;
use crochet_ast::*;
use unescape::unescape;

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

    string.boxed()
}
