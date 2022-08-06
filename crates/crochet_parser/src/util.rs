use chumsky::prelude::*;

pub fn comment_parser() -> BoxedParser<'static, char, &'static str, Simple<char>> {
    let comment = just("//")
        .then_ignore(take_until(just('\n')))
        .padded()
        .labelled("comment");

    comment.boxed()
}

pub fn just_with_padding(input: &str) -> BoxedParser<'_, char, &str, Simple<char>> {
    just(input)
        .padded()
        .padded_by(comment_parser().repeated())
        .boxed()
}
