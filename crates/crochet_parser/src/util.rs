use chumsky::prelude::*;

pub fn just_with_padding(input: &str) -> BoxedParser<'_, char, &str, Simple<char>> {
    let comment = just("//")
        .then(take_until(just('\n')))
        .padded()
        .labelled("comment");

    just(input).padded().padded_by(comment.repeated()).boxed()
}
