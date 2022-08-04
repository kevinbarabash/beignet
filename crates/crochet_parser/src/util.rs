use chumsky::prelude::*;

pub fn just_with_padding(input: &str) -> BoxedParser<'_, char, &str, Simple<char>> {
    let comment = just("//")
        .then(take_until(just('\n').rewind()))
        .padded()
        .labelled("comment");

    just(input).padded().then_ignore(comment.repeated()).boxed()
}
