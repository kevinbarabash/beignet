use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

pub fn just_with_padding(input: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(input).padded()
}
