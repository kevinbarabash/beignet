pub mod expr;
pub mod jsx;
pub mod pattern;
pub mod types;

use expr::expr_parser;
use pattern::pattern_parser;

use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use crate::ast::*;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    // We use `just` instead of `just_with_padding` here to ensure that
    // the span doesn't include leading whitespace.
    // TODO: add `declare` syntax
    let decl = just("let")
        .ignore_then(just_with_padding("rec").or_not())
        .then(pattern_parser())
        .then_ignore(just_with_padding("="))
        .then(expr_parser())
        .map_with_span(|((rec, pattern), value), span: Span| -> Statement {
            match rec {
                Some(_) => {
                    Statement::Decl {
                        span,
                        pattern: pattern.clone(),
                        // `let fib = fix((fib) => (n) => ...)`
                        // TODO: Fix always wraps a lambda
                        value: Expr::Fix(Fix {
                            span: value.span(),
                            expr: Box::from(Expr::Lambda(Lambda {
                                span: value.span(),
                                args: vec![pattern.clone()],
                                body: Box::from(value),
                                is_async: false,
                            })),
                        }),
                    }
                }
                None => Statement::Decl {
                    span,
                    pattern,
                    value,
                },
            }
        });

    let program = choice((
        decl,
        expr_parser().map_with_span(|expr, span: Span| Statement::Expr { expr, span }),
    ))
    .padded()
    .repeated()
    .map(|body| Program { body });

    program.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Program {
        parser().parse(input).unwrap()
    }

    #[test]
    fn literals() {
        insta::assert_debug_snapshot!(parse("10"));
        insta::assert_debug_snapshot!(parse("1.23"));
        insta::assert_debug_snapshot!(parse("\"hello\""));
    }

    #[test]
    fn operations() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3"));
        insta::assert_debug_snapshot!(parse("x * y / z"));
        insta::assert_debug_snapshot!(parse("(a + b) * c"));
        insta::assert_debug_snapshot!(parse("a == b"));
        insta::assert_debug_snapshot!(parse("a != b"));
        insta::assert_debug_snapshot!(parse("a > b"));
        insta::assert_debug_snapshot!(parse("a >= b"));
        insta::assert_debug_snapshot!(parse("a < b"));
        insta::assert_debug_snapshot!(parse("a <= b"));
        insta::assert_debug_snapshot!(parse("let cond = a != b"));
    }

    #[test]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"));
        insta::assert_debug_snapshot!(parse("() => 10"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""));
    }

    #[test]
    fn async_await() {
        insta::assert_debug_snapshot!(parse("async () => 10"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"));
    }

    #[test]
    fn function_application() {
        insta::assert_debug_snapshot!(parse("foo()"));
        insta::assert_debug_snapshot!(parse("foo(a, b)"));
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\")"));
        insta::assert_debug_snapshot!(parse("f(x)(g(x))"));
    }

    #[test]
    fn declarations() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b"));
        insta::assert_debug_snapshot!(parse("let foo = let x = 5 in x"));
        insta::assert_debug_snapshot!(parse("let rec f = () => f()")); // recursive
    }

    #[test]
    fn top_level_expressions() {
        insta::assert_debug_snapshot!(parse("a + b"));
        insta::assert_debug_snapshot!(parse("123\n\"hello\""));
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if (true) { 5 } else { 10 }"));
    }

    #[test]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"));
    }

    #[test]
    fn jsx() {
        insta::assert_debug_snapshot!(parse("<Foo>Hello</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{bar}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>Hello {world}!</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{<Bar>{baz}</Bar>}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo bar={baz} />"));
        insta::assert_debug_snapshot!(parse("<Foo msg=\"hello\" bar={baz}></Foo>"));
    }

    #[test]
    fn type_annotations() {
        insta::assert_debug_snapshot!(parse("let x: number = 5"));
        insta::assert_debug_snapshot!(parse("let msg: string = \"hello\""));
        insta::assert_debug_snapshot!(parse("let add = (a: number, b: number) => a + b"));
        insta::assert_debug_snapshot!(parse("let p: Point = {x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let FOO: \"foo\" = \"foo\""));
    }
}
