use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use super::expr::expr_parser;
use super::pattern::pattern_parser;

use crate::ast::*;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn decl_parser() -> impl Parser<char, Statement, Error = Simple<char>> {
    // We use `just` instead of `just_with_padding` here to ensure that
    // the span doesn't include leading whitespace.
    let r#let = just("declare")
        .or_not()
        .then_ignore(just_with_padding("let"))
        .then(just_with_padding("rec").or_not())
        .then(pattern_parser())
        .then_ignore(just_with_padding("="))
        .then(expr_parser())
        .map_with_span(
            |(((declare, rec), pattern), init), span: Span| -> Statement {
                match rec {
                    Some(_) => {
                        // `let fib = fix((fib) => (n) => ...)`
                        // TODO: Fix always wraps a lambda
                        let fix = Expr::Fix(Fix {
                            span: init.span(),
                            expr: Box::from(Expr::Lambda(Lambda {
                                span: init.span(),
                                params: vec![pattern.clone()],
                                body: Box::from(init),
                                is_async: false,
                                return_type: None,
                            })),
                        });

                        Statement::Decl {
                            span,
                            pattern,
                            init: Some(fix),
                            declare: declare.is_some(),
                        }
                    }
                    None => Statement::Decl {
                        span,
                        pattern,
                        init: Some(init),
                        declare: declare.is_some(),
                    },
                }
            },
        );

    let declare = just("declare")
        .or_not()
        .then_ignore(just_with_padding("let"))
        .then(pattern_parser())
        .map_with_span(|(declare, pattern), span: Span| -> Statement {
            Statement::Decl {
                span,
                pattern,
                init: None,
                declare: declare.is_some(),
            }
        });

    choice((r#let, declare))
}
