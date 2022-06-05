use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use super::expr::expr_parser;
use super::pattern::pattern_parser;
use super::types::type_parser;

use crate::ast::*;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn decl_parser() -> impl Parser<char, Statement, Error = Simple<char>> {
    // We use `just` instead of `just_with_padding` here to ensure that
    // the span doesn't include leading whitespace.
    let pattern = pattern_parser();
    let var_decl_with_init = just("declare")
        .or_not()
        .then_ignore(just_with_padding("let"))
        .then(just_with_padding("rec").or_not())
        .then(pattern.clone())
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

                        Statement::VarDecl {
                            span,
                            pattern,
                            init: Some(fix),
                            declare: declare.is_some(),
                        }
                    }
                    None => Statement::VarDecl {
                        span,
                        pattern,
                        init: Some(init),
                        declare: declare.is_some(),
                    },
                }
            },
        );

    let var_decl = just("declare")
        .or_not()
        .then_ignore(just_with_padding("let"))
        .then(pattern_parser())
        .map_with_span(|(declare, pattern), span: Span| -> Statement {
            Statement::VarDecl {
                span,
                pattern,
                init: None,
                declare: declare.is_some(),
            }
        });

    choice((type_decl(), var_decl_with_init, var_decl))
}

fn type_decl() -> impl Parser<char, Statement, Error = Simple<char>> {
    let type_ann = type_parser();
    let ident = text::ident().map_with_span(|name, span: Span| Ident { name, span });

    let type_param = ident
        .then(just_with_padding("extends").ignore_then(type_ann.clone()).or_not())
        .then(just_with_padding("=").ignore_then(type_ann.clone()).or_not())
        .map_with_span(|((name, constraint), default), span| TypeParam {
            span,
            name,
            constraint: constraint.map(Box::from),
            default: default.map(Box::from),
        });

    let type_params = type_param
        .separated_by(just_with_padding(","))
        .allow_trailing()
        .delimited_by(just_with_padding("<"), just_with_padding(">"));

    let type_decl = just("declare")
        .or_not()
        .then_ignore(just_with_padding("type"))
        .then(ident)
        .then(type_params.or_not())
        .then(just_with_padding("=").ignore_then(type_ann))
        .map_with_span(
            |(((declare, id), type_params), type_ann), span| Statement::TypeDecl {
                span,
                declare: declare.is_some(),
                id,
                type_ann,
                type_params,
            },
        );

    type_decl
}
