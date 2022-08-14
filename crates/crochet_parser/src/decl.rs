use chumsky::prelude::*;
use crochet_ast::*;

use super::expr::expr_parser;
use super::pattern::pattern_parser;
use super::type_ann::type_ann_parser;
use super::type_params::type_params;
use super::util::just_with_padding;

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
                        let id = match &pattern {
                            Pattern::Ident(bi) => bi.id.to_owned(),
                            _ => panic!("rec can only be used with identifier patterns")
                        };
                        let fix = Expr::Fix(Fix {
                            span: init.span(),
                            expr: Box::from(Expr::Lambda(Lambda {
                                span: init.span(),
                                params: vec![
                                    EFnParam {
                                        pat: EFnParamPat::Ident(EFnParamBindingIdent {
                                            span: 0..0,
                                            id,
                                        }),
                                        // TODO: grab this from the pattern
                                        type_ann: None,
                                    }
                                ],
                                body: Box::from(init),
                                is_async: false,
                                return_type: None,
                                type_params: None, // TODO: support type params on VarDecls
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
        ).labelled("var_decl_with_init");

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
    let type_ann = type_ann_parser();
    let ident = text::ident().map_with_span(|name, span: Span| Ident { name, span });

    let type_decl = just("declare")
        .or_not()
        .then_ignore(just_with_padding("type"))
        .then(ident)
        .then(type_params(type_ann.clone().boxed()).or_not())
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
