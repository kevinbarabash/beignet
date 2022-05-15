use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use super::literal::Literal;
use super::syntax::*;

pub type Span = std::ops::Range<usize>;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    let pattern = text::ident()
        .map_with_span(|name, span| Pattern::Ident { span, name })
        .padded();
    let binding_ident =
        text::ident().map_with_span(|name, span| BindingIdent::Ident { span, name });
    let ident = text::ident().map_with_span(|name, span| Expr::Ident { span, name });
    let r#true = just_with_padding("true").map_with_span(|value, span| Expr::Lit {
        span,
        literal: Literal::Bool(value.to_owned()),
    });
    let r#false = just_with_padding("false").map_with_span(|value, span| Expr::Lit {
        span,
        literal: Literal::Bool(value.to_owned()),
    });

    let expr = recursive(|expr| {
        let int = text::int::<char, Simple<char>>(10).map_with_span(|s: String, span| Expr::Lit {
            span,
            literal: Literal::Num(s),
        });

        let r#str = just("\"")
            .ignore_then(filter(|c| *c != '"').repeated())
            .then_ignore(just("\""))
            .collect::<String>()
            .map_with_span(|value, span| Expr::Lit {
                span,
                literal: Literal::Str(value),
            });

        let real = text::int(10)
            .chain(just('.'))
            .chain::<char, _, _>(text::digits(10))
            .collect::<String>()
            .map_with_span(|value, span| Expr::Lit {
                span,
                literal: Literal::Num(value),
            });

        // let r#true = r#true.map_with_span(add_span_info);
        // let r#false = r#false.map_with_span(add_span_info);
        // let ident = ident.map_with_span(add_span_info);

        // TODO: handle chaining of if-else
        let if_else = just_with_padding("if")
            .ignore_then(just_with_padding("("))
            .ignore_then(expr.clone())
            .then_ignore(just_with_padding(")"))
            .then_ignore(just_with_padding("{"))
            .then(expr.clone())
            .then_ignore(just_with_padding("}"))
            .then_ignore(just_with_padding("else"))
            .then_ignore(just_with_padding("{"))
            .then(expr.clone())
            .then_ignore(just_with_padding("}"))
            .map_with_span(|((cond, left), right), span: Span| Expr::If {
                span,
                cond: Box::from(cond),
                consequent: Box::from(left),
                alternate: Box::from(right),
            });

        let prop = text::ident()
            .then_ignore(just_with_padding(":"))
            .then(expr.clone())
            .map_with_span(|(name, value), span: Span| Property { span, name, value });

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|properties, span: Span| Expr::Obj { span, properties });

        // let jsx = just_with_padding("<")
        //     .ignore_then(text::ident().padded())
        //     .then_ignore(just_with_padding(">"))
        //     .then(
        //         filter(|c| *c != '<')
        //             .repeated()
        //             .collect::<String>()
        //             .map_with_span(|value, span| Expr::Lit {
        //                 span,
        //                 literal: Literal::Str(value),
        //             }),
        //     )
        //     .then_ignore(just("<"))
        //     .then_ignore(just("/"))
        //     .then(text::ident().padded())
        //     .then_ignore(just_with_padding(">"))
        //     .map_with_span(|((head, body), tail), span| {
        //         assert_eq!(head, tail);
        //         (
        //             Expr::JSXElement(JSXElement {
        //                 name: head,
        //                 children: vec![JSXElementChild::JSXExprContainer(JSXExprContainer {
        //                     span: span,
        //                     expr: body,
        //                 })],
        //             }),
        //             span,
        //         )
        //     });

        let atom = choice((
            if_else,
            real,
            int,
            r#str,
            r#true,
            r#false,
            ident,
            obj,
            // jsx,
            expr.clone()
                .delimited_by(just_with_padding("("), just_with_padding(")")),
        ));

        let app = atom
            .clone()
            .then(
                expr.clone()
                    .separated_by(just_with_padding(","))
                    .allow_trailing()
                    .delimited_by(just_with_padding("("), just_with_padding(")"))
                    .map_with_span(|args, span: Span| (args, span))
                    .repeated(),
            )
            .foldl(|f, (args, span)| {
                let start = f.span().start;
                let end = span.end;
                let span = start..end;

                Expr::App {
                    span,
                    lam: Box::new(f),
                    args,
                }
            });

        // Application is higher precedence than `await`
        let app = just_with_padding("await")
            .or_not()
            .then(app.clone())
            .map_with_span(|(option, app), span: Span| match option {
                Some(_) => Expr::Await {
                    span,
                    expr: Box::from(app),
                },
                None => app,
            });

        let product = app
            .clone()
            .then(
                choice((
                    just_with_padding("*").to(BinOp::Mul),
                    just_with_padding("/").to(BinOp::Div),
                ))
                .then(atom.clone())
                .repeated(),
            )
            .foldl(|left, (op, right)| {
                // atoms are already using source spans since they're WithSpan<Expr>
                let span = left.span().start..right.span().end;
                Expr::Op {
                    span,
                    op,
                    left: Box::from(left),
                    right: Box::from(right),
                }
            });

        let sum = product
            .clone()
            .then(
                choice((
                    just_with_padding("+").to(BinOp::Add),
                    just_with_padding("-").to(BinOp::Sub),
                ))
                .then(product.clone())
                .repeated(),
            )
            .foldl(|left, (op, right)| {
                // products are already using source spans since they're WithSpan<Expr>
                let span = left.span().start..right.span().end;
                Expr::Op {
                    span,
                    op,
                    left: Box::from(left),
                    right: Box::from(right),
                }
            });

        let param_list = binding_ident
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("("), just_with_padding(")"));

        let lam = just_with_padding("async")
            .or_not()
            .then(param_list)
            .then_ignore(just_with_padding("=>"))
            .then(choice((
                expr.clone()
                    .delimited_by(just_with_padding("{"), just_with_padding("}")),
                expr.clone(),
            )))
            .map_with_span(|((r#async, args), body), span: Span| Expr::Lam {
                span,
                args,
                body: Box::new(body),
                is_async: match r#async {
                    Some(_) => true,
                    None => false,
                },
            });

        // We use `just` instead of `just_with_padding` here to ensure that
        // the span doesn't include leading whitespace.
        let r#let = just("let")
            .ignore_then(just_with_padding("rec").or_not())
            .then(pattern)
            .then_ignore(just_with_padding("="))
            .then(expr.clone())
            .then_ignore(just_with_padding("in"))
            .then(expr.clone())
            .map_with_span(|(((rec, pattern), value), body), span: Span| {
                match rec {
                    // TODO: implement parsing of let-rec inside functions
                    Some(_) => todo!(),
                    None => Expr::Let {
                        span,
                        pattern,
                        value: Box::new(value),
                        body: Box::new(body),
                    },
                }
            });

        // NOTE: `let` is an expression because it currently models let-in.
        choice((lam, r#let, sum))
    });

    // We use `just` instead of `just_with_padding` here to ensure that
    // the span doesn't include leading whitespace.
    let decl = just("let")
        .ignore_then(just_with_padding("rec").or_not())
        .then(pattern)
        .then_ignore(just_with_padding("="))
        .then(expr.clone())
        .map_with_span(|((rec, pattern), value), span: Span| -> Statement {
            match rec {
                Some(_) => {
                    let ident = match &pattern {
                        Pattern::Ident { name, span } => BindingIdent::Ident {
                            span: span.clone(),
                            name: name.clone(),
                        },
                    };

                    Statement::Decl {
                        span,
                        pattern,
                        // `let fib = fix((fib) => (n) => ...)`
                        // TODO: Fix always wraps a lambda
                        value: Expr::Fix {
                            span: value.span(),
                            expr: Box::from(Expr::Lam {
                                span: value.span(),
                                args: vec![ident],
                                body: Box::from(value),
                                is_async: false,
                            }),
                        },
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
        expr.map_with_span(|expr, span: Span| Statement::Expr { expr, span }),
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
    fn int_literal() {
        insta::assert_debug_snapshot!(parse("10"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..2,
                    expr: Lit {
                        span: 0..2,
                        literal: Num(
                            "10",
                        ),
                    },
                },
            ],
        }
        "###)
    }

    #[test]
    fn real_literal() {
        insta::assert_debug_snapshot!(parse("1.23"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..4,
                    expr: Lit {
                        span: 0..4,
                        literal: Num(
                            "1.23",
                        ),
                    },
                },
            ],
        }
        "###)
    }

    #[test]
    fn fn_with_multiple_params() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..11,
                    expr: Lam {
                        span: 0..11,
                        args: [
                            Ident {
                                span: 1..2,
                                name: "a",
                            },
                            Ident {
                                span: 4..5,
                                name: "b",
                            },
                        ],
                        body: Ident {
                            span: 10..11,
                            name: "c",
                        },
                        is_async: false,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn fn_returning_num_literal() {
        insta::assert_debug_snapshot!(parse("() => 10"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..8,
                    expr: Lam {
                        span: 0..8,
                        args: [],
                        body: Lit {
                            span: 6..8,
                            literal: Num(
                                "10",
                            ),
                        },
                        is_async: false,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn fn_async() {
        insta::assert_debug_snapshot!(parse("async () => 10"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..14,
                    expr: Lam {
                        span: 0..14,
                        args: [],
                        body: Lit {
                            span: 12..14,
                            literal: Num(
                                "10",
                            ),
                        },
                        is_async: true,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn fn_async_with_await() {
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..34,
                    pattern: Ident {
                        span: 4..7,
                        name: "foo",
                    },
                    value: Lam {
                        span: 10..34,
                        args: [],
                        body: Await {
                            span: 24..32,
                            expr: Lit {
                                span: 30..32,
                                literal: Num(
                                    "10",
                                ),
                            },
                        },
                        is_async: true,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn fn_await_precedence() {
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..39,
                    pattern: Ident {
                        span: 4..7,
                        name: "foo",
                    },
                    value: Lam {
                        span: 10..39,
                        args: [],
                        body: Op {
                            span: 22..39,
                            op: Add,
                            left: Await {
                                span: 22..29,
                                expr: Ident {
                                    span: 28..29,
                                    name: "a",
                                },
                            },
                            right: Await {
                                span: 32..39,
                                expr: Ident {
                                    span: 38..39,
                                    name: "b",
                                },
                            },
                        },
                        is_async: true,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn fn_app_is_higher_precedence_than_await() {
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..33,
                    pattern: Ident {
                        span: 4..7,
                        name: "foo",
                    },
                    value: Lam {
                        span: 10..33,
                        args: [],
                        body: Await {
                            span: 22..33,
                            expr: App {
                                span: 28..33,
                                lam: Ident {
                                    span: 28..31,
                                    name: "bar",
                                },
                                args: [],
                            },
                        },
                        is_async: true,
                    },
                },
            ],
        }
        "###)
    }

    #[test]
    fn fn_returning_str_literal() {
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""), @r###"
        Program {
            body: [
                Expr {
                    span: 0..14,
                    expr: Lam {
                        span: 0..14,
                        args: [
                            Ident {
                                span: 1..2,
                                name: "a",
                            },
                        ],
                        body: Lit {
                            span: 7..14,
                            literal: Str(
                                "hello",
                            ),
                        },
                        is_async: false,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn app_with_no_args() {
        insta::assert_debug_snapshot!(parse("foo()"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..5,
                    expr: App {
                        span: 0..5,
                        lam: Ident {
                            span: 0..3,
                            name: "foo",
                        },
                        args: [],
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn app_with_multiple_args() {
        insta::assert_debug_snapshot!(parse("foo(a, b)"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..9,
                    expr: App {
                        span: 0..9,
                        lam: Ident {
                            span: 0..3,
                            name: "foo",
                        },
                        args: [
                            Ident {
                                span: 4..5,
                                name: "a",
                            },
                            Ident {
                                span: 7..8,
                                name: "b",
                            },
                        ],
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn app_with_multiple_lit_args() {
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\")"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..16,
                    expr: App {
                        span: 0..16,
                        lam: Ident {
                            span: 0..3,
                            name: "foo",
                        },
                        args: [
                            Lit {
                                span: 4..6,
                                literal: Num(
                                    "10",
                                ),
                            },
                            Lit {
                                span: 8..15,
                                literal: Str(
                                    "hello",
                                ),
                            },
                        ],
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn atom_number() {
        insta::assert_debug_snapshot!(parse("10"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..2,
                    expr: Lit {
                        span: 0..2,
                        literal: Num(
                            "10",
                        ),
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn atom_string() {
        insta::assert_debug_snapshot!(parse("\"hello\""), @r###"
        Program {
            body: [
                Expr {
                    span: 0..7,
                    expr: Lit {
                        span: 0..7,
                        literal: Str(
                            "hello",
                        ),
                    },
                },
            ],
        }
        "###);
    }

    // #[test]
    // #[should_panic]
    // fn top_level_let_in_panics() {
    //     parse("let x = 5 in x");
    // }

    #[test]
    fn add_sub_operations() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..9,
                    expr: Op {
                        span: 0..9,
                        op: Sub,
                        left: Op {
                            span: 0..5,
                            op: Add,
                            left: Lit {
                                span: 0..1,
                                literal: Num(
                                    "1",
                                ),
                            },
                            right: Lit {
                                span: 4..5,
                                literal: Num(
                                    "2",
                                ),
                            },
                        },
                        right: Lit {
                            span: 8..9,
                            literal: Num(
                                "3",
                            ),
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn mul_div_operations() {
        insta::assert_debug_snapshot!(parse("x * y / z"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..9,
                    expr: Op {
                        span: 0..9,
                        op: Div,
                        left: Op {
                            span: 0..5,
                            op: Mul,
                            left: Ident {
                                span: 0..1,
                                name: "x",
                            },
                            right: Ident {
                                span: 4..5,
                                name: "y",
                            },
                        },
                        right: Ident {
                            span: 8..9,
                            name: "z",
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn operator_precedence() {
        insta::assert_debug_snapshot!(parse("a + b * c"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..9,
                    expr: Op {
                        span: 0..9,
                        op: Add,
                        left: Ident {
                            span: 0..1,
                            name: "a",
                        },
                        right: Op {
                            span: 4..9,
                            op: Mul,
                            left: Ident {
                                span: 4..5,
                                name: "b",
                            },
                            right: Ident {
                                span: 8..9,
                                name: "c",
                            },
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn specifying_operator_precedence_with_parens() {
        insta::assert_debug_snapshot!(parse("(a + b) * c"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..11,
                    expr: Op {
                        span: 1..11,
                        op: Mul,
                        left: Op {
                            span: 1..6,
                            op: Add,
                            left: Ident {
                                span: 1..2,
                                name: "a",
                            },
                            right: Ident {
                                span: 5..6,
                                name: "b",
                            },
                        },
                        right: Ident {
                            span: 10..11,
                            name: "c",
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn single_decl() {
        insta::assert_debug_snapshot!(parse("let x = 5"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..9,
                    pattern: Ident {
                        span: 4..5,
                        name: "x",
                    },
                    value: Lit {
                        span: 8..9,
                        literal: Num(
                            "5",
                        ),
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn single_lambda_decl() {
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..23,
                    pattern: Ident {
                        span: 4..5,
                        name: "x",
                    },
                    value: Lam {
                        span: 8..23,
                        args: [
                            Ident {
                                span: 9..10,
                                name: "a",
                            },
                            Ident {
                                span: 12..13,
                                name: "b",
                            },
                        ],
                        body: Op {
                            span: 18..23,
                            op: Add,
                            left: Ident {
                                span: 18..19,
                                name: "a",
                            },
                            right: Ident {
                                span: 22..23,
                                name: "b",
                            },
                        },
                        is_async: false,
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn multiple_decls() {
        insta::assert_debug_snapshot!(parse("let x = 5\nlet y = \"hello\""), @r###"
        Program {
            body: [
                Decl {
                    span: 0..9,
                    pattern: Ident {
                        span: 4..5,
                        name: "x",
                    },
                    value: Lit {
                        span: 8..9,
                        literal: Num(
                            "5",
                        ),
                    },
                },
                Decl {
                    span: 10..25,
                    pattern: Ident {
                        span: 14..15,
                        name: "y",
                    },
                    value: Lit {
                        span: 18..25,
                        literal: Str(
                            "hello",
                        ),
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn top_level_expr() {
        insta::assert_debug_snapshot!(parse("a + b"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..5,
                    expr: Op {
                        span: 0..5,
                        op: Add,
                        left: Ident {
                            span: 0..1,
                            name: "a",
                        },
                        right: Ident {
                            span: 4..5,
                            name: "b",
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn multiple_top_level_expressions() {
        insta::assert_debug_snapshot!(parse("123\n\"hello\""), @r###"
        Program {
            body: [
                Expr {
                    span: 0..3,
                    expr: Lit {
                        span: 0..3,
                        literal: Num(
                            "123",
                        ),
                    },
                },
                Expr {
                    span: 4..11,
                    expr: Lit {
                        span: 4..11,
                        literal: Str(
                            "hello",
                        ),
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn simple_let_inside_decl() {
        insta::assert_debug_snapshot!(parse("let foo = let x = 5 in x"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..24,
                    pattern: Ident {
                        span: 4..7,
                        name: "foo",
                    },
                    value: Let {
                        span: 10..24,
                        pattern: Ident {
                            span: 14..15,
                            name: "x",
                        },
                        value: Lit {
                            span: 18..19,
                            literal: Num(
                                "5",
                            ),
                        },
                        body: Ident {
                            span: 23..24,
                            name: "x",
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn multiple_consequtive_apps() {
        insta::assert_debug_snapshot!(parse("f(x)(g(x))"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..10,
                    expr: App {
                        span: 0..10,
                        lam: App {
                            span: 0..4,
                            lam: Ident {
                                span: 0..1,
                                name: "f",
                            },
                            args: [
                                Ident {
                                    span: 2..3,
                                    name: "x",
                                },
                            ],
                        },
                        args: [
                            App {
                                span: 5..9,
                                lam: Ident {
                                    span: 5..6,
                                    name: "g",
                                },
                                args: [
                                    Ident {
                                        span: 7..8,
                                        name: "x",
                                    },
                                ],
                            },
                        ],
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if (true) { 5 } else { 10 }"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..27,
                    expr: If {
                        span: 0..27,
                        cond: Lit {
                            span: 4..8,
                            literal: Bool(
                                "true",
                            ),
                        },
                        consequent: Lit {
                            span: 12..13,
                            literal: Num(
                                "5",
                            ),
                        },
                        alternate: Lit {
                            span: 23..25,
                            literal: Num(
                                "10",
                            ),
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn recursive_decl() {
        insta::assert_debug_snapshot!(parse("let rec f = () => f()"), @r###"
        Program {
            body: [
                Decl {
                    span: 0..21,
                    pattern: Ident {
                        span: 8..9,
                        name: "f",
                    },
                    value: Fix {
                        span: 12..21,
                        expr: Lam {
                            span: 12..21,
                            args: [
                                Ident {
                                    span: 8..9,
                                    name: "f",
                                },
                            ],
                            body: Lam {
                                span: 12..21,
                                args: [],
                                body: App {
                                    span: 18..21,
                                    lam: Ident {
                                        span: 18..19,
                                        name: "f",
                                    },
                                    args: [],
                                },
                                is_async: false,
                            },
                            is_async: false,
                        },
                    },
                },
            ],
        }
        "###);
    }

    #[test]
    fn simple_obj() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..13,
                    expr: Obj {
                        span: 0..13,
                        properties: [
                            Property {
                                span: 1..5,
                                name: "x",
                                value: Lit {
                                    span: 4..5,
                                    literal: Num(
                                        "5",
                                    ),
                                },
                            },
                            Property {
                                span: 7..12,
                                name: "y",
                                value: Lit {
                                    span: 10..12,
                                    literal: Num(
                                        "10",
                                    ),
                                },
                            },
                        ],
                    },
                },
            ],
        }
        "###);
    }

    // #[test]
    // fn jsx_with_string_child() {
    //     insta::assert_debug_snapshot!(parse("<Foo>Hello</Foo>"), @r###"
    //     Program {
    //         body: [
    //             (
    //                 Expr(
    //                     (
    //                         Jsx {
    //                             name: "Foo",
    //                             body: [
    //                                 (
    //                                     Lit {
    //                                         literal: Str(
    //                                             "Hello",
    //                                         ),
    //                                     },
    //                                     5..10,
    //                                 ),
    //                             ],
    //                         },
    //                         0..16,
    //                     ),
    //                 ),
    //                 0..16,
    //             ),
    //         ],
    //     }
    //     "###);
    // }
}
