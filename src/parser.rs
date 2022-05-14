use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use super::literal::Literal;
use super::syntax::{BinOp, BindingIdent, Expr, Pattern, Program, Property, Statement};

pub type Span = std::ops::Range<usize>;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    let pattern = text::ident()
        .map_with_span(|name, span| (Pattern::Ident { name }, span))
        .padded();
    let binding_ident =
        text::ident().map_with_span(|name, span| (BindingIdent::Ident { name }, span));
    let ident = text::ident().map_with_span(|name, span| (Expr::Ident { name }, span));
    let r#true = just_with_padding("true").map_with_span(|value, span| {
        (
            Expr::Lit {
                literal: Literal::Bool(value.to_owned()),
            },
            span,
        )
    });
    let r#false = just_with_padding("false").map_with_span(|value, span| {
        (
            Expr::Lit {
                literal: Literal::Bool(value.to_owned()),
            },
            span,
        )
    });

    let expr = recursive(|expr| {
        let int = text::int::<char, Simple<char>>(10).map_with_span(|s: String, span| {
            (
                Expr::Lit {
                    literal: Literal::Num(s),
                },
                span,
            )
        });

        let r#str = just("\"")
            .ignore_then(filter(|c| *c != '"').repeated())
            .then_ignore(just("\""))
            .collect::<String>()
            .map_with_span(|value, span| {
                (
                    Expr::Lit {
                        literal: Literal::Str(value),
                    },
                    span,
                )
            });

        let real = text::int(10)
            .chain(just('.'))
            .chain::<char, _, _>(text::digits(10))
            .collect::<String>()
            .map_with_span(|value, span| {
                (
                    Expr::Lit {
                        literal: Literal::Num(value),
                    },
                    span,
                )
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
            .map_with_span(|((cond, left), right), span: Span| {
                (
                    Expr::If {
                        cond: Box::from(cond),
                        consequent: Box::from(left),
                        alternate: Box::from(right),
                    },
                    span,
                )
            });

        let prop = text::ident()
            .then_ignore(just_with_padding(":"))
            .then(expr.clone())
            .map_with_span(|(name, value), span: Span| (Property { name, value }, span));

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|properties, span: Span| (Expr::Obj { properties }, span));

        let atom = choice((
            if_else,
            real,
            int,
            r#str,
            r#true,
            r#false,
            ident,
            obj,
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
                let start = f.1.start;
                let end = span.end;
                let span = start..end;

                (
                    Expr::App {
                        lam: Box::new(f),
                        args,
                    },
                    span,
                )
            });

        // Application is higher precedence than `await`
        let app = just_with_padding("await")
            .or_not()
            .then(app.clone())
            .map_with_span(|(option, app), span: Span| match option {
                Some(_) => (
                    Expr::Await {
                        expr: Box::from(app),
                    },
                    span,
                ),
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
                let span = left.1.start..right.1.end;
                {
                    (
                        Expr::Op {
                            op,
                            left: Box::from(left),
                            right: Box::from(right),
                        },
                        span,
                    )
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
                let span = left.1.start..right.1.end;
                {
                    (
                        Expr::Op {
                            op,
                            left: Box::from(left),
                            right: Box::from(right),
                        },
                        span,
                    )
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
            .map_with_span(|((r#async, args), body), span: Span| {
                (
                    Expr::Lam {
                        args,
                        body: Box::new(body),
                        is_async: match r#async {
                            Some(_) => true,
                            None => false,
                        },
                    },
                    span,
                )
            });

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
                    None => (
                        Expr::Let {
                            pattern,
                            value: Box::new(value),
                            body: Box::new(body),
                        },
                        span,
                    ),
                }
            });

        // NOTE: `let` is an expression because it currently models let-in.
        choice((lam, r#let, sum))
    });

    let decl = just("let")
        .ignore_then(just_with_padding("rec").or_not())
        .then(pattern)
        .then_ignore(just_with_padding("="))
        .then(expr.clone())
        .map_with_span(
            |((rec, pattern), value), span: Span| -> (Statement, std::ops::Range<usize>) {
                match rec {
                    Some(_) => {
                        let ident = match &pattern.0 {
                            Pattern::Ident { name } => (
                                BindingIdent::Ident { name: name.clone() },
                                pattern.1.clone(),
                            ),
                        };
                        let value_span = value.1.clone();
                        (
                            Statement::Decl {
                                pattern,
                                // `let fib = fix((fib) => (n) => ...)`
                                // TODO: Fix always wraps a lambda
                                value: (
                                    Expr::Fix {
                                        expr: Box::from((
                                            Expr::Lam {
                                                args: vec![ident],
                                                body: Box::from(value),
                                                is_async: false,
                                            },
                                            value_span.clone(),
                                        )),
                                    },
                                    value_span.clone(),
                                ),
                            },
                            span,
                        )
                    }
                    None => (Statement::Decl { pattern, value }, span),
                }
            },
        );

    let program = choice((
        decl.padded(),
        expr.map_with_span(|e, span: Span| (Statement::Expr(e), span))
            .padded(),
    ))
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
                (
                    Expr(
                        (
                            Lit {
                                literal: Num(
                                    "10",
                                ),
                            },
                            0..2,
                        ),
                    ),
                    0..2,
                ),
            ],
        }
        "###)
    }

    #[test]
    fn real_literal() {
        insta::assert_debug_snapshot!(parse("1.23"), @r###"
        Program {
            body: [
                (
                    Expr(
                        (
                            Lit {
                                literal: Num(
                                    "1.23",
                                ),
                            },
                            0..4,
                        ),
                    ),
                    0..4,
                ),
            ],
        }
        "###)
    }

    #[test]
    fn fn_async() {
        insta::assert_debug_snapshot!(parse("async () => 10"), @r###"
        Program {
            body: [
                (
                    Expr(
                        (
                            Lam {
                                args: [],
                                body: (
                                    Lit {
                                        literal: Num(
                                            "10",
                                        ),
                                    },
                                    12..14,
                                ),
                                is_async: true,
                            },
                            0..14,
                        ),
                    ),
                    0..14,
                ),
            ],
        }
        "###);
    }

    #[test]
    fn fn_async_with_await() {
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"), @r###"
        Program {
            body: [
                (
                    Decl {
                        pattern: (
                            Ident {
                                name: "foo",
                            },
                            4..7,
                        ),
                        value: (
                            Lam {
                                args: [],
                                body: (
                                    Await {
                                        expr: (
                                            Lit {
                                                literal: Num(
                                                    "10",
                                                ),
                                            },
                                            30..32,
                                        ),
                                    },
                                    24..32,
                                ),
                                is_async: true,
                            },
                            10..34,
                        ),
                    },
                    0..34,
                ),
            ],
        }
        "###);
    }

    #[test]
    fn fn_await_precedence() {
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"), @r###"
        Program {
            body: [
                (
                    Decl {
                        pattern: (
                            Ident {
                                name: "foo",
                            },
                            4..7,
                        ),
                        value: (
                            Lam {
                                args: [],
                                body: (
                                    Op {
                                        op: Add,
                                        left: (
                                            Await {
                                                expr: (
                                                    Ident {
                                                        name: "a",
                                                    },
                                                    28..29,
                                                ),
                                            },
                                            22..29,
                                        ),
                                        right: (
                                            Await {
                                                expr: (
                                                    Ident {
                                                        name: "b",
                                                    },
                                                    38..39,
                                                ),
                                            },
                                            32..39,
                                        ),
                                    },
                                    22..39,
                                ),
                                is_async: true,
                            },
                            10..39,
                        ),
                    },
                    0..39,
                ),
            ],
        }
        "###);
    }

    #[test]
    fn fn_app_is_higher_precedence_than_await() {
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"), @r###"
        Program {
            body: [
                (
                    Decl {
                        pattern: (
                            Ident {
                                name: "foo",
                            },
                            4..7,
                        ),
                        value: (
                            Lam {
                                args: [],
                                body: (
                                    Await {
                                        expr: (
                                            App {
                                                lam: (
                                                    Ident {
                                                        name: "bar",
                                                    },
                                                    28..31,
                                                ),
                                                args: [],
                                            },
                                            28..33,
                                        ),
                                    },
                                    22..33,
                                ),
                                is_async: true,
                            },
                            10..33,
                        ),
                    },
                    0..33,
                ),
            ],
        }
        "###)
    }

    #[test]
    fn fn_with_multiple_params() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"), @r###"
        Program {
            body: [
                (
                    Expr(
                        (
                            Lam {
                                args: [
                                    (
                                        Ident {
                                            name: "a",
                                        },
                                        1..2,
                                    ),
                                    (
                                        Ident {
                                            name: "b",
                                        },
                                        4..5,
                                    ),
                                ],
                                body: (
                                    Ident {
                                        name: "c",
                                    },
                                    10..11,
                                ),
                                is_async: false,
                            },
                            0..11,
                        ),
                    ),
                    0..11,
                ),
            ],
        }
        "###);
    }

    #[test]
    fn fn_returning_num_literal() {
        insta::assert_debug_snapshot!(parse("() => 10"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Lam {
                                    args: [],
                                    body: (
                                        Lit {
                                            literal: Num(
                                                "10",
                                            ),
                                        },
                                        6..8,
                                    ),
                                    is_async: false,
                                },
                                0..8,
                            ),
                        ),
                        0..8,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn fn_returning_str_literal() {
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Lam {
                                    args: [
                                        (
                                            Ident {
                                                name: "a",
                                            },
                                            1..2,
                                        ),
                                    ],
                                    body: (
                                        Lit {
                                            literal: Str(
                                                "hello",
                                            ),
                                        },
                                        7..14,
                                    ),
                                    is_async: false,
                                },
                                0..14,
                            ),
                        ),
                        0..14,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn app_with_no_args() {
        insta::assert_debug_snapshot!(parse("foo()"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                App {
                                    lam: (
                                        Ident {
                                            name: "foo",
                                        },
                                        0..3,
                                    ),
                                    args: [],
                                },
                                0..5,
                            ),
                        ),
                        0..5,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn app_with_multiple_args() {
        insta::assert_debug_snapshot!(parse("foo(a, b)"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                App {
                                    lam: (
                                        Ident {
                                            name: "foo",
                                        },
                                        0..3,
                                    ),
                                    args: [
                                        (
                                            Ident {
                                                name: "a",
                                            },
                                            4..5,
                                        ),
                                        (
                                            Ident {
                                                name: "b",
                                            },
                                            7..8,
                                        ),
                                    ],
                                },
                                0..9,
                            ),
                        ),
                        0..9,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn app_with_multiple_lit_args() {
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\")"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                App {
                                    lam: (
                                        Ident {
                                            name: "foo",
                                        },
                                        0..3,
                                    ),
                                    args: [
                                        (
                                            Lit {
                                                literal: Num(
                                                    "10",
                                                ),
                                            },
                                            4..6,
                                        ),
                                        (
                                            Lit {
                                                literal: Str(
                                                    "hello",
                                                ),
                                            },
                                            8..15,
                                        ),
                                    ],
                                },
                                0..16,
                            ),
                        ),
                        0..16,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn atom_number() {
        insta::assert_debug_snapshot!(parse("10"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Lit {
                                    literal: Num(
                                        "10",
                                    ),
                                },
                                0..2,
                            ),
                        ),
                        0..2,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn atom_string() {
        insta::assert_debug_snapshot!(parse("\"hello\""), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Lit {
                                    literal: Str(
                                        "hello",
                                    ),
                                },
                                0..7,
                            ),
                        ),
                        0..7,
                    ),
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
                    (
                        Expr(
                            (
                                Op {
                                    op: Sub,
                                    left: (
                                        Op {
                                            op: Add,
                                            left: (
                                                Lit {
                                                    literal: Num(
                                                        "1",
                                                    ),
                                                },
                                                0..1,
                                            ),
                                            right: (
                                                Lit {
                                                    literal: Num(
                                                        "2",
                                                    ),
                                                },
                                                4..5,
                                            ),
                                        },
                                        0..5,
                                    ),
                                    right: (
                                        Lit {
                                            literal: Num(
                                                "3",
                                            ),
                                        },
                                        8..9,
                                    ),
                                },
                                0..9,
                            ),
                        ),
                        0..9,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn mul_div_operations() {
        insta::assert_debug_snapshot!(parse("x * y / z"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Op {
                                    op: Div,
                                    left: (
                                        Op {
                                            op: Mul,
                                            left: (
                                                Ident {
                                                    name: "x",
                                                },
                                                0..1,
                                            ),
                                            right: (
                                                Ident {
                                                    name: "y",
                                                },
                                                4..5,
                                            ),
                                        },
                                        0..5,
                                    ),
                                    right: (
                                        Ident {
                                            name: "z",
                                        },
                                        8..9,
                                    ),
                                },
                                0..9,
                            ),
                        ),
                        0..9,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn operator_precedence() {
        insta::assert_debug_snapshot!(parse("a + b * c"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Op {
                                    op: Add,
                                    left: (
                                        Ident {
                                            name: "a",
                                        },
                                        0..1,
                                    ),
                                    right: (
                                        Op {
                                            op: Mul,
                                            left: (
                                                Ident {
                                                    name: "b",
                                                },
                                                4..5,
                                            ),
                                            right: (
                                                Ident {
                                                    name: "c",
                                                },
                                                8..9,
                                            ),
                                        },
                                        4..9,
                                    ),
                                },
                                0..9,
                            ),
                        ),
                        0..9,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn specifying_operator_precedence_with_parens() {
        insta::assert_debug_snapshot!(parse("(a + b) * c"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Op {
                                    op: Mul,
                                    left: (
                                        Op {
                                            op: Add,
                                            left: (
                                                Ident {
                                                    name: "a",
                                                },
                                                1..2,
                                            ),
                                            right: (
                                                Ident {
                                                    name: "b",
                                                },
                                                5..6,
                                            ),
                                        },
                                        1..6,
                                    ),
                                    right: (
                                        Ident {
                                            name: "c",
                                        },
                                        10..11,
                                    ),
                                },
                                1..11,
                            ),
                        ),
                        0..11,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn single_decl() {
        insta::assert_debug_snapshot!(parse("let x = 5"), @r###"
            Program {
                body: [
                    (
                        Decl {
                            pattern: (
                                Ident {
                                    name: "x",
                                },
                                4..5,
                            ),
                            value: (
                                Lit {
                                    literal: Num(
                                        "5",
                                    ),
                                },
                                8..9,
                            ),
                        },
                        0..9,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn single_lambda_decl() {
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b"), @r###"
            Program {
                body: [
                    (
                        Decl {
                            pattern: (
                                Ident {
                                    name: "x",
                                },
                                4..5,
                            ),
                            value: (
                                Lam {
                                    args: [
                                        (
                                            Ident {
                                                name: "a",
                                            },
                                            9..10,
                                        ),
                                        (
                                            Ident {
                                                name: "b",
                                            },
                                            12..13,
                                        ),
                                    ],
                                    body: (
                                        Op {
                                            op: Add,
                                            left: (
                                                Ident {
                                                    name: "a",
                                                },
                                                18..19,
                                            ),
                                            right: (
                                                Ident {
                                                    name: "b",
                                                },
                                                22..23,
                                            ),
                                        },
                                        18..23,
                                    ),
                                    is_async: false,
                                },
                                8..23,
                            ),
                        },
                        0..23,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn multiple_decls() {
        insta::assert_debug_snapshot!(parse("let x = 5\nlet y = \"hello\""), @r###"
            Program {
                body: [
                    (
                        Decl {
                            pattern: (
                                Ident {
                                    name: "x",
                                },
                                4..5,
                            ),
                            value: (
                                Lit {
                                    literal: Num(
                                        "5",
                                    ),
                                },
                                8..9,
                            ),
                        },
                        0..9,
                    ),
                    (
                        Decl {
                            pattern: (
                                Ident {
                                    name: "y",
                                },
                                14..15,
                            ),
                            value: (
                                Lit {
                                    literal: Str(
                                        "hello",
                                    ),
                                },
                                18..25,
                            ),
                        },
                        10..25,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn top_level_expr() {
        insta::assert_debug_snapshot!(parse("a + b"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Op {
                                    op: Add,
                                    left: (
                                        Ident {
                                            name: "a",
                                        },
                                        0..1,
                                    ),
                                    right: (
                                        Ident {
                                            name: "b",
                                        },
                                        4..5,
                                    ),
                                },
                                0..5,
                            ),
                        ),
                        0..5,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn multiple_top_level_expressions() {
        insta::assert_debug_snapshot!(parse("123\n\"hello\""), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Lit {
                                    literal: Num(
                                        "123",
                                    ),
                                },
                                0..3,
                            ),
                        ),
                        0..3,
                    ),
                    (
                        Expr(
                            (
                                Lit {
                                    literal: Str(
                                        "hello",
                                    ),
                                },
                                4..11,
                            ),
                        ),
                        4..11,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn simple_let_inside_decl() {
        insta::assert_debug_snapshot!(parse("let foo = let x = 5 in x"), @r###"
            Program {
                body: [
                    (
                        Decl {
                            pattern: (
                                Ident {
                                    name: "foo",
                                },
                                4..7,
                            ),
                            value: (
                                Let {
                                    pattern: (
                                        Ident {
                                            name: "x",
                                        },
                                        14..15,
                                    ),
                                    value: (
                                        Lit {
                                            literal: Num(
                                                "5",
                                            ),
                                        },
                                        18..19,
                                    ),
                                    body: (
                                        Ident {
                                            name: "x",
                                        },
                                        23..24,
                                    ),
                                },
                                10..24,
                            ),
                        },
                        0..24,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn multiple_consequtive_apps() {
        insta::assert_debug_snapshot!(parse("f(x)(g(x))"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                App {
                                    lam: (
                                        App {
                                            lam: (
                                                Ident {
                                                    name: "f",
                                                },
                                                0..1,
                                            ),
                                            args: [
                                                (
                                                    Ident {
                                                        name: "x",
                                                    },
                                                    2..3,
                                                ),
                                            ],
                                        },
                                        0..4,
                                    ),
                                    args: [
                                        (
                                            App {
                                                lam: (
                                                    Ident {
                                                        name: "g",
                                                    },
                                                    5..6,
                                                ),
                                                args: [
                                                    (
                                                        Ident {
                                                            name: "x",
                                                        },
                                                        7..8,
                                                    ),
                                                ],
                                            },
                                            5..9,
                                        ),
                                    ],
                                },
                                0..10,
                            ),
                        ),
                        0..10,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if (true) { 5 } else { 10 }"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                If {
                                    cond: (
                                        Lit {
                                            literal: Bool(
                                                "true",
                                            ),
                                        },
                                        4..8,
                                    ),
                                    consequent: (
                                        Lit {
                                            literal: Num(
                                                "5",
                                            ),
                                        },
                                        12..13,
                                    ),
                                    alternate: (
                                        Lit {
                                            literal: Num(
                                                "10",
                                            ),
                                        },
                                        23..25,
                                    ),
                                },
                                0..27,
                            ),
                        ),
                        0..27,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn recursive_decl() {
        insta::assert_debug_snapshot!(parse("let rec f = () => f()"), @r###"
            Program {
                body: [
                    (
                        Decl {
                            pattern: (
                                Ident {
                                    name: "f",
                                },
                                8..9,
                            ),
                            value: (
                                Fix {
                                    expr: (
                                        Lam {
                                            args: [
                                                (
                                                    Ident {
                                                        name: "f",
                                                    },
                                                    8..9,
                                                ),
                                            ],
                                            body: (
                                                Lam {
                                                    args: [],
                                                    body: (
                                                        App {
                                                            lam: (
                                                                Ident {
                                                                    name: "f",
                                                                },
                                                                18..19,
                                                            ),
                                                            args: [],
                                                        },
                                                        18..21,
                                                    ),
                                                    is_async: false,
                                                },
                                                12..21,
                                            ),
                                            is_async: false,
                                        },
                                        12..21,
                                    ),
                                },
                                12..21,
                            ),
                        },
                        0..21,
                    ),
                ],
            }
            "###);
    }

    #[test]
    fn simple_obj() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"), @r###"
            Program {
                body: [
                    (
                        Expr(
                            (
                                Obj {
                                    properties: [
                                        (
                                            Property {
                                                name: "x",
                                                value: (
                                                    Lit {
                                                        literal: Num(
                                                            "5",
                                                        ),
                                                    },
                                                    4..5,
                                                ),
                                            },
                                            1..5,
                                        ),
                                        (
                                            Property {
                                                name: "y",
                                                value: (
                                                    Lit {
                                                        literal: Num(
                                                            "10",
                                                        ),
                                                    },
                                                    10..12,
                                                ),
                                            },
                                            7..12,
                                        ),
                                    ],
                                },
                                0..13,
                            ),
                        ),
                        0..13,
                    ),
                ],
            }
            "###);
    }
}
