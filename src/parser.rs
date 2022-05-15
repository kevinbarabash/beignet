use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use crate::ast::*;

pub type Span = std::ops::Range<usize>;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    let pattern = text::ident()
        .map_with_span(|name, span| Pattern::Ident(Ident { span, name }))
        .padded();
    let binding_ident =
        text::ident().map_with_span(|name, span| BindingIdent::Ident(Ident { span, name }));
    let ident = text::ident().map_with_span(|name, span| Expr::Ident(Ident { span, name }));
    let r#true =
        just_with_padding("true").map_with_span(|_, span| Expr::Lit(Lit::bool(true, span)));
    let r#false =
        just_with_padding("false").map_with_span(|_, span| Expr::Lit(Lit::bool(false, span)));

    let expr = recursive(|expr| {
        let int = text::int::<char, Simple<char>>(10)
            .map_with_span(|value, span| Expr::Lit(Lit::num(value, span)));

        let str_lit = just("\"")
            .ignore_then(filter(|c| *c != '"').repeated().at_least(1))
            .then_ignore(just("\""))
            .collect::<String>()
            .map_with_span(|value, span| Lit::str(value, span));

        let r#str = str_lit.map(|lit| Expr::Lit(lit));

        let real = text::int(10)
            .chain(just('.'))
            .chain::<char, _, _>(text::digits(10))
            .collect::<String>()
            .map_with_span(|value, span| Expr::Lit(Lit::num(value, span)));

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
                Expr::IfElse(IfElse {
                    span,
                    cond: Box::from(cond),
                    consequent: Box::from(left),
                    alternate: Box::from(right),
                })
            });

        let prop = text::ident()
            .then_ignore(just_with_padding(":"))
            .then(expr.clone())
            .map_with_span(|(name, value), span: Span| Property { span, name, value });

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|properties, span: Span| Expr::Obj(Obj { span, properties }));

        let jsx_text = filter(|c| *c != '<' && *c != '{')
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map_with_span(|value, span| JSXText { span, value });

        let jsx_expr = just_with_padding("{")
            .ignore_then(expr.clone())
            .then_ignore(just_with_padding("}"))
            .map_with_span(|expr, span| JSXExprContainer { span, expr });

        let jsx_element_child = choice((
            jsx_expr
                .clone()
                .map(|node| JSXElementChild::JSXExprContainer(node)),
            jsx_text.map(|node| JSXElementChild::JSXText(node)),
        ));

        let jsx_attr = text::ident()
            .map_with_span(|name, span| Ident { name, span })
            .then_ignore(just_with_padding("="))
            .then(choice((
                str_lit.map(|node| JSXAttrValue::Lit(node)),
                jsx_expr
                    .clone()
                    .map(|node| JSXAttrValue::JSXExprContainer(node)),
            )))
            .map_with_span(|(name, value), span| JSXAttr {
                span,
                ident: name,
                value,
            })
            .padded();

        let jsx = just_with_padding("<")
            .ignore_then(text::ident().padded()) // head
            .then(jsx_attr.repeated())
            .then_ignore(just_with_padding(">"))
            .then(jsx_element_child.repeated())
            .then_ignore(just("<"))
            .then_ignore(just("/"))
            .then(text::ident().padded()) // tail
            .then_ignore(just_with_padding(">"))
            .map_with_span(|(((head, attrs), children), tail), span| {
                assert_eq!(head, tail);

                Expr::JSXElement(JSXElement {
                    span,
                    name: head,
                    attrs,
                    children,
                })
            });

        let atom = choice((
            if_else,
            real,
            int,
            r#str,
            r#true,
            r#false,
            ident,
            obj,
            jsx,
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

                Expr::App(App {
                    span,
                    lam: Box::new(f),
                    args,
                })
            });

        // Application is higher precedence than `await`
        let app = just_with_padding("await")
            .or_not()
            .then(app.clone())
            .map_with_span(|(option, app), span: Span| match option {
                Some(_) => Expr::Await(Await {
                    span,
                    expr: Box::from(app),
                }),
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
                Expr::Op(Op {
                    span,
                    op,
                    left: Box::from(left),
                    right: Box::from(right),
                })
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
                Expr::Op(Op {
                    span,
                    op,
                    left: Box::from(left),
                    right: Box::from(right),
                })
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
                Expr::Lambda(Lambda {
                    span,
                    args,
                    body: Box::new(body),
                    is_async: match r#async {
                        Some(_) => true,
                        None => false,
                    },
                })
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
                    None => Expr::Let(Let {
                        span,
                        pattern,
                        value: Box::new(value),
                        body: Box::new(body),
                    }),
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
                        Pattern::Ident(Ident { name, span }) => BindingIdent::Ident(Ident {
                            span: span.clone(),
                            name: name.clone(),
                        }),
                    };

                    Statement::Decl {
                        span,
                        pattern,
                        // `let fib = fix((fib) => (n) => ...)`
                        // TODO: Fix always wraps a lambda
                        value: Expr::Fix(Fix {
                            span: value.span(),
                            expr: Box::from(Expr::Lambda(Lambda {
                                span: value.span(),
                                args: vec![ident],
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
                    expr: Lit(
                        Num(
                            Num {
                                span: 0..2,
                                value: "10",
                            },
                        ),
                    ),
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
                    expr: Lit(
                        Num(
                            Num {
                                span: 0..4,
                                value: "1.23",
                            },
                        ),
                    ),
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
                    expr: Lambda(
                        Lambda {
                            span: 0..11,
                            args: [
                                Ident(
                                    Ident {
                                        span: 1..2,
                                        name: "a",
                                    },
                                ),
                                Ident(
                                    Ident {
                                        span: 4..5,
                                        name: "b",
                                    },
                                ),
                            ],
                            body: Ident(
                                Ident {
                                    span: 10..11,
                                    name: "c",
                                },
                            ),
                            is_async: false,
                        },
                    ),
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
                    expr: Lambda(
                        Lambda {
                            span: 0..8,
                            args: [],
                            body: Lit(
                                Num(
                                    Num {
                                        span: 6..8,
                                        value: "10",
                                    },
                                ),
                            ),
                            is_async: false,
                        },
                    ),
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
                    expr: Lambda(
                        Lambda {
                            span: 0..14,
                            args: [],
                            body: Lit(
                                Num(
                                    Num {
                                        span: 12..14,
                                        value: "10",
                                    },
                                ),
                            ),
                            is_async: true,
                        },
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..7,
                            name: "foo",
                        },
                    ),
                    value: Lambda(
                        Lambda {
                            span: 10..34,
                            args: [],
                            body: Await(
                                Await {
                                    span: 24..32,
                                    expr: Lit(
                                        Num(
                                            Num {
                                                span: 30..32,
                                                value: "10",
                                            },
                                        ),
                                    ),
                                },
                            ),
                            is_async: true,
                        },
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..7,
                            name: "foo",
                        },
                    ),
                    value: Lambda(
                        Lambda {
                            span: 10..39,
                            args: [],
                            body: Op(
                                Op {
                                    span: 22..39,
                                    op: Add,
                                    left: Await(
                                        Await {
                                            span: 22..29,
                                            expr: Ident(
                                                Ident {
                                                    span: 28..29,
                                                    name: "a",
                                                },
                                            ),
                                        },
                                    ),
                                    right: Await(
                                        Await {
                                            span: 32..39,
                                            expr: Ident(
                                                Ident {
                                                    span: 38..39,
                                                    name: "b",
                                                },
                                            ),
                                        },
                                    ),
                                },
                            ),
                            is_async: true,
                        },
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..7,
                            name: "foo",
                        },
                    ),
                    value: Lambda(
                        Lambda {
                            span: 10..33,
                            args: [],
                            body: Await(
                                Await {
                                    span: 22..33,
                                    expr: App(
                                        App {
                                            span: 28..33,
                                            lam: Ident(
                                                Ident {
                                                    span: 28..31,
                                                    name: "bar",
                                                },
                                            ),
                                            args: [],
                                        },
                                    ),
                                },
                            ),
                            is_async: true,
                        },
                    ),
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
                    expr: Lambda(
                        Lambda {
                            span: 0..14,
                            args: [
                                Ident(
                                    Ident {
                                        span: 1..2,
                                        name: "a",
                                    },
                                ),
                            ],
                            body: Lit(
                                Str(
                                    Str {
                                        span: 7..14,
                                        value: "hello",
                                    },
                                ),
                            ),
                            is_async: false,
                        },
                    ),
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
                    expr: App(
                        App {
                            span: 0..5,
                            lam: Ident(
                                Ident {
                                    span: 0..3,
                                    name: "foo",
                                },
                            ),
                            args: [],
                        },
                    ),
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
                    expr: App(
                        App {
                            span: 0..9,
                            lam: Ident(
                                Ident {
                                    span: 0..3,
                                    name: "foo",
                                },
                            ),
                            args: [
                                Ident(
                                    Ident {
                                        span: 4..5,
                                        name: "a",
                                    },
                                ),
                                Ident(
                                    Ident {
                                        span: 7..8,
                                        name: "b",
                                    },
                                ),
                            ],
                        },
                    ),
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
                    expr: App(
                        App {
                            span: 0..16,
                            lam: Ident(
                                Ident {
                                    span: 0..3,
                                    name: "foo",
                                },
                            ),
                            args: [
                                Lit(
                                    Num(
                                        Num {
                                            span: 4..6,
                                            value: "10",
                                        },
                                    ),
                                ),
                                Lit(
                                    Str(
                                        Str {
                                            span: 8..15,
                                            value: "hello",
                                        },
                                    ),
                                ),
                            ],
                        },
                    ),
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
                    expr: Lit(
                        Num(
                            Num {
                                span: 0..2,
                                value: "10",
                            },
                        ),
                    ),
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
                    expr: Lit(
                        Str(
                            Str {
                                span: 0..7,
                                value: "hello",
                            },
                        ),
                    ),
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
                    expr: Op(
                        Op {
                            span: 0..9,
                            op: Sub,
                            left: Op(
                                Op {
                                    span: 0..5,
                                    op: Add,
                                    left: Lit(
                                        Num(
                                            Num {
                                                span: 0..1,
                                                value: "1",
                                            },
                                        ),
                                    ),
                                    right: Lit(
                                        Num(
                                            Num {
                                                span: 4..5,
                                                value: "2",
                                            },
                                        ),
                                    ),
                                },
                            ),
                            right: Lit(
                                Num(
                                    Num {
                                        span: 8..9,
                                        value: "3",
                                    },
                                ),
                            ),
                        },
                    ),
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
                    expr: Op(
                        Op {
                            span: 0..9,
                            op: Div,
                            left: Op(
                                Op {
                                    span: 0..5,
                                    op: Mul,
                                    left: Ident(
                                        Ident {
                                            span: 0..1,
                                            name: "x",
                                        },
                                    ),
                                    right: Ident(
                                        Ident {
                                            span: 4..5,
                                            name: "y",
                                        },
                                    ),
                                },
                            ),
                            right: Ident(
                                Ident {
                                    span: 8..9,
                                    name: "z",
                                },
                            ),
                        },
                    ),
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
                    expr: Op(
                        Op {
                            span: 0..9,
                            op: Add,
                            left: Ident(
                                Ident {
                                    span: 0..1,
                                    name: "a",
                                },
                            ),
                            right: Op(
                                Op {
                                    span: 4..9,
                                    op: Mul,
                                    left: Ident(
                                        Ident {
                                            span: 4..5,
                                            name: "b",
                                        },
                                    ),
                                    right: Ident(
                                        Ident {
                                            span: 8..9,
                                            name: "c",
                                        },
                                    ),
                                },
                            ),
                        },
                    ),
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
                    expr: Op(
                        Op {
                            span: 1..11,
                            op: Mul,
                            left: Op(
                                Op {
                                    span: 1..6,
                                    op: Add,
                                    left: Ident(
                                        Ident {
                                            span: 1..2,
                                            name: "a",
                                        },
                                    ),
                                    right: Ident(
                                        Ident {
                                            span: 5..6,
                                            name: "b",
                                        },
                                    ),
                                },
                            ),
                            right: Ident(
                                Ident {
                                    span: 10..11,
                                    name: "c",
                                },
                            ),
                        },
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..5,
                            name: "x",
                        },
                    ),
                    value: Lit(
                        Num(
                            Num {
                                span: 8..9,
                                value: "5",
                            },
                        ),
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..5,
                            name: "x",
                        },
                    ),
                    value: Lambda(
                        Lambda {
                            span: 8..23,
                            args: [
                                Ident(
                                    Ident {
                                        span: 9..10,
                                        name: "a",
                                    },
                                ),
                                Ident(
                                    Ident {
                                        span: 12..13,
                                        name: "b",
                                    },
                                ),
                            ],
                            body: Op(
                                Op {
                                    span: 18..23,
                                    op: Add,
                                    left: Ident(
                                        Ident {
                                            span: 18..19,
                                            name: "a",
                                        },
                                    ),
                                    right: Ident(
                                        Ident {
                                            span: 22..23,
                                            name: "b",
                                        },
                                    ),
                                },
                            ),
                            is_async: false,
                        },
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..5,
                            name: "x",
                        },
                    ),
                    value: Lit(
                        Num(
                            Num {
                                span: 8..9,
                                value: "5",
                            },
                        ),
                    ),
                },
                Decl {
                    span: 10..25,
                    pattern: Ident(
                        Ident {
                            span: 14..15,
                            name: "y",
                        },
                    ),
                    value: Lit(
                        Str(
                            Str {
                                span: 18..25,
                                value: "hello",
                            },
                        ),
                    ),
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
                    expr: Op(
                        Op {
                            span: 0..5,
                            op: Add,
                            left: Ident(
                                Ident {
                                    span: 0..1,
                                    name: "a",
                                },
                            ),
                            right: Ident(
                                Ident {
                                    span: 4..5,
                                    name: "b",
                                },
                            ),
                        },
                    ),
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
                    expr: Lit(
                        Num(
                            Num {
                                span: 0..3,
                                value: "123",
                            },
                        ),
                    ),
                },
                Expr {
                    span: 4..11,
                    expr: Lit(
                        Str(
                            Str {
                                span: 4..11,
                                value: "hello",
                            },
                        ),
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 4..7,
                            name: "foo",
                        },
                    ),
                    value: Let(
                        Let {
                            span: 10..24,
                            pattern: Ident(
                                Ident {
                                    span: 14..15,
                                    name: "x",
                                },
                            ),
                            value: Lit(
                                Num(
                                    Num {
                                        span: 18..19,
                                        value: "5",
                                    },
                                ),
                            ),
                            body: Ident(
                                Ident {
                                    span: 23..24,
                                    name: "x",
                                },
                            ),
                        },
                    ),
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
                    expr: App(
                        App {
                            span: 0..10,
                            lam: App(
                                App {
                                    span: 0..4,
                                    lam: Ident(
                                        Ident {
                                            span: 0..1,
                                            name: "f",
                                        },
                                    ),
                                    args: [
                                        Ident(
                                            Ident {
                                                span: 2..3,
                                                name: "x",
                                            },
                                        ),
                                    ],
                                },
                            ),
                            args: [
                                App(
                                    App {
                                        span: 5..9,
                                        lam: Ident(
                                            Ident {
                                                span: 5..6,
                                                name: "g",
                                            },
                                        ),
                                        args: [
                                            Ident(
                                                Ident {
                                                    span: 7..8,
                                                    name: "x",
                                                },
                                            ),
                                        ],
                                    },
                                ),
                            ],
                        },
                    ),
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
                    expr: IfElse(
                        IfElse {
                            span: 0..27,
                            cond: Lit(
                                Bool(
                                    Bool {
                                        span: 4..8,
                                        value: true,
                                    },
                                ),
                            ),
                            consequent: Lit(
                                Num(
                                    Num {
                                        span: 12..13,
                                        value: "5",
                                    },
                                ),
                            ),
                            alternate: Lit(
                                Num(
                                    Num {
                                        span: 23..25,
                                        value: "10",
                                    },
                                ),
                            ),
                        },
                    ),
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
                    pattern: Ident(
                        Ident {
                            span: 8..9,
                            name: "f",
                        },
                    ),
                    value: Fix(
                        Fix {
                            span: 12..21,
                            expr: Lambda(
                                Lambda {
                                    span: 12..21,
                                    args: [
                                        Ident(
                                            Ident {
                                                span: 8..9,
                                                name: "f",
                                            },
                                        ),
                                    ],
                                    body: Lambda(
                                        Lambda {
                                            span: 12..21,
                                            args: [],
                                            body: App(
                                                App {
                                                    span: 18..21,
                                                    lam: Ident(
                                                        Ident {
                                                            span: 18..19,
                                                            name: "f",
                                                        },
                                                    ),
                                                    args: [],
                                                },
                                            ),
                                            is_async: false,
                                        },
                                    ),
                                    is_async: false,
                                },
                            ),
                        },
                    ),
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
                    expr: Obj(
                        Obj {
                            span: 0..13,
                            properties: [
                                Property {
                                    span: 1..5,
                                    name: "x",
                                    value: Lit(
                                        Num(
                                            Num {
                                                span: 4..5,
                                                value: "5",
                                            },
                                        ),
                                    ),
                                },
                                Property {
                                    span: 7..12,
                                    name: "y",
                                    value: Lit(
                                        Num(
                                            Num {
                                                span: 10..12,
                                                value: "10",
                                            },
                                        ),
                                    ),
                                },
                            ],
                        },
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn jsx_with_jsx_text_child() {
        insta::assert_debug_snapshot!(parse("<Foo>Hello</Foo>"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..16,
                    expr: JSXElement(
                        JSXElement {
                            span: 0..16,
                            name: "Foo",
                            attrs: [],
                            children: [
                                JSXText(
                                    JSXText {
                                        span: 5..10,
                                        value: "Hello",
                                    },
                                ),
                            ],
                        },
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn jsx_with_jsx_expr_child() {
        insta::assert_debug_snapshot!(parse("<Foo>{bar}</Foo>"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..16,
                    expr: JSXElement(
                        JSXElement {
                            span: 0..16,
                            name: "Foo",
                            attrs: [],
                            children: [
                                JSXExprContainer(
                                    JSXExprContainer {
                                        span: 5..10,
                                        expr: Ident(
                                            Ident {
                                                span: 6..9,
                                                name: "bar",
                                            },
                                        ),
                                    },
                                ),
                            ],
                        },
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn jsx_with_jsx_expr_and_text_children() {
        insta::assert_debug_snapshot!(parse("<Foo>Hello {world}!</Foo>"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..25,
                    expr: JSXElement(
                        JSXElement {
                            span: 0..25,
                            name: "Foo",
                            attrs: [],
                            children: [
                                JSXText(
                                    JSXText {
                                        span: 5..11,
                                        value: "Hello ",
                                    },
                                ),
                                JSXExprContainer(
                                    JSXExprContainer {
                                        span: 11..18,
                                        expr: Ident(
                                            Ident {
                                                span: 12..17,
                                                name: "world",
                                            },
                                        ),
                                    },
                                ),
                                JSXText(
                                    JSXText {
                                        span: 18..19,
                                        value: "!",
                                    },
                                ),
                            ],
                        },
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn jsx_with_nested_jsx_in_expr_child() {
        insta::assert_debug_snapshot!(parse("<Foo>{<Bar>{baz}</Bar>}</Foo>"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..29,
                    expr: JSXElement(
                        JSXElement {
                            span: 0..29,
                            name: "Foo",
                            attrs: [],
                            children: [
                                JSXExprContainer(
                                    JSXExprContainer {
                                        span: 5..23,
                                        expr: JSXElement(
                                            JSXElement {
                                                span: 6..22,
                                                name: "Bar",
                                                attrs: [],
                                                children: [
                                                    JSXExprContainer(
                                                        JSXExprContainer {
                                                            span: 11..16,
                                                            expr: Ident(
                                                                Ident {
                                                                    span: 12..15,
                                                                    name: "baz",
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                ],
                                            },
                                        ),
                                    },
                                ),
                            ],
                        },
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn jsx_with_no_child() {
        insta::assert_debug_snapshot!(parse("<Foo></Foo>"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..16,
                    expr: JSXElement(
                        JSXElement {
                            span: 0..11,
                            name: "Foo",
                            attrs: [],
                            children: [],
                        },
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn jsx_with_attrs() {
        insta::assert_debug_snapshot!(parse("<Foo msg=\"hello\" bar={baz}></Foo>"), @r###"
        Program {
            body: [
                Expr {
                    span: 0..33,
                    expr: JSXElement(
                        JSXElement {
                            span: 0..33,
                            name: "Foo",
                            attrs: [
                                JSXAttr {
                                    span: 5..16,
                                    ident: Ident {
                                        span: 5..8,
                                        name: "msg",
                                    },
                                    value: Lit(
                                        Str(
                                            Str {
                                                span: 9..16,
                                                value: "hello",
                                            },
                                        ),
                                    ),
                                },
                                JSXAttr {
                                    span: 17..26,
                                    ident: Ident {
                                        span: 17..20,
                                        name: "bar",
                                    },
                                    value: JSXExprContainer(
                                        JSXExprContainer {
                                            span: 21..26,
                                            expr: Ident(
                                                Ident {
                                                    span: 22..25,
                                                    name: "baz",
                                                },
                                            ),
                                        },
                                    ),
                                },
                            ],
                            children: [],
                        },
                    ),
                },
            ],
        }
        "###);
    }
}
