use chumsky::prelude::*;

use super::lexer::Token;
use super::literal::Literal;
use super::syntax::{BinOp, BindingIdent, Expr, Pattern, Program, Statement};

pub type Span = std::ops::Range<usize>;

pub fn token_parser(
    source_spans: &[Span],
) -> impl Parser<Token, Program, Error = Simple<Token>> + '_ {
    let ident = select! { Token::Ident(name) => Expr::Ident { name } };
    let binding_ident = select! { Token::Ident(name) => BindingIdent::Ident { name } };
    let pattern = select! { Token::Ident(name) => Pattern::Ident { name } };
    let num = select! { Token::Num(value) => Expr::Lit { literal: Literal::Num(value) } };
    let r#true =
        select! { Token::True => Expr::Lit { literal: Literal::Bool(String::from("true")) } };
    let r#false =
        select! { Token::True => Expr::Lit { literal: Literal::Bool(String::from("false")) } };
    let r#str = select! { Token::Str(value) => Expr::Lit { literal: Literal::Str(value) } };

    let add_span_info = |node: Expr, token_span: Span| {
        let start = source_spans.get(token_span.start).unwrap().start;
        let end = source_spans.get(token_span.end - 1).unwrap().end;
        (node, start..end)
    };

    let expr = recursive(|expr| {
        let num = num.map_with_span(add_span_info);
        let r#str = r#str.map_with_span(add_span_info);
        let r#true = r#true.map_with_span(add_span_info);
        let r#false = r#false.map_with_span(add_span_info);
        let ident = ident.map_with_span(add_span_info);

        // TODO: handle chaining of if-else
        let if_else = just(Token::If)
            .ignore_then(just(Token::OpenParen))
            .ignore_then(expr.clone())
            .then_ignore(just(Token::CloseParen))
            .then_ignore(just(Token::OpenBrace))
            .then(expr.clone())
            .then_ignore(just(Token::CloseBrace))
            .then_ignore(just(Token::Else))
            .then_ignore(just(Token::OpenBrace))
            .then(expr.clone())
            .then_ignore(just(Token::CloseBrace))
            .map_with_span(|((cond, left), right), token_span: Span| {
                let start = source_spans.get(token_span.start).unwrap().start;
                let end = source_spans.get(token_span.end - 1).unwrap().end;
                (
                    Expr::If {
                        cond: Box::from(cond),
                        consequent: Box::from(left),
                        alternate: Box::from(right),
                    },
                    start..end,
                )
            });

        let atom = choice((
            if_else,
            num,
            r#str,
            r#true,
            r#false,
            ident,
            expr.clone()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        ));

        let app = atom
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                    .map_with_span(|args, token_span: Span| {
                        let start = source_spans.get(token_span.start).unwrap().start;
                        let end = source_spans.get(token_span.end - 1).unwrap().end;
                        (args, start..end)
                    })
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

        let product = app
            .clone()
            .then(
                choice((
                    just(Token::Times).to(BinOp::Mul),
                    just(Token::Div).to(BinOp::Div),
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
                    just(Token::Plus).to(BinOp::Add),
                    just(Token::Minus).to(BinOp::Sub),
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
            .map_with_span(|node, token_span: Span| {
                let start = source_spans.get(token_span.start).unwrap().start;
                let end = source_spans.get(token_span.end - 1).unwrap().end;
                (node, start..end)
            })
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

        let lam = param_list
            .then_ignore(just(Token::FatArrow))
            .then(
                choice((
                    expr.clone().delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
                    expr.clone(),
                )),
            )
            .map_with_span(|(args, body), token_span: Span| {
                let start = source_spans.get(token_span.start).unwrap().start;
                let end = source_spans.get(token_span.end - 1).unwrap().end;
                (
                    Expr::Lam {
                        args,
                        body: Box::new(body),
                        is_async: false,
                    },
                    start..end,
                )
            });

        let r#let = just(Token::Let)
            .ignore_then(just(Token::Rec).or_not())
            .then(pattern.map_with_span(|node, token_span: Span| {
                let start = source_spans.get(token_span.start).unwrap().start;
                let end = source_spans.get(token_span.end - 1).unwrap().end;
                (node, start..end)
            }))
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map_with_span(|(((rec, pattern), value), body), token_span: Span| {
                let start = source_spans.get(token_span.start).unwrap().start;
                let span = start..body.1.end;
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

        choice((lam, r#let, sum))
    });

    let decl = just(Token::Let)
        .ignore_then(just(Token::Rec).or_not())
        .then(pattern.map_with_span(|node, token_span: Span| {
            let start = source_spans.get(token_span.start).unwrap().start;
            let end = source_spans.get(token_span.end - 1).unwrap().end;
            (node, start..end)
        }))
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map_with_span(
            |((rec, pattern), value), token_span: Span| -> (Statement, std::ops::Range<usize>) {
                // excludes whitespace from end of span
                let start = source_spans.get(token_span.start).unwrap().start;
                let span = start..value.1.end.clone();
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
        decl,
        expr.map_with_span(|e, token_span: Span| {
            // excludes whitespace from end of span
            let start = source_spans.get(token_span.start).unwrap().start;
            let end = source_spans.get(token_span.end - 1).unwrap().end;
            (Statement::Expr(e), start..end)
        }),
    ))
    .repeated()
    .map(|body| Program { body });

    program.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::lexer::lexer;

    fn parse(input: &str) -> Program {
        let result = lexer().parse(input).unwrap();
        let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
        let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
        let program = token_parser(&spans).parse(tokens).unwrap();
        program
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

    #[test]
    #[should_panic]
    fn top_level_let_in_panics() {
        parse("let x = 5 in x");
    }

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
}
