use chumsky::prelude::*;

use super::literal::Literal;
use super::syntax::{BinOp, BindingIdent, Expr, ExprWithSpan, Pattern};

pub type Span = std::ops::Range<usize>;

pub fn parser() -> impl Parser<char, ExprWithSpan, Error = Simple<char>> {
    let ident = text::ident().padded();

    let num = text::int(10)
        .map_with_span(|s, span: Span| {
            (
                Expr::Lit {
                    literal: Literal::Num(s),
                },
                span,
            )
        })
        .padded();

    let str_ = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map_with_span(|s, span| {
            (
                Expr::Lit {
                    literal: Literal::Str(s),
                },
                span,
            )
        });

    let lit = num.or(str_);

    let expr = recursive(|expr| {
        let atom = num
            .or(expr.clone().delimited_by(just("("), just(")")))
            .or(ident.map_with_span(|name, span| (Expr::Ident { name }, span)))
            .or(lit);

        let product = atom
            .clone()
            .then(
                choice((
                    just("*").padded().to(BinOp::Mul),
                    just("/").padded().to(BinOp::Div),
                ))
                .then(atom.clone())
                .repeated(),
            )
            .foldl(|left, (op, right)| {
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
                    just("+").padded().to(BinOp::Add),
                    just("-").padded().to(BinOp::Sub),
                ))
                .then(product.clone())
                .repeated(),
            )
            .foldl(|left, (op, right)| {
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

        let app = atom
            .clone()
            .then(
                expr.clone()
                    .padded()
                    .separated_by(just(","))
                    .allow_trailing()
                    .delimited_by(just("("), just(")")),
            )
            .map_with_span(|(func, args), span| {
                (
                    Expr::App {
                        lam: Box::new(func),
                        args,
                    },
                    span,
                )
            });

        let param_list = ident
            .map_with_span(|name, span| (BindingIdent::Ident { name }, span))
            .padded()
            .separated_by(just(","))
            .allow_trailing()
            .delimited_by(just("("), just(")"));

        let lam = param_list
            .then_ignore(just("=>").padded())
            .then(expr.clone())
            .map_with_span(|(args, body), span| {
                (
                    Expr::Lam {
                        args,
                        body: Box::new(body),
                        is_async: false,
                    },
                    span,
                )
            });

        let r#let = just("let")
            .ignore_then(ident.map_with_span(|name, span| (Pattern::Ident { name }, span)))
            .then_ignore(just("=").padded())
            .then(expr.clone())
            .then_ignore(just("in").padded())
            .then(expr.clone())
            .map_with_span(|((pattern, value), body), span| {
                (
                    Expr::Let {
                        pattern,
                        value: Box::new(value),
                        body: Box::new(body),
                    },
                    span,
                )
            });

        app.or(lam).or(r#let).or(sum)
    });

    expr.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fn_with_multiple_params() {
        insta::assert_debug_snapshot!(parser().parse("(a, b) => c").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn fn_returning_num_literal() {
        insta::assert_debug_snapshot!(parser().parse("() => 10").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn fn_returning_str_literal() {
        insta::assert_debug_snapshot!(parser().parse("(a) => \"hello\"").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn app_with_no_args() {
        insta::assert_debug_snapshot!(parser().parse("foo()").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn app_with_multiple_args() {
        insta::assert_debug_snapshot!(parser().parse("foo(a, b)").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn app_with_multiple_lit_args() {
        insta::assert_debug_snapshot!(parser().parse("foo(10, \"hello\")").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn atom_number() {
        insta::assert_debug_snapshot!(parser().parse("10").unwrap(), @r###"
        (
            Lit {
                literal: Num(
                    "10",
                ),
            },
            0..2,
        )
        "###);
    }

    #[test]
    fn atom_string() {
        insta::assert_debug_snapshot!(parser().parse("\"hello\"").unwrap(), @r###"
        (
            Lit {
                literal: Str(
                    "hello",
                ),
            },
            0..7,
        )
        "###);
    }

    #[test]
    fn simple_let() {
        // TODO: don't include whitespace and the '=' in the span for 'x'
        insta::assert_debug_snapshot!(parser().parse("let x = 5 in x").unwrap(), @r###"
        (
            Let {
                pattern: (
                    Ident {
                        name: "x",
                    },
                    3..6,
                ),
                value: (
                    Lit {
                        literal: Num(
                            "5",
                        ),
                    },
                    8..9,
                ),
                body: (
                    Ident {
                        name: "x",
                    },
                    13..14,
                ),
            },
            0..14,
        )
        "###);
    }

    #[test]
    fn nested_let() {
        // TODO: don't include whitespace and the '=' in the span of the patterns
        insta::assert_debug_snapshot!(parser().parse("let x = 5 in let y = 10 in x + y").unwrap(), @r###"
        (
            Let {
                pattern: (
                    Ident {
                        name: "x",
                    },
                    3..6,
                ),
                value: (
                    Lit {
                        literal: Num(
                            "5",
                        ),
                    },
                    8..9,
                ),
                body: (
                    Let {
                        pattern: (
                            Ident {
                                name: "y",
                            },
                            16..19,
                        ),
                        value: (
                            Lit {
                                literal: Num(
                                    "10",
                                ),
                            },
                            21..23,
                        ),
                        body: (
                            Op {
                                op: Add,
                                left: (
                                    Ident {
                                        name: "x",
                                    },
                                    27..29,
                                ),
                                right: (
                                    Ident {
                                        name: "y",
                                    },
                                    31..32,
                                ),
                            },
                            27..32,
                        ),
                    },
                    13..32,
                ),
            },
            0..32,
        )
        "###);
    }

    #[test]
    fn add_sub_operations() {
        insta::assert_debug_snapshot!(parser().parse("1 + 2 - 3").unwrap(), @r###"
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
        )
        "###);
    }

    #[test]
    fn mul_div_operations() {
        // TODO: don't include trailing whitespace in identifiers
        insta::assert_debug_snapshot!(parser().parse("x * y / z").unwrap(), @r###"
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
                            0..2,
                        ),
                        right: (
                            Ident {
                                name: "y",
                            },
                            4..6,
                        ),
                    },
                    0..6,
                ),
                right: (
                    Ident {
                        name: "z",
                    },
                    8..9,
                ),
            },
            0..9,
        )
        "###);
    }

    #[test]
    fn operator_precedence() {
        // TODO: don't include trailing whitespace in identifiers
        insta::assert_debug_snapshot!(parser().parse("a + b * c").unwrap(), @r###"
        (
            Op {
                op: Add,
                left: (
                    Ident {
                        name: "a",
                    },
                    0..2,
                ),
                right: (
                    Op {
                        op: Mul,
                        left: (
                            Ident {
                                name: "b",
                            },
                            4..6,
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
        )
        "###);
    }

    #[test]
    fn specifying_operator_precedence_with_parens() {
        // TODO: don't include trailing whitespace in identifiers
        insta::assert_debug_snapshot!(parser().parse("(a + b) * c").unwrap(), @r###"
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
                            1..3,
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
        )
        "###);
    }
}
