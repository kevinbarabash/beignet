use chumsky::prelude::*;

use super::literal::Literal;
use super::syntax::{BinOp, BindingIdent, Expr, Pattern};

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    let num = text::int(10)
        .map_with_span(|s, _| Expr::Lit {
            literal: Literal::Num(s),
        })
        .padded();

    let str_ = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map_with_span(|s, _| Expr::Lit {
            literal: Literal::Str(s),
        });

    let lit = num.or(str_);

    let expr = recursive(|expr| {
        let atom = num
            .or(expr.clone().delimited_by(just("("), just(")")))
            .or(ident.map_with_span(|name, _| Expr::Ident { name }))
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
            .foldl(|left, (op, right)| Expr::Op {
                op,
                left: Box::from(left),
                right: Box::from(right),
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
            .foldl(|left, (op, right)| Expr::Op {
                op,
                left: Box::from(left),
                right: Box::from(right),
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
            .map_with_span(|(func, args), _| Expr::App {
                lam: Box::new(func),
                args,
            });

        let param_list = ident
            .map_with_span(|name, _| BindingIdent::Ident(name))
            .padded()
            .separated_by(just(","))
            .allow_trailing()
            .delimited_by(just("("), just(")"));

        let lam = param_list
            .then_ignore(just("=>").padded())
            .then(expr.clone())
            .map_with_span(|(args, body), _| Expr::Lam {
                args,
                body: Box::new(body),
                is_async: false,
            });

        let r#let = just("let")
            .ignore_then(ident)
            .then_ignore(just("=").padded())
            .then(expr.clone())
            .then_ignore(just("in").padded())
            .then(expr.clone())
            .map_with_span(|((name, value), body), _| Expr::Let {
                pattern: Pattern::Ident(name),
                value: Box::new(value),
                body: Box::new(body),
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
        Lam {
            args: [
                Ident(
                    "a",
                ),
                Ident(
                    "b",
                ),
            ],
            body: Ident {
                name: "c",
            },
            is_async: false,
        }
        "###);
    }

    #[test]
    fn fn_returning_num_literal() {
        insta::assert_debug_snapshot!(parser().parse("() => 10").unwrap(), @r###"
        Lam {
            args: [],
            body: Lit {
                literal: Num(
                    "10",
                ),
            },
            is_async: false,
        }
        "###);
    }

    #[test]
    fn fn_returning_str_literal() {
        insta::assert_debug_snapshot!(parser().parse("(a) => \"hello\"").unwrap(), @r###"
        Lam {
            args: [
                Ident(
                    "a",
                ),
            ],
            body: Lit {
                literal: Str(
                    "hello",
                ),
            },
            is_async: false,
        }
        "###);
    }

    #[test]
    fn app_with_no_args() {
        insta::assert_debug_snapshot!(parser().parse("foo()").unwrap(), @r###"
        App {
            lam: Ident {
                name: "foo",
            },
            args: [],
        }
        "###);
    }

    #[test]
    fn app_with_multiple_args() {
        insta::assert_debug_snapshot!(parser().parse("foo(a, b)").unwrap(), @r###"
        App {
            lam: Ident {
                name: "foo",
            },
            args: [
                Ident {
                    name: "a",
                },
                Ident {
                    name: "b",
                },
            ],
        }
        "###);
    }

    #[test]
    fn app_with_multiple_lit_args() {
        insta::assert_debug_snapshot!(parser().parse("foo(10, \"hello\")").unwrap(), @r###"
        App {
            lam: Ident {
                name: "foo",
            },
            args: [
                Lit {
                    literal: Num(
                        "10",
                    ),
                },
                Lit {
                    literal: Str(
                        "hello",
                    ),
                },
            ],
        }
        "###);
    }

    #[test]
    fn atom_number() {
        insta::assert_debug_snapshot!(parser().parse("10").unwrap(), @r###"
        Lit {
            literal: Num(
                "10",
            ),
        }
        "###);
    }

    #[test]
    fn atom_string() {
        insta::assert_debug_snapshot!(parser().parse("\"hello\"").unwrap(), @r###"
        Lit {
            literal: Str(
                "hello",
            ),
        }
        "###);
    }

    #[test]
    fn simple_let() {
        insta::assert_debug_snapshot!(parser().parse("let x = 5 in x").unwrap(), @r###"
        Let {
            pattern: Ident(
                "x",
            ),
            value: Lit {
                literal: Num(
                    "5",
                ),
            },
            body: Ident {
                name: "x",
            },
        }
        "###);
    }

    #[test]
    fn nested_let() {
        insta::assert_debug_snapshot!(parser().parse("let x = 5 in let y = 10 in x + y").unwrap(), @r###"
        Let {
            pattern: Ident(
                "x",
            ),
            value: Lit {
                literal: Num(
                    "5",
                ),
            },
            body: Let {
                pattern: Ident(
                    "y",
                ),
                value: Lit {
                    literal: Num(
                        "10",
                    ),
                },
                body: Op {
                    op: Add,
                    left: Ident {
                        name: "x",
                    },
                    right: Ident {
                        name: "y",
                    },
                },
            },
        }
        "###);
    }

    #[test]
    fn add_sub_operations() {
        insta::assert_debug_snapshot!(parser().parse("1 + 2 - 3").unwrap(), @r###"
        Op {
            op: Sub,
            left: Op {
                op: Add,
                left: Lit {
                    literal: Num(
                        "1",
                    ),
                },
                right: Lit {
                    literal: Num(
                        "2",
                    ),
                },
            },
            right: Lit {
                literal: Num(
                    "3",
                ),
            },
        }
        "###);
    }

    #[test]
    fn mul_div_operations() {
        insta::assert_debug_snapshot!(parser().parse("x * y / z").unwrap(), @r###"
        Op {
            op: Div,
            left: Op {
                op: Mul,
                left: Ident {
                    name: "x",
                },
                right: Ident {
                    name: "y",
                },
            },
            right: Ident {
                name: "z",
            },
        }
        "###);
    }

    #[test]
    fn operator_precedence() {
        insta::assert_debug_snapshot!(parser().parse("a + b * c").unwrap(), @r###"
        Op {
            op: Add,
            left: Ident {
                name: "a",
            },
            right: Op {
                op: Mul,
                left: Ident {
                    name: "b",
                },
                right: Ident {
                    name: "c",
                },
            },
        }
        "###);
    }

    #[test]
    fn specifying_operator_precedence_with_parens() {
        insta::assert_debug_snapshot!(parser().parse("(a + b) * c").unwrap(), @r###"
        Op {
            op: Mul,
            left: Op {
                op: Add,
                left: Ident {
                    name: "a",
                },
                right: Ident {
                    name: "b",
                },
            },
            right: Ident {
                name: "c",
            },
        }
        "###);
    }
}
