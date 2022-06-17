use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::jsx::jsx_parser;
use crate::parser::pattern::pattern_parser;
use crate::parser::type_params::type_params;
use crate::parser::types::type_parser;
use crate::parser::util::just_with_padding;

pub fn expr_parser() -> BoxedParser<'static, char, Expr, Simple<char>> {
    let type_ann = type_parser();
    let pattern = pattern_parser();

    let ident = text::ident().map_with_span(|name, span| Ident { span, name });

    let r#true =
        just_with_padding("true").map_with_span(|_, span| Expr::Lit(Lit::bool(true, span)));
    let r#false =
        just_with_padding("false").map_with_span(|_, span| Expr::Lit(Lit::bool(false, span)));
    let r#bool = choice((r#true, r#false));

    let int = text::int::<char, Simple<char>>(10)
        .map_with_span(|value, span| Expr::Lit(Lit::num(value, span)));
    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map_with_span(|value, span| Expr::Lit(Lit::num(value, span)));
    let num = choice((real, int));

    let r#str = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated().at_least(1))
        .then_ignore(just("\""))
        .collect::<String>()
        .map_with_span(|value, span| Expr::Lit(Lit::str(value, span)));

    let parser = recursive(|expr: Recursive<'_, char, Expr, Simple<char>>| {
        // TODO: support recursive functions to be declared within another function
        // let let_rec = ...

        let block = just("let")
            .ignore_then(pattern.clone())
            .then_ignore(just_with_padding("="))
            .or_not()
            .then(expr.clone())
            .separated_by(just_with_padding(";"))
            .then(just_with_padding(";").or_not())
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map(|(lets, trailing_semi)| {
                let mut iter = lets.iter().rev();

                // TODO: if `lets` is empty then we should return the empty type
                
                let last = match trailing_semi {
                    Some(_) => {
                        Expr::Empty(Empty {
                            span: 0..0,
                        })
                    }
                    None => {
                        match iter.next() {
                            Some(term) => match term {
                                // TODO: if we do get a `let` last, we should be able to type
                                // is as `empty`
                                (Some(_), _) => panic!("Didn't expect `let` here"),
                                (_, expr) => expr.clone(),
                            },
                            None => {
                                Expr::Empty(Empty {
                                    span: 0..0,
                                })
                            },
                        }
                    }
                };

                let result: Expr = iter.fold(last, |body, (pattern, value)| {
                    let start = match pattern {
                        Some(pattern) => pattern.span().start,
                        None => value.span().start,
                    };
                    let end = body.span().end;

                    Expr::Let(Let {
                        span: start..end,
                        pattern: pattern.to_owned(),
                        init: Box::new(value.to_owned()),
                        body: Box::new(body),
                    })
                });

                result
            });

        let if_else = recursive(|if_else| {
            just_with_padding("if")
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(just_with_padding("else").ignore_then(block.clone().or(if_else)).or_not())
                .map_with_span(|((cond, cons), alt), span: Span| {
                    Expr::IfElse(IfElse {
                        span,
                        cond: Box::from(cond),
                        consequent: Box::from(cons),
                        alternate: alt.map(Box::from)
                    })
                })
        });

        let key_value_prop = text::ident()
            .then_ignore(just_with_padding(":"))
            .then(expr.clone())
            .map_with_span(|(name, value), span: Span| {
                Prop::KeyValue(KeyValueProp { span, name, value })
            });

        let shorthand_prop =
            text::ident().map_with_span(|name, span: Span| Prop::Shorthand(Ident { span, name }));

        let prop = choice((key_value_prop, shorthand_prop));

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|props, span: Span| Expr::Obj(Obj { span, props }));

        let tuple = expr
            .clone()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("["), just_with_padding("]"))
            .map_with_span(|elems, span: Span| Expr::Tuple(Tuple { span, elems }));

        let atom = choice((
            if_else,
            r#bool,
            num,
            r#str,
            ident.map(Expr::Ident),
            obj,
            tuple,
            jsx_parser(expr.clone().boxed()),
            expr.clone()
                .delimited_by(just_with_padding("("), just_with_padding(")")),
        ));

        let mem = atom
            .clone()
            .then(
                just_with_padding(".")
                    .ignore_then(text::ident())
                    .map_with_span(|name, span: Span| {
                        let prop = MemberProp::Ident(Ident {
                            span: span.clone(),
                            name,
                        });
                        (prop, span)
                    })
                    .repeated(),
            )
            .foldl(|f, (prop, span)| {
                let start = f.span().start;
                let end = span.end;
                let span = start..end;

                Expr::Member(Member {
                    span,
                    obj: Box::new(f),
                    prop,
                })
            });

        let app = mem
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
        let r#await = just_with_padding("await")
            .or_not()
            .then(app.clone())
            .map_with_span(|(option, arg), span: Span| match option {
                Some(_) => Expr::Await(Await {
                    span,
                    expr: Box::from(arg),
                }),
                None => arg,
            });

        let product = r#await
            .clone()
            .then(
                choice((
                    just_with_padding("*").to(BinOp::Mul),
                    just_with_padding("/").to(BinOp::Div),
                ))
                .then(r#await.clone())
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

        // TODO: logic operators
        let comp = sum
            .clone()
            .then(
                choice((
                    just_with_padding("==").to(BinOp::EqEq),
                    just_with_padding("!=").to(BinOp::NotEq),
                    // Must appear before ">"
                    just_with_padding(">=").to(BinOp::GtEq),
                    just_with_padding(">").to(BinOp::Gt),
                    // Must appear before "<"
                    just_with_padding("<=").to(BinOp::LtEq),
                    just_with_padding("<").to(BinOp::Lt),
                ))
                .then(sum.clone())
                .repeated(),
            )
            .foldl(|left, (op, right)| {
                // sums are already using source spans since they're WithSpan<Expr>
                let span = left.span().start..right.span().end;
                Expr::Op(Op {
                    span,
                    op,
                    left: Box::from(left),
                    right: Box::from(right),
                })
            });

        let param_list = pattern_parser()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("("), just_with_padding(")"));

        let lam = just_with_padding("async")
            .or_not()
            .then(type_params(type_ann.clone().boxed()).or_not())
            .then(param_list)
            .then(just_with_padding(":").ignore_then(type_ann).or_not())
            .then_ignore(just_with_padding("=>"))
            .then(choice((block.clone(), expr.clone())))
            .map_with_span(
                |((((is_async, type_params), args), return_type), body), span: Span| {
                    Expr::Lambda(Lambda {
                        span,
                        params: args,
                        body: Box::new(body),
                        is_async: is_async.is_some(),
                        return_type,
                        type_params,
                    })
                },
            );

        choice((lam, block, comp))
    });

    parser.boxed()
}
