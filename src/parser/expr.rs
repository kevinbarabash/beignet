use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::jsx::jsx_parser;
use crate::parser::pattern::pattern_parser;
use crate::parser::types::type_parser;
use crate::parser::util::just_with_padding;

pub fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let type_ann = type_parser();

    let ident = text::ident().map_with_span(|name, span| Expr::Ident(Ident { span, name }));

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

    recursive(|expr| {
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

        let tuple = expr.clone()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("["), just_with_padding("]"))
            .map_with_span(|elements, span: Span| Expr::Tuple(Tuple { span, elements }));

        let atom = choice((
            if_else,
            r#bool,
            num,
            r#str,
            ident,
            obj,
            tuple,
            jsx_parser(expr.clone().boxed()).boxed(),
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
            .then(param_list)
            .then(just_with_padding(":").ignore_then(type_ann).or_not())
            .then_ignore(just_with_padding("=>"))
            .then(choice((
                expr.clone()
                    .delimited_by(just_with_padding("{"), just_with_padding("}")),
                expr.clone(),
            )))
            .map_with_span(|(((is_async, args), return_type), body), span: Span| {
                Expr::Lambda(Lambda {
                    span,
                    params: args,
                    body: Box::new(body),
                    is_async: is_async.is_some(),
                    return_type,
                })
            });

        // We use `just` instead of `just_with_padding` here to ensure that
        // the span doesn't include leading whitespace.
        let r#let = just("let")
            .ignore_then(just_with_padding("rec").or_not())
            .then(pattern_parser())
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
        choice((lam, r#let, comp))
    })
}
