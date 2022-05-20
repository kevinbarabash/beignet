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
            .then(jsx_attr.clone().repeated())
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

        let jsx_self_closing = just_with_padding("<")
            .ignore_then(text::ident().padded())
            .then(jsx_attr.clone().repeated())
            .then_ignore(just_with_padding("/>"))
            .map_with_span(|(head, attrs), span| {
                Expr::JSXElement(JSXElement {
                    span,
                    name: head,
                    attrs,
                    children: vec![],
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
            jsx_self_closing,
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
                    just_with_padding("==").to(BinOp::Eq),
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
            .map_with_span(|((is_async, args), body), span: Span| {
                Expr::Lambda(Lambda {
                    span,
                    args,
                    body: Box::new(body),
                    is_async: match is_async {
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
        choice((lam, r#let, comp))
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
    fn literals() {
        insta::assert_debug_snapshot!(parse("10"));
        insta::assert_debug_snapshot!(parse("1.23"));
        insta::assert_debug_snapshot!(parse("\"hello\""));
    }

    #[test]
    fn operations() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3"));
        insta::assert_debug_snapshot!(parse("x * y / z"));
        insta::assert_debug_snapshot!(parse("(a + b) * c"));
        insta::assert_debug_snapshot!(parse("a == b"));
        insta::assert_debug_snapshot!(parse("a != b"));
        insta::assert_debug_snapshot!(parse("a > b"));
        insta::assert_debug_snapshot!(parse("a >= b"));
        insta::assert_debug_snapshot!(parse("a < b"));
        insta::assert_debug_snapshot!(parse("a <= b"));
        insta::assert_debug_snapshot!(parse("let cond = a != b"));
    }

    #[test]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"));
        insta::assert_debug_snapshot!(parse("() => 10"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""));
    }

    #[test]
    fn async_await() {
        insta::assert_debug_snapshot!(parse("async () => 10"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"));
    }

    #[test]
    fn function_application() {
        insta::assert_debug_snapshot!(parse("foo()"));
        insta::assert_debug_snapshot!(parse("foo(a, b)"));
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\")"));
        insta::assert_debug_snapshot!(parse("f(x)(g(x))"));
    }

    #[test]
    fn declarations() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b"));
        insta::assert_debug_snapshot!(parse("let foo = let x = 5 in x"));
        insta::assert_debug_snapshot!(parse("let rec f = () => f()")); // recursive
    }

    #[test]
    fn top_level_expressions() {
        insta::assert_debug_snapshot!(parse("a + b"));
        insta::assert_debug_snapshot!(parse("123\n\"hello\""));
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if (true) { 5 } else { 10 }"));
    }

    #[test]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"));
    }

    #[test]
    fn jsx() {
        insta::assert_debug_snapshot!(parse("<Foo>Hello</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{bar}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>Hello {world}!</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{<Bar>{baz}</Bar>}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo bar={baz} />"));
        insta::assert_debug_snapshot!(parse("<Foo msg=\"hello\" bar={baz}></Foo>"));
    }
}
