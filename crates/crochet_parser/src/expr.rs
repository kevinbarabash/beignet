use chumsky::prelude::*;
use crochet_ast::*;
use unescape::unescape;

use crate::fn_param::fn_param_parser;
use crate::jsx::jsx_parser;
use crate::lit::{boolean_parser, number_parser, string_parser};
use crate::pattern::pattern_parser;
use crate::type_ann::type_ann_parser;
use crate::type_params::type_params;
use crate::util::{comment_parser, just_with_padding};

pub fn expr_parser() -> BoxedParser<'static, char, Expr, Simple<char>> {
    let type_ann = type_ann_parser();
    let pattern = pattern_parser();

    // NOTE: It isn't possible to use comment_parser() here because rust will complain
    // about `ident` being moved below.
    let comment = just("//")
        .then_ignore(take_until(just('\n')))
        .padded()
        .labelled("comment");

    let ident = text::ident()
        .map_with_span(|name, span| Ident { span, name })
        .padded()
        .padded_by(comment.repeated());

    let parser = recursive(|expr: Recursive<'_, char, Expr, Simple<char>>| {
        // TODO: support recursive functions to be declared within another function
        // let let_rec = ...

        let block = just("let")
            .ignore_then(pattern.clone())
            .then(
                just_with_padding(":")
                    .ignore_then(type_ann.clone())
                    .or_not(),
            )
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
                    Some(_) => Expr::Empty(Empty { span: 0..0 }),
                    None => {
                        match iter.next() {
                            Some(term) => match term {
                                // TODO: if we do get a `let` last, we should be able to type
                                // is as `empty`
                                (Some(_), _) => panic!("Didn't expect `let` here"),
                                (_, expr) => expr.clone(),
                            },
                            None => Expr::Empty(Empty { span: 0..0 }),
                        }
                    }
                };

                let result: Expr = iter.fold(last, |body, (pattern_and_type, value)| {
                    let start = match pattern_and_type {
                        Some(pattern_and_type) => pattern_and_type.0.span().start,
                        None => value.span().start,
                    };
                    let end = body.span().end;

                    let (pattern, type_ann) = match pattern_and_type {
                        Some(pattern_and_type) => {
                            let (pattern, type_ann) = pattern_and_type;
                            match type_ann {
                                Some(type_ann) => {
                                    (Some(pattern.to_owned()), Some(type_ann.to_owned()))
                                }
                                None => (Some(pattern.to_owned()), None),
                            }
                        }
                        None => (None, None),
                    };

                    Expr::Let(Let {
                        span: start..end,
                        pattern,
                        type_ann,
                        init: Box::new(value.to_owned()),
                        body: Box::new(body),
                    })
                });

                result
            })
            .labelled("block");

        let let_expr = just_with_padding("let")
            .map_with_span(|_, span| span)
            .then(pattern.clone())
            .then_ignore(just_with_padding("="))
            .then(expr.clone())
            .map_with_span(|((let_span, pat), expr), _: Span| {
                let start = let_span.start;
                let end = expr.span().end;
                Expr::LetExpr(LetExpr {
                    span: start..end,
                    pat,
                    expr: Box::from(expr),
                })
            });

        let if_else = recursive(|if_else| {
            just_with_padding("if")
                .ignore_then(choice((let_expr, expr.clone())))
                .then(block.clone())
                .then(
                    just_with_padding("else")
                        .ignore_then(block.clone().or(if_else))
                        .or_not(),
                )
                .map_with_span(|((cond, cons), alt), span: Span| {
                    Expr::IfElse(IfElse {
                        span,
                        cond: Box::from(cond),
                        consequent: Box::from(cons),
                        alternate: alt.map(Box::from),
                    })
                })
        });

        let arm = pattern_parser()
            .then(just_with_padding("if").ignore_then(expr.clone()).or_not())
            .then_ignore(just_with_padding("=>"))
            .then(expr.clone())
            .map_with_span(|((pattern, cond), expr), span: Span| Arm {
                span,
                pattern,
                guard: cond,
                expr,
            });

        let r#match = just_with_padding("match")
            .ignore_then(expr.clone())
            .then(
                arm.separated_by(just_with_padding(","))
                    .allow_trailing()
                    .delimited_by(just_with_padding("{"), just_with_padding("}")),
            )
            .map_with_span(|(expr, arms), span: Span| {
                Expr::Match(Match {
                    span,
                    expr: Box::from(expr),
                    arms,
                })
            });

        let key_value_prop = text::ident()
            .then_ignore(just_with_padding(":"))
            .then(expr.clone())
            .map_with_span(|(name, value), span: Span| {
                PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
                    span,
                    name,
                    value: Box::from(value),
                })))
            });

        let shorthand_prop = text::ident().map_with_span(|name, span: Span| {
            PropOrSpread::Prop(Box::from(Prop::Shorthand(Ident { span, name })))
        });

        let rest_prop = just_with_padding("...")
            .ignore_then(expr.clone())
            .map_with_span(|expr, span: Span| {
                PropOrSpread::Spread(SpreadElement {
                    span,
                    expr: Box::from(expr),
                })
            });

        let prop = choice((key_value_prop, shorthand_prop, rest_prop));

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|props, span: Span| Expr::Obj(Obj { span, props }));

        let tuple = just_with_padding("...")
            .or_not()
            .then(expr.clone())
            .map_with_span(|(spread, elem), span: Span| match spread {
                Some(_) => ExprOrSpread {
                    spread: Some(span),
                    expr: Box::from(elem),
                },
                None => ExprOrSpread {
                    spread: None,
                    expr: Box::from(elem),
                },
            })
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("["), just_with_padding("]"))
            .map_with_span(|elems, span: Span| Expr::Tuple(Tuple { span, elems }));

        // NOTE: rewind() is used here so that we can tell later on if we should be
        // parsing an interpolation or the end of the template literal.
        let str_seg = take_until(just("${").rewind().or(just("`").rewind())).map_with_span(
            |(chars, _), span: Span| {
                let raw = String::from_iter(chars);
                let cooked = unescape(&raw).unwrap();
                TemplateElem {
                    span: span.clone(),
                    raw: Lit::str(raw, span.clone()),
                    cooked: Lit::str(cooked, span),
                }
            },
        );

        let interpolation = just("${").ignore_then(expr.clone()).then_ignore(just("}"));

        let template_str = ident
            .or_not()
            .then_ignore(just("`"))
            .then(str_seg)
            .then(interpolation.then(str_seg).repeated().map(|values| values))
            .then_ignore(just("`"))
            .map_with_span(|((tag, head), tail), span: Span| {
                let (exprs, mut quasis): (Vec<_>, Vec<_>) = tail.iter().cloned().unzip();
                quasis.insert(0, head);
                let template = TemplateLiteral {
                    span: span.clone(),
                    exprs,
                    quasis,
                };

                match tag {
                    Some(tag) => {
                        Expr::TaggedTemplateLiteral(TaggedTemplateLiteral {
                            span, // TODO: adjust this to include the tag
                            tag,
                            template,
                        })
                    }
                    None => Expr::TemplateLiteral(template),
                }
            });

        let atom = choice((
            // quarks (can't be broken down any further)
            boolean_parser().map(Expr::Lit).labelled("boolean"),
            number_parser().map(Expr::Lit).labelled("number"),
            string_parser().map(Expr::Lit).labelled("string"),
            // can contain sub-expressions, but have the highest precedence
            template_str.labelled("template_str"),
            if_else.labelled("if_else"),
            r#match.labelled("match"),
            ident.map(Expr::Ident).labelled("ident"),
            obj.labelled("obj"),
            tuple.labelled("tuple"),
            jsx_parser(expr.clone().boxed()),
            expr.clone()
                .delimited_by(just_with_padding("("), just_with_padding(")"))
                .labelled("parens"),
        ))
        .labelled("atom");

        enum Suffix {
            Member(MemberProp, Span),
            Call(Vec<ExprOrSpread>, Span),
        }

        let arg = just_with_padding("...")
            .map_with_span(|_, span: Span| span)
            .or_not()
            .then(expr.clone())
            .map_with_span(|(spread, arg), _: Span| ExprOrSpread {
                spread,
                expr: Box::from(arg),
            });

        let fn_call = arg
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("("), just_with_padding(")"))
            .map_with_span(Suffix::Call);

        let dot_member = just_with_padding(".")
            .ignore_then(text::ident())
            .map_with_span(|name, span: Span| {
                let prop = MemberProp::Ident(Ident {
                    span: span.clone(),
                    name,
                });
                Suffix::Member(prop, span)
            });

        let brackets_member = just_with_padding("[")
            .ignore_then(expr.clone())
            .then_ignore(just_with_padding("]"))
            .map_with_span(|expr, span: Span| {
                let prop = MemberProp::Computed(ComputedPropName {
                    span: span.clone(),
                    expr: Box::from(expr),
                });
                Suffix::Member(prop, span)
            });

        // NOTE: We use this approach of parsing suffixes instead of using a recursive
        // parser which would be left recursive (this causes a stack overflow).
        let atom_with_suffix = atom
            .clone()
            .then(choice((fn_call, dot_member, brackets_member)).repeated())
            .foldl(|f, suffix| {
                // TODO: check the type of `f` to verify if the current `suffix`
                // makes sense for that type of node.  For instance, calling a
                // method on a literal doesn't make sense.
                match suffix {
                    Suffix::Member(prop, span) => {
                        let start = f.span().start;
                        let end = span.end;

                        Expr::Member(Member {
                            span: start..end,
                            obj: Box::new(f),
                            prop,
                        })
                    }
                    Suffix::Call(args, span) => {
                        let start = f.span().start;
                        let end = span.end;

                        Expr::App(App {
                            span: start..end,
                            lam: Box::new(f),
                            args,
                        })
                    }
                }
            })
            .labelled("atom_with_suffix");

        // Application is higher precedence than `await`
        let r#await = just_with_padding("await")
            .or_not()
            .then(atom_with_suffix)
            .map_with_span(|(option, arg), span: Span| match option {
                Some(_) => Expr::Await(Await {
                    span,
                    expr: Box::from(arg),
                }),
                None => arg,
            });

        let negative = just_with_padding("-")
            .or_not()
            .then(r#await)
            .map_with_span(|(neg, arg), span: Span| match neg {
                None => arg,
                Some(_) => Expr::UnaryExpr(UnaryExpr {
                    span,
                    op: UnaryOp::Minus,
                    arg: Box::from(arg),
                }),
            })
            .labelled("negative");

        let product = negative
            .clone()
            .then(
                choice((
                    just_with_padding("*").to(BinOp::Mul),
                    just_with_padding("/").to(BinOp::Div),
                ))
                .then(negative.clone())
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
            })
            .labelled("product");

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

        let param_list = fn_param_parser()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("("), just_with_padding(")"))
            .labelled("param_list");

        let lam = just_with_padding("async")
            .or_not()
            .then(type_params(type_ann.clone().boxed()).or_not())
            .then(param_list)
            .then(just_with_padding(":").ignore_then(type_ann).or_not())
            .then_ignore(just_with_padding("=>"))
            .then(choice((block.clone(), expr.clone())))
            .map_with_span(
                |((((is_async, type_params), params), return_type), body), span: Span| {
                    for (i, param) in params.iter().enumerate() {
                        if let EFnParamPat::Rest(_) = param.pat {
                            if i < params.len() - 1 {
                                panic!("rest params must come last");
                            };
                        }
                    }
                    Expr::Lambda(Lambda {
                        span,
                        params,
                        body: Box::new(body),
                        is_async: is_async.is_some(),
                        return_type,
                        type_params,
                    })
                },
            );

        choice((lam, block, comp)).padded_by(comment_parser().repeated())
    })
    .labelled("expr");

    parser.boxed()
}
