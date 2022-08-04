use chumsky::prelude::*;
use crochet_ast::*;

use crate::util::just_with_padding;

pub fn jsx_parser(
    expr: BoxedParser<'static, char, Expr, Simple<char>>,
) -> BoxedParser<'static, char, Expr, Simple<char>> {
    let str_lit = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated().at_least(1))
        .then_ignore(just("\""))
        .collect::<String>()
        .map_with_span(Lit::str);

    let jsx_text = filter(|c| *c != '<' && *c != '{')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map_with_span(|value, span| JSXText { span, value });

    let jsx_expr = just_with_padding("{")
        .ignore_then(expr.clone())
        .then_ignore(just_with_padding("}"))
        .map_with_span(|expr, span| JSXExprContainer { span, expr });

    let jsx_attr = text::ident()
        .map_with_span(|name, span| Ident { name, span })
        .then_ignore(just_with_padding("="))
        .then(choice((
            str_lit.map(JSXAttrValue::Lit),
            jsx_expr.clone().map(JSXAttrValue::JSXExprContainer),
        )))
        .map_with_span(|(name, value), span| JSXAttr {
            span,
            ident: name,
            value,
        })
        .padded();

    let jsx_head = just_with_padding("<")
        .ignore_then(text::ident().padded())
        .then(jsx_attr.clone().repeated())
        .then_ignore(just_with_padding(">"));

    let jsx_tail = just("<")
        .ignore_then(just("/"))
        .ignore_then(text::ident().padded())
        .then_ignore(just_with_padding(">"))
        .labelled("jsx_tail");

    let jsx_element_self_closing = just_with_padding("<")
        .ignore_then(text::ident().padded())
        .then(jsx_attr.clone().repeated())
        .then_ignore(just_with_padding("/>"))
        .map_with_span(|(head, attrs), span| JSXElement {
            span,
            name: head,
            attrs,
            children: vec![],
        });

    let jsx_element_child = recursive(|jsx_element_child| {
        // TODO: dedupe with `jsx_element` below
        let jsx_element = jsx_head
            .clone()
            .then(jsx_element_child.repeated())
            .then(jsx_tail.clone())
            .map_with_span(|(((head, attrs), children), tail), span| {
                if head != tail {
                    panic!("JSX head and tail elements must match")
                }

                JSXElementChild::JSXElement(Box::from(JSXElement {
                    span,
                    name: head,
                    attrs,
                    children,
                }))
            });

        choice((
            jsx_element,
            jsx_element_self_closing
                .clone()
                .map(|elem| JSXElementChild::JSXElement(Box::from(elem))),
            jsx_expr.clone().map(JSXElementChild::JSXExprContainer),
            jsx_text.map(JSXElementChild::JSXText),
        ))
    });

    // TODO: dedupe with `jsx_element` above
    let jsx_element = jsx_head
        .then(jsx_element_child.repeated())
        .then(jsx_tail)
        .map_with_span(|(((head, attrs), children), tail), span| {
            if head != tail {
                panic!("JSX head and tail elements must match")
            }

            Expr::JSXElement(JSXElement {
                span,
                name: head,
                attrs,
                children,
            })
        });

    let jsx_parser = choice((jsx_element, jsx_element_self_closing.map(Expr::JSXElement)))
        .labelled("jsx_parser");

    jsx_parser.boxed()
}
