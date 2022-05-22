use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use crate::ast::*;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn jsx_parser<'a>(
    expr: BoxedParser<'a, char, Expr, Simple<char>>,
) -> impl Parser<char, Expr, Error = Simple<char>> + 'a {
    let str_lit = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated().at_least(1))
        .then_ignore(just("\""))
        .collect::<String>()
        .map_with_span(|value, span| Lit::str(value, span));

    let jsx_text = filter(|c| *c != '<' && *c != '{')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map_with_span(|value, span| JSXText { span, value });

    let jsx_expr = just_with_padding("{")
        .ignore_then(expr)
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

    choice((jsx, jsx_self_closing))
}
