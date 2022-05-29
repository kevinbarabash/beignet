use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::util::just_with_padding;

pub fn jsx_parser(
    expr: BoxedParser<'_, char, Expr, Simple<char>>,
) -> impl Parser<char, Expr, Error = Simple<char>> + '_ {
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
        .ignore_then(expr)
        .then_ignore(just_with_padding("}"))
        .map_with_span(|expr, span| JSXExprContainer { span, expr });

    let jsx_element_child = choice((
        jsx_expr
            .clone()
            .map(JSXElementChild::JSXExprContainer),
        jsx_text.map(JSXElementChild::JSXText),
    ));

    let jsx_attr = text::ident()
        .map_with_span(|name, span| Ident { name, span })
        .then_ignore(just_with_padding("="))
        .then(choice((
            str_lit.map(JSXAttrValue::Lit),
            jsx_expr
                .clone()
                .map(JSXAttrValue::JSXExprContainer),
        )))
        .map_with_span(|(name, value), span| JSXAttr {
            span,
            ident: name,
            value,
        })
        .padded();

    // TODO: make jsx recursive so that we can nested elements, e.g.
    // <Foo><Bar>{baz}</Bar></Foo>
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
