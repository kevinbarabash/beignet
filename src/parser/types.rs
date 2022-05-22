use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use crate::ast::*;
use crate::types::Primitive;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn type_parser() -> impl Parser<char, TypeAnn, Error = Simple<char>> {
    let prim = choice((
        just("number").to(Primitive::Num),
        just("string").to(Primitive::Str),
        just("boolean").to(Primitive::Bool),
        just("null").to(Primitive::Null),
        just("undefined").to(Primitive::Undefined),
    ))
    .map_with_span(|prim, span| TypeAnn::Prim(PrimType { span, prim }))
    .padded();

    let int = text::int::<char, Simple<char>>(10).map_with_span(|value, span: Span| {
        TypeAnn::Lit(LitType {
            span: span.clone(),
            lit: Lit::num(value, span.clone()),
        })
    });

    let r#str = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated().at_least(1))
        .then_ignore(just("\""))
        .collect::<String>()
        .map_with_span(|value, span: Span| {
            TypeAnn::Lit(LitType {
                span: span.clone(),
                lit: Lit::str(value, span.clone()),
            })
        });

    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map_with_span(|value, span: Span| {
            TypeAnn::Lit(LitType {
                span: span.clone(),
                lit: Lit::num(value, span.clone()),
            })
        });

    let type_ann = recursive(|type_ann| {
        let alias_params = type_ann
            .clone()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("<"), just_with_padding(">"));

        let alias =
            text::ident()
                .then(alias_params.or_not())
                .map_with_span(|(name, type_params), span| {
                    TypeAnn::TypeRef(TypeRef {
                        span,
                        name,
                        type_params,
                    })
                });

        let lam_params = type_ann
            .clone()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("("), just_with_padding(")"));

        let lam = lam_params
            .then_ignore(just_with_padding("=>"))
            .then(type_ann.clone())
            .map_with_span(|(args, ret), span| {
                TypeAnn::Lam(LamType {
                    span,
                    args,
                    ret: Box::from(ret),
                })
            });

        let prop = text::ident()
            .then_ignore(just_with_padding(":"))
            .then(type_ann.clone())
            .map_with_span(|(name, type_ann), span: Span| TProp {
                span,
                name,
                type_ann: Box::from(type_ann),
            });

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|props, span: Span| TypeAnn::Object(ObjectType { span, props }));

        let atom = choice((
            int,
            real,
            r#str,
            prim,
            obj,
            alias,
            type_ann
                .clone()
                .delimited_by(just_with_padding("("), just_with_padding(")")),
        ));

        // We have to use `atom` here instead of `type_ann` to avoid a stack
        // overflow.
        let union = atom
            .clone()
            .separated_by(just_with_padding("|"))
            .at_least(2)
            .map_with_span(|types, span| TypeAnn::Union(UnionType { span, types }));

        choice((
            // lambda types have higher precedence than union types so that
            // `(A, B) => C | D` parse as lambda type with a return type that
            // happens to be a union.
            lam,
            union,
            atom,
        ))
    });

    type_ann
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_type(input: &str) -> TypeAnn {
        type_parser().then_ignore(end()).parse(input).unwrap()
    }

    #[test]
    fn type_annotations() {
        insta::assert_debug_snapshot!(parse_type("Promise<number>"));
        insta::assert_debug_snapshot!(parse_type("(number, string) => boolean"));
        insta::assert_debug_snapshot!(parse_type("{x: number, y: number}"));
        insta::assert_debug_snapshot!(parse_type("(A, B) => C | D"));
        insta::assert_debug_snapshot!(parse_type("((A, B) => C) | D"));
    }
}