use chumsky::prelude::*;
use crochet_ast::*;

use crate::lit::{boolean_parser, number_parser, string_parser};
use crate::type_ann::Primitive;
use crate::util::just_with_padding;

use super::type_params::type_params;

pub fn type_ann_parser() -> BoxedParser<'static, char, TypeAnn, Simple<char>> {
    let prim = choice((
        just("number").to(Primitive::Num),
        just("string").to(Primitive::Str),
        just("boolean").to(Primitive::Bool),
        just("null").to(Primitive::Null),
        just("undefined").to(Primitive::Undefined),
    ))
    .map_with_span(|prim, span| TypeAnn::Prim(PrimType { span, prim }))
    .padded();

    let parser = recursive(|type_ann| {
        let type_args = type_ann
            .clone()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("<"), just_with_padding(">"));

        let type_ref =
            text::ident()
                .then(type_args.or_not())
                .map_with_span(|(name, type_params), span| {
                    TypeAnn::TypeRef(TypeRef {
                        span,
                        name,
                        type_params,
                    })
                });

        let prop = text::ident()
            .then(just("?").or_not())
            .then_ignore(just_with_padding(":"))
            .then(type_ann.clone())
            .map_with_span(|((name, optional), type_ann), span: Span| TProp {
                span,
                name,
                optional: optional.is_some(),
                // TODO: properties are immutable by default, add a `mut` keyword
                // to make them mutable
                mutable: false,
                type_ann: Box::from(type_ann),
            });

        let obj = prop
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("{"), just_with_padding("}"))
            .map_with_span(|props, span: Span| TypeAnn::Object(ObjectType { span, props }));

        let tuple = type_ann
            .clone()
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("["), just_with_padding("]"))
            .map_with_span(|types, span: Span| TypeAnn::Tuple(TupleType { span, types }));

        let atom = choice((
            boolean_parser().map(TypeAnn::Lit),
            number_parser().map(TypeAnn::Lit),
            string_parser().map(TypeAnn::Lit),
            prim,
            obj,
            tuple,
            type_ref,
            type_ann
                .clone()
                .delimited_by(just_with_padding("("), just_with_padding(")")),
        ));

        let atom_with_suffix =
            atom.clone()
                .then(just_with_padding("[]").repeated())
                .foldl(|accum, _| {
                    TypeAnn::Array(ArrayType {
                        span: 0..0, // FIXME
                        elem_type: Box::from(accum),
                    })
                });

        // We have to use `atom` here instead of `type_ann` to avoid a stack
        // overflow.
        let intersection = atom_with_suffix
            .clone()
            .separated_by(just_with_padding("&"))
            .map_with_span(|types, span| match types.len() {
                1 => types[0].clone(),
                _ => TypeAnn::Intersection(IntersectionType { span, types }),
            });

        let union = intersection
            .separated_by(just_with_padding("|"))
            .map_with_span(|types, span| match types.len() {
                1 => types[0].clone(),
                _ => TypeAnn::Union(UnionType { span, types }),
            });

        let ident = text::ident().map_with_span(|name, span: Span| Ident {
            span,
            name,
        });

        let ident_param = ident
            .then_ignore(just_with_padding(":"))
            .then(type_ann.clone())
            .map_with_span(|(ident, type_ann), span: Span| {
                FnParam::Ident(BindingIdent {
                    id: ident,
                    span,
                    type_ann: Some(type_ann),
                })
            });

        let rest_param = just_with_padding("...")
            .ignore_then(ident.map_with_span(|id, span: Span| {
                Pattern::Ident(BindingIdent {
                    id,
                    span,
                    type_ann: None,
                })
            }))
            .then_ignore(just_with_padding(":"))
            .then(type_ann.clone())
            .map_with_span(|(ident, type_ann), span: Span| {
                FnParam::Rest(RestPat {
                    span,
                    arg: Box::from(ident),
                    type_ann: Some(type_ann),
                })
            });

        let lam_params = choice((ident_param, rest_param))
            .separated_by(just_with_padding(","))
            .allow_trailing()
            .delimited_by(just_with_padding("("), just_with_padding(")"));

        let lam = type_params(type_ann.clone().boxed())
            .or_not()
            .then(lam_params)
            .then_ignore(just_with_padding("=>"))
            .then(type_ann.clone())
            .map_with_span(|((type_params, params), ret), span| {
                TypeAnn::Lam(LamType {
                    span,
                    params,
                    ret: Box::from(ret),
                    type_params,
                })
            });

        choice((
            // lambda types have higher precedence than union types so that
            // `(a: A, b: B) => C | D` parse as lambda type with a return type that
            // happens to be a union.
            lam, union, atom,
        ))
    });

    parser.boxed()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_type(input: &str) -> TypeAnn {
        type_ann_parser().then_ignore(end()).parse(input).unwrap()
    }

    #[test]
    fn fn_type_annotations() {
        insta::assert_debug_snapshot!(parse_type("(a: number, b: string) => boolean"));
        insta::assert_debug_snapshot!(parse_type("(a: A) => B & C | D"));
        insta::assert_debug_snapshot!(parse_type("(a: A, b: B) => C & D"));
        insta::assert_debug_snapshot!(parse_type("(a: A, b: B) => C | D"));
        insta::assert_debug_snapshot!(parse_type("((a: A, b: B) => C) | D"));
    }

    #[test]
    fn union_and_intersection_type_annotations() {
        insta::assert_debug_snapshot!(parse_type("A & B & C"));
        insta::assert_debug_snapshot!(parse_type("A & B | C & D"));
        insta::assert_debug_snapshot!(parse_type("(A | B) & (C | D)"));
    }

    #[test]
    fn literal_type_annotations() {
        insta::assert_debug_snapshot!(parse_type(r#""hello""#));
        insta::assert_debug_snapshot!(parse_type("true"));
        insta::assert_debug_snapshot!(parse_type("false"));
        insta::assert_debug_snapshot!(parse_type("1.23"));
        insta::assert_debug_snapshot!(parse_type("5"));
    }

    #[test]
    fn misc_type_annotations() {
        insta::assert_debug_snapshot!(parse_type("Promise<number>"));
        insta::assert_debug_snapshot!(parse_type("{x: number, y: number}"));
    }
}
