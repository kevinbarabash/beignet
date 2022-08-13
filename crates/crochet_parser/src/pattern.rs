use chumsky::prelude::*;
use crochet_ast::*;

use crate::lit::string_parser;
use crate::type_ann::*;
use crate::util::just_with_padding;

// NOTE: Destructuring assignments admits different patterns from destructuring
// function params.  We'll need to have different parsers for those.
pub fn pattern_parser() -> BoxedParser<'static, char, Pattern, Simple<char>> {
    let type_ann = type_ann_parser();
    let mut top_level = true;

    let r#true = just_with_padding("true").map_with_span(|_, span| Lit::bool(true, span));
    let r#false = just_with_padding("false").map_with_span(|_, span| Lit::bool(false, span));
    let r#bool = choice((r#true, r#false));

    let int = text::int::<char, Simple<char>>(10).map_with_span(Lit::num);
    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map_with_span(Lit::num);
    let num = choice((real, int));

    let comment = just("//")
        .then(take_until(just('\n')))
        .padded()
        .labelled("comment");

    let ident = text::ident()
        .map_with_span(|name, span| Ident { name, span })
        .padded()
        .padded_by(comment.repeated());

    let parser = recursive(|pat| {
        let wildcard_pat =
            just_with_padding("_").map_with_span(|_, span| Pattern::Wildcard(WildcardPat { span }));

        let is_pat = ident
            .then_ignore(just_with_padding("is"))
            .then(ident)
            .map_with_span(|(id, is_id), _: Span| {
                let start = id.span.start;
                let end = is_id.span.end;
                Pattern::Is(IsPat { span: start..end, id, is_id })
            });

        let lit_pat = choice((r#bool, num, string_parser()))
            .map_with_span(|lit, span| Pattern::Lit(LitPat { span, lit }));

        let ident_pat = ident
            .then(
                just_with_padding(":")
                    .ignore_then(type_ann.clone())
                    .or_not(),
            )
            .map_with_span(|(id, type_ann), _: Span| {
                Pattern::Ident(BindingIdent { span: id.span.clone(), id, type_ann })
            })
            .padded()
            .labelled("ident_pat");

        // NOTE: rest patterns are only valid in certain locations, for instance
        // let ...foo = bar is not valid, but bar(...foo) is valid.
        let rest_pat = just("...")
            .ignore_then(pat.clone())
            .map_with_span(|arg, span| RestPat {
                span,
                arg: Box::from(arg),
                type_ann: None,
            });

        let array_pat = pat
            .clone()
            .separated_by(just_with_padding(","))
            .delimited_by(just_with_padding("["), just_with_padding("]"))
            .then(
                just_with_padding(":")
                    .ignore_then(type_ann.clone())
                    .or_not(),
            )
            .map_with_span(|(elems, type_ann), span| {
                Pattern::Array(ArrayPat {
                    span,
                    // The reason why each elem is wrapped in Some() is that
                    // it's possible to have a pattern with skipped elements
                    // althought the parser doesn't support this yet.
                    elems: elems.iter().cloned().map(Some).collect(),
                    optional: false,
                    type_ann,
                })
            });

        let key_value_pat_prop = text::ident()
            .map_with_span(|name, span| Ident { span, name })
            .then_ignore(just_with_padding(":"))
            .then(pat.clone())
            .map(|(key, value)| {
                ObjectPatProp::KeyValue(KeyValuePatProp {
                    key,
                    value: Box::from(value),
                })
            });

        // TODO: support default values
        let assign_pat_prop = text::ident()
            .map_with_span(|name, span| Ident { span, name })
            .map_with_span(|key, span| {
                ObjectPatProp::Assign(AssignPatProp {
                    span,
                    key,
                    value: None,
                })
            });

        let obj_pat_prop = choice((
            rest_pat.clone().map(ObjectPatProp::Rest),
            key_value_pat_prop,
            assign_pat_prop,
        ))
        .separated_by(just_with_padding(","))
        .delimited_by(just_with_padding("{"), just_with_padding("}"))
        .then(
            just_with_padding(":")
                .ignore_then(type_ann.clone())
                .or_not(),
        )
        .map_with_span(|(props, type_ann), span| -> Pattern {
            let rest_count = props
                .iter()
                .filter(|p| matches!(p, ObjectPatProp::Rest(_)))
                .count();

            match rest_count {
                0 => Pattern::Object(ObjectPat {
                    span,
                    props,
                    optional: false,
                    type_ann,
                }),
                1 => match props.last() {
                    Some(last) => match last {
                        ObjectPatProp::Rest(_) => Pattern::Object(ObjectPat {
                            span,
                            props,
                            optional: false,
                            type_ann,
                        }),
                        _ => {
                            panic!("Rest should come last in object pattern")
                        }
                    },
                    None => panic!("This should never happen"),
                },
                _ => panic!("Only one rest is allowed in an object pattern"),
            }
        });

        top_level = false;

        choice((
            wildcard_pat,
            is_pat,
            lit_pat, // TODO: restrict lit_pat from being parsed at the top-level
            ident_pat,
            array_pat,
            obj_pat_prop,
            rest_pat.map(Pattern::Rest),
        )).labelled("inner_pattern")
    })
    .labelled("pattern");

    parser.boxed()
}
