use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::types::*;
use crate::parser::util::just_with_padding;

// NOTE: Destructuring assignments admits different patterns from destructuring
// function params.  We'll need to have different parsers for those.
pub fn pattern_parser() -> BoxedParser<'static, char, Pattern, Simple<char>> {
    let type_ann = type_parser();
    let mut top_level = true;

    let parser = recursive(|pat| {
        let ident_pat = text::ident()
            .map_with_span(|name, span| Ident { name, span })
            .then(just_with_padding(":").ignore_then(type_ann).or_not())
            .map_with_span(|(id, type_ann), span: Span| {
                Pattern::Ident(BindingIdent { span, id, type_ann })
            })
            .padded();

        // NOTE: rest patterns must appear instead of other patterns
        // It's an error for them appear at the top-level, e.g.
        // let ...x = [1,2,3];
        // Also, type annotation can only appear at the top-level as well.
        let rest_pat = just("...")
            .ignore_then(pat.clone())
            .map_with_span(|arg, span| RestPat {
                span,
                arg: Box::from(arg),
                type_ann: None,
            });

        let array_pat = pat.clone()
            .separated_by(just_with_padding(","))
            .delimited_by(just_with_padding("["), just_with_padding("]"))
            .map_with_span(|elems, span| {
                Pattern::Array(ArrayPat {
                    span,
                    // The reason why each elem is wrapped in Some() is that
                    // it's possible to have a pattern with skipped elements
                    // althought the parser doesn't support this yet.
                    elems: elems.iter().cloned().map(Some).collect(),
                    optional: false,
                    type_ann: None,
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

        // NOTE: There can only be a single rest element and it must be last
        let obj_pat_prop = choice((
            rest_pat.clone().map(ObjectPatProp::Rest),
            key_value_pat_prop,
            assign_pat_prop,
        ))
        .separated_by(just_with_padding(","))
        .delimited_by(just_with_padding("{"), just_with_padding("}"))
        .map_with_span(|props, span| {
            Pattern::Object(ObjectPat {
                span,
                props,
                optional: false,
                type_ann: None,
            })
        });

        top_level = false;

        choice((ident_pat, array_pat, obj_pat_prop, rest_pat.map(Pattern::Rest)))
    });

    parser.boxed()
}
