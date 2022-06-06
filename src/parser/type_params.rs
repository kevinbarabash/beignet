use chumsky::prelude::*;

use crate::ast::*;
use crate::parser::util::just_with_padding;

pub fn type_params(
    type_ann: BoxedParser<'static, char, TypeAnn, Simple<char>>,
) -> BoxedParser<'static, char, Vec<TypeParam>, Simple<char>> {

    let ident = text::ident().map_with_span(|name, span| Ident { span, name });

    let type_param = ident
       .then(
           just_with_padding("extends")
               .ignore_then(type_ann.clone())
               .or_not(),
       )
       .then(
           just_with_padding("=")
               .ignore_then(type_ann.clone())
               .or_not(),
       )
       .map_with_span(|((name, constraint), default), span| TypeParam {
           span,
           name,
           constraint: constraint.map(Box::from),
           default: default.map(Box::from),
       });

   // TODO: dedupe with decl.rs
   let type_params = type_param
       .separated_by(just_with_padding(","))
       .allow_trailing()
       .delimited_by(just_with_padding("<"), just_with_padding(">"));

    type_params.boxed()
}
