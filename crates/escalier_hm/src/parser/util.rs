use crate::ast::*;

use crate::parser::parse_error::ParseError;
use crate::parser::pattern::parse_pattern;
use crate::parser::type_ann::parse_type_ann;

pub fn text_for_node(node: &tree_sitter::Node, src: &str) -> Result<String, ParseError> {
    match src.get(node.byte_range()) {
        Some(text) => Ok(text.to_owned()),
        None => Err(ParseError::from("error in text_for_node")),
    }
}

pub fn parse_type_params_for_node(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<Option<Vec<TypeParam>>, ParseError> {
    let type_params = match node.child_by_field_name("type_parameters") {
        Some(type_params) => {
            let mut cursor = type_params.walk();
            let type_params = type_params
                .named_children(&mut cursor)
                .into_iter()
                .map(|type_param| {
                    let name_node = type_param.child_by_field_name("name").unwrap();
                    let name = Ident {
                        loc: SourceLocation::from(&name_node),
                        span: name_node.byte_range(),
                        name: text_for_node(&name_node, src)?,
                    };

                    let constraint =
                        if let Some(constraint) = type_param.child_by_field_name("constraint") {
                            Some(Box::from(parse_type_ann(&constraint, src)?))
                        } else {
                            None
                        };

                    let default = if let Some(value) = type_param.child_by_field_name("value") {
                        Some(Box::from(parse_type_ann(&value, src)?))
                    } else {
                        None
                    };

                    Ok(TypeParam {
                        span: type_param.byte_range(),
                        name,
                        constraint,
                        default,
                    })
                })
                .collect::<Result<Vec<_>, ParseError>>()?;
            Some(type_params)
        }
        None => None,
    };

    Ok(type_params)
}

pub fn parse_formal_parameters(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<Vec<EFnParam>, ParseError> {
    assert_eq!(node.kind(), "formal_parameters");

    let mut cursor = node.walk();
    node.named_children(&mut cursor)
        .into_iter()
        .map(|param| {
            let optional = match param.kind() {
                "required_parameter" => false,
                "optional_parameter" => true,
                kind => panic!("Unexpected param kind: {kind}"),
            };
            let pattern = param.child_by_field_name("pattern").unwrap();
            let type_ann = if let Some(type_ann) = param.child_by_field_name("type") {
                Some(parse_type_ann(&type_ann, src)?)
            } else {
                None
            };

            Ok(EFnParam {
                pat: parse_pattern(&pattern, src)?,
                type_ann,
                optional,
            })
        })
        .collect::<Result<Vec<_>, ParseError>>()
}
