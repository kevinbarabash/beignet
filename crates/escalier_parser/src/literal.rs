use itertools::free::join;
use unescape::unescape;

use escalier_ast::values::*;

use crate::parse_error::ParseError;
use crate::util::*;

pub fn parse_literal(node: &tree_sitter::Node, src: &str) -> Result<Lit, ParseError> {
    match node.kind() {
        "number" => Ok(Lit::num(
            src.get(node.byte_range()).unwrap().to_owned(),
            node.byte_range(),
            SourceLocation::from(node),
        )),
        "string" => {
            let mut cursor = node.walk();
            let raw = join(
                node.named_children(&mut cursor)
                    .into_iter()
                    .map(|fragment_or_escape| text_for_node(&fragment_or_escape, src))
                    .collect::<Result<Vec<_>, ParseError>>()?,
                "",
            );

            let cooked = unescape(&raw).unwrap();
            Ok(Lit::str(
                cooked,
                node.byte_range(),
                SourceLocation::from(node),
            ))
        }
        "true" => Ok(Lit::bool(
            true,
            node.byte_range(),
            SourceLocation::from(node),
        )),
        "false" => Ok(Lit::bool(
            false,
            node.byte_range(),
            SourceLocation::from(node),
        )),
        "null" => todo!(),
        "undefined" => todo!(),
        kind => panic!("Unexpected literal kind: '{kind}'"),
    }
}
