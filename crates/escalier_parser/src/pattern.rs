use escalier_ast::types::Type;
use escalier_ast::values::*;

use crate::expr::parse_expression;
use crate::literal::parse_literal;
use crate::parse_error::ParseError;
use crate::util::*;

pub fn parse_pattern(node: &tree_sitter::Node, src: &str) -> Result<Pattern<Type>, ParseError> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(ParseError::from("Error parsing pattern"));
    }
    let kind = match node.kind() {
        "binding_identifier" => {
            let name = node.child_by_field_name("name").unwrap();
            let name = src.get(name.byte_range()).unwrap().to_owned();
            let mutable = node.child_by_field_name("mut").is_some();
            PatternKind::Ident(BindingIdent {
                loc: SourceLocation::from(node),
                span: node.byte_range(),
                name,
                mutable,
            })
        }
        "object_pattern" => {
            let mut cursor = node.walk();
            let props = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|child| match child.kind() {
                    "pair_pattern" => {
                        let key_node = child.child_by_field_name("key").unwrap();
                        let key = Ident {
                            loc: SourceLocation::from(&key_node),
                            span: key_node.byte_range(),
                            name: text_for_node(&key_node, src)?,
                        };

                        let value = Box::from(parse_pattern(
                            &child.child_by_field_name("value").unwrap(),
                            src,
                        )?);

                        // TODO: include `span` in this node
                        Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                            loc: SourceLocation::from(&child),
                            span: child.byte_range(),
                            key,
                            value,
                            init: None,
                        }))
                    }
                    "rest_pattern" => {
                        let pattern = child.named_child(0).unwrap();
                        let pattern = parse_pattern(&pattern, src)?;

                        Ok(ObjectPatProp::Rest(RestPat {
                            arg: Box::from(pattern),
                        }))
                    }
                    "object_assignment_pattern" => {
                        let left = child.child_by_field_name("left").unwrap();
                        let right = child.child_by_field_name("right").unwrap();
                        let init = parse_expression(&right, src)?;
                        match left.kind() {
                            "shorthand_property_identifier_pattern" => {
                                let name_node = left.child_by_field_name("name").unwrap();
                                let name = src.get(name_node.byte_range()).unwrap().to_owned();
                                let mutable = left.child_by_field_name("mut").is_some();

                                Ok(ObjectPatProp::Shorthand(ShorthandPatProp {
                                    loc: SourceLocation::from(&left),
                                    span: left.byte_range(),
                                    ident: BindingIdent {
                                        loc: SourceLocation::from(&name_node),
                                        span: name_node.byte_range(),
                                        name,
                                        mutable,
                                    },
                                    init: Some(Box::from(init)),
                                }))
                            },
                            "pair_pattern" => {
                                let key_node = left.child_by_field_name("key").unwrap();
                                let key = Ident {
                                    loc: SourceLocation::from(&key_node),
                                    span: key_node.byte_range(),
                                    name: text_for_node(&key_node, src)?,
                                };

                                let value = Box::from(parse_pattern(
                                    &left.child_by_field_name("value").unwrap(),
                                    src,
                                )?);

                                // TODO: include `span` in this node
                                Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                                    loc: SourceLocation::from(&left),
                                    span: left.byte_range(),
                                    key,
                                    value,
                                    init: Some(Box::from(init)),
                                }))
                            },
                            kind => panic!("unexpected .right property on object_assignment_pattern of type {kind}"),
                        }
                    }
                    "shorthand_property_identifier_pattern" => {
                        let name_node = child.child_by_field_name("name").unwrap();
                        let name = src.get(name_node.byte_range()).unwrap().to_owned();
                        let mutable = child.child_by_field_name("mut").is_some();
                        Ok(ObjectPatProp::Shorthand(ShorthandPatProp {
                            loc: SourceLocation::from(&child),
                            span: child.byte_range(),
                            ident: BindingIdent {
                                loc: SourceLocation::from(&name_node),
                                span: name_node.byte_range(),
                                name,
                                mutable,
                            },
                            init: None,
                        }))
                    }
                    kind => panic!("Unexpected object property kind: '{kind}'"),
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            PatternKind::Object(ObjectPat {
                props,
                optional: false,
            })
        }
        "array_pattern" => {
            let mut cursor = node.walk();
            // TODO: handle sparse array patterns
            // NOTE: named_children() does not include gaps in the array
            let elems = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|child| match child.kind() {
                    "assignment_pattern" => {
                        let left = child.child_by_field_name("left").ok_or_else(|| {
                            ParseError::from("'left' field not found on assignment_pattern")
                        })?;
                        let right = child.child_by_field_name("right").ok_or_else(|| {
                            ParseError::from("'right' field not found on assignment_pattern")
                        })?;

                        Ok(Some(ArrayPatElem {
                            pattern: parse_pattern(&left, src)?,
                            init: Some(Box::from(parse_expression(&right, src)?)),
                        }))
                    }
                    _ => Ok(Some(ArrayPatElem {
                        pattern: parse_pattern(&child, src)?,
                        init: None,
                    })),
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            PatternKind::Array(ArrayPat {
                elems,
                optional: false,
            })
        }
        "rest_pattern" => {
            let arg = node.named_child(0).unwrap();
            let arg = parse_pattern(&arg, src)?;

            PatternKind::Rest(RestPat {
                arg: Box::from(arg),
            })
        }
        "assignment_pattern" => {
            todo!("handle assignment_pattern");
        }
        "wildcard" => PatternKind::Wildcard,
        name => panic!("unrecognized pattern: {name} @ {node:#?}"),
    };

    Ok(Pattern {
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })
}

pub fn parse_refutable_pattern(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<Pattern<Type>, ParseError> {
    let child = if node.kind() == "refutable_pattern" {
        node.named_child(0).unwrap()
    } else {
        node.to_owned()
    };

    let kind = match child.kind() {
        "number" | "string" | "true" | "false" | "null" | "undefined" => {
            let lit = parse_literal(&child, src)?;
            PatternKind::Lit(LitPat { lit })
        }
        "binding_identifier" => {
            let name = text_for_node(&child, src)?;
            match name.as_str() {
                "undefined" => {
                    let lit = parse_literal(&child, src)?;
                    PatternKind::Lit(LitPat { lit })
                }
                _ => PatternKind::Ident(BindingIdent {
                    loc: SourceLocation::from(&child),
                    span: child.byte_range(),
                    name,
                    mutable: false,
                }),
            }
        }
        "refutable_array_pattern" => {
            let mut cursor = child.walk();
            let elems = child
                .named_children(&mut cursor)
                .into_iter()
                // TODO: make elems in ArrayPat non-optional
                .map(|elem| {
                    Ok(Some(ArrayPatElem {
                        pattern: parse_refutable_pattern(&elem, src)?,
                        init: None,
                    }))
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            PatternKind::Array(ArrayPat {
                elems,
                optional: false,
            })
        }
        "refutable_object_pattern" => {
            let mut cursor = child.walk();
            let props = child
                .named_children(&mut cursor)
                .into_iter()
                .map(|prop| match prop.kind() {
                    "refutable_pair_pattern" => {
                        let key_node = prop.child_by_field_name("key").unwrap();
                        let key = text_for_node(&key_node, src)?;
                        let value = prop.child_by_field_name("value").unwrap();
                        let value = parse_refutable_pattern(&value, src)?;

                        Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                            loc: SourceLocation::from(&prop),
                            span: prop.byte_range(),
                            key: Ident {
                                loc: SourceLocation::from(&key_node),
                                span: key_node.byte_range(),
                                name: key,
                            },
                            value: Box::from(value),
                            init: None,
                        }))
                    }
                    "refutable_rest_pattern" => {
                        let arg = prop.named_child(0).unwrap();
                        let arg = parse_refutable_pattern(&arg, src)?;

                        Ok(ObjectPatProp::Rest(RestPat {
                            arg: Box::from(arg),
                        }))
                    }
                    "shorthand_property_identifier_pattern" => {
                        let mutable = prop.child_by_field_name("mut").is_some();
                        Ok(ObjectPatProp::Shorthand(ShorthandPatProp {
                            loc: SourceLocation::from(&prop),
                            span: prop.byte_range(),
                            ident: BindingIdent {
                                loc: SourceLocation::from(&prop),
                                span: prop.byte_range(),
                                name: text_for_node(&prop, src)?,
                                mutable,
                            },
                            init: None,
                        }))
                    }
                    kind => panic!("Unexected prop.kind() = {kind}"),
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            PatternKind::Object(ObjectPat {
                props,
                optional: false,
            })
        }
        "refutable_rest_pattern" => {
            let arg = child.named_child(0).unwrap();
            let arg = parse_refutable_pattern(&arg, src)?;

            PatternKind::Rest(RestPat {
                arg: Box::from(arg),
            })
        }
        "refutable_is_pattern" => {
            let left = child.named_child(0).unwrap();
            let right = child.named_child(1).unwrap();

            PatternKind::Is(IsPat {
                ident: BindingIdent {
                    loc: SourceLocation::from(&left),
                    span: left.byte_range(),
                    name: text_for_node(&left, src)?,
                    mutable: false,
                },
                is_id: Ident {
                    loc: SourceLocation::from(&right),
                    span: right.byte_range(),
                    name: text_for_node(&right, src)?,
                },
            })
        }
        "wildcard" => PatternKind::Wildcard,
        kind => todo!("Unhandled refutable pattern of kind '{kind}'"),
    };

    Ok(Pattern {
        loc: SourceLocation::from(&child),
        span: child.byte_range(),
        kind,
        inferred_type: None,
    })
}
