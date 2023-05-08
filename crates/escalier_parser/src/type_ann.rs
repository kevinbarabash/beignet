use escalier_ast::values::*;

use crate::expr::parse_expression;
use crate::literal::parse_literal;
use crate::parse_error::ParseError;
use crate::pattern::parse_pattern;
use crate::util::*;

pub fn parse_type_ann(node: &tree_sitter::Node, src: &str) -> Result<TypeAnn, ParseError> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(ParseError::from("Error parsing type annotation"));
    }

    let node = if node.kind() == "type_annotation"
        || node.kind() == "constraint"
        // TODO: write some tests that verify default types work, e.g.
        // type Foo<T extends U = V> = { ... }
        || node.kind() == "default_type"
    {
        node.named_child(0).unwrap()
    } else {
        node.to_owned()
    };

    let kind = match node.kind() {
        // Primary types
        "parenthesized_type" => {
            let wrapped_type = node.named_child(0).unwrap();
            return parse_type_ann(&wrapped_type, src);
        }
        "predefined_type" => match text_for_node(&node, src)?.as_str() {
            "any" => todo!("remove support for 'any'"),
            "number" => TypeAnnKind::Keyword(KeywordType {
                keyword: Keyword::Number,
            }),
            "boolean" => TypeAnnKind::Keyword(KeywordType {
                keyword: Keyword::Boolean,
            }),
            "string" => TypeAnnKind::Keyword(KeywordType {
                keyword: Keyword::String,
            }),
            "symbol" => todo!(),
            "void" => todo!(),
            "unknown" => todo!(),
            "never" => TypeAnnKind::Keyword(KeywordType {
                keyword: Keyword::Never,
            }),
            "object" => todo!("remove support for 'object'"),
            name => panic!("Unkwnown predefined_type: '{name}'"),
        },
        "type_identifier" => TypeAnnKind::TypeRef(TypeRef {
            name: text_for_node(&node, src)?,
            type_args: None,
        }),
        "nested_type_identifier" => todo!(),
        "generic_type" => {
            let name = node.child_by_field_name("name").unwrap();
            let type_arguments = node.child_by_field_name("type_arguments").unwrap();
            let mut cursor = type_arguments.walk();
            let type_params = type_arguments
                .named_children(&mut cursor)
                .map(|arg| parse_type_ann(&arg, src))
                .collect::<Result<Vec<_>, ParseError>>()?;

            TypeAnnKind::TypeRef(TypeRef {
                name: text_for_node(&name, src)?,
                type_args: Some(type_params),
            })
        }
        "object_type" => {
            let mut cursor = node.walk();
            let elem_count = node.named_child_count();

            if elem_count == 1 {
                let child = node.named_child(0).unwrap();
                if child.kind() == "index_signature" {
                    let type_ann = child.child_by_field_name("type").unwrap();
                    let type_ann = parse_type_ann(&type_ann, src)?;

                    if let Some(mapped_type_clause) =
                        child.child_by_field_name("mapped_type_clause")
                    {
                        let mutable = if let Some(mut_node) = child.child_by_field_name("mut") {
                            if let Some(sign) = child.child_by_field_name("mut_sign") {
                                match sign.kind() {
                                    "+" => Some(TMappedTypeChange {
                                        span: mut_node.byte_range(),
                                        change: TMappedTypeChangeProp::Plus,
                                    }),
                                    "-" => Some(TMappedTypeChange {
                                        span: mut_node.byte_range(),
                                        change: TMappedTypeChangeProp::Minus,
                                    }),
                                    _ => todo!(),
                                }
                            } else {
                                Some(TMappedTypeChange {
                                    span: mut_node.byte_range(),
                                    change: TMappedTypeChangeProp::Plus,
                                })
                            }
                        } else {
                            None
                        };

                        let optional = if let Some(mut_node) = child.child_by_field_name("opt") {
                            if let Some(sign) = child.child_by_field_name("opt_sign") {
                                match sign.kind() {
                                    "+" => Some(TMappedTypeChange {
                                        span: mut_node.byte_range(),
                                        change: TMappedTypeChangeProp::Plus,
                                    }),
                                    "-" => Some(TMappedTypeChange {
                                        span: mut_node.byte_range(),
                                        change: TMappedTypeChangeProp::Minus,
                                    }),
                                    _ => todo!(),
                                }
                            } else {
                                Some(TMappedTypeChange {
                                    span: mut_node.byte_range(),
                                    change: TMappedTypeChangeProp::Plus,
                                })
                            }
                        } else {
                            None
                        };

                        let name_node = mapped_type_clause.child_by_field_name("name").unwrap();
                        let name = text_for_node(&name_node, src)?;

                        let constraint = mapped_type_clause.child_by_field_name("type").unwrap();
                        let constraint = parse_type_ann(&constraint, src)?;

                        // TODO: handle optional 'as AliasT' after "type"

                        let kind = TypeAnnKind::Mapped(MappedType {
                            type_param: TypeParam {
                                span: mapped_type_clause.byte_range(),
                                name: Ident {
                                    loc: SourceLocation::from(&name_node),
                                    span: name_node.byte_range(),
                                    name,
                                },
                                constraint: Some(Box::from(constraint)),
                                default: None, // `default` is always None for MappedType
                            },
                            optional,
                            mutable,
                            type_ann: Box::from(type_ann),
                        });

                        return Ok(TypeAnn {
                            loc: SourceLocation::from(&node),
                            span: node.byte_range(),
                            kind,
                            inferred_type: None,
                        });
                    }
                }
            }

            let elems: Vec<TObjElem> = node
                .named_children(&mut cursor)
                .map(|prop| {
                    match prop.kind() {
                        "export_statement" => todo!("remove export_statement from object_type"),
                        "property_signature" => {
                            // NOTE: _property_name is defined as:
                            // choice(
                            //   alias(
                            //     choice($.identifier, $._reserved_identifier),
                            //     $.property_identifier
                            //   ),
                            //   $.private_property_identifier,
                            //   $.string,
                            //   $.number,
                            //   $.computed_property_name
                            // ),
                            // TODO: handle more than just "identifier"
                            let name_node = prop.child_by_field_name("name").unwrap();
                            let name = text_for_node(&name_node, src)?;

                            let type_ann = prop.child_by_field_name("type").unwrap();
                            let type_ann = parse_type_ann(&type_ann, src)?;

                            let mut optional = false;
                            let mut mutable = false;
                            let mut cursor = prop.walk();
                            for child in prop.children(&mut cursor) {
                                if text_for_node(&child, src)? == "?" {
                                    optional = true;
                                }
                                if text_for_node(&child, src)? == "mut" {
                                    mutable = true;
                                }
                            }

                            let elem = TObjElem::Prop(TProp {
                                loc: SourceLocation::from(&prop),
                                span: prop.byte_range(),
                                name,
                                optional,
                                mutable,
                                type_ann: Box::from(type_ann),
                            });
                            Ok(elem)
                        }
                        "call_signature" => todo!("call_signature"),
                        "construct_signature" => todo!("construct_signature"),
                        "index_signature" => {
                            // NOTE: `name` is optional an will not be present
                            // for mapped types.
                            // NOTE: `mapped_type_clause` should've already been
                            // handle so if we get here and there's a `mapped_type_clause`
                            // we should report that as an error.
                            let name_node = prop.child_by_field_name("name").unwrap();
                            let name = text_for_node(&name_node, src)?;

                            let index_type_ann = prop.child_by_field_name("index_type").unwrap();
                            let index_type_ann = parse_type_ann(&index_type_ann, src)?;

                            let type_ann = prop.child_by_field_name("type").unwrap();
                            let type_ann = parse_type_ann(&type_ann, src)?;

                            let mut optional = false;
                            let mut mutable = false;
                            let mut cursor = prop.walk();
                            for child in prop.children(&mut cursor) {
                                if text_for_node(&child, src)? == "?" {
                                    optional = true;
                                }
                                if text_for_node(&child, src)? == "mut" {
                                    mutable = true;
                                }
                            }

                            let pat: Pattern = Pattern {
                                loc: SourceLocation::from(&name_node),
                                span: name_node.byte_range(),
                                kind: PatternKind::Ident(BindingIdent {
                                    loc: SourceLocation::from(&name_node),
                                    span: name_node.byte_range(),
                                    name,
                                    mutable: false,
                                }),
                                inferred_type: None,
                            };

                            let elem = TObjElem::Index(TIndex {
                                loc: SourceLocation::from(&prop),
                                span: prop.byte_range(),
                                key: Box::from(TypeAnnFnParam {
                                    pat,
                                    type_ann: index_type_ann,
                                    optional,
                                }),
                                mutable,
                                type_ann: Box::from(type_ann),
                            });
                            Ok(elem)
                        }
                        // TODO: remove method_signature, methods should look the same as properties
                        // The reason why JavaScript has both is that methods on object literals can
                        // access the other properties on the object using this.
                        "method_signature" => todo!("remove method_signature from object_type"),
                        kind => panic!("Unsupport prop kind in object_type: '{kind}'"),
                    }
                })
                .collect::<Result<Vec<TObjElem>, ParseError>>()?;

            TypeAnnKind::Object(ObjectType { elems })
        }
        "array_type" => {
            let elem_type = node.named_child(0).unwrap();
            let elem_type = parse_type_ann(&elem_type, src)?;

            TypeAnnKind::Array(ArrayType {
                elem_type: Box::from(elem_type),
            })
        }
        "tuple_type" => {
            let mut cursor = node.walk();
            let types = node
                .named_children(&mut cursor)
                .map(|elem| {
                    eprintln!("parsing elem: {elem:#?}");
                    parse_type_ann(&elem, src)
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            TypeAnnKind::Tuple(TupleType { types })
        }
        "flow_maybe_type" => todo!(),
        "type_query" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src)?;

            TypeAnnKind::Query(QueryType {
                expr: Box::from(expr),
            })
        }
        "index_type_query" => {
            let type_ann = node.named_child(0).unwrap();
            let type_ann = parse_type_ann(&type_ann, src)?;

            TypeAnnKind::KeyOf(KeyOfType {
                type_ann: Box::from(type_ann),
            })
        }
        // alias($.this, $.this_type),
        "existential_type" => todo!(),
        "literal_type" => {
            let child = node.named_child(0).unwrap();
            match child.kind() {
                "undefined" => TypeAnnKind::Keyword(KeywordType {
                    keyword: Keyword::Undefined,
                }),
                "null" => TypeAnnKind::Keyword(KeywordType {
                    keyword: Keyword::Null,
                }),
                _ => {
                    let lit = parse_literal(&child, src)?;
                    TypeAnnKind::Lit(lit)
                }
            }
        }
        "lookup_type" => {
            let obj_type = node.named_child(0).unwrap();
            let obj_type = parse_type_ann(&obj_type, src)?;
            let index_type = node.named_child(1).unwrap();
            let index_type = parse_type_ann(&index_type, src)?;
            TypeAnnKind::IndexedAccess(IndexedAccessType {
                obj_type: Box::from(obj_type),
                index_type: Box::from(index_type),
            })
        }
        "conditional_type" => {
            // TODO: Track whether we're inside a conditional type or not.  To
            // do this we'll need to use a counter and increase it every time
            // we enter a conditional type and decrement it each time we leave
            // one.
            // TODO: Attach all of these functions to a struct so that we can
            // have shared data.
            let left = node.child_by_field_name("left").unwrap();
            let left = parse_type_ann(&left, src)?;
            let right = node.child_by_field_name("right").unwrap();
            let right = parse_type_ann(&right, src)?;
            let consequent = node.child_by_field_name("consequence").unwrap();
            let consequent = parse_type_ann(&consequent, src)?;
            let alternate = node.child_by_field_name("alternative").unwrap();
            let alternate = parse_type_ann(&alternate, src)?;

            TypeAnnKind::Conditional(ConditionalType {
                check_type: Box::from(left),
                extends_type: Box::from(right),
                true_type: Box::from(consequent),
                false_type: Box::from(alternate),
            })
        }
        "infer_type" => {
            let name_node = node.named_child(0).unwrap();

            TypeAnnKind::Infer(InferType {
                name: text_for_node(&name_node, src)?,
            })
        }
        "template_literal_type" => todo!(),
        "intersection_type" => {
            let mut cursor = node.walk();
            let mut types: Vec<TypeAnn> = vec![];

            for t in node.named_children(&mut cursor) {
                let t = parse_type_ann(&t, src)?;
                match &t.kind {
                    TypeAnnKind::Intersection(intersection) => {
                        types.extend(intersection.types.to_owned());
                    }
                    _ => {
                        types.push(t);
                    }
                }
            }

            TypeAnnKind::Intersection(IntersectionType { types })
        }
        "union_type" => {
            let mut cursor = node.walk();
            let mut types: Vec<TypeAnn> = vec![];

            for t in node.named_children(&mut cursor) {
                let t = parse_type_ann(&t, src)?;
                match &t.kind {
                    TypeAnnKind::Union(union) => {
                        types.extend(union.types.to_owned());
                    }
                    _ => {
                        types.push(t);
                    }
                }
            }

            TypeAnnKind::Union(UnionType { types })
        }

        // Non-primary types
        "function_type" => {
            let params = node.child_by_field_name("parameters").unwrap();
            let mut cursor = params.walk();
            let params = params
                .named_children(&mut cursor)
                .map(|param| {
                    let optional = match param.kind() {
                        "required_parameter" => false,
                        "optional_parameter" => true,
                        kind => panic!("Unexpected param kind: {kind}"),
                    };
                    let pattern = param.child_by_field_name("pattern").unwrap();
                    let type_ann = param.child_by_field_name("type").unwrap();
                    let type_ann = parse_type_ann(&type_ann, src)?;

                    Ok(TypeAnnFnParam {
                        pat: parse_pattern(&pattern, src)?,
                        type_ann,
                        optional,
                    })
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            let return_type = node.child_by_field_name("return_type").unwrap();
            let return_type = parse_type_ann(&return_type, src)?;

            let type_params = parse_type_params_for_node(&node, src)?;

            TypeAnnKind::Lam(LamType {
                params,
                ret: Box::from(return_type),
                type_params,
            })
        }
        "mutable_type" => {
            let type_ann = node.named_child(0).unwrap();

            TypeAnnKind::Mutable(MutableType {
                type_ann: Box::from(parse_type_ann(&type_ann, src)?),
            })
        }
        "constructor_type" => todo!(),

        kind => panic!("Unexpected type_annotation kind: '{kind}'"),
    };

    Ok(TypeAnn {
        loc: SourceLocation::from(&node),
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })

    // _primary_type: ($) =>
    // choice(
    //   $.parenthesized_type,
    //   $.predefined_type,
    //   $._type_identifier,
    //   $.nested_type_identifier,
    //   $.generic_type,
    //   $.object_type,
    //   $.array_type,
    //   $.tuple_type,
    //   $.flow_maybe_type,
    //   $.type_query,
    //   $.index_type_query,
    //   alias($.this, $.this_type),
    //   $.existential_type,
    //   $.literal_type,
    //   $.lookup_type,
    //   $.conditional_type,
    //   $.template_literal_type,
    //   $.intersection_type,
    //   $.union_type
    // ),

    // $._primary_type,
    // $.function_type,
    // $.readonly_type,
    // $.constructor_type,
    // $.infer_type
}
