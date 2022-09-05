use itertools::free::join;
use unescape::unescape;

use crochet_ast::*;

pub fn parse(src: &str) -> Result<Program, String> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_crochet::language())
        .expect("Error loading crochet language");

    let tree = parser.parse(src, None).unwrap();

    let root = tree.root_node();

    let kind = root.kind();
    if kind == "program" {
        let mut cursor = root.walk();

        let children = root.children(&mut cursor);

        let mut body: Vec<Statement> = vec![];
        for child in children {
            let mut stmts = parse_statement(&child, src);
            body.append(&mut stmts);
        }

        Ok(Program { body })
    } else {
        Err("not implemented yet".to_string())
    }
}

fn parse_statement(node: &tree_sitter::Node, src: &str) -> Vec<Statement> {
    match node.kind() {
        "lexical_declaration" => parse_declaration(node, false, src),
        "expression_statement" => {
            let mut cursor = node.walk();
            let expressions = node.children(&mut cursor);
            expressions
                .into_iter()
                .map(|expr| {
                    let expr = parse_expression(&expr, src);
                    Statement::Expr {
                        span: node.byte_range(),
                        expr,
                    }
                })
                .collect()
        }
        "ambient_declaration" => {
            let decl = node.named_child(0).unwrap();
            parse_declaration(&decl, true, src)
        }
        "type_alias_declaration" => {
            let name = node.child_by_field_name("name").unwrap();
            let id = Ident {
                span: name.byte_range(),
                name: text_for_node(&name, src),
            };
            let type_ann = node.child_by_field_name("value").unwrap();
            let type_ann = parse_type_ann(&type_ann, src);

            let type_params = match node.child_by_field_name("type_parameters") {
                Some(type_params) => {
                    let mut cursor = type_params.walk();
                    let type_params = type_params
                        .named_children(&mut cursor)
                        .into_iter()
                        .map(|type_param| {
                            let name = type_param.child_by_field_name("name").unwrap();
                            let name = Ident {
                                span: name.byte_range(),
                                name: text_for_node(&name, src),
                            };

                            let constraint = type_param
                                .child_by_field_name("constraint")
                                .map(|constraint| Box::from(parse_type_ann(&constraint, src)));

                            let default = type_param
                                .child_by_field_name("value")
                                .map(|value| Box::from(parse_type_ann(&value, src)));

                            TypeParam {
                                span: type_param.byte_range(),
                                name,
                                constraint,
                                default,
                            }
                        })
                        .collect();
                    Some(type_params)
                }
                None => None,
            };

            vec![Statement::TypeDecl {
                span: node.byte_range(),
                declare: false,
                id,
                type_ann,
                type_params,
            }]
        }
        _ => todo!("unhandled: {:#?}", node),
    }

    // $.export_statement,
    // $.import_statement,
    // $.debugger_statement,
    // $.expression_statement,
    // $.declaration,
    // $.statement_block,

    // $.if_statement,
    // $.switch_statement,
    // $.for_statement,
    // $.for_in_statement,
    // $.while_statement,
    // $.do_statement,
    // $.try_statement,
    // $.with_statement,

    // $.break_statement,
    // $.continue_statement,
    // $.return_statement,
    // $.throw_statement,
    // $.empty_statement,
    // $.labeled_statement
}

// TODO: replace with node.utf8_text()
fn text_for_node(node: &tree_sitter::Node, src: &str) -> String {
    src.get(node.byte_range()).unwrap().to_owned()
}

fn parse_declaration(node: &tree_sitter::Node, declare: bool, src: &str) -> Vec<Statement> {
    let mut cursor = node.child(1).unwrap().walk();
    let declarators = node.named_children(&mut cursor);

    declarators
        .into_iter()
        .map(|decl| parse_declarator(&decl, declare, src))
        .collect()
}

fn parse_declarator(node: &tree_sitter::Node, declare: bool, src: &str) -> Statement {
    let name = node.child_by_field_name("name").unwrap();
    let pattern = parse_pattern(&name, src);

    let init = node
        .child_by_field_name("value")
        .map(|init| parse_expression(&init, src));

    let type_ann = node
        .child_by_field_name("type")
        .map(|type_ann| parse_type_ann(&type_ann, src));

    Statement::VarDecl {
        span: node.byte_range(),
        pattern,
        type_ann,
        init,
        declare,
    }
}

fn parse_pattern(node: &tree_sitter::Node, src: &str) -> Pattern {
    match node.kind() {
        "identifier" => {
            let span = node.byte_range();
            let name = src.get(span.clone()).unwrap().to_owned();
            Pattern::Ident(BindingIdent {
                span: span.clone(),
                id: Ident { span, name },
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
                            span: key_node.byte_range(),
                            name: text_for_node(&key_node, src),
                        };

                        let value = Box::from(parse_pattern(
                            &child.child_by_field_name("value").unwrap(),
                            src,
                        ));

                        // TODO: include `span` in this node
                        ObjectPatProp::KeyValue(KeyValuePatProp { key, value })
                    }
                    "rest_pattern" => {
                        let pattern = child.named_child(0).unwrap();
                        let pattern = parse_pattern(&pattern, src);

                        ObjectPatProp::Rest(RestPat {
                            span: child.byte_range(),
                            arg: Box::from(pattern),
                        })
                    }
                    "object_assignment_pattern" => todo!(),
                    "shorthand_property_identifier_pattern" => {
                        let key = Ident {
                            span: child.byte_range(),
                            name: text_for_node(&child, src),
                        };

                        ObjectPatProp::Assign(AssignPatProp {
                            span: child.byte_range(),
                            key,
                            value: None,
                        })
                    }
                    kind => panic!("Unexpected object property kind: '{kind}'"),
                })
                .collect();

            Pattern::Object(ObjectPat {
                span: node.byte_range(),
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
                    "assignment_pattern" => todo!(),
                    _ => Some(parse_pattern(&child, src)),
                })
                .collect();

            Pattern::Array(ArrayPat {
                span: node.byte_range(),
                elems,
                optional: false,
            })
        }
        "rest_pattern" => {
            let arg = node.named_child(0).unwrap();
            let arg = parse_pattern(&arg, src);

            Pattern::Rest(RestPat {
                span: node.byte_range(),
                arg: Box::from(arg),
            })
        }
        _ => panic!("unrecognized pattern {node:#?}"),
    }
}

fn parse_func_param_pattern(node: &tree_sitter::Node, src: &str) -> EFnParamPat {
    match node.kind() {
        "identifier" => {
            let span = node.byte_range();
            let name = src.get(span.clone()).unwrap().to_owned();
            EFnParamPat::Ident(EFnParamBindingIdent {
                span: span.clone(),
                id: Ident { span, name },
            })
        }
        "object_pattern" => {
            let mut cursor = node.walk();
            let props = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|child| match child.kind() {
                    "pair_pattern" => {
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
                        let key_node = child.child_by_field_name("key").unwrap();
                        let key = Ident {
                            span: key_node.byte_range(),
                            name: text_for_node(&key_node, src),
                        };
                        // value is choice($.pattern, $.assignment_pattern)
                        // TODO: handle assignment_pattern
                        let value = Box::from(parse_func_param_pattern(
                            &child.child_by_field_name("value").unwrap(),
                            src,
                        ));
                        EFnParamObjectPatProp::KeyValue(EFnParamKeyValuePatProp { key, value })
                    }
                    "rest_pattern" => {
                        // TODO: dedup with "rest_pattern" arm below
                        let arg = node.child(1).unwrap(); // child(0) is the '...'
                        EFnParamObjectPatProp::Rest(EFnParamRestPat {
                            span: node.byte_range(),
                            arg: Box::from(parse_func_param_pattern(&arg, src)),
                        })
                    }
                    "object_assign_pattern" => todo!(),
                    "shorthand_property_identifier_pattern" => {
                        // alias of choice($.identifier, $._reserved_identifier),
                        let key = Ident {
                            span: child.byte_range(),
                            name: text_for_node(&child, src),
                        };
                        EFnParamObjectPatProp::Assign(EFnParamAssignPatProp { key, value: None })
                    }
                    kind => panic!("Unexpected object_pattern prop kind: {kind}"),
                })
                .collect();

            EFnParamPat::Object(EFnParamObjectPat {
                span: node.byte_range(),
                props,
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
                    "assignment_pattern" => todo!(),
                    _ => Some(parse_func_param_pattern(&child, src)),
                })
                .collect();

            EFnParamPat::Array(EFnParamArrayPat {
                span: node.byte_range(),
                elems,
            })
        }
        "rest_pattern" => {
            let arg = node.named_child(0).unwrap();
            EFnParamPat::Rest(EFnParamRestPat {
                span: node.byte_range(),
                arg: Box::from(parse_func_param_pattern(&arg, src)),
            })
        }
        _ => panic!("unrecognized pattern {node:#?}"),
    }
}

fn parse_formal_parameters(node: &tree_sitter::Node, src: &str) -> Vec<EFnParam> {
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
            let type_ann = param
                .child_by_field_name("type")
                .map(|type_ann| parse_type_ann(&type_ann, src));

            EFnParam {
                pat: parse_func_param_pattern(&pattern, src),
                type_ann,
                optional,
                mutable: false,
            }
        })
        .collect()
}

fn parse_block_statement(node: &tree_sitter::Node, src: &str) -> Expr {
    assert_eq!(node.kind(), "statement_block");

    let mut cursor = node.walk();
    let stmts: Vec<Statement> = node
        .named_children(&mut cursor)
        .into_iter()
        .flat_map(|stmt| parse_statement(&stmt, src))
        .collect();

    let mut iter = stmts.iter().rev();

    let last = match iter.next() {
        Some(term) => match term {
            Statement::VarDecl { .. } => panic!("Didn't expect `let` here"),
            Statement::TypeDecl { .. } => {
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { expr, .. } => expr.to_owned(),
        },
        None => Expr::Empty(Empty { span: 0..0 }),
    };

    let result: Expr = iter.fold(last, |body, stmt| {
        match stmt {
            Statement::VarDecl {
                span,
                pattern,
                type_ann,
                init,
                declare: _,
            } => Expr::Let(Let {
                span: span.to_owned(),
                pattern: Some(pattern.to_owned()),
                type_ann: type_ann.to_owned(),
                // TODO: decide if we need to keep uninitialized variable declarations
                init: Box::new(init.to_owned().unwrap()),
                body: Box::new(body),
            }),
            Statement::TypeDecl { .. } => {
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { span, expr } => Expr::Let(Let {
                span: span.to_owned(),
                pattern: None,
                type_ann: None,
                init: Box::new(expr.to_owned()),
                body: Box::new(body),
            }),
        }
    });

    result
}

fn parse_template_string(node: &tree_sitter::Node, src: &str) -> TemplateLiteral {
    assert_eq!(node.kind(), "template_string");

    let mut cursor = node.walk();
    let children = node.named_children(&mut cursor);

    let mut start = node.byte_range().start + 1; // + 1 skips initial backtick
    let mut quasis: Vec<TemplateElem> = vec![];
    let mut exprs: Vec<Expr> = vec![];

    for child in children {
        if child.kind() != "template_substitution" {
            continue;
        }

        let expr = child.named_child(0).unwrap();
        exprs.push(parse_expression(&expr, src));

        let end = child.byte_range().start;
        let span = start..end;

        let raw = src.get(span.clone()).unwrap().to_owned();
        let cooked = unescape(&raw).unwrap();

        let raw = Lit::str(raw, span.clone());
        let cooked = Lit::str(cooked, span.clone());

        quasis.push(TemplateElem { span, raw, cooked });

        start = child.byte_range().end;
    }

    let end = node.byte_range().end - 1;
    let span = start..end;

    let raw = src.get(span.clone()).unwrap().to_owned();
    let cooked = unescape(&raw).unwrap();

    let raw = Lit::str(raw, span.clone());
    let cooked = Lit::str(cooked, span.clone());

    quasis.push(TemplateElem { span, raw, cooked });

    TemplateLiteral {
        span: node.byte_range(),
        exprs,
        quasis,
    }
}

fn parse_expression(node: &tree_sitter::Node, src: &str) -> Expr {
    match node.kind() {
        "arrow_function" => {
            let first_child = node.child(0).unwrap();
            let is_async = text_for_node(&first_child, src) == *"async";

            // TODO: check if the body is a statement_block otherwise parse
            // as a simple expression
            let body = node.child_by_field_name("body").unwrap();
            let body = match body.kind() {
                "statement_block" => parse_block_statement(&body, src),
                _ => parse_expression(&body, src),
            };

            let params = node.child_by_field_name("parameters").unwrap();
            let params = parse_formal_parameters(&params, src);

            let return_type = node
                .child_by_field_name("return_type")
                .map(|return_type| parse_type_ann(&return_type, src));

            // TODO: report an error if there are multiple rest params

            Expr::Lambda(Lambda {
                span: node.byte_range(),
                params,
                is_async,
                body: Box::from(body),
                return_type,
                type_params: None,
            })
        }
        "binary_expression" => {
            let left = node.child_by_field_name("left").unwrap();
            let left = Box::from(parse_expression(&left, src));
            let operator = node.child_by_field_name("operator").unwrap();
            let operator = text_for_node(&operator, src);
            let right = node.child_by_field_name("right").unwrap();
            let right = Box::from(parse_expression(&right, src));

            let op = match operator.as_str() {
                "&&" => todo!(),
                "||" => todo!(),
                // TODO: decide what to do with bitwise operators
                ">>" => todo!(),
                ">>>" => todo!(),
                "<<" => todo!(),
                "&" => todo!(),
                "^" => todo!(),
                "|" => todo!(),
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => todo!(),
                "**" => todo!(),
                "<" => BinOp::Lt,
                "<=" => BinOp::LtEq,
                "==" => BinOp::EqEq,
                "===" => todo!("remove ==="),
                "!=" => BinOp::NotEq,
                "!==" => todo!("remove !=="),
                ">=" => BinOp::GtEq,
                ">" => BinOp::Gt,
                "??" => todo!(),
                "instanceof" => todo!(),
                "in" => todo!(),
                _ => todo!("Unhandle operator: {operator}"),
            };

            Expr::Op(Op {
                span: node.byte_range(),
                left,
                op,
                right,
            })
        }
        "unary_expression" => {
            let operator = node.child_by_field_name("operator").unwrap();
            let operator = text_for_node(&operator, src);
            let arg = node.child_by_field_name("argument").unwrap();
            let arg = Box::from(parse_expression(&arg, src));

            // choice("!", "~", "-", "+", "typeof", "void", "delete")
            let op = match operator.as_str() {
                "-" => UnaryOp::Minus,
                _ => todo!("Unhandle operator: {operator}"),
            };

            Expr::UnaryExpr(UnaryExpr {
                span: node.byte_range(),
                arg,
                op,
            })
        }
        "parenthesized_expression" => {
            let expr = node.child(1).unwrap();
            parse_expression(&expr, src)
        }
        "call_expression" => {
            let func = node.child_by_field_name("function").unwrap();
            let func = parse_expression(&func, src);

            let args = node.child_by_field_name("arguments").unwrap();

            let mut cursor = args.walk();
            let args = args.named_children(&mut cursor);

            if args.len() > 0 {
                let first_arg = node.child_by_field_name("arguments").unwrap();
                if first_arg.kind() == "template_string" {
                    // TODO: handle non-identifiers
                    let tag = match func {
                        Expr::Ident(ident) => ident,
                        _ => panic!("non-identifier expressions cannot be used as tags"),
                    };

                    return Expr::TaggedTemplateLiteral(TaggedTemplateLiteral {
                        span: node.byte_range(),
                        tag,
                        template: parse_template_string(&first_arg, src),
                    });
                }
            }

            let args = args
                .into_iter()
                .map(|arg| {
                    if arg.kind() == "spread_element" {
                        let spread = arg.child(0).unwrap();
                        let arg = arg.child(1).unwrap();
                        let expr = parse_expression(&arg, src);
                        ExprOrSpread {
                            spread: Some(spread.byte_range()),
                            expr: Box::from(expr),
                        }
                    } else {
                        let expr = parse_expression(&arg, src);
                        ExprOrSpread {
                            spread: None,
                            expr: Box::from(expr),
                        }
                    }
                })
                .collect();

            // TODO: handle template string
            Expr::App(App {
                span: node.byte_range(),
                lam: Box::from(func),
                args,
            })
        }
        "identifier" => {
            let span = node.byte_range();
            let name = src.get(span.clone()).unwrap().to_owned();
            Expr::Ident(Ident { span, name })
        }
        "number" | "string" | "true" | "false" | "null" | "undefined" => {
            let lit = parse_literal(node, src);
            Expr::Lit(lit)
        }
        "object" => {
            let mut cursor = node.walk();
            let props = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|child| match child.kind() {
                    "pair" => {
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
                        let key_node = child.child_by_field_name("key").unwrap();
                        // let key = Ident {
                        //     span: key_node.byte_range(),
                        //     name: text_for_node(&key_node, src),
                        // };
                        let name = text_for_node(&key_node, src);
                        let value = child.child_by_field_name("value").unwrap();
                        let value = parse_expression(&value, src);
                        PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
                            span: child.byte_range(),
                            name,
                            value: Box::from(value),
                        })))
                    }
                    "spread_element" => {
                        let expr = child.named_child(0).unwrap();
                        PropOrSpread::Spread(SpreadElement {
                            span: node.byte_range(),
                            expr: Box::from(parse_expression(&expr, src)),
                        })
                    }
                    "method_definition" => todo!(),
                    "shorthand_property_identifier" => {
                        // choice($.identifier, $._reserved_identifier),
                        let name = text_for_node(&child, src);
                        PropOrSpread::Prop(Box::from(Prop::Shorthand(Ident {
                            span: node.byte_range(),
                            name,
                        })))
                    }
                    kind => panic!("Unexpect object property kind: {kind}"),
                })
                .collect();

            Expr::Obj(Obj {
                span: node.byte_range(),
                props,
            })
        }
        "array" => {
            // TODO: handle sparse tuples
            let mut cursor = node.walk();
            let elems = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|elem| match elem.kind() {
                    "spread_element" => {
                        let expr = elem.named_child(0).unwrap();
                        let expr = Box::from(parse_expression(&expr, src));
                        ExprOrSpread {
                            spread: Some(elem.byte_range()),
                            expr,
                        }
                    }
                    _ => ExprOrSpread {
                        spread: None,
                        expr: Box::from(parse_expression(&elem, src)),
                    },
                })
                .collect();

            Expr::Tuple(Tuple {
                span: node.byte_range(),
                elems,
            })
        }
        "jsx_element" | "jsx_self_closing_element" => {
            Expr::JSXElement(parse_jsx_element(node, src))
        }
        "member_expression" => {
            let obj = node.child_by_field_name("object").unwrap();
            let obj = parse_expression(&obj, src);
            let prop = node.child_by_field_name("property").unwrap();
            let name = text_for_node(&prop, src);

            Expr::Member(Member {
                span: node.byte_range(),
                obj: Box::from(obj),
                prop: MemberProp::Ident(Ident {
                    span: prop.byte_range(),
                    name,
                }),
            })
        }
        "subscript_expression" => {
            let obj = node.child_by_field_name("object").unwrap();
            let obj = parse_expression(&obj, src);
            let index = node.child_by_field_name("index").unwrap();
            let expr = parse_expression(&index, src);

            Expr::Member(Member {
                span: node.byte_range(),
                obj: Box::from(obj),
                prop: MemberProp::Computed(ComputedPropName {
                    span: index.byte_range(),
                    expr: Box::from(expr),
                }),
            })
        }
        "await_expression" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src);

            Expr::Await(Await {
                span: node.byte_range(),
                expr: Box::from(expr),
            })
        }
        "template_string" => Expr::TemplateLiteral(parse_template_string(node, src)),
        _ => {
            todo!("unhandled {node:#?} = '{}'", text_for_node(node, src))
        }
    }

    // Expression

    // $.primary_expression,
    // $.glimmer_template,
    // $._jsx_element,
    // $.jsx_fragment,
    // $.assignment_expression,
    // $.augmented_assignment_expression,
    // $.await_expression,
    // $.unary_expression,
    // $.binary_expression,
    // $.ternary_expression,
    // $.update_expression,
    // $.new_expression,
    // $.yield_expression

    // Primary Expressions

    // $.subscript_expression,
    // $.member_expression,
    // $.parenthesized_expression,
    // $._identifier,
    // alias($._reserved_identifier, $.identifier),
    // $.this,
    // $.super,
    // $.number,
    // $.string,
    // $.template_string,
    // $.regex,
    // $.true,
    // $.false,
    // $.null,
    // $.import,
    // $.object,
    // $.array,
    // $.function,
    // $.arrow_function,
    // $.generator_function,
    // $.class,
    // $.meta_property,
    // $.call_expression
}

fn parse_type_ann(node: &tree_sitter::Node, src: &str) -> TypeAnn {
    let node = if node.kind() == "type_annotation"
        || node.kind() == "constraint"
        || node.kind() == "default_type"
    {
        node.named_child(0).unwrap()
    } else {
        node.to_owned()
    };
    match node.kind() {
        // Primary types
        "parenthesized_type" => todo!(),
        "predefined_type" => match text_for_node(&node, src).as_str() {
            "any" => todo!("remove support for 'any'"),
            "number" => TypeAnn::Prim(PrimType {
                span: node.byte_range(),
                prim: Primitive::Num,
            }),
            "boolean" => TypeAnn::Prim(PrimType {
                span: node.byte_range(),
                prim: Primitive::Bool,
            }),
            "string" => TypeAnn::Prim(PrimType {
                span: node.byte_range(),
                prim: Primitive::Str,
            }),
            "symbol" => todo!(),
            "void" => todo!(),
            "unknown" => todo!(),
            "never" => todo!(),
            "object" => todo!("remove support for 'object'"),
            name => panic!("Unkwnown predefined_type: '{name}'"),
        },
        "type_identifier" => TypeAnn::TypeRef(TypeRef {
            span: node.byte_range(),
            name: text_for_node(&node, src),
            type_params: None,
        }),
        "nested_type_identifier" => todo!(),
        "generic_type" => {
            let name = node.child_by_field_name("name").unwrap();
            let type_arguments = node.child_by_field_name("type_arguments").unwrap();
            let mut cursor = type_arguments.walk();
            let type_params = type_arguments
                .named_children(&mut cursor)
                .into_iter()
                .map(|arg| parse_type_ann(&arg, src))
                .collect();

            TypeAnn::TypeRef(TypeRef {
                span: node.byte_range(),
                name: text_for_node(&name, src),
                type_params: Some(type_params),
            })
        }
        "object_type" => {
            let mut cursor = node.walk();
            let props = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|prop| match prop.kind() {
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
                        let name = text_for_node(&name_node, src);

                        let type_ann = prop.child_by_field_name("type").unwrap();
                        let type_ann = parse_type_ann(&type_ann, src);

                        TProp {
                            span: prop.byte_range(),
                            name,
                            optional: false, // TODO
                            mutable: false,  // TODO,
                            type_ann: Box::from(type_ann),
                        }
                    }
                    "call_signature" => todo!("call_signature"),
                    "construct_signature" => todo!("construct_signature"),
                    "index_signature" => todo!("index_signature"),
                    // TODO: remove method_signature, methods should look the same as properties
                    "method_signature" => todo!("remove method_signature from object_type"),
                    kind => panic!("Unsupport prop kind in object_type: '{kind}'"),
                })
                .collect();

            TypeAnn::Object(ObjectType {
                span: node.byte_range(),
                props,
            })
        }
        "array_type" => todo!(),
        "tuple_type" => {
            let mut cursor = node.walk();
            let types = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|elem| {
                    println!("parsing elem: {elem:#?}");
                    parse_type_ann(&elem, src)
                })
                .collect();

            TypeAnn::Tuple(TupleType {
                span: node.byte_range(),
                types,
            })
        }
        "flow_maybe_type" => todo!(),
        "type_query" => todo!(),
        "index_type_query" => todo!(),
        // alias($.this, $.this_type),
        "existential_type" => todo!(),
        "literal_type" => {
            let child = node.named_child(0).unwrap();
            let lit = parse_literal(&child, src);
            TypeAnn::Lit(lit)
        }
        "lookup_type" => todo!(),
        "conditional_type" => todo!(),
        "template_literal_type" => todo!(),
        "intersection_type" => todo!(),
        "union_type" => todo!(),

        // Non-primary types
        "function_type" => todo!(),
        "readonly_type" => todo!(),
        "constructor_type" => todo!(),
        "infer_type" => todo!(),

        kind => panic!("Unexpected type_annotation kind: '{kind}'"),
    }

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

fn parse_literal(node: &tree_sitter::Node, src: &str) -> Lit {
    match node.kind() {
        "number" => Lit::num(
            src.get(node.byte_range()).unwrap().to_owned(),
            node.byte_range(),
        ),
        "string" => {
            let mut cursor = node.walk();
            let raw = join(
                node.named_children(&mut cursor)
                    .into_iter()
                    .map(|fragment_or_escape| text_for_node(&fragment_or_escape, src)),
                "",
            );

            let cooked = unescape(&raw).unwrap();
            Lit::str(cooked, node.byte_range())
        }
        "true" => Lit::bool(true, node.byte_range()),
        "false" => Lit::bool(false, node.byte_range()),
        "null" => todo!(),
        "undefined" => todo!(),
        kind => panic!("Unexpected literal kind: '{kind}'"),
    }
}

fn parse_jsx_attrs(node: &tree_sitter::Node, src: &str) -> Vec<JSXAttr> {
    let mut cursor = node.walk();
    let attrs = node.children_by_field_name("attribute", &mut cursor);
    attrs
        .into_iter()
        .map(|attr| {
            let ident = attr.named_child(0).unwrap();
            let ident = Ident {
                span: ident.byte_range(),
                name: text_for_node(&ident, src),
            };
            // TODO: handle JSX attr shorthand
            let value = attr.named_child(1).unwrap();
            let value = match value.kind() {
                "jsx_expression" => {
                    // TODO: handle None case
                    let expr = value.named_child(0).unwrap();
                    let expr = parse_expression(&expr, src);
                    JSXAttrValue::JSXExprContainer(JSXExprContainer {
                        span: value.byte_range(),
                        expr,
                    })
                }
                "string" => {
                    let lit = parse_literal(&value, src);
                    JSXAttrValue::Lit(lit)
                }
                kind => panic!("Unexpected JSX attr value with kind: '{kind}'"),
            };
            JSXAttr {
                span: attr.byte_range(),
                ident,
                value,
            }
        })
        .collect()
}

fn parse_jsx_element(node: &tree_sitter::Node, src: &str) -> JSXElement {
    match node.kind() {
        "jsx_element" => {
            let mut cursor = node.walk();
            let children: Vec<JSXElementChild> = node
                .named_children(&mut cursor)
                .into_iter()
                .filter(|child| {
                    child.kind() != "jsx_opening_element" && child.kind() != "jsx_closing_element"
                })
                .map(|child| match child.kind() {
                    "jsx_text" => JSXElementChild::JSXText(JSXText {
                        span: child.byte_range(),
                        value: text_for_node(&child, src),
                    }),
                    "jsx_element" | "jsx_self_closing_element" => {
                        JSXElementChild::JSXElement(Box::from(parse_jsx_element(&child, src)))
                    }
                    "jsx_fragment" => todo!(),
                    "jsx_expression" => {
                        // TODO: handle None case
                        let expr = child.named_child(0).unwrap();
                        let expr = parse_expression(&expr, src);
                        JSXElementChild::JSXExprContainer(JSXExprContainer {
                            span: child.byte_range(),
                            expr,
                        })
                    }
                    kind => panic!("Unexpected JSXElementChild kind: '{kind}'"),
                })
                .collect();

            let open_tag = node.child_by_field_name("open_tag").unwrap();
            let name = open_tag.child_by_field_name("name").unwrap();

            JSXElement {
                span: node.byte_range(),
                name: text_for_node(&name, src),
                attrs: parse_jsx_attrs(&open_tag, src),
                children,
            }
        }
        "jsx_self_closing_element" => {
            let name = node.child_by_field_name("name").unwrap();
            JSXElement {
                span: node.byte_range(),
                name: text_for_node(&name, src),
                attrs: parse_jsx_attrs(node, src),
                children: vec![],
            }
        }
        kind => panic!("Unexpected kind when parsing jsx: '{kind}'"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let src = r#"
        const add = (a, b) => a + b, sub = (a, b) => a - b;
        const sum = add(5, 10);
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn numbers() {
        insta::assert_debug_snapshot!(parse("10"));
        insta::assert_debug_snapshot!(parse("1.23"));
        insta::assert_debug_snapshot!(parse("-10"));
    }

    #[test]
    fn strings() {
        insta::assert_debug_snapshot!(parse(r#""""#));
        insta::assert_debug_snapshot!(parse(r#""hello""#));
        insta::assert_debug_snapshot!(parse("\"line 1\\nline 2\\nline 3\""));
        insta::assert_debug_snapshot!(parse("\"a \\u2212 b\""));
        insta::assert_debug_snapshot!(parse("\"hello, \\\"world\\\"!\""));
    }

    #[test]
    fn template_literals() {
        insta::assert_debug_snapshot!(parse("`Hello, world`"));
        insta::assert_debug_snapshot!(parse("`Hello, ${name}`"));
        insta::assert_debug_snapshot!(parse("`(${x}, ${y})`"));
        insta::assert_debug_snapshot!(parse(r#"`Hello, "world"`"#));
        insta::assert_debug_snapshot!(parse("`foo ${`bar ${baz}`}`"));
        insta::assert_debug_snapshot!(parse("`line 1\\nline 2\\nline 3`"));
        insta::assert_debug_snapshot!(parse("`a \\u2212 b`"));
        // insta::assert_debug_snapshot!(parse(r#"if cond { `${foo}` } else { `${bar}` }"#));
    }

    #[test]
    fn tagged_template_literals() {
        insta::assert_debug_snapshot!(parse("sql`SELECT * FROM ${table} WHERE id = ${id}`"));
    }

    #[test]
    #[ignore]
    fn template_literal_with_mismatched_backtick() {
        insta::assert_debug_snapshot!(parse("`foo ${bar`}`"));
    }

    #[test]
    #[ignore]
    fn interpolation_outside_of_template_literal() {
        insta::assert_debug_snapshot!(parse("`foo ${bar}`${baz}`"));
    }

    #[test]
    fn operations() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3"));
        insta::assert_debug_snapshot!(parse("x * y / z"));
        insta::assert_debug_snapshot!(parse("(a + b) * c"));
        insta::assert_debug_snapshot!(parse("a == b"));
        insta::assert_debug_snapshot!(parse("a != b"));
        insta::assert_debug_snapshot!(parse("a > b"));
        insta::assert_debug_snapshot!(parse("a >= b"));
        insta::assert_debug_snapshot!(parse("a < b"));
        insta::assert_debug_snapshot!(parse("a <= b"));
        insta::assert_debug_snapshot!(parse("let cond = a != b"));
        insta::assert_debug_snapshot!(parse("-a"));
        insta::assert_debug_snapshot!(parse("-(a + b)"));
    }

    #[test]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"));
        insta::assert_debug_snapshot!(parse("() => 10"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""));
        insta::assert_debug_snapshot!(parse("(a, ...b) => true"));
        insta::assert_debug_snapshot!(parse("({x, y}) => x + y"));
        insta::assert_debug_snapshot!(parse("({x: p, y: q}) => p + q"));
        insta::assert_debug_snapshot!(parse("(a?: boolean, b?) => c"));
    }

    #[test]
    #[ignore]
    fn multiple_rest_params() {
        assert_eq!(
            parse("(...a, ...b) => true"),
            Err("rest params must come last".to_string())
        );
    }

    #[test]
    #[ignore]
    fn optional_params_must_appear_last() {
        assert_eq!(
            parse("(a?, b) => true"),
            Err("optional params must come last".to_string()),
        );
    }

    #[test]
    fn async_await() {
        insta::assert_debug_snapshot!(parse("async () => 10"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"));
    }

    #[test]
    fn function_application() {
        insta::assert_debug_snapshot!(parse("foo()"));
        insta::assert_debug_snapshot!(parse("foo(a, b)"));
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\")"));
        insta::assert_debug_snapshot!(parse("f(x)(g(x))"));
        insta::assert_debug_snapshot!(parse("foo(a, ...b)"));
        let src = r#"
        let S = (f) => (g) => (x) => f(x)(g(x))
        let K = (x) => (y) => x
        let I = S(K)(K)
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    #[ignore]
    fn declarations() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b"));
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; x}"));
        insta::assert_debug_snapshot!(parse("let rec f = () => f()")); // recursive
    }

    #[test]
    fn top_level_expressions() {
        insta::assert_debug_snapshot!(parse("a + b"));
        insta::assert_debug_snapshot!(parse("123\n\"hello\""));
    }

    #[test]
    #[ignore]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if true { 5 } else { 10 }"));
        insta::assert_debug_snapshot!(parse("if a { 5 } else if b { 10 } else { 20 }"));
    }

    #[test]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let obj = {x, y}"));
        insta::assert_debug_snapshot!(parse("let obj = {a, b, ...others}"));
    }

    #[test]
    fn jsx() {
        insta::assert_debug_snapshot!(parse("<Foo>Hello</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{bar}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>Hello {world}!</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{<Bar>{baz}</Bar>}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo bar={baz} />"));
        insta::assert_debug_snapshot!(parse("<Foo msg=\"hello\" bar={baz}></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo><Bar>{baz}</Bar></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>hello<Bar/>{world}<Baz/></Foo>"));
        insta::assert_debug_snapshot!(parse(
            "let elem = <div point={point} id=\"point\">Hello, {msg}</div>"
        ));
    }

    #[test]
    #[ignore]
    fn jsx_head_and_tail_must_match() {
        assert_eq!(
            parse("<Foo>Hello</Bar>"),
            Err("JSX head and tail elements must match".to_string()),
        );
    }

    #[test]
    fn type_annotations() {
        insta::assert_debug_snapshot!(parse("let x: number = 5"));
        insta::assert_debug_snapshot!(parse("let msg: string = \"hello\""));
        insta::assert_debug_snapshot!(parse("let add = (a: number, b: number): number => a + b"));
        insta::assert_debug_snapshot!(parse("let p: Point = {x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let FOO: \"foo\" = \"foo\""));
    }

    #[test]
    fn decls() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("   let x = 5")); // with leading whitespace
        insta::assert_debug_snapshot!(parse("declare let x: number"));
        insta::assert_debug_snapshot!(parse("declare let foo: Foo<string>"));
    }

    #[test]
    fn tuples() {
        insta::assert_debug_snapshot!(parse("let x = []"));
        insta::assert_debug_snapshot!(parse("let x = [1, 2, 3]"));
        insta::assert_debug_snapshot!(parse("let x = [1, [a, b]]"));
        insta::assert_debug_snapshot!(parse("let foo = () => [a, b]"));
    }

    #[test]
    fn member_access() {
        insta::assert_debug_snapshot!(parse("a.b.c"));
        insta::assert_debug_snapshot!(parse("foo.bar()"));
        insta::assert_debug_snapshot!(parse("p.x * p.x + p.y * p.y"));
        insta::assert_debug_snapshot!(parse("foo().bar()"));
        insta::assert_debug_snapshot!(parse("arr[0][1]"));
        insta::assert_debug_snapshot!(parse("arr[x](y)"));
        insta::assert_debug_snapshot!(parse("arr[arr.length - 1]"));
        insta::assert_debug_snapshot!(parse("foo[bar[-1]]"));
    }

    #[test]
    fn type_decls() {
        insta::assert_debug_snapshot!(parse("type Num = number"));
        insta::assert_debug_snapshot!(parse("type Point = {x: number, y: number}"));
        insta::assert_debug_snapshot!(parse("type Foo<T> = {bar: T}"));
        insta::assert_debug_snapshot!(parse("type Foo<T extends string> = {bar: T}"));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T = "foo"> = {bar: T}"#));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T extends string = "foo"> = {bar: T}"#));
    }

    #[test]
    #[ignore]
    fn blocks() {
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; x}"));
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; let y = 10; x + y}"));
        insta::assert_debug_snapshot!(parse("{let x = 5; let y = 10; x + y}"));
        insta::assert_debug_snapshot!(parse("{let sum = {let x = 5; let y = 10; x + y}; sum}"));
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; console.log(x); x}"));
        insta::assert_debug_snapshot!(parse("let foo = {console.log(x); x}"));
    }

    #[test]
    fn destructuring() {
        insta::assert_debug_snapshot!(parse("let {x, y} = point"));
        insta::assert_debug_snapshot!(parse("let {a, b, ...rest} = letters"));
        insta::assert_debug_snapshot!(parse("let {p0: {x, y}, p1: {x, y}} = line"));
        insta::assert_debug_snapshot!(parse("let [a, b, ...rest] = letters"));
        insta::assert_debug_snapshot!(parse("let [foo, ...[bar, ...rest]] = baz"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]) => a"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]: [string, number]) => a"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}) => b"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}: {a: string, b: number}) => b"));
        // TODO: assigning defaults
        // TODO: type annotations
        // TODO: function params
        // TODO: disallowed patterns, e.g. top-level rest, non-top-level type annotations
    }

    #[test]
    fn array_spread() {
        insta::assert_debug_snapshot!(parse("let tuple = [...a, b]"));
        insta::assert_debug_snapshot!(parse("let tuple = [a, ...b]"));
        insta::assert_debug_snapshot!(parse("let tuple = [1, ...[2, 3]]"));
    }

    #[test]
    #[ignore]
    #[should_panic = "Only one rest is allowed in an object pattern"]
    fn multiple_rests_is_invalid() {
        insta::assert_debug_snapshot!(parse("let {z, ...p, ...q} = point"));
    }

    #[test]
    #[ignore]
    #[should_panic = "Rest should come last in object pattern"]
    fn rest_that_isnt_last_is_invalid() {
        insta::assert_debug_snapshot!(parse("let {...p, z} = point"));
    }

    #[test]
    #[ignore]
    fn types() {
        insta::assert_debug_snapshot!(parse("let get_bar = <T>(foo: Foo<T>) => foo.bar"));
        insta::assert_debug_snapshot!(parse("declare let get_bar: (foo: Foo) => T"));
        insta::assert_debug_snapshot!(parse("let str_arr: string[] = []"));
        insta::assert_debug_snapshot!(parse("let thunk_arr: (() => undefined)[] = []"));
        insta::assert_debug_snapshot!(parse("let arr: string[] | number[] = []"));
        insta::assert_debug_snapshot!(parse("let nested_arr: string[][] = []"));
        let src = r#"
        type Event = 
          | {type: "mousedown", x: number, y: number}
          | {type: "keydown", key: string}
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }
}
