use itertools::free::join;
#[cfg(target_family = "wasm")]
use std::ffi::CString;
use std::os::raw::c_char;
use unescape::unescape;

use crochet_ast::values::*;

#[link(wasm_import_module = "my_custom_module")]
extern "C" {
    fn _log(wasm_str: WasmString);
}

#[repr(C)]
pub struct WasmString {
    pub offset: *const c_char,
    pub length: u32,
}

#[cfg(target_family = "wasm")]
unsafe fn string_to_wasm_string(input: &str) -> WasmString {
    let length = input.len() as u32;
    let offset = CString::new(input).unwrap().into_raw();
    WasmString { offset, length }
}

#[cfg(target_family = "wasm")]
fn log(str: &str) {
    unsafe {
        _log(string_to_wasm_string(str));
    }
}

#[cfg(target_family = "unix")]
fn log(str: &str) {
    println!("{}", str);
}

pub fn parse(src: &str) -> Result<Program, String> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_crochet::language())
        .expect("Error loading crochet language");

    log("hello, world!");

    let tree = parser.parse(src, None).unwrap();

    let root = tree.root_node();

    let kind = root.kind();
    if kind == "program" {
        let mut cursor = root.walk();

        let children = root.children(&mut cursor);

        let mut body: Vec<Statement> = vec![];
        for child in children {
            println!("child.kind = {}", child.kind());
            let mut stmts = parse_statement(&child, src)?;
            body.append(&mut stmts);
        }

        Ok(Program { body })
    } else {
        Err("not implemented yet".to_string())
    }
}

fn parse_statement(node: &tree_sitter::Node, src: &str) -> Result<Vec<Statement>, String> {
    match node.kind() {
        "lexical_declaration" => parse_declaration(node, false, src),
        "expression_statement" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src)?;
            Ok(vec![Statement::Expr {
                span: node.byte_range(),
                expr: Box::from(expr),
            }])
        }
        "ambient_declaration" => {
            let decl = node.named_child(0).unwrap();
            parse_declaration(&decl, true, src)
        }
        "type_alias_declaration" => {
            let name = node.child_by_field_name("name").unwrap();
            let id = Ident {
                span: name.byte_range(),
                name: text_for_node(&name, src)?,
            };
            let type_ann = node.child_by_field_name("value").unwrap();
            let type_ann = parse_type_ann(&type_ann, src)?;

            let type_params = parse_type_params_for_node(node, src)?;

            Ok(vec![Statement::TypeDecl {
                span: node.byte_range(),
                declare: false,
                id,
                type_ann,
                type_params,
            }])
        }
        "comment" => {
            Ok(vec![]) // ignore comments
        }
        "ERROR" => Err(format!("failed to parse: '{}'", text_for_node(node, src)?)),
        _ => Err(format!("unhandled: {:#?}", node)),
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
fn text_for_node(node: &tree_sitter::Node, src: &str) -> Result<String, String> {
    match src.get(node.byte_range()) {
        Some(text) => Ok(text.to_owned()),
        None => Err(String::from("error in text_for_node")),
    }
}

fn parse_declaration(
    node: &tree_sitter::Node,
    declare: bool,
    src: &str,
) -> Result<Vec<Statement>, String> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(String::from("Error parsing declaration"));
    }

    let decl = node.child_by_field_name("decl").unwrap();
    let rec = node.child_by_field_name("rec").is_some();

    let name = decl.child_by_field_name("name").unwrap();
    let pattern = parse_pattern(&name, src)?;

    let init = if let Some(init) = decl.child_by_field_name("value") {
        Some(Box::from(parse_expression(&init, src)?))
    } else {
        None
    };

    let type_ann = if let Some(type_ann) = decl.child_by_field_name("type") {
        Some(parse_type_ann(&type_ann, src)?)
    } else {
        None
    };

    let stmt = if rec {
        // `let fib = fix((fib) => (n) => ...)`
        // TODO: Fix always wraps a lambda

        let lambda_expr = Expr {
            span: decl.byte_range(),
            kind: ExprKind::Lambda(Lambda {
                params: vec![EFnParam {
                    pat: pattern.clone(),
                    type_ann: type_ann.clone(),
                    optional: false,
                    mutable: false,
                }],
                body: init.unwrap(),
                is_async: false,
                return_type: None,
                type_params: None, // TODO: support type params on VarDecls
            }),
            inferred_type: None,
        };

        let fix = Expr {
            span: decl.byte_range(),
            kind: ExprKind::Fix(Fix {
                expr: Box::from(lambda_expr),
            }),
            inferred_type: None,
        };

        Statement::VarDecl {
            span: node.byte_range(),
            pattern,
            type_ann,
            init: Some(Box::from(fix)),
            declare,
        }
    } else {
        Statement::VarDecl {
            span: node.byte_range(),
            pattern,
            type_ann,
            init,
            declare,
        }
    };

    Ok(vec![stmt])
}

fn parse_pattern(node: &tree_sitter::Node, src: &str) -> Result<Pattern, String> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(String::from("Error parsing pattern"));
    }
    let kind = match node.kind() {
        "binding_identifier" => {
            let name = node.child_by_field_name("name").unwrap();
            let name = src.get(name.byte_range()).unwrap().to_owned();
            let mutable = node.child_by_field_name("mut").is_some();
            PatternKind::Ident(BindingIdent {
                name,
                mutable,
                span: node.byte_range(),
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
                            name: text_for_node(&key_node, src)?,
                        };

                        let value = Box::from(parse_pattern(
                            &child.child_by_field_name("value").unwrap(),
                            src,
                        )?);

                        // TODO: include `span` in this node
                        Ok(ObjectPatProp::KeyValue(KeyValuePatProp { key, value }))
                    }
                    "rest_pattern" => {
                        let pattern = child.named_child(0).unwrap();
                        let pattern = parse_pattern(&pattern, src)?;

                        Ok(ObjectPatProp::Rest(RestPat {
                            arg: Box::from(pattern),
                        }))
                    }
                    "object_assignment_pattern" => todo!(),
                    "shorthand_property_identifier_pattern" => {
                        let name_node = child.child_by_field_name("name").unwrap();
                        let name = src.get(name_node.byte_range()).unwrap().to_owned();
                        let mutable = child.child_by_field_name("mut").is_some();
                        Ok(ObjectPatProp::Assign(AssignPatProp {
                            span: child.byte_range(),
                            key: BindingIdent {
                                span: name_node.byte_range(),
                                name,
                                mutable,
                            },
                            value: None,
                        }))
                    }
                    kind => panic!("Unexpected object property kind: '{kind}'"),
                })
                .collect::<Result<Vec<_>, String>>()?;

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
                    "assignment_pattern" => todo!(),
                    _ => Ok(Some(parse_pattern(&child, src)?)),
                })
                .collect::<Result<Vec<_>, String>>()?;

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
        _ => panic!("unrecognized pattern {node:#?}"),
    };

    Ok(Pattern {
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })
}

fn parse_formal_parameters(node: &tree_sitter::Node, src: &str) -> Result<Vec<EFnParam>, String> {
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
                mutable: false,
            })
        })
        .collect::<Result<Vec<_>, String>>()
}

fn parse_block_statement(node: &tree_sitter::Node, src: &str) -> Result<Expr, String> {
    assert_eq!(node.kind(), "statement_block");

    let mut cursor = node.walk();

    let child_count = node.named_child_count();

    let mut stmts: Vec<Statement> = vec![];

    for (i, child) in node.named_children(&mut cursor).into_iter().enumerate() {
        let is_last = i == child_count - 1;
        if is_last {
            // This is the only place where a named `expression` node
            // should exist.  Everywhere else its contents should be
            // inline.
            if child.kind() == "expression" {
                let expr = child.named_child(0).unwrap();
                let expr = parse_expression(&expr, src)?;
                stmts.push(Statement::Expr {
                    span: child.byte_range(),
                    expr: Box::from(expr),
                })
            } else {
                let mut result = parse_statement(&child, src)?;
                // TODO: get the span of the semicolon
                let expr = Expr {
                    span: 0..0,
                    kind: ExprKind::Empty,
                    inferred_type: None,
                };
                result.push(Statement::Expr {
                    span: 0..0,
                    expr: Box::from(expr),
                });
                stmts.append(&mut result);
            }
        } else {
            let mut result = parse_statement(&child, src)?;
            stmts.append(&mut result);
        }
    }

    let mut iter = stmts.iter().rev();

    let last: Expr = match iter.next() {
        Some(term) => match term {
            Statement::VarDecl { .. } => panic!("Didn't expect `let` here"),
            Statement::TypeDecl { .. } => {
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { expr, .. } => *expr.to_owned(),
        },
        None => Expr {
            span: 0..0,
            kind: ExprKind::Empty,
            inferred_type: None,
        },
    };

    let result: Expr = iter.fold(last, |body, stmt| {
        match stmt {
            Statement::VarDecl {
                span,
                pattern,
                type_ann,
                init,
                declare: _,
            } => {
                let kind = ExprKind::Let(Let {
                    pattern: Some(pattern.to_owned()),
                    type_ann: type_ann.to_owned(),
                    // TODO: decide if we need to keep uninitialized variable declarations
                    init: init.to_owned().unwrap(),
                    body: Box::new(body),
                });
                Expr {
                    span: span.to_owned(),
                    kind,
                    inferred_type: None,
                }
            }
            Statement::TypeDecl { .. } => {
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { span, expr } => {
                let kind = ExprKind::Let(Let {
                    pattern: None,
                    type_ann: None,
                    init: expr.to_owned(),
                    body: Box::new(body),
                });
                Expr {
                    span: span.to_owned(),
                    kind,
                    inferred_type: None,
                }
            }
        }
    });

    Ok(result)
}

fn parse_template_string(node: &tree_sitter::Node, src: &str) -> Result<TemplateLiteral, String> {
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
        exprs.push(parse_expression(&expr, src)?);

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

    Ok(TemplateLiteral { exprs, quasis })
}

fn parse_expression(node: &tree_sitter::Node, src: &str) -> Result<Expr, String> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(String::from("Error parsing expression"));
    }
    let kind = match node.kind() {
        "arrow_function" => {
            let first_child = node.child(0).unwrap();
            let is_async = text_for_node(&first_child, src)? == *"async";

            // TODO: check if the body is a statement_block otherwise parse
            // as a simple expression
            let body = node.child_by_field_name("body").unwrap();
            let body = match body.kind() {
                "statement_block" => parse_block_statement(&body, src),
                _ => parse_expression(&body, src),
            };

            let params = node.child_by_field_name("parameters").unwrap();
            let params = parse_formal_parameters(&params, src)?;

            let return_type = if let Some(return_type) = node.child_by_field_name("return_type") {
                Some(parse_type_ann(&return_type, src)?)
            } else {
                None
            };

            let type_params = parse_type_params_for_node(node, src)?;

            // TODO: report an error if there are multiple rest params

            ExprKind::Lambda(Lambda {
                params,
                is_async,
                body: Box::from(body?),
                return_type,
                type_params,
            })
        }
        "assignment_expression" => {
            let left = node.child_by_field_name("left").unwrap();
            let left = Box::from(parse_expression(&left, src)?);
            let right = node.child_by_field_name("right").unwrap();
            let right = Box::from(parse_expression(&right, src)?);

            ExprKind::Assign(Assign {
                left,
                right,
                op: AssignOp::Eq,
            })
        }
        "augmented_assignment_expression" => {
            // TODO: handle +=, -=, *=, /=, etc.
            todo!()
        }
        "binary_expression" => {
            let left = node.child_by_field_name("left").unwrap();
            let left = Box::from(parse_expression(&left, src)?);
            let operator = node.child_by_field_name("operator").unwrap();
            let operator = text_for_node(&operator, src)?;
            let right = node.child_by_field_name("right").unwrap();
            let right = Box::from(parse_expression(&right, src)?);

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

            ExprKind::BinaryExpr(BinaryExpr { left, op, right })
        }
        "unary_expression" => {
            let operator = node.child_by_field_name("operator").unwrap();
            let operator = text_for_node(&operator, src)?;
            let arg = node.child_by_field_name("argument").unwrap();
            let arg = Box::from(parse_expression(&arg, src)?);

            // choice("!", "~", "-", "+", "typeof", "void", "delete")
            let op = match operator.as_str() {
                "-" => UnaryOp::Minus,
                _ => todo!("Unhandle operator: {operator}"),
            };

            ExprKind::UnaryExpr(UnaryExpr { arg, op })
        }
        "parenthesized_expression" => {
            let expr = node.child(1).unwrap();
            return parse_expression(&expr, src);
        }
        "call_expression" => {
            let func = node.child_by_field_name("function").unwrap();
            let func = parse_expression(&func, src)?;

            let args = node.child_by_field_name("arguments").unwrap();

            let mut cursor = args.walk();
            let args = args.named_children(&mut cursor);

            if args.len() > 0 {
                let first_arg = node.child_by_field_name("arguments").unwrap();
                if first_arg.kind() == "template_string" {
                    // TODO: handle non-identifiers
                    let tag = match func.kind {
                        ExprKind::Ident(ident) => ident,
                        _ => panic!("non-identifier expressions cannot be used as tags"),
                    };

                    let kind = ExprKind::TaggedTemplateLiteral(TaggedTemplateLiteral {
                        tag,
                        template: parse_template_string(&first_arg, src)?,
                    });

                    return Ok(Expr {
                        span: node.byte_range(),
                        kind,
                        inferred_type: None,
                    });
                }
            }

            let args = args
                .into_iter()
                .map(|arg| {
                    if arg.kind() == "spread_element" {
                        let spread = arg.child(0).unwrap();
                        let arg = arg.child(1).unwrap();
                        let expr = parse_expression(&arg, src)?;
                        Ok(ExprOrSpread {
                            spread: Some(spread.byte_range()),
                            expr: Box::from(expr),
                        })
                    } else {
                        let expr = parse_expression(&arg, src)?;
                        Ok(ExprOrSpread {
                            spread: None,
                            expr: Box::from(expr),
                        })
                    }
                })
                .collect::<Result<Vec<_>, String>>()?;

            // TODO: handle template string
            ExprKind::App(App {
                lam: Box::from(func),
                args,
            })
        }
        "identifier" => {
            let span = node.byte_range();
            let name = src.get(span.clone()).unwrap().to_owned();
            ExprKind::Ident(Ident { span, name })
        }
        "number" | "string" | "true" | "false" => {
            let lit = parse_literal(node, src)?;
            ExprKind::Lit(lit)
        }
        "null" | "undefined" => {
            todo!()
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
                        let name = text_for_node(&key_node, src)?;
                        let value = child.child_by_field_name("value").unwrap();
                        let value = parse_expression(&value, src)?;
                        Ok(PropOrSpread::Prop(Box::from(Prop::KeyValue(
                            KeyValueProp {
                                name,
                                value: Box::from(value),
                            },
                        ))))
                    }
                    "spread_element" => {
                        let expr = child.named_child(0).unwrap();
                        Ok(PropOrSpread::Spread(SpreadElement {
                            expr: Box::from(parse_expression(&expr, src)?),
                        }))
                    }
                    "method_definition" => todo!(),
                    "shorthand_property_identifier" => {
                        // choice($.identifier, $._reserved_identifier),
                        let name = text_for_node(&child, src)?;
                        Ok(PropOrSpread::Prop(Box::from(Prop::Shorthand(Ident {
                            span: node.byte_range(),
                            name,
                        }))))
                    }
                    kind => panic!("Unexpect object property kind: {kind}"),
                })
                .collect::<Result<Vec<_>, String>>()?;

            ExprKind::Obj(Obj { props })
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
                        let expr = Box::from(parse_expression(&expr, src)?);
                        Ok(ExprOrSpread {
                            spread: Some(elem.byte_range()),
                            expr,
                        })
                    }
                    _ => Ok(ExprOrSpread {
                        spread: None,
                        expr: Box::from(parse_expression(&elem, src)?),
                    }),
                })
                .collect::<Result<Vec<_>, String>>()?;

            ExprKind::Tuple(Tuple { elems })
        }
        "jsx_element" | "jsx_self_closing_element" => {
            ExprKind::JSXElement(parse_jsx_element(node, src)?)
        }
        "member_expression" => {
            let obj = node.child_by_field_name("object").unwrap();
            let obj = parse_expression(&obj, src)?;
            let prop = node.child_by_field_name("property").unwrap();
            let name = text_for_node(&prop, src)?;

            ExprKind::Member(Member {
                obj: Box::from(obj),
                prop: MemberProp::Ident(Ident {
                    span: prop.byte_range(),
                    name,
                }),
            })
        }
        "subscript_expression" => {
            let obj = node.child_by_field_name("object").unwrap();
            let obj = parse_expression(&obj, src)?;
            let index = node.child_by_field_name("index").unwrap();
            let expr = parse_expression(&index, src)?;

            ExprKind::Member(Member {
                obj: Box::from(obj),
                prop: MemberProp::Computed(ComputedPropName {
                    span: index.byte_range(),
                    expr: Box::from(expr),
                }),
            })
        }
        "await_expression" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src)?;

            ExprKind::Await(Await {
                expr: Box::from(expr),
            })
        }
        "template_string" => ExprKind::TemplateLiteral(parse_template_string(node, src)?),
        "if_expression" => {
            return parse_if_expression(node, src);
        }
        "let_expression" => {
            let pat = node.child_by_field_name("name").unwrap();
            let pat = parse_refutable_pattern(&pat, src)?;
            let expr = node.child_by_field_name("value").unwrap();
            let expr = parse_expression(&expr, src)?;

            ExprKind::LetExpr(LetExpr {
                pat,
                expr: Box::from(expr),
            })
        }
        "do_expression" => {
            let child = node.named_child(0).unwrap();
            return parse_block_statement(&child, src);
        }
        "match_expression" => {
            let expr = node.child_by_field_name("expression").unwrap();
            let expr = parse_expression(&expr, src)?;

            let arms = node.child_by_field_name("arms").unwrap();
            let mut cursor = arms.walk();
            let arms = arms
                .named_children(&mut cursor)
                .into_iter()
                .map(|arm| parse_arm(&arm, src))
                .collect::<Result<Vec<Arm>, String>>()?;

            ExprKind::Match(Match {
                expr: Box::from(expr),
                arms,
            })
        }
        _ => {
            return Err(format!(
                "unhandled {node:#?} = '{}'",
                text_for_node(node, src)?
            ));
        }
    };

    Ok(Expr {
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })

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

fn parse_arm(node: &tree_sitter::Node, src: &str) -> Result<Arm, String> {
    let pat = node.child_by_field_name("pattern").unwrap();
    let body = node.child_by_field_name("value").unwrap();
    let body = match body.kind() {
        "statement_block" => parse_block_statement(&body, src)?,
        _ => parse_expression(&body, src)?,
    };

    let guard = if let Some(cond) = node.child_by_field_name("condition") {
        Some(parse_expression(&cond, src)?)
    } else {
        None
    };

    Ok(Arm {
        span: node.byte_range(),
        pattern: parse_refutable_pattern(&pat, src)?,
        guard,
        body,
    })
}

fn parse_refutable_pattern(node: &tree_sitter::Node, src: &str) -> Result<Pattern, String> {
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
                "_" => PatternKind::Wildcard(WildcardPat {}),
                _ => PatternKind::Ident(BindingIdent {
                    name,
                    mutable: false,
                    span: child.byte_range(),
                }),
            }
        }
        "refutable_array_pattern" => {
            let mut cursor = child.walk();
            let elems = child
                .named_children(&mut cursor)
                .into_iter()
                // TODO: make elems in ArrayPat non-optional
                .map(|elem| Ok(Some(parse_refutable_pattern(&elem, src)?)))
                .collect::<Result<Vec<_>, String>>()?;

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
                            key: Ident {
                                span: key_node.byte_range(),
                                name: key,
                            },
                            value: Box::from(value),
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
                        let mutable = node.child_by_field_name("mut").is_some();
                        Ok(ObjectPatProp::Assign(AssignPatProp {
                            span: prop.byte_range(),
                            key: BindingIdent {
                                span: prop.byte_range(),
                                name: text_for_node(&prop, src)?,
                                mutable,
                            },
                            value: None,
                        }))
                    }
                    kind => panic!("Unexected prop.kind() = {kind}"),
                })
                .collect::<Result<Vec<_>, String>>()?;

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
                id: Ident {
                    span: left.byte_range(),
                    name: text_for_node(&left, src)?,
                },
                is_id: Ident {
                    span: right.byte_range(),
                    name: text_for_node(&right, src)?,
                },
            })
        }
        kind => todo!("Unhandled refutable pattern of kind '{kind}'"),
    };

    Ok(Pattern {
        span: child.byte_range(),
        kind,
        inferred_type: None,
    })
}

fn parse_if_expression(node: &tree_sitter::Node, src: &str) -> Result<Expr, String> {
    assert_eq!(node.kind(), "if_expression");

    let condition = node.child_by_field_name("condition").unwrap();
    let condition = parse_expression(&condition, src)?;
    let consequent = node.child_by_field_name("consequence").unwrap();
    let consequent = parse_block_statement(&consequent, src)?;
    let alternate = if let Some(alt) = node.child_by_field_name("alternative") {
        let expr = match alt.kind() {
            "statement_block" => parse_block_statement(&alt, src),
            "else_clause" => {
                let else_clause = alt.named_child(0).unwrap();
                match else_clause.kind() {
                    "if_expression" => parse_if_expression(&else_clause, src),
                    "statement_block" => parse_block_statement(&else_clause, src),
                    kind => panic!("Unexpected else_clause child kind: '{kind}'"),
                }
            }
            kind => panic!("Unexpected alternative kind: '{kind}'"),
        };
        Some(Box::from(expr?))
    } else {
        None
    };

    let kind = ExprKind::IfElse(IfElse {
        cond: Box::from(condition),
        consequent: Box::from(consequent),
        alternate,
    });

    Ok(Expr {
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })
}

fn parse_type_ann(node: &tree_sitter::Node, src: &str) -> Result<TypeAnn, String> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(String::from("Error parsing type annotation"));
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
                span: node.byte_range(),
                keyword: Keyword::Number,
            }),
            "boolean" => TypeAnnKind::Keyword(KeywordType {
                span: node.byte_range(),
                keyword: Keyword::Boolean,
            }),
            "string" => TypeAnnKind::Keyword(KeywordType {
                span: node.byte_range(),
                keyword: Keyword::String,
            }),
            "symbol" => todo!(),
            "void" => todo!(),
            "unknown" => todo!(),
            "never" => todo!(),
            "object" => todo!("remove support for 'object'"),
            name => panic!("Unkwnown predefined_type: '{name}'"),
        },
        "type_identifier" => TypeAnnKind::TypeRef(TypeRef {
            span: node.byte_range(),
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
                .into_iter()
                .map(|arg| parse_type_ann(&arg, src))
                .collect::<Result<Vec<_>, String>>()?;

            TypeAnnKind::TypeRef(TypeRef {
                span: node.byte_range(),
                name: text_for_node(&name, src)?,
                type_args: Some(type_params),
            })
        }
        "object_type" => {
            let mut cursor = node.walk();
            let elems: Vec<TObjElem> = node
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
                        let name_node = prop.child_by_field_name("name").unwrap();
                        let name = text_for_node(&name_node, src)?;

                        let index_type_ann = prop.child_by_field_name("index_type").unwrap();
                        let index_type_ann = parse_type_ann(&index_type_ann, src)?;

                        let type_ann = prop.child_by_field_name("type").unwrap();
                        let type_ann = parse_type_ann(&type_ann, src)?;

                        let mut optional = false;
                        let mut cursor = prop.walk();
                        for child in prop.children(&mut cursor) {
                            if text_for_node(&child, src)? == "?" {
                                optional = true;
                            }
                        }

                        let pat: Pattern = Pattern {
                            span: name_node.byte_range(),
                            kind: PatternKind::Ident(BindingIdent {
                                name,
                                mutable: false,
                                span: name_node.byte_range(),
                            }),
                            inferred_type: None,
                        };

                        let elem = TObjElem::Index(TIndex {
                            span: prop.byte_range(),
                            key: Box::from(TypeAnnFnParam {
                                pat,
                                type_ann: index_type_ann,
                                optional,
                            }),
                            mutable: false, // TODO,
                            type_ann: Box::from(type_ann),
                        });
                        Ok(elem)
                    }
                    // TODO: remove method_signature, methods should look the same as properties
                    // The reason why JavaScript has both is that methods on object literals can
                    // access the other properties on the object using this.
                    "method_signature" => todo!("remove method_signature from object_type"),
                    kind => panic!("Unsupport prop kind in object_type: '{kind}'"),
                })
                .collect::<Result<Vec<TObjElem>, String>>()?;

            TypeAnnKind::Object(ObjectType {
                span: node.byte_range(),
                elems,
            })
        }
        "array_type" => {
            let elem_type = node.named_child(0).unwrap();
            let elem_type = parse_type_ann(&elem_type, src)?;

            TypeAnnKind::Array(ArrayType {
                span: node.byte_range(),
                elem_type: Box::from(elem_type),
            })
        }
        "tuple_type" => {
            let mut cursor = node.walk();
            let types = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|elem| {
                    println!("parsing elem: {elem:#?}");
                    parse_type_ann(&elem, src)
                })
                .collect::<Result<Vec<_>, String>>()?;

            TypeAnnKind::Tuple(TupleType {
                span: node.byte_range(),
                types,
            })
        }
        "flow_maybe_type" => todo!(),
        "type_query" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src)?;

            TypeAnnKind::Query(QueryType {
                span: node.byte_range(),
                expr: Box::from(expr),
            })
        }
        "index_type_query" => {
            let type_ann = node.named_child(0).unwrap();
            let type_ann = parse_type_ann(&type_ann, src)?;

            TypeAnnKind::KeyOf(KeyOfType {
                span: node.byte_range(),
                type_ann: Box::from(type_ann),
            })
        }
        // alias($.this, $.this_type),
        "existential_type" => todo!(),
        "literal_type" => {
            let child = node.named_child(0).unwrap();
            match child.kind() {
                "undefined" => TypeAnnKind::Keyword(KeywordType {
                    span: node.byte_range(),
                    keyword: Keyword::Undefined,
                }),
                "null" => TypeAnnKind::Keyword(KeywordType {
                    span: node.byte_range(),
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
                span: node.byte_range(),
                obj_type: Box::from(obj_type),
                index_type: Box::from(index_type),
            })
        }
        "conditional_type" => todo!(),
        "template_literal_type" => todo!(),
        "intersection_type" => {
            let mut cursor = node.walk();
            let types = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|t| parse_type_ann(&t, src))
                .collect::<Result<Vec<_>, String>>()?;
            TypeAnnKind::Intersection(IntersectionType {
                span: node.byte_range(),
                types,
            })
        }
        "union_type" => {
            let mut cursor = node.walk();
            let types = node
                .named_children(&mut cursor)
                .into_iter()
                .map(|t| parse_type_ann(&t, src))
                .collect::<Result<Vec<_>, String>>()?;
            TypeAnnKind::Union(UnionType {
                span: node.byte_range(),
                types,
            })
        }

        // Non-primary types
        "function_type" => {
            let params = node.child_by_field_name("parameters").unwrap();
            let mut cursor = params.walk();
            let params = params
                .named_children(&mut cursor)
                .into_iter()
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
                .collect::<Result<Vec<_>, String>>()?;

            let return_type = node.child_by_field_name("return_type").unwrap();
            let return_type = parse_type_ann(&return_type, src)?;

            let type_params = parse_type_params_for_node(&node, src)?;

            TypeAnnKind::Lam(LamType {
                span: node.byte_range(),
                params,
                ret: Box::from(return_type),
                type_params,
            })
        }
        "mutable_type" => {
            let type_ann = node.named_child(0).unwrap();

            TypeAnnKind::Mutable(MutableType {
                span: node.byte_range(),
                type_ann: Box::from(parse_type_ann(&type_ann, src)?),
            })
        }
        "constructor_type" => todo!(),
        "infer_type" => todo!(),

        kind => panic!("Unexpected type_annotation kind: '{kind}'"),
    };

    Ok(TypeAnn {
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

fn parse_type_params_for_node(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<Option<Vec<TypeParam>>, String> {
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
                        name: text_for_node(&name, src)?,
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
                .collect::<Result<Vec<_>, String>>()?;
            Some(type_params)
        }
        None => None,
    };

    Ok(type_params)
}

fn parse_literal(node: &tree_sitter::Node, src: &str) -> Result<Lit, String> {
    match node.kind() {
        "number" => Ok(Lit::num(
            src.get(node.byte_range()).unwrap().to_owned(),
            node.byte_range(),
        )),
        "string" => {
            let mut cursor = node.walk();
            let raw = join(
                node.named_children(&mut cursor)
                    .into_iter()
                    .map(|fragment_or_escape| text_for_node(&fragment_or_escape, src))
                    .collect::<Result<Vec<_>, String>>()?,
                "",
            );

            let cooked = unescape(&raw).unwrap();
            Ok(Lit::str(cooked, node.byte_range()))
        }
        "true" => Ok(Lit::bool(true, node.byte_range())),
        "false" => Ok(Lit::bool(false, node.byte_range())),
        "null" => todo!(),
        "undefined" => todo!(),
        kind => panic!("Unexpected literal kind: '{kind}'"),
    }
}

fn parse_jsx_attrs(node: &tree_sitter::Node, src: &str) -> Result<Vec<JSXAttr>, String> {
    let mut cursor = node.walk();
    let attrs = node.children_by_field_name("attribute", &mut cursor);
    attrs
        .into_iter()
        .map(|attr| {
            let ident = attr.named_child(0).unwrap();
            let ident = Ident {
                span: ident.byte_range(),
                name: text_for_node(&ident, src)?,
            };
            // TODO: handle JSX attr shorthand
            let value = attr.named_child(1).unwrap();
            let value = match value.kind() {
                "jsx_expression" => {
                    // TODO: handle None case
                    let expr = value.named_child(0).unwrap();
                    let expr = parse_expression(&expr, src)?;
                    JSXAttrValue::JSXExprContainer(JSXExprContainer {
                        span: value.byte_range(),
                        expr: Box::from(expr),
                    })
                }
                "string" => {
                    let lit = parse_literal(&value, src)?;
                    JSXAttrValue::Lit(lit)
                }
                kind => panic!("Unexpected JSX attr value with kind: '{kind}'"),
            };
            Ok(JSXAttr {
                span: attr.byte_range(),
                ident,
                value,
            })
        })
        .collect::<Result<Vec<_>, String>>()
}

fn parse_jsx_element(node: &tree_sitter::Node, src: &str) -> Result<JSXElement, String> {
    match node.kind() {
        "jsx_element" => {
            let mut cursor = node.walk();
            let children = node
                .named_children(&mut cursor)
                .into_iter()
                .filter(|child| {
                    child.kind() != "jsx_opening_element" && child.kind() != "jsx_closing_element"
                })
                .map(|child| match child.kind() {
                    "jsx_text" => Ok(JSXElementChild::JSXText(JSXText {
                        span: child.byte_range(),
                        value: text_for_node(&child, src)?,
                    })),
                    "jsx_element" | "jsx_self_closing_element" => Ok(JSXElementChild::JSXElement(
                        Box::from(parse_jsx_element(&child, src)?),
                    )),
                    "jsx_fragment" => todo!(),
                    "jsx_expression" => {
                        // TODO: handle None case
                        let expr = child.named_child(0).unwrap();
                        let expr = parse_expression(&expr, src)?;
                        Ok(JSXElementChild::JSXExprContainer(JSXExprContainer {
                            span: child.byte_range(),
                            expr: Box::from(expr),
                        }))
                    }
                    kind => panic!("Unexpected JSXElementChild kind: '{kind}'"),
                })
                .collect::<Result<Vec<_>, String>>()?;

            let open_tag = node.child_by_field_name("open_tag").unwrap();
            let name = open_tag.child_by_field_name("name").unwrap();

            Ok(JSXElement {
                span: node.byte_range(),
                name: text_for_node(&name, src)?,
                attrs: parse_jsx_attrs(&open_tag, src)?,
                children,
            })
        }
        "jsx_self_closing_element" => {
            let name = node.child_by_field_name("name").unwrap();
            Ok(JSXElement {
                span: node.byte_range(),
                name: text_for_node(&name, src)?,
                attrs: parse_jsx_attrs(node, src)?,
                children: vec![],
            })
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
        let add = (a, b) => a + b;
        let sub = (a, b) => a - b;
        let sum = add(5, 10);
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn numbers() {
        insta::assert_debug_snapshot!(parse("10;"));
        insta::assert_debug_snapshot!(parse("1.23;"));
        insta::assert_debug_snapshot!(parse("-10;"));
    }

    #[test]
    fn strings() {
        insta::assert_debug_snapshot!(parse(r#""";"#));
        insta::assert_debug_snapshot!(parse(r#""hello";"#));
        insta::assert_debug_snapshot!(parse("\"line 1\\nline 2\\nline 3\";"));
        insta::assert_debug_snapshot!(parse("\"a \\u2212 b\";"));
        insta::assert_debug_snapshot!(parse("\"hello, \\\"world\\\"!\";"));
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
        insta::assert_debug_snapshot!(parse("1 + 2 - 3;"));
        insta::assert_debug_snapshot!(parse("x * y / z;"));
        insta::assert_debug_snapshot!(parse("(a + b) * c;"));
        insta::assert_debug_snapshot!(parse("a == b;"));
        insta::assert_debug_snapshot!(parse("a != b;"));
        insta::assert_debug_snapshot!(parse("a > b;"));
        insta::assert_debug_snapshot!(parse("a >= b;"));
        insta::assert_debug_snapshot!(parse("a < b;"));
        insta::assert_debug_snapshot!(parse("a <= b;"));
        insta::assert_debug_snapshot!(parse("let cond = a != b;"));
        insta::assert_debug_snapshot!(parse("-a;"));
        insta::assert_debug_snapshot!(parse("-(a + b);"));
    }

    #[test]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c;"));
        insta::assert_debug_snapshot!(parse("() => 10;"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\";"));
        insta::assert_debug_snapshot!(parse("(a, ...b) => true;"));
        insta::assert_debug_snapshot!(parse("({x, y}) => x + y;"));
        insta::assert_debug_snapshot!(parse("({x: p, y: q}) => p + q;"));
        insta::assert_debug_snapshot!(parse("(a?: boolean, b?) => c;"));
    }

    #[test]
    fn rust_style_functions() {
        insta::assert_debug_snapshot!(parse("let foo = () => { let x = Math.random(); x };"));
        insta::assert_debug_snapshot!(parse("let bar = () => { let x = Math.random(); x; };"));
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
        insta::assert_debug_snapshot!(parse("async () => 10;"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 };"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b;"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar();"));
    }

    #[test]
    fn function_application() {
        insta::assert_debug_snapshot!(parse("foo();"));
        insta::assert_debug_snapshot!(parse("foo(a, b);"));
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\");"));
        insta::assert_debug_snapshot!(parse("f(x)(g(x));"));
        insta::assert_debug_snapshot!(parse("foo(a, ...b);"));
        let src = r#"
        let S = (f) => (g) => (x) => f(x)(g(x));
        let K = (x) => (y) => x;
        let I = S(K)(K);
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn declarations() {
        insta::assert_debug_snapshot!(parse("let x = 5;"));
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b;"));
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; x};"));
        // recursive
        insta::assert_debug_snapshot!(parse("let rec f = () => f();"));
        // mutable
        insta::assert_debug_snapshot!(parse(r#"let mut msg: string = "hello, world";"#));
        insta::assert_debug_snapshot!(parse(
            r#"
            let foo = () => {
                let mut msg: string = "hello, world";
                msg
            };
        "#
        ));
    }

    #[test]
    fn assignments() {
        insta::assert_debug_snapshot!(parse("x = 5;"));
        insta::assert_debug_snapshot!(parse("a.b = c;"));
        insta::assert_debug_snapshot!(parse("a[b] = c;"));
        insta::assert_debug_snapshot!(parse(r#"a["b"] = c;"#));
    }

    #[test]
    fn top_level_expressions() {
        insta::assert_debug_snapshot!(parse("a + b;"));
        insta::assert_debug_snapshot!(parse("123;\n\"hello\";"));
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if (true) { 5 } else { 10 };"));
        insta::assert_debug_snapshot!(parse("if (a) { 5 } else if (b) { 10 } else { 20 };"));
    }

    #[test]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10};"));
        insta::assert_debug_snapshot!(parse("let obj = {x, y};"));
        insta::assert_debug_snapshot!(parse("let obj = {a, b, ...others};"));
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
            "let elem = <div point={point} id=\"point\">Hello, {msg}</div>;"
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
        insta::assert_debug_snapshot!(parse("let x: number = 5;"));
        insta::assert_debug_snapshot!(parse("let msg: string = \"hello\";"));
        insta::assert_debug_snapshot!(parse("let add = (a: number, b: number): number => a + b;"));
        insta::assert_debug_snapshot!(parse("let p: Point = {x: 5, y: 10};"));
        insta::assert_debug_snapshot!(parse("let FOO: \"foo\" = \"foo\";"));
    }

    #[test]
    fn decls() {
        insta::assert_debug_snapshot!(parse("let x = 5;"));
        insta::assert_debug_snapshot!(parse("   let x = 5;")); // with leading whitespace
        insta::assert_debug_snapshot!(parse("declare let x: number;"));
        insta::assert_debug_snapshot!(parse("declare let foo: Foo<string>;"));
    }

    #[test]
    fn tuples() {
        insta::assert_debug_snapshot!(parse("let x = [];"));
        insta::assert_debug_snapshot!(parse("let x = [1, 2, 3];"));
        insta::assert_debug_snapshot!(parse("let x = [1, [a, b]];"));
        insta::assert_debug_snapshot!(parse("let foo = () => [a, b];"));
    }

    #[test]
    fn member_access() {
        insta::assert_debug_snapshot!(parse("a.b.c;"));
        insta::assert_debug_snapshot!(parse("foo.bar();"));
        insta::assert_debug_snapshot!(parse("p.x * p.x + p.y * p.y;"));
        insta::assert_debug_snapshot!(parse("foo().bar();"));
        insta::assert_debug_snapshot!(parse("arr[0][1];"));
        insta::assert_debug_snapshot!(parse("arr[x](y);"));
        insta::assert_debug_snapshot!(parse("arr[arr.length - 1];"));
        insta::assert_debug_snapshot!(parse("foo[bar[-1]];"));
    }

    #[test]
    fn type_decls() {
        insta::assert_debug_snapshot!(parse("type Num = number;"));
        insta::assert_debug_snapshot!(parse("type Point = {x: number, y: number};"));
        insta::assert_debug_snapshot!(parse("type Foo<T> = {bar: T};"));
        insta::assert_debug_snapshot!(parse("type Foo<T extends string> = {bar: T};"));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T = "foo"> = {bar: T};"#));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T extends string = "foo"> = {bar: T};"#));
        insta::assert_debug_snapshot!(parse("type CoordNames = keyof Point;"));
        insta::assert_debug_snapshot!(parse("type Foo = typeof foo;"));
        insta::assert_debug_snapshot!(parse("type FooBar = typeof foo.bar;"));
        insta::assert_debug_snapshot!(parse(r#"type C = A["b"][C_Key];"#));
        insta::assert_debug_snapshot!(parse("type Array<T> = {[key: number]: T};"));
        insta::assert_debug_snapshot!(parse("type MutPoint = mut {x: number, y: number};"));
        insta::assert_debug_snapshot!(parse("type MutPoint = {mut x: number, mut y: number};"));
    }

    #[test]
    fn blocks() {
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; x};"));
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; let y = 10; x + y};"));
        insta::assert_debug_snapshot!(parse("do {let x = 5; let y = 10; x + y};"));
        insta::assert_debug_snapshot!(parse(
            "do {let sum = do {let x = 5; let y = 10; x + y}; sum};"
        ));
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; console.log(x); x};"));
        insta::assert_debug_snapshot!(parse("let foo = do {console.log(x); x};"));
    }

    #[test]
    fn destructuring() {
        insta::assert_debug_snapshot!(parse("let {x, y} = point;"));
        insta::assert_debug_snapshot!(parse("let {a, b, ...rest} = letters;"));
        insta::assert_debug_snapshot!(parse("let {p0: {x, y}, p1: {x, y}} = line;"));
        insta::assert_debug_snapshot!(parse("let [a, b, ...rest] = letters;"));
        insta::assert_debug_snapshot!(parse("let [foo, ...[bar, ...rest]] = baz;"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]) => a;"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]: [string, number]) => a;"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}) => b;"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}: {a: string, b: number}) => b;"));
        insta::assert_debug_snapshot!(parse("let {mut x, y: mut z} = point;"));
        insta::assert_debug_snapshot!(parse("let foo = ({mut x, y: mut z}) => {};"));
        insta::assert_debug_snapshot!(parse("let [a, mut b, ...rest] = letters;"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, mut b, ...rest]) => {};"));
        // TODO: assigning defaults
        // insta::assert_debug_snapshot!(parse("let {x, mut y = 10} = point;"));
        // insta::assert_debug_snapshot!(parse("let [a, mut b = 98, ...rest] = letters;"));
        // TODO: disallowed patterns, e.g. top-level rest, non-top-level type annotations
    }

    #[test]
    fn array_spread() {
        insta::assert_debug_snapshot!(parse("let tuple = [...a, b];"));
        insta::assert_debug_snapshot!(parse("let tuple = [a, ...b];"));
        insta::assert_debug_snapshot!(parse("let tuple = [1, ...[2, 3]];"));
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
    fn types() {
        insta::assert_debug_snapshot!(parse("let get_bar = <T>(foo: Foo<T>) => foo.bar;"));
        insta::assert_debug_snapshot!(parse("declare let get_bar: (foo: Foo) => T;"));
        insta::assert_debug_snapshot!(parse("let str_arr: string[] = [];"));
        insta::assert_debug_snapshot!(parse("let thunk_arr: (() => undefined)[] = [];"));
        insta::assert_debug_snapshot!(parse("let arr: string[] | number[] = [];"));
        insta::assert_debug_snapshot!(parse(
            "declare let add: ((a: number, b: number) => number) & (a: string, b: string) => string;"
        ));
        insta::assert_debug_snapshot!(parse("let nested_arr: string[][] = [];"));
        let src = r#"
        type Event = 
          | {type: "mousedown", x: number, y: number}
          | {type: "keydown", key: string};
        "#;
        insta::assert_debug_snapshot!(parse(src));
        insta::assert_debug_snapshot!(parse("let mut_arr: mut string[] = [];"));
    }

    #[test]
    fn pattern_matching() {
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                {x, y: b, z: 5, ...rest} -> "object",
                [a, _, ...rest] -> "array",
                "string" -> "string",
                true -> "true",
                false -> "false",
                n -> "variable",
                _ -> "wildcard"
            };
        "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                {a: {b: {c}}} -> "object",
                _ -> "fallthrough"
            };              
            "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                n is number -> "number",
                {a: a is Array} -> "Array",
                _ -> "fallthrough"
            };
            "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                1 -> "one",
                2 -> "two",
                n if (n < 5) -> "few",
                _ -> "many"
            };
            "#
        ))
    }

    #[test]
    fn if_let() {
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = if (let {x, y: b, ...rest} = foo) {
                "object"
            } else if (let [a, _, ...rest] = foo) {
                "array"
            } else {
                "other"
            };              
        "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = if (let {x: x is string} = foo) {
                "object"
            } else if (let [a is Array, _, ...rest] = foo) {
                "array"
            } else {
                "other"
            };              
            "#
        ));
    }

    #[test]
    fn top_level_parse_error() {
        let result = parse(
            r#"
            let obj = {foo: "hello"};
            let foo = obj."#,
        );
        assert_eq!(
            result,
            Err(String::from("failed to parse: 'let foo = obj.'"))
        );
    }
}
