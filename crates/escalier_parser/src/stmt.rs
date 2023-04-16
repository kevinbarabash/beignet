use escalier_ast::values::*;

use crate::expr::parse_expression;
use crate::parse_error::ParseError;
use crate::pattern::*;
use crate::type_ann::parse_type_ann;
use crate::util::*;

pub fn parse_statement(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<Option<Statement>, ParseError> {
    let kind = match node.kind() {
        "lexical_declaration" => {
            return parse_declaration(node, false, src);
        }
        "class_declaration" => {
            return parse_class_decl(node, src);
        }
        "expression_statement" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src)?;
            StmtKind::ExprStmt(expr)
        }
        "ambient_declaration" => {
            let decl = node.named_child(0).unwrap();
            return parse_declaration(&decl, true, src);
        }
        "type_alias_declaration" => {
            let name = node.child_by_field_name("name").unwrap();
            let id = Ident {
                loc: SourceLocation::from(&name),
                span: name.byte_range(),
                name: text_for_node(&name, src)?,
            };
            let type_ann = node.child_by_field_name("value").unwrap();
            let type_ann = parse_type_ann(&type_ann, src)?;

            let type_params = parse_type_params_for_node(node, src)?;

            StmtKind::TypeDecl(TypeDecl {
                declare: false,
                id,
                type_ann,
                type_params,
            })
        }
        "for_in_statement" => {
            let left_node = node.child_by_field_name("left").unwrap();
            let right_node = node.child_by_field_name("right").unwrap();
            let body_node = node.child_by_field_name("body").unwrap();

            StmtKind::ForStmt(ForStmt {
                pattern: Box::from(parse_pattern(&left_node, src)?),
                expr: Box::from(parse_expression(&right_node, src)?),
                body: parse_block_statement(&body_node, src)?,
            })
        }
        "return_statement" => {
            let arg = match node.named_child(0) {
                Some(arg) => Some(Box::from(parse_expression(&arg, src)?)),
                None => None,
            };

            StmtKind::ReturnStmt(ReturnStmt { arg })
        }
        "comment" => {
            return Ok(None); // ignore comments
        }
        "ERROR" => {
            return Err(ParseError::from(format!(
                "failed to parse: '{}'",
                text_for_node(node, src)?
            )))
        }
        _ => return Err(ParseError::from(format!("unhandled: {:#?}", node))),
    };

    Ok(Some(Statement {
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        kind,
    }))

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

fn parse_declaration(
    node: &tree_sitter::Node,
    declare: bool,
    src: &str,
) -> Result<Option<Statement>, ParseError> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(ParseError::from("Error parsing declaration"));
    }

    let decl = node.child_by_field_name("decl").unwrap();
    let rec = node.child_by_field_name("rec").is_some();

    let name = decl.child_by_field_name("name").unwrap();
    let pattern = parse_pattern(&name, src)?;

    let type_ann = if let Some(type_ann) = decl.child_by_field_name("type") {
        Some(parse_type_ann(&type_ann, src)?)
    } else {
        None
    };

    let kind = if rec {
        // `let fib = fix((fib) => (n) => ...)`
        // TODO: Fix always wraps a lambda

        let value = decl.child_by_field_name("value").unwrap();

        let lambda_expr = Expr {
            loc: SourceLocation::from(&decl),
            span: decl.byte_range(),
            kind: ExprKind::Lambda(Lambda {
                params: vec![EFnParam {
                    pat: pattern.clone(),
                    type_ann: type_ann.clone(),
                    optional: false,
                }],
                body: BlockOrExpr::Expr(Box::from(parse_expression(&value, src)?)),
                is_async: false,
                return_type: None,
                type_params: None, // TODO: support type params on VarDecls
            }),
            inferred_type: None,
        };

        let fix = Expr {
            loc: SourceLocation::from(&decl),
            span: decl.byte_range(),
            kind: ExprKind::Fix(Fix {
                expr: Box::from(lambda_expr),
            }),
            inferred_type: None,
        };

        StmtKind::VarDecl(VarDecl {
            pattern,
            type_ann,
            init: Some(Box::from(fix)),
            declare,
        })
    } else {
        let init = if let Some(init) = decl.child_by_field_name("value") {
            Some(Box::from(parse_expression(&init, src)?))
        } else {
            None
        };

        StmtKind::VarDecl(VarDecl {
            pattern,
            type_ann,
            init,
            declare,
        })
    };

    Ok(Some(Statement {
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        kind,
    }))
}

fn parse_class_decl(node: &tree_sitter::Node, src: &str) -> Result<Option<Statement>, ParseError> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(ParseError::from("Error parsing declaration"));
    }

    let name_node = node.child_by_field_name("name").unwrap();
    let body_node = node.child_by_field_name("body").unwrap();

    let mut class_members: Vec<ClassMember> = vec![];
    let mut cursor = body_node.walk();
    for child in body_node.named_children(&mut cursor) {
        let kind = child.kind();

        eprintln!("child.kind() = {kind}");

        match kind {
            "method_definition" => {
                let name_node = child.child_by_field_name("name").unwrap();
                let key = Ident {
                    loc: SourceLocation::from(&name_node),
                    span: name_node.byte_range(),
                    name: text_for_node(&name_node, src)?,
                };

                let params = parse_formal_parameters(
                    &child.child_by_field_name("parameters").unwrap(),
                    src,
                )?;
                let body_node = child.child_by_field_name("body").unwrap();
                let body = parse_block_statement(&body_node, src)?;
                let return_type = match child.child_by_field_name("return_type") {
                    Some(type_ann) => Some(parse_type_ann(&type_ann, src)?),
                    None => None,
                };

                let kind = match child.child_by_field_name("kind") {
                    Some(kind) => match text_for_node(&kind, src)?.as_str() {
                        "get" => MethodKind::Getter,
                        "set" => MethodKind::Setter,
                        "*" => todo!(),
                        _ => panic!("Invalid method kind"), // TODO: report an error
                    },
                    None => MethodKind::Method,
                };

                // TODO: add support to the AST for these
                let is_static = child.child_by_field_name("static").is_some();
                let is_mutating = child.child_by_field_name("mut").is_some();

                let type_params = parse_type_params_for_node(&child, src)?;

                if key.name.as_str() == "constructor" {
                    class_members.push(ClassMember::Constructor(Constructor { params, body }));
                } else {
                    class_members.push(ClassMember::Method(ClassMethod {
                        key,
                        kind,
                        lambda: Lambda {
                            params,
                            body: BlockOrExpr::Block(body),
                            is_async: child.child_by_field_name("async").is_some(),
                            return_type,
                            type_params,
                        },
                        is_static,
                        is_mutating,
                    }))
                }
            }
            "public_field_definition" => {
                let name_node = child.child_by_field_name("name").unwrap();
                let key = Ident {
                    loc: SourceLocation::from(&name_node),
                    span: name_node.byte_range(),
                    name: text_for_node(&name_node, src)?,
                };

                let value = match child.child_by_field_name("value") {
                    Some(value) => Some(Box::from(parse_expression(&value, src)?)),
                    None => None,
                };
                let type_ann = match child.child_by_field_name("type") {
                    Some(type_ann) => Some(Box::from(parse_type_ann(&type_ann, src)?)),
                    None => None,
                };

                class_members.push(ClassMember::Prop(ClassProp {
                    key,
                    value,
                    type_ann,
                    is_static: child.child_by_field_name("static").is_some(),
                    is_optional: child.child_by_field_name("optional").is_some(),
                    is_mutable: child.child_by_field_name("mut").is_some(),
                }))
            }
            "private_field_definition" => todo!(),
            _ => todo!(),
        }
    }

    let stmt = Statement {
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        kind: StmtKind::ClassDecl(ClassDecl {
            ident: Ident {
                loc: SourceLocation::from(&name_node),
                span: name_node.byte_range(),
                name: text_for_node(&name_node, src)?,
            },
            class: Box::from(Class {
                ident: Ident {
                    loc: SourceLocation::from(&name_node),
                    span: name_node.byte_range(),
                    name: text_for_node(&name_node, src)?,
                },
                body: class_members,
                type_params: parse_type_params_for_node(node, src)?, // TODO
            }),
        }),
    };

    Ok(Some(stmt))
}

pub fn parse_block_statement(node: &tree_sitter::Node, src: &str) -> Result<Block, ParseError> {
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
                stmts.push(Statement {
                    loc: SourceLocation::from(&child),
                    span: child.byte_range(),
                    kind: StmtKind::ExprStmt(expr),
                })
            } else if let Some(result) = parse_statement(&child, src)? {
                stmts.push(result);
            }
        } else if let Some(result) = parse_statement(&child, src)? {
            stmts.push(result);
        }
    }

    Ok(Block {
        span: node.byte_range(),
        stmts,
    })
}
