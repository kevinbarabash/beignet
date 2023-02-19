use escalier_ast::values::*;

use crate::expr::parse_expression;
use crate::parse_error::ParseError;
use crate::pattern::*;
use crate::type_ann::parse_type_ann;
use crate::util::*;

pub fn parse_statement(node: &tree_sitter::Node, src: &str) -> Result<Vec<Statement>, ParseError> {
    let kind = node.kind();
    eprintln!("parse_statement: kind = {kind}");

    match node.kind() {
        "lexical_declaration" => parse_declaration(node, false, src),
        "class_declaration" => parse_class_decl(node, src),
        "expression_statement" => {
            let expr = node.named_child(0).unwrap();
            let expr = parse_expression(&expr, src)?;
            Ok(vec![Statement::Expr {
                loc: SourceLocation::from(node),
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
                loc: SourceLocation::from(&name),
                span: name.byte_range(),
                name: text_for_node(&name, src)?,
            };
            let type_ann = node.child_by_field_name("value").unwrap();
            let type_ann = parse_type_ann(&type_ann, src)?;

            let type_params = parse_type_params_for_node(node, src)?;

            Ok(vec![Statement::TypeDecl {
                loc: SourceLocation::from(node),
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
        "ERROR" => Err(ParseError::from(format!(
            "failed to parse: '{}'",
            text_for_node(node, src)?
        ))),
        _ => Err(ParseError::from(format!("unhandled: {:#?}", node))),
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

fn parse_declaration(
    node: &tree_sitter::Node,
    declare: bool,
    src: &str,
) -> Result<Vec<Statement>, ParseError> {
    eprintln!("node.kind = {}", node.kind());
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(ParseError::from("Error parsing declaration"));
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
            loc: SourceLocation::from(&decl),
            span: decl.byte_range(),
            kind: ExprKind::Lambda(Lambda {
                params: vec![EFnParam {
                    pat: pattern.clone(),
                    type_ann: type_ann.clone(),
                    optional: false,
                }],
                body: init.unwrap(),
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

        Statement::VarDecl {
            loc: SourceLocation::from(node),
            span: node.byte_range(),
            pattern,
            type_ann,
            init: Some(Box::from(fix)),
            declare,
        }
    } else {
        Statement::VarDecl {
            loc: SourceLocation::from(node),
            span: node.byte_range(),
            pattern,
            type_ann,
            init,
            declare,
        }
    };

    Ok(vec![stmt])
}

fn parse_class_decl(node: &tree_sitter::Node, src: &str) -> Result<Vec<Statement>, ParseError> {
    eprintln!("node.kind = {}", node.kind());
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
                let body = parse_block_statement(&child.child_by_field_name("body").unwrap(), src)?;
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
                    class_members.push(ClassMember::Constructor(Constructor {
                        params,
                        body: Box::from(body),
                    }));
                } else {
                    class_members.push(ClassMember::Method(ClassMethod {
                        key,
                        kind,
                        lambda: Lambda {
                            params,
                            body: Box::from(body),
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

    let stmt = Statement::ClassDecl {
        loc: SourceLocation::from(node),
        span: node.byte_range(),
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
    };

    Ok(vec![stmt])
}

pub fn parse_block_statement(node: &tree_sitter::Node, src: &str) -> Result<Expr, ParseError> {
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
                    loc: SourceLocation::from(&child),
                    span: child.byte_range(),
                    expr: Box::from(expr),
                })
            } else {
                let mut result = parse_statement(&child, src)?;
                stmts.append(&mut result);
            }
        } else {
            let mut result = parse_statement(&child, src)?;
            stmts.append(&mut result);
        }
    }

    let mut iter = stmts.iter().rev();

    let empty_expr = Box::from(Expr {
        loc: DUMMY_LOC,
        span: 0..0,
        kind: ExprKind::Empty,
        inferred_type: None,
    });

    // NOTE: If a declaration appears last it will go unused.  This is because
    // only expressions can be returned from blocks.
    // TODO: add a warning when that happens.
    let last: Expr = match iter.next() {
        Some(term) => match term {
            Statement::ClassDecl {
                loc,
                span,
                ident,
                class,
            } => Expr {
                loc: loc.to_owned(),
                span: span.to_owned(),
                kind: ExprKind::Let(Let {
                    pattern: Some(Pattern {
                        loc: ident.loc.to_owned(),
                        span: ident.span.to_owned(),
                        kind: PatternKind::Ident(BindingIdent {
                            loc: ident.loc.to_owned(),
                            span: ident.span.to_owned(),
                            name: ident.name.to_owned(),
                            mutable: false,
                        }),
                        inferred_type: None,
                    }),
                    init: Box::from(Expr {
                        loc: loc.to_owned(),
                        span: span.to_owned(),
                        kind: ExprKind::Class(class.as_ref().to_owned()),
                        inferred_type: None,
                    }),
                    type_ann: None,
                    body: empty_expr,
                }),
                inferred_type: None,
            },
            Statement::VarDecl {
                loc,
                span,
                pattern,
                init,
                type_ann,
                ..
            } => Expr {
                loc: loc.to_owned(),
                span: span.to_owned(),
                kind: ExprKind::Let(Let {
                    pattern: Some(pattern.to_owned()),
                    // TODO: Think about how to deal with variable declarations
                    // without initializers.  Right now these are allowed by our
                    // tree-sitter grammar, but aren't handled by the rust code
                    // which processes its CST.  We probably don't want to allow
                    // this for normal `let` declarations, we need to allow this
                    // for `declare let`.
                    init: init.as_ref().unwrap().to_owned(),
                    type_ann: type_ann.to_owned(),
                    body: empty_expr,
                }),
                inferred_type: None,
            },
            Statement::TypeDecl { .. } => {
                // We can't convert this `let` expression so we can't include it
                // in the `body`.  I think we'll want move the conversion of block
                // statements to lambdas later in the process.  This will allow us
                // to handle type declarations within the body of a function.  It
                // will also help with handling statements like loops.
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { expr, .. } => *expr.to_owned(),
        },
        None => Expr {
            loc: DUMMY_LOC,
            span: 0..0,
            kind: ExprKind::Empty,
            inferred_type: None,
        },
    };

    let result: Expr = iter.fold(last, |body, stmt| {
        match stmt {
            Statement::ClassDecl {
                loc,
                span,
                ident,
                class,
            } => Expr {
                loc: loc.to_owned(),
                span: span.to_owned(),
                kind: ExprKind::Let(Let {
                    pattern: Some(Pattern {
                        loc: ident.loc.to_owned(),
                        span: ident.span.to_owned(),
                        kind: PatternKind::Ident(BindingIdent {
                            loc: ident.loc.to_owned(),
                            span: ident.span.to_owned(),
                            name: ident.name.to_owned(),
                            mutable: false,
                        }),
                        inferred_type: None,
                    }),
                    init: Box::from(Expr {
                        loc: loc.to_owned(),
                        span: span.to_owned(),
                        kind: ExprKind::Class(class.as_ref().to_owned()),
                        inferred_type: None,
                    }),
                    type_ann: None,
                    body: Box::new(body),
                }),
                inferred_type: None,
            },
            Statement::VarDecl {
                loc,
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
                    loc: loc.to_owned(),
                    span: span.to_owned(),
                    kind,
                    inferred_type: None,
                }
            }
            Statement::TypeDecl { .. } => {
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { loc, span, expr } => {
                let kind = ExprKind::Let(Let {
                    pattern: None,
                    type_ann: None,
                    init: expr.to_owned(),
                    body: Box::new(body),
                });
                Expr {
                    loc: loc.to_owned(),
                    span: span.to_owned(),
                    kind,
                    inferred_type: None,
                }
            }
        }
    });

    Ok(result)
}

pub fn parse_block_statement_as_vec(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<Vec<Expr>, ParseError> {
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
                    loc: SourceLocation::from(&child),
                    span: child.byte_range(),
                    expr: Box::from(expr),
                })
            } else {
                let mut result = parse_statement(&child, src)?;
                stmts.append(&mut result);
            }
        } else {
            let mut result = parse_statement(&child, src)?;
            stmts.append(&mut result);
        }
    }

    let mut result: Vec<Expr> = vec![];

    for stmt in stmts {
        let expr = match stmt {
            Statement::ClassDecl {
                loc,
                span,
                ident,
                class,
            } => Expr {
                loc: loc.to_owned(),
                span: span.to_owned(),
                kind: ExprKind::LetDecl(LetDecl {
                    pattern: Pattern {
                        loc: ident.loc.to_owned(),
                        span: ident.span.to_owned(),
                        kind: PatternKind::Ident(BindingIdent {
                            loc: ident.loc.to_owned(),
                            span: ident.span.to_owned(),
                            name: ident.name.to_owned(),
                            mutable: false,
                        }),
                        inferred_type: None,
                    },
                    init: Box::from(Expr {
                        loc: loc.to_owned(),
                        span: span.to_owned(),
                        kind: ExprKind::Class(class.as_ref().to_owned()),
                        inferred_type: None,
                    }),
                    type_ann: None,
                    // body: empty_expr,
                }),
                inferred_type: None,
            },
            Statement::VarDecl {
                loc,
                span,
                pattern,
                init,
                type_ann,
                ..
            } => Expr {
                loc: loc.to_owned(),
                span: span.to_owned(),
                kind: ExprKind::LetDecl(LetDecl {
                    pattern: pattern.to_owned(),
                    // TODO: Think about how to deal with variable declarations
                    // without initializers.  Right now these are allowed by our
                    // tree-sitter grammar, but aren't handled by the rust code
                    // which processes its CST.  We probably don't want to allow
                    // this for normal `let` declarations, we need to allow this
                    // for `declare let`.
                    init: init.as_ref().unwrap().to_owned(),
                    type_ann: type_ann.to_owned(),
                    // body: empty_expr,
                }),
                inferred_type: None,
            },
            Statement::TypeDecl { .. } => {
                // We can't convert this `let` expression so we can't include it
                // in the `body`.  I think we'll want move the conversion of block
                // statements to lambdas later in the process.  This will allow us
                // to handle type declarations within the body of a function.  It
                // will also help with handling statements like loops.
                todo!("decide how to handle type decls within BlockStatements")
            }
            Statement::Expr { expr, .. } => *expr.to_owned(),
        };
        result.push(expr);
    }

    Ok(result)
}
