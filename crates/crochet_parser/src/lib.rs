use itertools::free::join;
#[cfg(target_family = "wasm")]
use std::ffi::CString;
use std::os::raw::c_char;
use unescape::unescape;

use crochet_ast::values::*;

mod parse_error;

pub use parse_error::ParseError;

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
    eprintln!("{}", str);
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
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
            let mut stmts = parse_statement(&child, src)?;
            body.append(&mut stmts);
        }

        Ok(Program { body })
    } else {
        Err(ParseError::from("not implemented yet"))
    }
}

fn parse_statement(node: &tree_sitter::Node, src: &str) -> Result<Vec<Statement>, ParseError> {
    let kind = node.kind();
    println!("parse_statement: kind = {kind}");

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

// TODO: replace with node.utf8_text()
fn text_for_node(node: &tree_sitter::Node, src: &str) -> Result<String, ParseError> {
    match src.get(node.byte_range()) {
        Some(text) => Ok(text.to_owned()),
        None => Err(ParseError::from("error in text_for_node")),
    }
}

fn parse_declaration(
    node: &tree_sitter::Node,
    declare: bool,
    src: &str,
) -> Result<Vec<Statement>, ParseError> {
    println!("node.kind = {}", node.kind());
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
    println!("node.kind = {}", node.kind());
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

        println!("child.kind() = {kind}");

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
                // TODO: add support to the AST for this
                let _mutable = child.child_by_field_name("mut").is_some();

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

fn parse_pattern(node: &tree_sitter::Node, src: &str) -> Result<Pattern, ParseError> {
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

fn parse_formal_parameters(
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

fn parse_block_statement(node: &tree_sitter::Node, src: &str) -> Result<Expr, ParseError> {
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

fn parse_template_string(
    node: &tree_sitter::Node,
    src: &str,
) -> Result<TemplateLiteral, ParseError> {
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
        let loc = SourceLocation::from(&child);

        let raw = src.get(span.clone()).unwrap().to_owned();
        let cooked = unescape(&raw).unwrap();

        let raw = Lit::str(raw, span.clone(), loc);
        let cooked = Lit::str(cooked, span.clone(), loc);

        quasis.push(TemplateElem {
            loc,
            span,
            raw,
            cooked,
        });

        start = child.byte_range().end;
    }

    let end = node.byte_range().end - 1;
    let span = start..end;
    let loc = SourceLocation::from(node);

    let raw = src.get(span.clone()).unwrap().to_owned();
    let cooked = unescape(&raw).unwrap();

    let raw = Lit::str(raw, span.clone(), loc);
    let cooked = Lit::str(cooked, span.clone(), loc);

    quasis.push(TemplateElem {
        loc,
        span,
        raw,
        cooked,
    });

    Ok(TemplateLiteral { exprs, quasis })
}

fn parse_expression(node: &tree_sitter::Node, src: &str) -> Result<Expr, ParseError> {
    if node.has_error() {
        // TODO: get actual error node so that we can report where the error is
        return Err(ParseError::from("Error parsing expression"));
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
                        loc: SourceLocation::from(node),
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
                .collect::<Result<Vec<_>, ParseError>>()?;

            let type_args = match node.child_by_field_name("type_arguments") {
                Some(type_args) => {
                    let mut cursor = type_args.walk();
                    Some(
                        type_args
                            .named_children(&mut cursor)
                            .into_iter()
                            .map(|arg| parse_type_ann(&arg, src))
                            .collect::<Result<Vec<_>, ParseError>>()?,
                    )
                }
                None => None,
            };

            // TODO: handle template string
            ExprKind::App(App {
                lam: Box::from(func),
                args,
                type_args,
            })
        }
        "new_expression" => {
            let constructor = node.child_by_field_name("constructor").unwrap();
            let constructor = parse_expression(&constructor, src)?;

            let args = node.child_by_field_name("arguments").unwrap();

            let mut cursor = args.walk();
            let args = args.named_children(&mut cursor);

            // TODO: dedupe with `call_expression`
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
                .collect::<Result<Vec<_>, ParseError>>()?;

            let type_args = match node.child_by_field_name("type_arguments") {
                Some(type_args) => {
                    let mut cursor = type_args.walk();
                    Some(
                        type_args
                            .named_children(&mut cursor)
                            .into_iter()
                            .map(|arg| parse_type_ann(&arg, src))
                            .collect::<Result<Vec<_>, ParseError>>()?,
                    )
                }
                None => None,
            };

            ExprKind::New(New {
                expr: Box::from(constructor),
                args,
                type_args,
            })
        }
        "identifier" => {
            let span = node.byte_range();
            let loc = SourceLocation::from(node);
            let name = src.get(span.clone()).unwrap().to_owned();
            ExprKind::Ident(Ident { loc, span, name })
        }
        "number" | "string" | "true" | "false" => {
            let lit = parse_literal(node, src)?;
            ExprKind::Lit(lit)
        }
        "null" => ExprKind::Keyword(Keyword::Null),
        "undefined" => ExprKind::Keyword(Keyword::Undefined),
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
                        let key = Ident {
                            loc: SourceLocation::from(&key_node),
                            span: key_node.byte_range(),
                            name: text_for_node(&key_node, src)?,
                        };
                        let value = child.child_by_field_name("value").unwrap();
                        let value = parse_expression(&value, src)?;
                        Ok(PropOrSpread::Prop(Box::from(Prop::KeyValue(
                            KeyValueProp {
                                key,
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
                            loc: SourceLocation::from(&child),
                            span: child.byte_range(),
                            name,
                        }))))
                    }
                    kind => panic!("Unexpect object property kind: {kind}"),
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

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
                .collect::<Result<Vec<_>, ParseError>>()?;

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
                    loc: SourceLocation::from(&prop),
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
                    loc: SourceLocation::from(&index),
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
                .collect::<Result<Vec<Arm>, ParseError>>()?;

            ExprKind::Match(Match {
                expr: Box::from(expr),
                arms,
            })
        }
        _ => {
            return Err(ParseError::from(format!(
                "unhandled {node:#?} = '{}'",
                text_for_node(node, src)?
            )));
        }
    };

    Ok(Expr {
        loc: SourceLocation::from(node),
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

fn parse_arm(node: &tree_sitter::Node, src: &str) -> Result<Arm, ParseError> {
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
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        pattern: parse_refutable_pattern(&pat, src)?,
        guard,
        body,
    })
}

fn parse_refutable_pattern(node: &tree_sitter::Node, src: &str) -> Result<Pattern, ParseError> {
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

fn parse_if_expression(node: &tree_sitter::Node, src: &str) -> Result<Expr, ParseError> {
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
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })
}

fn parse_type_ann(node: &tree_sitter::Node, src: &str) -> Result<TypeAnn, ParseError> {
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
                .into_iter()
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
                .into_iter()
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
                .into_iter()
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

fn parse_type_params_for_node(
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

fn parse_literal(node: &tree_sitter::Node, src: &str) -> Result<Lit, ParseError> {
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

fn parse_jsx_attrs(node: &tree_sitter::Node, src: &str) -> Result<Vec<JSXAttr>, ParseError> {
    let mut cursor = node.walk();
    let attrs = node.children_by_field_name("attribute", &mut cursor);
    attrs
        .into_iter()
        .map(|attr| {
            let ident_node = attr.named_child(0).unwrap();
            let ident = Ident {
                loc: SourceLocation::from(&ident_node),
                span: ident_node.byte_range(),
                name: text_for_node(&ident_node, src)?,
            };
            // TODO: handle JSX attr shorthand
            let value = attr.named_child(1).unwrap();
            let value = match value.kind() {
                "jsx_expression" => {
                    // TODO: handle None case
                    let expr = value.named_child(0).unwrap();
                    let expr = parse_expression(&expr, src)?;
                    JSXAttrValue::JSXExprContainer(JSXExprContainer {
                        loc: SourceLocation::from(&value),
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
                loc: SourceLocation::from(&attr),
                span: attr.byte_range(),
                ident,
                value,
            })
        })
        .collect::<Result<Vec<_>, ParseError>>()
}

fn parse_jsx_element(node: &tree_sitter::Node, src: &str) -> Result<JSXElement, ParseError> {
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
                        loc: SourceLocation::from(&child),
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
                            loc: SourceLocation::from(&child),
                            span: child.byte_range(),
                            expr: Box::from(expr),
                        }))
                    }
                    kind => panic!("Unexpected JSXElementChild kind: '{kind}'"),
                })
                .collect::<Result<Vec<_>, ParseError>>()?;

            let open_tag = node.child_by_field_name("open_tag").unwrap();
            let name = open_tag.child_by_field_name("name").unwrap();

            Ok(JSXElement {
                loc: SourceLocation::from(node),
                span: node.byte_range(),
                name: text_for_node(&name, src)?,
                attrs: parse_jsx_attrs(&open_tag, src)?,
                children,
            })
        }
        "jsx_self_closing_element" => {
            let name = node.child_by_field_name("name").unwrap();
            Ok(JSXElement {
                loc: SourceLocation::from(node),
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

    pub fn messages(report: &ParseError) -> String {
        report.to_string()
    }

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
    fn multiple_rest_params() {
        match parse("(...a, ...b) => true") {
            Ok(_) => panic!("expected test parse() to return an error"),
            Err(report) => {
                assert_eq!(
                    messages(&report),
                    "ParseError: failed to parse: '(...a, ...b) => true'"
                );
            }
        }
    }

    // #[test]
    // #[ignore]
    // fn optional_params_must_appear_last() {
    //     assert_eq!(
    //         parse("(a?, b) => true"),
    //         Err(ParseError::from("optional params must come last")),
    //     );
    // }

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
    fn new_expression() {
        insta::assert_debug_snapshot!(parse("new Array();"));
        insta::assert_debug_snapshot!(parse("new Array(1, 2, 3);"));
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

    // #[test]
    // #[ignore]
    // fn jsx_head_and_tail_must_match() {
    //     assert_eq!(
    //         parse("<Foo>Hello</Bar>"),
    //         Err(ParseError::from("JSX head and tail elements must match")),
    //     );
    // }

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
        insta::assert_debug_snapshot!(parse("let {x, mut y = 10} = point;"));
        insta::assert_debug_snapshot!(parse("let [a, mut b = 98, ...rest] = letters;"));
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
    fn object_types() {
        insta::assert_debug_snapshot!(parse("type Pick<T, K extends keyof T> = {[P in K]: T[P]};"));
        insta::assert_debug_snapshot!(parse("type Foo<T> = {mut [P in keyof T]?: T[P]};"));
        insta::assert_debug_snapshot!(parse("type Bar<T> = {+mut [P in keyof T]+?: T[P]};"));
        insta::assert_debug_snapshot!(parse("type Baz<T> = {-mut [P in keyof T]-?: T[P]};"));
    }

    #[test]
    fn conditional_types() {
        insta::assert_debug_snapshot!(parse(
            r#"type GetTypeName<T extends number | string> = T extends number ? "number" : "string";"#
        ));
        insta::assert_debug_snapshot!(parse(
            "type Flatten<Type> = Type extends Array<infer Item> ? Item : Type;"
        ));
        insta::assert_debug_snapshot!(parse(
            r#"type GetReturnType<Type> = Type extends (...args: never[]) => infer Return
                ? Return
                : never;
        "#
        ));
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
    fn classes() {
        insta::assert_debug_snapshot!(parse("class Foo { a: number; }"));
        insta::assert_debug_snapshot!(parse("class Foo { a: number = 5; }"));
        insta::assert_debug_snapshot!(parse("class Foo { a = 5; }"));
        insta::assert_debug_snapshot!(parse("class Foo { static a: number; }"));
        insta::assert_debug_snapshot!(parse("class Foo { foo() {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { foo(): string {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { static foo(): string {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { get foo() {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { set foo(x) {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { constructor(x) {} }"));

        let src = r#"
        class Foo {
            constructor(self, x) {
                self.x = x;
            }
            bar(self, y) {
                self.x + y;
            }
            baz(mut self, x) {
                self.x = x;
            }
        }
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    // #[test]
    // fn top_level_parse_error() {
    //     let result = parse(
    //         r#"
    //         let obj = {foo: "hello"};
    //         let foo = obj."#,
    //     );
    //     assert_eq!(
    //         result,
    //         Err(ParseError::from("failed to parse: 'let foo = obj.'"))
    //     );
    // }
}
