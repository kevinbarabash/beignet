use unescape::unescape;

use crate::ast::*;
use crate::parser::literal::parse_literal;
use crate::parser::parse_error::ParseError;
use crate::parser::pattern::*;
use crate::parser::stmt::*;
use crate::parser::type_ann::parse_type_ann;
use crate::parser::util::*;

pub fn parse_expression(node: &tree_sitter::Node, src: &str) -> Result<Expr, ParseError> {
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
                "statement_block" => BlockOrExpr::Block(parse_block_statement(&body, src)?),
                _ => BlockOrExpr::Expr(Box::from(parse_expression(&body, src)?)),
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
                body,
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
                "&&" => BinOp::And,
                "||" => BinOp::Or,
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
                "!" => UnaryOp::Not,
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
            let body = parse_block_statement(&child, src)?;
            ExprKind::DoExpr(DoExpr { body })
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
        "regex" => {
            let flags = match node.child_by_field_name("flags") {
                Some(flags_node) => Some(text_for_node(&flags_node, src)?),
                None => None,
            };
            let pattern_node = node.child_by_field_name("pattern").unwrap();
            let pattern = text_for_node(&pattern_node, src)?;

            ExprKind::Regex(Regex { pattern, flags })
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

fn parse_if_expression(node: &tree_sitter::Node, src: &str) -> Result<Expr, ParseError> {
    assert_eq!(node.kind(), "if_expression");

    let condition = node.child_by_field_name("condition").unwrap();
    let condition = parse_expression(&condition, src)?;
    let consequent = node.child_by_field_name("consequence").unwrap();
    let consequent = parse_block_statement(&consequent, src)?;
    let alternate = if let Some(alt) = node.child_by_field_name("alternative") {
        let exprs = match alt.kind() {
            "statement_block" => parse_block_statement(&alt, src)?,
            "else_clause" => {
                let else_clause = alt.named_child(0).unwrap();
                match else_clause.kind() {
                    "if_expression" => Block {
                        span: else_clause.byte_range(),
                        stmts: vec![Statement {
                            loc: SourceLocation::from(&else_clause),
                            span: else_clause.byte_range(),
                            kind: StmtKind::ExprStmt(parse_if_expression(&else_clause, src)?),
                            inferred_type: None,
                        }],
                    },
                    "statement_block" => parse_block_statement(&else_clause, src)?,
                    kind => panic!("Unexpected else_clause child kind: '{kind}'"),
                }
            }
            kind => panic!("Unexpected alternative kind: '{kind}'"),
        };
        Some(exprs)
    } else {
        None
    };

    let kind = ExprKind::IfElse(IfElse {
        cond: Box::from(condition),
        consequent,
        alternate,
    });

    Ok(Expr {
        loc: SourceLocation::from(node),
        span: node.byte_range(),
        kind,
        inferred_type: None,
    })
}

fn parse_arm(node: &tree_sitter::Node, src: &str) -> Result<Arm, ParseError> {
    let pat = node.child_by_field_name("pattern").unwrap();
    let body = node.child_by_field_name("value").unwrap();
    let body = match body.kind() {
        "statement_block" => parse_block_statement(&body, src)?,
        _ => Block {
            span: body.byte_range(),
            stmts: vec![Statement {
                loc: SourceLocation::from(&body),
                span: body.byte_range(),
                kind: StmtKind::ExprStmt(parse_expression(&body, src)?),
                inferred_type: None,
            }],
        },
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
