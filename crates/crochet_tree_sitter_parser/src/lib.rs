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
        "lexical_declaration" => parse_declaration(node, src),
        _ => todo!("unhandled: {:#?}", node),
    }
}

fn text_for_node(node: &tree_sitter::Node, src: &str) -> String {
    src.get(node.byte_range()).unwrap().to_owned()
}

fn parse_declaration(node: &tree_sitter::Node, src: &str) -> Vec<Statement> {
    let mut cursor = node.child(1).unwrap().walk();
    let declarators = node.named_children(&mut cursor);

    declarators
        .into_iter()
        .map(|decl| parse_declarator(&decl, src))
        .collect()
}

fn parse_declarator(node: &tree_sitter::Node, src: &str) -> Statement {
    let name = node.child_by_field_name("name").unwrap();
    let name = parse_pattern(&name, src);

    // We skip index:1 because that's the '='
    let init = node.child(2).unwrap();
    let init = parse_expression(&init, src);

    Statement::VarDecl {
        span: node.byte_range(),
        pattern: name,
        type_ann: None,
        init: Some(init),
        declare: false,
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
        "object_pattern" => todo!(),
        "array_pattern" => todo!(),
        "rest_pattern" => todo!(),
        _ => panic!("unrecognized pattern {node:#?}"),
    }
}

fn parse_func_param(node: &tree_sitter::Node, src: &str) -> EFnParamPat {
    match node.kind() {
        "identifier" => {
            let span = node.byte_range();
            let name = src.get(span.clone()).unwrap().to_owned();
            EFnParamPat::Ident(EFnParamBindingIdent {
                span: span.clone(),
                id: Ident { span, name },
            })
        }
        "object_pattern" => todo!(),
        "array_pattern" => todo!(),
        "rest_pattern" => todo!(),
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
            let name = param.child(0).unwrap();

            EFnParam {
                pat: parse_func_param(&name, src),
                type_ann: None,
                optional,
                mutable: false,
            }
        })
        .collect()
}

fn parse_expression(node: &tree_sitter::Node, src: &str) -> Expr {
    match node.kind() {
        "arrow_function" => {
            println!("arrow_function = {}", text_for_node(node, src));
            let is_async = match node.child_count() {
                3 => false,
                4 => true,
                _ => panic!("incorrect child_count for arrow function"),
            };

            // TODO: check if the body is a statement_block otherwise parse
            // as a simple expression
            let body = node.child_by_field_name("body").unwrap();
            let body = match body.kind() {
                "statement_block" => todo!(),
                _ => parse_expression(&body, src),
            };

            let params = node.child_by_field_name("parameters").unwrap();
            let params = parse_formal_parameters(&params, src);

            Expr::Lambda(Lambda {
                span: node.byte_range(),
                params,
                is_async,
                body: Box::from(body),
                return_type: None,
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
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                _ => todo!("Unhandle operator: {operator}"),
            };

            Expr::Op(Op {
                span: node.byte_range(),
                left,
                op,
                right,
            })
        }
        "call_expression" => {
            let func = node.child_by_field_name("function").unwrap();
            let func = parse_expression(&func, src);

            let args = node.child_by_field_name("arguments").unwrap();

            let mut cursor = args.walk();
            let args = args.named_children(&mut cursor);

            let args = args
                .into_iter()
                .map(|arg| {
                    let expr = parse_expression(&arg, src);
                    ExprOrSpread {
                        spread: None,
                        expr: Box::from(expr),
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
        "number" => Expr::Lit(Lit::num(
            src.get(node.byte_range()).unwrap().to_owned(),
            node.byte_range(),
        )),
        "string" => {
            // TODO: interpret escapes to generate "cooked" version of string
            let str_frag = node.child_by_field_name("string_fragment").unwrap();
            Expr::Lit(Lit::str(
                src.get(str_frag.byte_range()).unwrap().to_owned(),
                node.byte_range(),
            ))
        }
        "true" => Expr::Lit(Lit::bool(true, node.byte_range())),
        "false" => Expr::Lit(Lit::bool(false, node.byte_range())),
        // TODO: handle null and undefined
        _ => {
            todo!("unhandled {node:#?} = '{}'", text_for_node(node, src))
        }
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
}
