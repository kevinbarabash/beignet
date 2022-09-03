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
        "lexical_declaration" => parse_declaration(node, src),
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
            let mut cursor = node.walk();
            let raw = join(
                node.named_children(&mut cursor)
                    .into_iter()
                    .map(|fragment_or_escape| text_for_node(&fragment_or_escape, src)),
                "",
            );

            let cooked = unescape(&raw).unwrap();
            Expr::Lit(Lit::str(cooked, node.byte_range()))
        }
        "true" => Expr::Lit(Lit::bool(true, node.byte_range())),
        "false" => Expr::Lit(Lit::bool(false, node.byte_range())),
        // TODO: handle null and undefined
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
    #[ignore]
    fn template_literals() {
        insta::assert_debug_snapshot!(parse("`Hello, world`"));
        insta::assert_debug_snapshot!(parse("`Hello, ${name}`"));
        insta::assert_debug_snapshot!(parse("`(${x}, ${y})`"));
        insta::assert_debug_snapshot!(parse("`Hello, \"world\"`"));
        insta::assert_debug_snapshot!(parse("`foo ${`bar ${baz}`}`"));
        insta::assert_debug_snapshot!(parse("sql`SELECT * FROM ${table} WHERE id = ${id}`"));
        insta::assert_debug_snapshot!(parse("`line 1\\nline 2\\nline 3`"));
        insta::assert_debug_snapshot!(parse("`a \\u2212 b`"));
        insta::assert_debug_snapshot!(parse(r#"if cond { `${foo}` } else { `${bar}` }"#));
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
    #[ignore]
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
    #[ignore]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"));
        insta::assert_debug_snapshot!(parse("() => 10"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""));
        insta::assert_debug_snapshot!(parse("(a, ...b) => true"));
        insta::assert_debug_snapshot!(parse("({x, y}) => x + y"));
        insta::assert_debug_snapshot!(parse("({x: p, y: q}) => p + q"));
        insta::assert_debug_snapshot!(parse("(a?: bool, b?) => c"));
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
    #[ignore]
    fn async_await() {
        insta::assert_debug_snapshot!(parse("async () => 10"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"));
    }

    #[test]
    #[ignore]
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
    #[ignore]
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
    #[ignore]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let obj = {x, y}"));
        insta::assert_debug_snapshot!(parse("let obj = {a, b, ...others}"));
    }

    #[test]
    #[ignore]
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
    #[ignore]
    fn type_annotations() {
        insta::assert_debug_snapshot!(parse("let x: number = 5"));
        insta::assert_debug_snapshot!(parse("let msg: string = \"hello\""));
        insta::assert_debug_snapshot!(parse("let add = (a: number, b: number) => a + b"));
        insta::assert_debug_snapshot!(parse("let p: Point = {x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let FOO: \"foo\" = \"foo\""));
    }

    #[test]
    #[ignore]
    fn decls() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("   let x = 5")); // with leading whitespace
        insta::assert_debug_snapshot!(parse("declare let x: number"));
        insta::assert_debug_snapshot!(parse("declare let foo: Foo<string>"));
    }

    #[test]
    #[ignore]
    fn tuples() {
        insta::assert_debug_snapshot!(parse("let x = []"));
        insta::assert_debug_snapshot!(parse("let x = [1, 2, 3]"));
        insta::assert_debug_snapshot!(parse("let x = [1, [a, b]]"));
        insta::assert_debug_snapshot!(parse("let foo = () => [a, b]"));
    }

    #[test]
    #[ignore]
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
    #[ignore]
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
    #[ignore]
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
        insta::assert_debug_snapshot!(parse("let [_, b] = letters"));
        // TODO: assigning defaults
        // TODO: type annotations
        // TODO: function params
        // TODO: disallowed patterns, e.g. top-level rest, non-top-level type annotations
    }

    #[test]
    #[ignore]
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
