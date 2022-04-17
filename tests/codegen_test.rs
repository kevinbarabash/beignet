use chumsky::prelude::*;
use test_case::test_case;

use nouveau_lib::codegen::codegen_prog;
use nouveau_lib::parser::token_parser;
use nouveau_lib::lexer::lexer;
use nouveau_lib::js_builder::build_js;
use nouveau_lib::js_printer::print_js;

#[test_case("\"hello\"", "\"hello\""; "string literal")]
#[test_case("123", "123"; "number literal (whole)")]
#[test_case("foo(a, b)", "foo(a, b)"; "function call with multiple args")]
#[test_case("foo(a)", "foo(a)"; "function call with a single arg")]
#[test_case("foo()", "foo()"; "function call with no args")]
#[test_case("(a, b) => a + b", "(a, b) => a + b"; "lambda with two args")]
#[test_case("(a) => a", "(a) => a"; "lambda with one arg")]
#[test_case("() => 5", "() => 5"; "lambda with no args")]
#[test_case("a + b * c", "a + b * c"; "mixed operations")]
#[test_case("(a + b) * c", "(a + b) * c"; "mixed operations requiring parens")]
#[test_case("(a + b) * (c - d)", "(a + b) * (c - d)"; "mixed operations requiring multiple parens")]
#[test_case("a / b / c", "a / b / c"; "division without parens")]
#[test_case("a / (b / c)", "a / (b / c)"; "division with parens")]
#[test_case("a - b - c", "a - b - c"; "subtraction without parens")]
#[test_case("a - (b - c)", "a - (b - c)"; "subtraction with parens")]
#[test_case("let add = (a, b) => a + b", "var add = (a, b) => a + b"; "function declaration")]
#[test_case("let five = 5", "var five = 5"; "variable declaration with literal value")]
#[test_case("let foo = let x = 5 in x", "var foo = {\nvar x = 5;\nreturn x;\n}"; "let-in expression inside declaration")]
#[test_case("let foo = let x = 5 in let y = 10 in x + y", "var foo = {\nvar x = 5;\nvar y = 10;\nreturn x + y;\n}"; "nested let-in expressions inside declaration")]
// TODO: add a test case with multiple declarations
fn parse_then_codegen(input: &str, output: &str) {
    let result = lexer().parse(input).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();
    let result: String = codegen_prog(&prog);

    assert_eq!(result, output);
}

#[test]
fn js_print_simple_lambda() {
    let result = lexer().parse("let add = (a, b) => a + b").unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();

    let js_tree = build_js(&prog);
    let js_output = print_js(&js_tree);

    insta::assert_snapshot!(js_output, @r###"
    const add = (a, b) => {
        return a + b;
    };
    "###);
}

#[test]
fn js_print_let_in() {
    let result = lexer().parse("let foo = let x = 5 in let y = 10 in x + y").unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();

    let js_tree = build_js(&prog);
    let js_output = print_js(&js_tree);

    insta::assert_snapshot!(js_output, @r###"
    const foo = (() => {
        const x = 5;
        const y = 10;
        return x + y;
    })();
    "###);
}

#[test]
fn js_print_variable_shadowing() {
    let result = lexer().parse("let foo = let x = 5 in let x = 10 in x").unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();

    let js_tree = build_js(&prog);
    let js_output = print_js(&js_tree);

    insta::assert_snapshot!(js_output, @r###"
    const foo = (() => {
        const x = 5;
        const x = 10;
        return x;
    })();
    "###);
}

#[test]
fn js_print_let_in_inside_lambda() {
    let result = lexer().parse("let foo = () => let x = 5 in let y = 10 in x + y").unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();

    let js_tree = build_js(&prog);
    let js_output = print_js(&js_tree);

    // TODO: check if the body of a lambda is a Let and unwrap the resulting IIFE
    insta::assert_snapshot!(js_output, @r###"
    const foo = () => {
        const x = 5;
        const y = 10;
        return x + y;
    };
    "###);
}

#[test]
fn js_print_nested_lambdas() {
    let result = lexer().parse("let foo = (a) => (b) => a + b").unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();

    let js_tree = build_js(&prog);
    let js_output = print_js(&js_tree);

    // TODO: check if the body of a lambda is a Let and unwrap the resulting IIFE
    insta::assert_snapshot!(js_output, @r###"
    const foo = (a) => {
        return (b) => {
            return a + b;
        };
    };
    "###);
}
