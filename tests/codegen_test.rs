use chumsky::prelude::*;
use test_case::test_case;

use nouveau_lib::codegen::codegen_expr;
use nouveau_lib::parser::parser;

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
fn parse_then_codegen(input: &str, output: &str) {
    let expr = parser().parse(input).unwrap();
    let result: String = codegen_expr(&expr);

    assert_eq!(result, output);
}
