use chumsky::prelude::*;

use nouveau_lib::js_builder::build_js;
use nouveau_lib::js_printer::print_js;
use nouveau_lib::lexer::lexer;
use nouveau_lib::parser::token_parser;

fn compile(input: &str) -> String {
    let result = lexer().parse(input).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();

    let js_tree = build_js(&prog);
    print_js(&js_tree)
}

#[test]
fn string_literal() {
    insta::assert_snapshot!(compile("\"hello\""), @r###""hello";"###);
}

#[test]
fn number_literal_whole() {
    insta::assert_snapshot!(compile("123"), @"123;");
}

#[test]
fn number_literal_real() {
    insta::assert_snapshot!(compile("5.0"), @"5.0;");
}

#[test]
fn call_with_multiple_args() {
    insta::assert_snapshot!(compile("foo(a, b)"), @"foo(a, b);");
}

#[test]
fn call_with_single_arg() {
    insta::assert_snapshot!(compile("foo(a)"), @"foo(a);");
}

#[test]
fn call_with_no_args() {
    insta::assert_snapshot!(compile("foo()"), @"foo();");
}

#[test]
fn lambda_with_two_args() {
    insta::assert_snapshot!(compile("(a, b) => a + b"), @"(a, b) => a + b;");
}

#[test]
fn lambda_with_one_arg() {
    insta::assert_snapshot!(compile("(a) => a"), @"(a) => a;");
}

#[test]
fn lambda_with_no_args() {
    insta::assert_snapshot!(compile("() => 5"), @"() => 5;");
}

#[test]
fn mixed_operations() {
    insta::assert_snapshot!(compile("a + b * c"), @"a + b * c;");
}

#[test]
fn mixed_operations_requiring_parens() {
    insta::assert_snapshot!(compile("(a + b) * c"), @"(a + b) * c;");
}

#[test]
fn mixed_operations_requiring_multiple_parens() {
    insta::assert_snapshot!(compile("(a + b) * (c + d)"), @"(a + b) * (c + d);");
}

#[test]
fn division_without_parens() {
    insta::assert_snapshot!(compile("a / b / c"), @"a / b / c;");
}

#[test]
fn division_with_parens() {
    insta::assert_snapshot!(compile("a / (b / c)"), @"a / (b / c);");
}

#[test]
fn subtraction_without_parens() {
    insta::assert_snapshot!(compile("a - b - c"), @"a - b - c;");
}

#[test]
fn subtraction_with_parens() {
    insta::assert_snapshot!(compile("a - (b - c)"), @"a - (b - c);");
}

#[test]
fn function_declaration() {
    insta::assert_snapshot!(compile("let add = (a, b) => a + b"), @r###"
    const add = (a, b) => a + b;

    export {add};
    "###);
}

#[test]
fn variable_declaration_with_number_literal() {
    insta::assert_snapshot!(compile("let five = 5"), @r###"
    const five = 5;

    export {five};
    "###);
}

#[test]
fn let_in_inside_declaration() {
    // TODO: only allow `let-in` inside of non-top-level scopes
    insta::assert_snapshot!(compile("let foo = let x = 5 in x"), @r###"
    const foo = (() => {
        const x = 5;
        return x;
    })();

    export {foo};
    "###);
}

#[test]
fn nested_let_in_inside_declaration() {
    // TODO: only allow `let-in` inside of non-top-level scopes
    insta::assert_snapshot!(compile("let foo = let x = 5 in let y = 10 in x + y"), @r###"
    const foo = (() => {
        const x = 5;
        const y = 10;
        return x + y;
    })();

    export {foo};
    "###);
}

#[test]
fn js_print_simple_lambda() {
    insta::assert_snapshot!(compile("let add = (a, b) => a + b"), @r###"
    const add = (a, b) => a + b;

    export {add};
    "###);
}

#[test]
fn js_print_let_in() {
    let input = "let foo = let x = 5 in let y = 10 in x + y";
    insta::assert_snapshot!(compile(input), @r###"
    const foo = (() => {
        const x = 5;
        const y = 10;
        return x + y;
    })();

    export {foo};
    "###);
}

#[test]
fn js_print_variable_shadowing() {
    insta::assert_snapshot!(compile("let foo = let x = 5 in let x = 10 in x"), @r###"
    const foo = (() => {
        const x = 5;
        const x = 10;
        return x;
    })();

    export {foo};
    "###);
}

#[test]
fn js_print_let_in_inside_lambda() {
    insta::assert_snapshot!(compile("let foo = () => let x = 5 in let y = 10 in x + y"), @r###"
    const foo = () => {
        const x = 5;
        const y = 10;
        return x + y;
    };

    export {foo};
    "###);
}

#[test]
fn js_print_nested_lambdas() {
    insta::assert_snapshot!(compile("let foo = (a) => (b) => a + b"), @r###"
    const foo = (a) => (b) => a + b;

    export {foo};
    "###);
}

#[test]
fn js_print_nested_lambdas_with_multiple_lines() {
    insta::assert_snapshot!(compile("let foo = (a) => (b) => let sum = a + b in sum"), @r###"
    const foo = (a) => (b) => {
        const sum = a + b;
        return sum;
    };

    export {foo};
    "###);
}

#[test]
fn js_print_multiple_decls() {
    insta::assert_snapshot!(compile("let foo = \"hello\"\nlet bar = \"world\""), @r###"
    const foo = "hello";
    const bar = "world";

    export {foo, bar};
    "###);
}
