use chumsky::prelude::*;

use crochet::js::builder::build_js;
use crochet::js::printer::print_js;
use crochet::parser::parser;

fn compile(input: &str) -> String {
    let prog = parser().parse(input).unwrap();
    // println!("{:#?}", &prog);
    let js_tree = build_js(&prog);
    // println!("{:#?}", &js_tree);
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
    insta::assert_snapshot!(compile("(a, b) => a + b"), @r###"
    (a, b)=>a + b
    ;
    "###);
}

#[test]
fn lambda_with_one_arg() {
    insta::assert_snapshot!(compile("(a) => a"), @r###"
    (a)=>a
    ;
    "###);
}

#[test]
fn lambda_with_no_args() {
    insta::assert_snapshot!(compile("() => 5"), @r###"
    ()=>5
    ;
    "###);
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
    export const add = (a, b)=>a + b
    ;
    "###);
}

#[test]
fn variable_declaration_with_number_literal() {
    insta::assert_snapshot!(compile("let five = 5"), @"export const five = 5;\n");
}

#[test]
fn let_in_inside_declaration() {
    // TODO: only allow `let-in` inside of non-top-level scopes
    insta::assert_snapshot!(compile("let foo = let x = 5 in x"), @r###"
    export const foo = ()=>{
        const x = 5;
        return x;
    }();
    "###);
}

#[test]
fn nested_let_in_inside_declaration() {
    // TODO: only allow `let-in` inside of non-top-level scopes
    insta::assert_snapshot!(compile("let foo = let x = 5 in let y = 10 in x + y"), @r###"
    export const foo = ()=>{
        const x = 5;
        const y = 10;
        return x + y;
    }();
    "###);
}

#[test]
fn js_print_simple_lambda() {
    insta::assert_snapshot!(compile("let add = (a, b) => a + b"), @r###"
    export const add = (a, b)=>a + b
    ;
    "###);
}

#[test]
fn js_print_let_in() {
    let input = "let foo = let x = 5 in let y = 10 in x + y";
    insta::assert_snapshot!(compile(input), @r###"
    export const foo = ()=>{
        const x = 5;
        const y = 10;
        return x + y;
    }();
    "###);
}

#[test]
fn js_print_variable_shadowing() {
    insta::assert_snapshot!(compile("let foo = let x = 5 in let x = 10 in x"), @r###"
    export const foo = ()=>{
        const x = 5;
        const x = 10;
        return x;
    }();
    "###);
}

#[test]
fn js_print_let_in_inside_lambda() {
    insta::assert_snapshot!(compile("let foo = () => let x = 5 in let y = 10 in x + y"), @r###"
    export const foo = ()=>{
        const x = 5;
        const y = 10;
        return x + y;
    };
    "###);
}

#[test]
fn js_print_nested_lambdas() {
    insta::assert_snapshot!(compile("let foo = (a) => (b) => a + b"), @r###"
    export const foo = (a)=>(b)=>a + b
    ;
    "###);
}

#[test]
fn js_print_nested_lambdas_with_multiple_lines() {
    insta::assert_snapshot!(compile("let foo = (a) => (b) => let sum = a + b in sum"), @r###"
    export const foo = (a)=>(b)=>{
            const sum = a + b;
            return sum;
        }
    ;
    "###);
}

#[test]
fn js_print_multiple_decls() {
    insta::assert_snapshot!(compile("let foo = \"hello\"\nlet bar = \"world\""), @r###"
    export const foo = "hello";
    export const bar = "world";
    "###);
}

#[test]
fn js_print_object() {
    insta::assert_snapshot!(compile("let point = {x: 5, y: 10}"), @r###"
    export const point = {
        x: 5,
        y: 10
    };
    "###);
}
