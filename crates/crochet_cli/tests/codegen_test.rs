use crochet_codegen::js::*;
use crochet_parser::parse;

fn compile(input: &str) -> String {
    let program = parse(input).unwrap();
    let (js, _) = codegen_js(input, &program);
    js
}

#[test]
fn string_literal() {
    insta::assert_snapshot!(compile("\"hello\";"), @r###""hello";"###);
}

#[test]
fn template_literal() {
    let src = r#"
    let name = "world";
    let msg = `hello, ${name}!`;
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const name = "world";
    export const msg = `hello, ${name}!`;
    "###);
}

#[test]
fn number_literal_whole() {
    insta::assert_snapshot!(compile("123;"), @"123;");
}

#[test]
fn number_literal_real() {
    insta::assert_snapshot!(compile("5.0;"), @"5.0;");
}

#[test]
fn call_with_multiple_args() {
    insta::assert_snapshot!(compile("foo(a, b);"), @"foo(a, b);");
}

#[test]
fn call_with_single_arg() {
    insta::assert_snapshot!(compile("foo(a);"), @"foo(a);");
}

#[test]
fn call_with_no_args() {
    insta::assert_snapshot!(compile("foo();"), @"foo();");
}

#[test]
fn lambda_with_two_args() {
    insta::assert_snapshot!(compile("(a, b) => a + b;"), @"(a, b)=>a + b;
");
}

#[test]
fn lambda_with_one_arg() {
    insta::assert_snapshot!(compile("(a) => a;"), @"(a)=>a;
");
}

#[test]
fn lambda_with_no_args() {
    insta::assert_snapshot!(compile("() => 5;"), @"()=>5;
");
}

#[test]
fn mixed_operations() {
    insta::assert_snapshot!(compile("a + b * c;"), @"a + b * c;");
}

#[test]
fn mixed_operations_requiring_parens() {
    insta::assert_snapshot!(compile("(a + b) * c;"), @"(a + b) * c;");
}

#[test]
fn mixed_operations_requiring_multiple_parens() {
    insta::assert_snapshot!(compile("(a + b) * (c + d);"), @"(a + b) * (c + d);");
}

#[test]
fn division_without_parens() {
    insta::assert_snapshot!(compile("a / b / c;"), @"a / b / c;");
}

#[test]
fn division_with_parens() {
    insta::assert_snapshot!(compile("a / (b / c);"), @"a / (b / c);");
}

#[test]
fn subtraction_without_parens() {
    insta::assert_snapshot!(compile("a - b - c;"), @"a - b - c;");
}

#[test]
fn subtraction_with_parens() {
    insta::assert_snapshot!(compile("a - (b - c);"), @"a - (b - c);");
}

#[test]
fn function_declaration() {
    insta::assert_snapshot!(compile("let add = (a, b) => a + b;"), @"export const add = (a, b)=>a + b;
");
}

#[test]
fn variable_declaration_with_number_literal() {
    insta::assert_snapshot!(compile("let five = 5;"), @"export const five = 5;\n");
}

#[test]
fn let_in_inside_declaration() {
    insta::assert_snapshot!(compile("let foo = do {let x = 5; x};"), @r###"
    let $temp_0;
    {
        const x = 5;
        $temp_0 = x;
    }export const foo = $temp_0;
    "###);
}

#[test]
fn nested_let_in_inside_declaration() {
    insta::assert_snapshot!(compile("let foo = do {let x = 5; let y = 10; x + y};"), @r###"
    let $temp_0;
    {
        const x = 5;
        const y = 10;
        $temp_0 = x + y;
    }export const foo = $temp_0;
    "###);
}

#[test]
fn js_print_simple_lambda() {
    insta::assert_snapshot!(compile("let add = (a, b) => a + b;"), @"export const add = (a, b)=>a + b;
");
}

#[test]
fn js_print_let_in() {
    let input = r#"
    let foo = do {let x = 5; let y = 10; x + y};
    "#;
    insta::assert_snapshot!(compile(input), @r###"
    let $temp_0;
    {
        const x = 5;
        const y = 10;
        $temp_0 = x + y;
    }export const foo = $temp_0;
    "###);
}

#[test]
fn js_print_variable_shadowing() {
    let input = r#"
    let foo = do {
        let x = 5;
        let x = 10;
        x
    };"#;
    insta::assert_snapshot!(compile(input), @r###"
    let $temp_0;
    {
        const x = 5;
        const x = 10;
        $temp_0 = x;
    }export const foo = $temp_0;
    "###);
}

#[test]
fn js_print_let_in_inside_lambda() {
    insta::assert_snapshot!(compile(r#"
    let foo = () => {
        let x = 5;
        let y = 10;
        x + y
    };"#), @r###"
    export const foo = ()=>{
        const x = 5;
        const y = 10;
        return x + y;
    };
    "###);
}

#[test]
fn js_print_nested_lambdas() {
    insta::assert_snapshot!(compile("let foo = (a) => (b) => a + b;"), @"export const foo = (a)=>(b)=>a + b;
");
}

#[test]
fn js_print_nested_lambdas_with_multiple_lines() {
    insta::assert_snapshot!(compile("let foo = (a) => (b) => {let sum = a + b; sum};"), @r###"
    export const foo = (a)=>(b)=>{
            const sum = a + b;
            return sum;
        };
    "###);
}

#[test]
fn js_print_nested_blocks() {
    insta::assert_snapshot!(compile(r#"
    let result = do {
        let sum = do {
            let x = 5;
            let y = 10;
            x + y
        };
        sum
    };"#), @r###"
    let $temp_0;
    {
        let $temp_1;
        {
            const x = 5;
            const y = 10;
            $temp_1 = x + y;
        }
        const sum = $temp_1;
        $temp_0 = sum;
    }export const result = $temp_0;
    "###);
}

#[test]
fn js_print_multiple_decls() {
    insta::assert_snapshot!(compile("let foo = \"hello\";\nlet bar = \"world\";"), @r###"
    export const foo = "hello";
    export const bar = "world";
    "###);
}

#[test]
fn js_print_object() {
    insta::assert_snapshot!(compile("let point = {x: 5, y: 10};"), @r###"
    export const point = {
        x: 5,
        y: 10
    };
    "###);
}

#[test]
fn codegen_jsx() {
    insta::assert_snapshot!(compile("<Foo>Hello</Foo>;"), @r###"
    import { jsx as _jsx } from "react/jsx-runtime";
    _jsx(Foo, {
        children: "Hello"
    });
    "###);
    insta::assert_snapshot!(compile("<Foo>{bar}</Foo>;"), @r###"
    import { jsx as _jsx } from "react/jsx-runtime";
    _jsx(Foo, {
        children: bar
    });
    "###);
    insta::assert_snapshot!(compile("<Foo>Hello {world}!</Foo>;"), @r###"
    import { jsxs as _jsxs } from "react/jsx-runtime";
    _jsxs(Foo, {
        children: [
            "Hello ",
            world,
            "!"
        ]
    });
    "###);
    insta::assert_snapshot!(compile("<Foo>{<Bar>{baz}</Bar>}</Foo>;"), @r###"
    import { jsx as _jsx } from "react/jsx-runtime";
    _jsx(Foo, {
        children: _jsx(Bar, {
            children: baz
        })
    });
    "###);
    insta::assert_snapshot!(compile("<Foo></Foo>;"), @r###"
    import { jsx as _jsx } from "react/jsx-runtime";
    _jsx(Foo, {});
    "###);
    insta::assert_snapshot!(compile("<Foo bar={baz} />;"), @r###"
    import { jsx as _jsx } from "react/jsx-runtime";
    _jsx(Foo, {
        bar: baz
    });
    "###);
    insta::assert_snapshot!(compile("<Foo msg=\"hello\" bar={baz}></Foo>;"), @r###"
    import { jsx as _jsx } from "react/jsx-runtime";
    _jsx(Foo, {
        msg: "hello",
        bar: baz
    });
    "###);
}

#[test]
fn js_print_member_access() {
    insta::assert_snapshot!(compile("a.b.c;"), @"a.b.c;");
}
