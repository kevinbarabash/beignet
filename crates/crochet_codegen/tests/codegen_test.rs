use chumsky::prelude::*;

use crochet_codegen::d_ts::codegen_d_ts;
use crochet_codegen::js::codegen_js;
use crochet_infer::{infer_prog, Context};
use crochet_parser::parser;

fn compile(input: &str) -> String {
    let program = parser().parse(input).unwrap();
    codegen_js(&program)
}

#[test]
fn js_print_multiple_decls() {
    insta::assert_snapshot!(compile("let foo = \"hello\"\nlet bar = \"world\""), @r###"
    export const foo = "hello";
    export const bar = "world";
    "###);
}

#[test]
fn unary_minus() {
    let src = r#"
    let negate = (x) => -x
    "#;

    insta::assert_snapshot!(compile(src), @"export const negate = (x)=>-x;
");
}

#[test]
fn template_literals() {
    let src = r#"
    let a = `hello, world`
    let p = {x: 5, y: 10}
    console.log(`p = (${p.x}, ${p.y})`)
    "#;
    insta::assert_snapshot!(compile(src), @r###"
    export const a = `hello, world`;
    export const p = {
        x: 5,
        y: 10
    };
    console.log(`p = (${p.x}, ${p.y})`);
    "###);
}

#[test]
fn tagged_template_literals() {
    let src = r#"
    let id = "12345"
    let query = sql`SELECT * FROM users WHERE id = "${id}"`
    "#;
    insta::assert_snapshot!(compile(src), @r###"
    export const id = "12345";
    export const query = sql`SELECT * FROM users WHERE id = "${id}"`;
    "###);
}

#[test]
fn pattern_matching() {
    let src = r#"
    let result = match count + 1 {
        0 => "none",
        1 => "one",
        2 => "a couple",
        n if n < 5 => {
            console.log(`n = ${n}`);
            "a few"
        },
        _ => {
            console.log("fallthrough");
            "many"
        },
    }
    "#;
    insta::assert_snapshot!(compile(src), @r###"
    let $temp_0;
    const $temp_1 = count + 1;
    if ($temp_1 === 0) {
        $temp_0 = "none";
    } else if ($temp_1 === 1) {
        $temp_0 = "one";
    } else if ($temp_1 === 2) {
        $temp_0 = "a couple";
    } else if (n < 5) {
        const n = $temp_1;
        console.log(`n = ${n}`);
        $temp_0 = "a few";
    } else {
        console.log("fallthrough");
        $temp_0 = "many";
    }
    export const result = $temp_0;
    "###);
}

#[test]
fn pattern_matching_with_disjoint_union() {
    let src = r#"
    type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string}
    declare let event: Event
    let result = match event {
        {type: "mousedown", x, y} => `mousedown: (${x}, ${y})`,
        {type: "keydown", key} if key != "Escape" => key,
    }
    "#;
    insta::assert_snapshot!(compile(src), @r###"
    ;
    ;
    let $temp_0;
    const $temp_1 = event;
    if ($temp_1.type === "mousedown") {
        const { x , y  } = $temp_1;
        $temp_0 = `mousedown: (${x}, ${y})`;
    } else if ($temp_1.type === "keydown" && key !== "Escape") {
        const { key  } = $temp_1;
        $temp_0 = key;
    }
    export const result = $temp_0;
    "###);
}

#[test]
#[should_panic = "Catchall must appear last in match"]
fn pattern_matching_multiple_catchall_panics() {
    let src = r#"
    let result = match value {
        n => "foo",
        _ => "bar",
    }
    "#;

    compile(src);
}

#[test]
#[should_panic = "No arms in match"]
fn pattern_matching_no_arms_panics() {
    let src = r#"
    let result = match value {
    }
    "#;

    compile(src);
}

#[test]
fn simple_if_else() {
    let src = r#"
    let result = if cond {
        console.log("true");
        5
    } else {
        console.log("false");
        10
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    let $temp_0;
    if (cond) {
        console.log("true");
        $temp_0 = 5;
    } else {
        console.log("false");
        $temp_0 = 10;
    }
    export const result = $temp_0;
    "###);
}

#[test]
fn simple_if_else_inside_fn() {
    let src = r#"
    let foo = () => {
        let result = if cond {
            console.log("true");
            5
        } else {
            console.log("false");
            10
        };
        result
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const foo = ()=>{
        let $temp_0;
        if (cond) {
            console.log("true");
            $temp_0 = 5;
        } else {
            console.log("false");
            $temp_0 = 10;
        }
        const result = $temp_0;
        return result;
    };
    "###);
}

#[test]
fn simple_if_else_inside_fn_as_expr() {
    let src = r#"
    let foo = () => if cond {
        console.log("true");
        5
    } else {
        console.log("false");
        10
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const foo = ()=>{
        let $temp_0;
        if (cond) {
            console.log("true");
            $temp_0 = 5;
        } else {
            console.log("false");
            $temp_0 = 10;
        }
        return $temp_0;
    };
    "###);
}

#[test]
fn nested_if_else() {
    let src = r#"
    let result = if c1 {
        if c2 {
            5
        } else {
            10
        }
    } else {
        if c3 {
            "hello"
        } else {
            "world"
        }
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    let $temp_0;
    if (c1) {
        let $temp_1;
        if (c2) {
            $temp_1 = 5;
        } else {
            $temp_1 = 10;
        }
        $temp_0 = $temp_1;
    } else {
        let $temp_2;
        if (c3) {
            $temp_2 = "hello";
        } else {
            $temp_2 = "world";
        }
        $temp_0 = $temp_2;
    }
    export const result = $temp_0;
    "###);
}

#[test]
fn multiple_lets_inside_a_function() {
    let src = r#"
    let do_math = () => {
        let x = 5;
        let y = 10;
        let result = x + y;
        result
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const do_math = ()=>{
        const x = 5;
        const y = 10;
        const result = x + y;
        return result;
    };
    "###);
}

#[test]
fn codegen_if_let_with_rename() {
    // TODO: don't allow irrefutable patterns to be used with if-let
    let src = r#"
    let result = if let {x: a, y: b} = {x: 5, y: 10} {
        a + b
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    let $temp_0;
    const $temp_1 = {
        x: 5,
        y: 10
    };
    {
        const { x: a , y: b  } = $temp_1;
        $temp_0 = a + b;
    }export const result = $temp_0;
    "###);
}

#[test]
fn codegen_if_let_refutable_pattern_nested_obj() {
    let src = r#"
    let action = {type: "moveto", point: {x: 5, y: 10}}
    if let {type: "moveto", point: {x, y}} = action {
        x + y
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const action = {
        type: "moveto",
        point: {
            x: 5,
            y: 10
        }
    };
    let $temp_0;
    const $temp_1 = action;
    if ($temp_1.type === "moveto") {
        const { point: { x , y  }  } = $temp_1;
        $temp_0 = x + y;
    }
    $temp_0;
    "###);
}

#[test]
fn codegen_if_let_with_else() {
    let src = r#"
    declare let a: string | number
    let result = if let x is number = a {
        x + 5
    } else if let y is string = a {
        y
    } else {
        true
    }
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    ;
    let $temp_0;
    const $temp_1 = a;
    if (typeof $temp_1 === "number") {
        const x = $temp_1;
        $temp_0 = x + 5;
    } else {
        let $temp_2;
        const $temp_3 = a;
        if (typeof $temp_3 === "string") {
            const y = $temp_3;
            $temp_2 = y;
        } else {
            $temp_2 = true;
        }
        $temp_0 = $temp_2;
    }
    export const result = $temp_0;
    "###);
}

#[test]
fn codegen_block_with_multiple_non_let_lines() {
    let src = "let result = {let x = 5; x + 0; x}";

    insta::assert_snapshot!(compile(src), @r###"
    let $temp_0;
    {
        const x = 5;
        x + 0;
        $temp_0 = x;
    }export const result = $temp_0;
    "###);
}

#[test]
fn destructuring_function_object_params() {
    let src = r#"
    let foo = ({x, y: b}) => x + b
    "#;

    insta::assert_snapshot!(compile(src), @"export const foo = ({ x , y: b  })=>x + b;
");

    let program = parser().parse(src).unwrap();
    let mut ctx = Context::default();
    infer_prog(&program, &mut ctx).unwrap();
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const foo: ({ x , y: b  }: {
        readonly x: number;
        readonly y: number;
    }) => number;
    "###);
}

#[test]
fn destructuring_function_array_params() {
    let src = r#"
    let foo = ([a, b]) => a + b
    "#;

    insta::assert_snapshot!(compile(src), @"export const foo = ([a, b])=>a + b;
");

    let program = parser().parse(src).unwrap();
    let mut ctx = Context::default();
    infer_prog(&program, &mut ctx).unwrap();
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const foo: ([a, b]: [number, number]) => number;
    "###);
}

#[test]
fn function_with_rest_param() {
    let src = r#"
    let foo = (x: number, ...y: number[]) => x
    "#;

    insta::assert_snapshot!(compile(src), @"export const foo = (x, ...y)=>x;
");

    let program = parser().parse(src).unwrap();
    let mut ctx = Context::default();
    infer_prog(&program, &mut ctx).unwrap();
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const foo: (x: number, ...y: number[]) => number;
");
}

#[test]
fn function_with_optional_param() {
    let src = r#"
    let foo = (x: number, y?: number) => x
    "#;

    insta::assert_snapshot!(compile(src), @"export const foo = (x, y)=>x;
");

    let program = parser().parse(src).unwrap();
    let mut ctx = Context::default();
    infer_prog(&program, &mut ctx).unwrap();
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const foo: (x: number, y?: number) => number;
");
}

#[test]
fn function_with_optional_param_and_rest_param() {
    let src = r#"
    let foo = (x?: number, ...y: number[]) => x
    "#;

    insta::assert_snapshot!(compile(src), @"export const foo = (x, ...y)=>x;
");

    let program = parser().parse(src).unwrap();
    let mut ctx = Context::default();
    infer_prog(&program, &mut ctx).unwrap();
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const foo: (x?: number, ...y: number[]) => number | undefined;
");
}

#[test]
fn variable_declaration_with_destructuring() {
    let src = r#"
    let [x, y] = [5, 10]
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const [x, y] = [
        5,
        10
    ];
    "###);

    // TODO: Support destructuring in top-level decls
    let program = parser().parse(src).unwrap();
    let mut ctx = Context::default();
    infer_prog(&program, &mut ctx).unwrap();
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const x: 5;
    export declare const y: 10;
    "###);
}

#[test]
fn computed_property() {
    let src = r#"
    let p = {x: 5, y: 10}
    let x = p["x"]
    let q = [5, 10]
    let y = q[1]
    "#;
    insta::assert_snapshot!(compile(src), @r###"
    export const p = {
        x: 5,
        y: 10
    };
    export const x = p["x"];
    export const q = [
        5,
        10
    ];
    export const y = q[1];
    "###);
}

#[test]
fn partial_application() {
    let src = r#"
    let add = (a, b) => a + b
    let add5 = add(5, _)
    let sum = add5(10)
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const add = (a, b)=>a + b;
    export const add5 = ($arg0)=>add(5, $arg0);
    export const sum = add5(10);
    "###);
}

#[test]
fn partial_application_with_spread() {
    let src = r#"
    let add = (a, b, c) => a + b + c
    let add5 = add(5, ..._)
    let sum = add5(10, 15)
    "#;

    // Instead of treating ..._ like a spread operation, it should just expand
    // to add(5, _, _)

    // TODO: handle spreading a wildcard
    // The output should be:
    // export const add5 = ($arg0, $arg1)=>add(5, $arg0, $arg1);
    // This requires having type information in the expression tree
    insta::assert_snapshot!(compile(src), @r###"
    export const add = (a, b, c)=>a + b + c;
    export const add5 = ($arg0)=>add(5, $arg0);
    export const sum = add5(10, 15);
    "###);
}

#[test]
#[ignore]
fn partial_application_of_fn_with_rest_params() {
    let _src = r#"
    let add = (a, b, ...c) => a + b + c
    let add5 = add(5, _, _)
    let sum = add5(10, 15)
    "#;

    // What should the type of `add5` be?
    // It should not have a rest param
}

#[test]
#[ignore]
fn partial_application_of_fn_with_rest_params_using_spread() {
    let _src = r#"
    let add = (a, b, ...c) => a + b + c
    let add5 = add(5, ..._)
    let sum = add5(10, 15)
    "#;

    // Instead of treating ..._ like a spread operation, it should just expand
    // to add(5, _, ..._)

    // We need to know the type of the callee  In order to build the partially
    // applied function correctly.

    // What should the type of `add5` be?
    // add5: (arg0: number, ...rest: number[])
}

#[test]
fn spread_args() {
    let src = r#"
    let add = (a, b) => a + b
    let sum = add(...[5, 10])
    "#;

    insta::assert_snapshot!(compile(src), @r###"
    export const add = (a, b)=>a + b;
    export const sum = add(...[
        5,
        10
    ]);
    "###);
}
