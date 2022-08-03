use chumsky::prelude::*;

use crochet_codegen::js::codegen_js;
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
fn infer_if_let_refutable_pattern_nested_obj() {
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
