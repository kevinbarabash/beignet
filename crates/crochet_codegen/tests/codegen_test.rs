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
    export const result = (()=>{
        const value = count + 1;
        if (value === 0) {
            return "none";
        } else if (value === 1) {
            return "one";
        } else if (value === 2) {
            return "a couple";
        } else if (n < 5) {
            const n = value;
            console.log(`n = ${n}`);
            return "a few";
        } else {
            console.log("fallthrough");
            return "many";
        }
    })();
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
    export const result = (()=>{
        const value = event;
        if (value.type === "mousedown") {
            const { x , y  } = value;
            return `mousedown: (${x}, ${y})`;
        } else if (value.type === "keydown" && key !== "Escape") {
            const { key  } = value;
            return key;
        }
    })();
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
