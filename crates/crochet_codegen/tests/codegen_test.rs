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
        }
        if (value === 1) {
            return "one";
        }
        if (value === 2) {
            return "a couple";
        }
        if (n < 5) {
            const n = value;
            console.log(`n = ${n}`);
            return "a few";
        }
        console.log("fallthrough");
        return "many";
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
        }
        if (value.type === "keydown" && key !== "Escape") {
            const { key  } = value;
            return key;
        }
    })();
    "###);
}

#[test]
#[should_panic = "match can only have one catchall"]
fn pattern_matching_multiple_catchall_panics() {
    let src = r#"
    let result = match value {
        n => "foo",
        _ => "bar",
    }
    "#;
    
    compile(src);
}
