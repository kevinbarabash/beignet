#[cfg(target_family = "wasm")]
use std::ffi::CString;
use std::os::raw::c_char;

use escalier_ast::types::Type;
use escalier_ast::values::*;

mod expr;
mod literal;
mod parse_error;
mod pattern;
mod stmt;
mod type_ann;
mod util;

pub use crate::parse_error::ParseError;

use crate::stmt::parse_statement;

#[link(wasm_import_module = "my_custom_module")]
extern "C" {
    fn _log(wasm_str: WasmString);
}

#[repr(C)]
pub struct WasmString {
    pub offset: *const c_char,
    pub length: u32,
}

#[cfg(target_family = "wasm")]
unsafe fn string_to_wasm_string(input: &str) -> WasmString {
    let length = input.len() as u32;
    let offset = CString::new(input).unwrap().into_raw();
    WasmString { offset, length }
}

#[cfg(target_family = "wasm")]
fn log(str: &str) {
    unsafe {
        _log(string_to_wasm_string(str));
    }
}

#[cfg(target_family = "unix")]
fn log(str: &str) {
    eprintln!("{}", str);
}

pub fn parse(src: &str) -> Result<Program<Type>, ParseError> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_escalier::language())
        .expect("Error loading escalier language");

    log("hello, world!");

    let tree = parser.parse(src, None).unwrap();

    let root = tree.root_node();

    let kind = root.kind();
    if kind == "program" {
        let mut cursor = root.walk();

        let children = root.children(&mut cursor);

        let mut body: Vec<Statement<Type>> = vec![];
        for child in children {
            if let Some(stmt) = parse_statement(&child, src)? {
                body.push(stmt);
            }
        }

        Ok(Program { body })
    } else {
        Err(ParseError::from("not implemented yet"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn messages(report: &ParseError) -> String {
        report.to_string()
    }

    #[test]
    fn it_works() {
        let src = r#"
        let add = (a, b) => a + b;
        let sub = (a, b) => a - b;
        let sum = add(5, 10);
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn numbers() {
        insta::assert_debug_snapshot!(parse("10;"));
        insta::assert_debug_snapshot!(parse("1.23;"));
        insta::assert_debug_snapshot!(parse("-10;"));
    }

    #[test]
    fn strings() {
        insta::assert_debug_snapshot!(parse(r#""";"#));
        insta::assert_debug_snapshot!(parse(r#""hello";"#));
        insta::assert_debug_snapshot!(parse("\"line 1\\nline 2\\nline 3\";"));
        insta::assert_debug_snapshot!(parse("\"a \\u2212 b\";"));
        insta::assert_debug_snapshot!(parse("\"hello, \\\"world\\\"!\";"));
    }

    #[test]
    fn template_literals() {
        insta::assert_debug_snapshot!(parse("`Hello, world`"));
        insta::assert_debug_snapshot!(parse("`Hello, ${name}`"));
        insta::assert_debug_snapshot!(parse("`(${x}, ${y})`"));
        insta::assert_debug_snapshot!(parse(r#"`Hello, "world"`"#));
        insta::assert_debug_snapshot!(parse("`foo ${`bar ${baz}`}`"));
        insta::assert_debug_snapshot!(parse("`line 1\\nline 2\\nline 3`"));
        insta::assert_debug_snapshot!(parse("`a \\u2212 b`"));
        // insta::assert_debug_snapshot!(parse(r#"if cond { `${foo}` } else { `${bar}` }"#));
    }

    #[test]
    fn tagged_template_literals() {
        insta::assert_debug_snapshot!(parse("sql`SELECT * FROM ${table} WHERE id = ${id}`"));
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
    fn operations() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3;"));
        insta::assert_debug_snapshot!(parse("x * y / z;"));
        insta::assert_debug_snapshot!(parse("(a + b) * c;"));
        insta::assert_debug_snapshot!(parse("a == b;"));
        insta::assert_debug_snapshot!(parse("a != b;"));
        insta::assert_debug_snapshot!(parse("a > b;"));
        insta::assert_debug_snapshot!(parse("a >= b;"));
        insta::assert_debug_snapshot!(parse("a < b;"));
        insta::assert_debug_snapshot!(parse("a <= b;"));
        insta::assert_debug_snapshot!(parse("let cond = a != b;"));
        insta::assert_debug_snapshot!(parse("-a;"));
        insta::assert_debug_snapshot!(parse("-(a + b);"));
    }

    #[test]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c;"));
        insta::assert_debug_snapshot!(parse("() => 10;"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\";"));
        insta::assert_debug_snapshot!(parse("(a, ...b) => true;"));
        insta::assert_debug_snapshot!(parse("({x, y}) => x + y;"));
        insta::assert_debug_snapshot!(parse("({x: p, y: q}) => p + q;"));
        insta::assert_debug_snapshot!(parse("(a?: boolean, b?) => c;"));
    }

    #[test]
    fn rust_style_functions() {
        insta::assert_debug_snapshot!(parse("let foo = () => { let x = Math.random(); x };"));
        insta::assert_debug_snapshot!(parse("let bar = () => { let x = Math.random(); x; };"));
    }

    #[test]
    fn multiple_rest_params() {
        match parse("(...a, ...b) => true") {
            Ok(_) => panic!("expected test parse() to return an error"),
            Err(report) => {
                assert_eq!(
                    messages(&report),
                    "ParseError: failed to parse: '(...a, ...b) => true'"
                );
            }
        }
    }

    // #[test]
    // #[ignore]
    // fn optional_params_must_appear_last() {
    //     assert_eq!(
    //         parse("(a?, b) => true"),
    //         Err(ParseError::from("optional params must come last")),
    //     );
    // }

    #[test]
    fn async_await() {
        insta::assert_debug_snapshot!(parse("async () => 10;"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 };"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b;"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar();"));
    }

    #[test]
    fn function_application() {
        insta::assert_debug_snapshot!(parse("foo();"));
        insta::assert_debug_snapshot!(parse("foo(a, b);"));
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\");"));
        insta::assert_debug_snapshot!(parse("f(x)(g(x));"));
        insta::assert_debug_snapshot!(parse("foo(a, ...b);"));
        let src = r#"
        let S = (f) => (g) => (x) => f(x)(g(x));
        let K = (x) => (y) => x;
        let I = S(K)(K);
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn new_expression() {
        insta::assert_debug_snapshot!(parse("new Array();"));
        insta::assert_debug_snapshot!(parse("new Array(1, 2, 3);"));
    }

    #[test]
    fn declarations() {
        insta::assert_debug_snapshot!(parse("let x = 5;"));
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b;"));
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; x};"));
        // recursive
        insta::assert_debug_snapshot!(parse("let rec f = () => f();"));
        // mutable
        insta::assert_debug_snapshot!(parse(r#"let mut msg: string = "hello, world";"#));
        insta::assert_debug_snapshot!(parse(
            r#"
            let foo = () => {
                let mut msg: string = "hello, world";
                msg
            };
        "#
        ));
    }

    #[test]
    fn assignments() {
        insta::assert_debug_snapshot!(parse("x = 5;"));
        insta::assert_debug_snapshot!(parse("a.b = c;"));
        insta::assert_debug_snapshot!(parse("a[b] = c;"));
        insta::assert_debug_snapshot!(parse(r#"a["b"] = c;"#));
    }

    #[test]
    fn top_level_expressions() {
        insta::assert_debug_snapshot!(parse("a + b;"));
        insta::assert_debug_snapshot!(parse("123;\n\"hello\";"));
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if (true) { 5 } else { 10 };"));
        insta::assert_debug_snapshot!(parse("if (a) { 5 } else if (b) { 10 } else { 20 };"));
    }

    #[test]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10};"));
        insta::assert_debug_snapshot!(parse("let obj = {x, y};"));
        insta::assert_debug_snapshot!(parse("let obj = {a, b, ...others};"));
    }

    #[test]
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
            "let elem = <div point={point} id=\"point\">Hello, {msg}</div>;"
        ));
    }

    // #[test]
    // #[ignore]
    // fn jsx_head_and_tail_must_match() {
    //     assert_eq!(
    //         parse("<Foo>Hello</Bar>"),
    //         Err(ParseError::from("JSX head and tail elements must match")),
    //     );
    // }

    #[test]
    fn type_annotations() {
        insta::assert_debug_snapshot!(parse("let x: number = 5;"));
        insta::assert_debug_snapshot!(parse("let msg: string = \"hello\";"));
        insta::assert_debug_snapshot!(parse("let add = (a: number, b: number): number => a + b;"));
        insta::assert_debug_snapshot!(parse("let p: Point = {x: 5, y: 10};"));
        insta::assert_debug_snapshot!(parse("let FOO: \"foo\" = \"foo\";"));
    }

    #[test]
    fn decls() {
        insta::assert_debug_snapshot!(parse("let x = 5;"));
        insta::assert_debug_snapshot!(parse("   let x = 5;")); // with leading whitespace
        insta::assert_debug_snapshot!(parse("declare let x: number;"));
        insta::assert_debug_snapshot!(parse("declare let foo: Foo<string>;"));
    }

    #[test]
    fn tuples() {
        insta::assert_debug_snapshot!(parse("let x = [];"));
        insta::assert_debug_snapshot!(parse("let x = [1, 2, 3];"));
        insta::assert_debug_snapshot!(parse("let x = [1, [a, b]];"));
        insta::assert_debug_snapshot!(parse("let foo = () => [a, b];"));
    }

    #[test]
    fn member_access() {
        insta::assert_debug_snapshot!(parse("a.b.c;"));
        insta::assert_debug_snapshot!(parse("foo.bar();"));
        insta::assert_debug_snapshot!(parse("p.x * p.x + p.y * p.y;"));
        insta::assert_debug_snapshot!(parse("foo().bar();"));
        insta::assert_debug_snapshot!(parse("arr[0][1];"));
        insta::assert_debug_snapshot!(parse("arr[x](y);"));
        insta::assert_debug_snapshot!(parse("arr[arr.length - 1];"));
        insta::assert_debug_snapshot!(parse("foo[bar[-1]];"));
    }

    #[test]
    fn type_decls() {
        insta::assert_debug_snapshot!(parse("type Num = number;"));
        insta::assert_debug_snapshot!(parse("type Point = {x: number, y: number};"));
        insta::assert_debug_snapshot!(parse("type Foo<T> = {bar: T};"));
        insta::assert_debug_snapshot!(parse("type Foo<T extends string> = {bar: T};"));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T = "foo"> = {bar: T};"#));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T extends string = "foo"> = {bar: T};"#));
        insta::assert_debug_snapshot!(parse("type CoordNames = keyof Point;"));
        insta::assert_debug_snapshot!(parse("type Foo = typeof foo;"));
        insta::assert_debug_snapshot!(parse("type FooBar = typeof foo.bar;"));
        insta::assert_debug_snapshot!(parse(r#"type C = A["b"][C_Key];"#));
        insta::assert_debug_snapshot!(parse("type Array<T> = {[key: number]: T};"));
        insta::assert_debug_snapshot!(parse("type MutPoint = mut {x: number, y: number};"));
        insta::assert_debug_snapshot!(parse("type MutPoint = {mut x: number, mut y: number};"));
    }

    #[test]
    fn blocks() {
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; x};"));
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; let y = 10; x + y};"));
        insta::assert_debug_snapshot!(parse("do {let x = 5; let y = 10; x + y};"));
        insta::assert_debug_snapshot!(parse(
            "do {let sum = do {let x = 5; let y = 10; x + y}; sum};"
        ));
        insta::assert_debug_snapshot!(parse("let foo = do {let x = 5; console.log(x); x};"));
        insta::assert_debug_snapshot!(parse("let foo = do {console.log(x); x};"));
    }

    #[test]
    fn destructuring() {
        insta::assert_debug_snapshot!(parse("let {x, y} = point;"));
        insta::assert_debug_snapshot!(parse("let {a, b, ...rest} = letters;"));
        insta::assert_debug_snapshot!(parse("let {p0: {x, y}, p1: {x, y}} = line;"));
        insta::assert_debug_snapshot!(parse("let [a, b, ...rest] = letters;"));
        insta::assert_debug_snapshot!(parse("let [foo, ...[bar, ...rest]] = baz;"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]) => a;"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]: [string, number]) => a;"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}) => b;"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}: {a: string, b: number}) => b;"));
        insta::assert_debug_snapshot!(parse("let {mut x, y: mut z} = point;"));
        insta::assert_debug_snapshot!(parse("let foo = ({mut x, y: mut z}) => {};"));
        insta::assert_debug_snapshot!(parse("let [a, mut b, ...rest] = letters;"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, mut b, ...rest]) => {};"));
        insta::assert_debug_snapshot!(parse("let {x, mut y = 10} = point;"));
        insta::assert_debug_snapshot!(parse("let [a, mut b = 98, ...rest] = letters;"));
        // TODO: disallowed patterns, e.g. top-level rest, non-top-level type annotations
    }

    #[test]
    fn array_spread() {
        insta::assert_debug_snapshot!(parse("let tuple = [...a, b];"));
        insta::assert_debug_snapshot!(parse("let tuple = [a, ...b];"));
        insta::assert_debug_snapshot!(parse("let tuple = [1, ...[2, 3]];"));
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
    fn types() {
        insta::assert_debug_snapshot!(parse("let get_bar = <T>(foo: Foo<T>) => foo.bar;"));
        insta::assert_debug_snapshot!(parse("declare let get_bar: (foo: Foo) => T;"));
        insta::assert_debug_snapshot!(parse("let str_arr: string[] = [];"));
        insta::assert_debug_snapshot!(parse("let thunk_arr: (() => undefined)[] = [];"));
        insta::assert_debug_snapshot!(parse("let arr: string[] | number[] = [];"));
        insta::assert_debug_snapshot!(parse(
            "declare let add: ((a: number, b: number) => number) & (a: string, b: string) => string;"
        ));
        insta::assert_debug_snapshot!(parse("let nested_arr: string[][] = [];"));
        let src = r#"
        type Event = 
          | {type: "mousedown", x: number, y: number}
          | {type: "keydown", key: string};
        "#;
        insta::assert_debug_snapshot!(parse(src));
        insta::assert_debug_snapshot!(parse("let mut_arr: mut string[] = [];"));
    }

    #[test]
    fn object_types() {
        insta::assert_debug_snapshot!(parse("type Pick<T, K extends keyof T> = {[P in K]: T[P]};"));
        insta::assert_debug_snapshot!(parse("type Foo<T> = {mut [P in keyof T]?: T[P]};"));
        insta::assert_debug_snapshot!(parse("type Bar<T> = {+mut [P in keyof T]+?: T[P]};"));
        insta::assert_debug_snapshot!(parse("type Baz<T> = {-mut [P in keyof T]-?: T[P]};"));
    }

    #[test]
    fn conditional_types() {
        insta::assert_debug_snapshot!(parse(
            r#"type GetTypeName<T extends number | string> = T extends number ? "number" : "string";"#
        ));
        insta::assert_debug_snapshot!(parse(
            "type Flatten<Type> = Type extends Array<infer Item> ? Item : Type;"
        ));
        insta::assert_debug_snapshot!(parse(
            r#"type GetReturnType<Type> = Type extends (...args: never[]) => infer Return
                ? Return
                : never;
        "#
        ));
    }

    #[test]
    fn pattern_matching() {
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                {x, y: b, z: 5, ...rest} -> "object",
                [a, _, ...rest] -> "array",
                "string" -> "string",
                true -> "true",
                false -> "false",
                n -> "variable",
                _ -> "wildcard"
            };
        "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                {a: {b: {c}}} -> "object",
                _ -> "fallthrough"
            };              
            "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                n is number -> "number",
                {a: a is Array} -> "Array",
                _ -> "fallthrough"
            };
            "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = match (foo) {
                1 -> "one",
                2 -> "two",
                n if (n < 5) -> "few",
                _ -> "many"
            };
            "#
        ))
    }

    #[test]
    fn if_let() {
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = if (let {x, y: b, ...rest} = foo) {
                "object"
            } else if (let [a, _, ...rest] = foo) {
                "array"
            } else {
                "other"
            };              
        "#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"
            let bar = if (let {x: x is string} = foo) {
                "object"
            } else if (let [a is Array, _, ...rest] = foo) {
                "array"
            } else {
                "other"
            };              
            "#
        ));
    }

    #[test]
    fn classes() {
        insta::assert_debug_snapshot!(parse("class Foo { a: number; }"));
        insta::assert_debug_snapshot!(parse("class Foo { a: number = 5; }"));
        insta::assert_debug_snapshot!(parse("class Foo { a = 5; }"));
        insta::assert_debug_snapshot!(parse("class Foo { static a: number; }"));
        insta::assert_debug_snapshot!(parse("class Foo { foo() {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { foo(): string {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { static foo(): string {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { get foo() {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { set foo(x) {} }"));
        insta::assert_debug_snapshot!(parse("class Foo { constructor(x) {} }"));

        let src = r#"
        class Foo {
            constructor(self, x) {
                self.x = x;
            }
            bar(self, y) {
                self.x + y;
            }
            baz(mut self, x) {
                self.x = x;
            }
        }
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn for_loops() {
        let src = r#"
        let mut sum = 0;
        for (const num of [1, 2, 3]) {
            sum = sum + num;
        }
        "#;
        insta::assert_debug_snapshot!(parse(src));
    }

    // #[test]
    // fn top_level_parse_error() {
    //     let result = parse(
    //         r#"
    //         let obj = {foo: "hello"};
    //         let foo = obj."#,
    //     );
    //     assert_eq!(
    //         result,
    //         Err(ParseError::from("failed to parse: 'let foo = obj.'"))
    //     );
    // }
}
