use chumsky::prelude::*;
use std::collections::HashMap;

use crochet::ast::Program;
use crochet::infer::*;
use crochet::codegen::*;
use crochet::parser::parser;

fn infer(input: &str) -> String {
    let env: Env = HashMap::new();
    let ctx = Context::from(env);
    let prog = parser().parse(input).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(&ctx, &stmt);
    format!("{}", result)
}

fn infer_prog(src: &str) -> (Program, Env) {
    let env: Env = HashMap::new();
    let result = parser().parse(src);
    let prog = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };
    // println!("prog = {:#?}", &prog);
    // let prog = token_parser(&spans).parse(tokens).unwrap();
    let env = crochet::infer::infer_prog(env, &prog);

    (prog, env)
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5"), "5");
}

#[test]
fn infer_lam() {
    assert_eq!(infer("(x) => x"), "<A>(A) => A");
}

#[test]
fn infer_let_inside_function() {
    assert_eq!(infer("() => let x = 5 in x"), "() => 5");
}

#[test]
fn infer_op() {
    assert_eq!(infer("5 + 10"), "number");
}

#[test]
fn infer_fn_param() {
    assert_eq!(
        infer("(f, x) => f(x) + x"),
        "((number) => number, number) => number"
    );
}

#[test]
fn infer_fn_param_used_with_multiple_other_params() {
    assert_eq!(
        infer("(f, x, y) => f(x) + f(y)"),
        "<A>((A) => number, A, A) => number"
    );
}

#[test]
fn infer_i_combinator() {
    let (_, env) = infer_prog("let I = (x) => x");
    let result = format!("{}", env.get("I").unwrap());
    assert_eq!(result, "<A>(A) => A");
}

#[test]
fn infer_k_combinator_not_curried() {
    let (program, env) = infer_prog("let K = (x, y) => x");
    let result = format!("{}", env.get("K").unwrap());
    assert_eq!(result, "<A, B>(A, B) => A");

    let result = codegen_d_ts(&program, &env);
    insta::assert_snapshot!(result, @"export declare const K: <A, B>(x: A, y: B) => A;\n");
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, env) = infer_prog("let S = (f, g, x) => f(x, g(x))");
    let result = format!("{}", env.get("S").unwrap());
    assert_eq!(result, "<A, B, C>((A, B) => C, (A) => B, A) => C");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, env) = infer_prog("let K = (x) => (y) => x");
    let result = format!("{}", env.get("K").unwrap());
    assert_eq!(result, "<A, B>(A) => (B) => A");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, env) = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x))");
    let result = format!("{}", env.get("S").unwrap());
    assert_eq!(
        result,
        "<A, B, C>((A) => (B) => C) => ((A) => B) => (A) => C"
    );
}

#[test]
fn infer_skk() {
    let src = r#"
    let S = (f) => (g) => (x) => f(x)(g(x))
    let K = (x) => (y) => x
    let I = S(K)(K)
    "#;
    let (_, env) = infer_prog(src);
    let result = format!("{}", env.get("I").unwrap());
    assert_eq!(result, "<A>(A) => A");
}

#[test]
fn infer_adding_variables() {
    let src = r#"
    let x = 5
    let y = 10
    let z = x + y
    "#;
    let (_, env) = infer_prog(src);
    let result = format!("{}", env.get("z").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_decl() {
    let src = r#"
    let foo = (a, b) => a + b
    let bar = "hello"
    "#;
    let (program, env) = infer_prog(src);
    let result = codegen_d_ts(&program, &env);

    insta::assert_snapshot!(result, @r###"
    export declare const foo: (a: number, b: number) => number;
    export declare const bar: "hello";
    "###);
}

#[test]
fn infer_with_subtyping() {
    let src = r#"
    let foo = (a, b) => a + b
    let bar = foo(5, 10)
    "#;
    let (program, env) = infer_prog(src);
    let result = codegen_d_ts(&program, &env);

    insta::assert_snapshot!(result, @r###"
    export declare const foo: (a: number, b: number) => number;
    export declare const bar: number;
    "###);
}

#[test]
fn infer_if_else_without_widening() {
    let (_, env) = infer_prog("let x = if (true) { 5 } else { 5 }");
    let result = format!("{}", env.get("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_if_else_with_widening() {
    let (_, env) = infer_prog("let x = if (true) { 5 } else { 10 }");
    let result = format!("{}", env.get("x").unwrap());
    assert_eq!(result, "5 | 10");
}

#[test]
fn infer_if_else_with_multiple_widenings() {
    let src = r#"
    let x = if (true) { 5 } else { 10 }
    let y = if (false) { x } else { 15 }
    "#;
    let (program, env) = infer_prog(src);
    let result = format!("{}", env.get("y").unwrap());
    assert_eq!(result, "5 | 10 | 15");

    let result = codegen_d_ts(&program, &env);
    insta::assert_snapshot!(result, @r###"
    export declare const x: 5 | 10;
    export declare const y: 5 | 10 | 15;
    "###);
}

#[test]
fn infer_equal_with_numbers() {
    let (_, env) = infer_prog("let cond = 5 == 10");
    let result = format!("{}", env.get("cond").unwrap());
    assert_eq!(result, "boolean");
}

#[test]
fn infer_not_equal_with_variables() {
    let (_, env) = infer_prog("let neq = (a, b) => a != b");
    let result = format!("{}", env.get("neq").unwrap());
    assert_eq!(result, "<A>(A, A) => boolean");
}

#[test]
fn infer_inequalities() {
    let src = r###"
    let lt = (a, b) => a < b
    let lte = (a, b) => a <= b
    let gt = (a, b) => a > b
    let gte = (a, b) => a >= b
    "###;
    let (_, env) = infer_prog(src);
    assert_eq!(format!("{}", env.get("lt").unwrap()), "(number, number) => boolean");
    assert_eq!(format!("{}", env.get("lte").unwrap()), "(number, number) => boolean");
    assert_eq!(format!("{}", env.get("gt").unwrap()), "(number, number) => boolean");
    assert_eq!(format!("{}", env.get("gte").unwrap()), "(number, number) => boolean");
}

#[test]
fn infer_let_rec_until() {
    let src = "let rec until = (p, f, x) => if (p(x)) { x } else { until(p, f, f(x)) }";
    let (program, env) = infer_prog(src);
    let result = format!("{}", env.get("until").unwrap());
    assert_eq!(result, "<A>((A) => boolean, (A) => A, A) => A");

    let result = codegen_d_ts(&program, &env);
    insta::assert_snapshot!(result, @"export declare const until: <A>(p: (arg0: A) => boolean, f: (arg0: A) => A, x: A) => A;\n");
}

#[test]
fn infer_fib() {
    let src = r###"
    let rec fib = (n) => if (n == 0) {
        0
    } else {
        if (n == 1) {
            1
        } else {
            fib(n - 1) + fib(n - 2)
        }
    }
    "###;

    let (_, env) = infer_prog(src);
    // TODO: simplify union types before returning them
    assert_eq!(format!("{}", env.get("fib").unwrap()), "(number | 1 | 0 | number | number) => number | 0 | 1");
}

#[test]
fn infer_obj() {
    let src = r#"
    let point = {x: 5, y: 10, msg: "Hello, world!"}
    "#;
    let (_, env) = infer_prog(src);
    let result = format!("{}", env.get("point").unwrap());
    assert_eq!(result, "{x: 5, y: 10, msg: \"Hello, world!\"}");
}

#[test]
fn infer_async() {
    let src = "let foo = async () => 10";
    let (_, env) = infer_prog(src);
    let result = format!("{}", env.get("foo").unwrap());
    assert_eq!(result, "() => Promise<10>");
}

#[test]
fn infer_async_math() {
    let src = "let add = async (a, b) => await a() + await b()";
    let (program, env) = infer_prog(src);
    let result = format!("{}", env.get("add").unwrap());
    assert_eq!(
        result,
        "(() => Promise<number>, () => Promise<number>) => Promise<number>"
    );

    let result = codegen_d_ts(&program, &env);
    insta::assert_snapshot!(result, @"export declare const add: (a: () => Promise<number>, b: () => Promise<number>) => Promise<number>;\n");
}

#[test]
fn codegen_let_rec() {
    let src = "let rec f = () => f()";
    let (program, env) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @"export const f = ()=>f();
");

    let result = codegen_d_ts(&program, &env);

    insta::assert_snapshot!(result, @"export declare const f: <A>() => A;\n");
}

#[test]
fn codegen_if_else() {
    let src = r#"
    let cond = true
    let result = if (cond) { 5 } else { 5 }
    "#;
    let (program, env) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const cond = true;
    export const result = (()=>{
        if (cond) {
            return 5;
        } else {
            return 5;
        }
    })();
    "###);

    let result = codegen_d_ts(&program, &env);

    insta::assert_snapshot!(result, @r###"
    export declare const cond: true;
    export declare const result: 5;
    "###);
}

#[test]
fn codegen_object() {
    let src = "let point = {x: 5, y: 10}";
    let (program, env) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @r###"
    export const point = {
        x: 5,
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &env);

    insta::assert_snapshot!(result, @r###"
    export declare const point: {
        x: 5;
        y: 10;
    };
    "###);
}

#[test]
fn codegen_async_math() {
    let src = "let add = async (a, b) => await a() + await b()";
    let (program, env) = infer_prog(src);

    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @"export const add = async (a, b)=>await a() + await b();
");

    let result = codegen_d_ts(&program, &env);

    insta::assert_snapshot!(result, @"export declare const add: (a: () => Promise<number>, b: () => Promise<number>) => Promise<number>;\n");
}
