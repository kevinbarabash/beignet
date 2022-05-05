use chumsky::prelude::*;
use std::collections::HashMap;

use crochet::context::{Context, Env};
use crochet::infer::infer_stmt;
use crochet::js::builder::*;
use crochet::js::printer::*;
use crochet::lexer::lexer;
use crochet::parser::token_parser;
use crochet::syntax::{Pattern, Program};
use crochet::ts::convert::extend_scheme;
use crochet::types::*;

fn infer(input: &str) -> String {
    let env: Env = HashMap::new();
    let ctx = Context::from(env);
    let result = lexer().parse(input).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(&ctx, &stmt);
    format!("{}", result)
}

fn infer_prog(src: &str) -> (Program, Env) {
    let env: Env = HashMap::new();
    let result = lexer().parse(src).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let result = token_parser(&spans).parse(tokens);
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

fn build_d_ts(env: &Env, prog: &Program) -> String {
    let mut lines: Vec<_> = vec![];

    for (statement, _) in &prog.body {
        match statement {
            crochet::syntax::Statement::Decl {
                pattern: (pat, _),
                value: (expr, _),
            } => {
                let name = match pat {
                    Pattern::Ident { name } => name,
                };
                let scheme = env.get(name).unwrap();
                let result = extend_scheme(&scheme, Some(&expr));
                let line = format!("export declare const {name} = {result};");
                lines.push(line.to_owned());
            }
            _ => (),
        }
    }

    lines.join("\n")
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5"), "5");
}

#[test]
fn infer_lam() {
    assert_eq!(infer("(x) => x"), "<T1>(T1) => T1");
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
        "<T2>((T2) => number, T2, T2) => number"
    );
}

#[test]
fn infer_i_combinator() {
    let (_, env) = infer_prog("let I = (x) => x");
    let result = format!("{}", env.get("I").unwrap());
    assert_eq!(result, "<T1>(T1) => T1");
}

#[test]
fn infer_k_combinator_not_curried() {
    let (_, env) = infer_prog("let K = (x, y) => x");
    let result = format!("{}", env.get("K").unwrap());
    assert_eq!(result, "<T1, T2>(T1, T2) => T1");
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, env) = infer_prog("let S = (f, g, x) => f(x, g(x))");
    let result = format!("{}", env.get("S").unwrap());
    assert_eq!(result, "<T3, T4, T5>((T3, T4) => T5, (T3) => T4, T3) => T5");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, env) = infer_prog("let K = (x) => (y) => x");
    let result = format!("{}", env.get("K").unwrap());
    assert_eq!(result, "<T1, T2>(T1) => (T2) => T1");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, env) = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x))");
    let result = format!("{}", env.get("S").unwrap());
    // "<a, b, c>((a) => (b) => c) => ((a) => b) => (a) => c"
    assert_eq!(
        result,
        "<T3, T5, T6>((T3) => (T5) => T6) => ((T3) => T5) => (T3) => T6"
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
    assert_eq!(result, "<T11>(T11) => T11");
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
    let (prog, env) = infer_prog(src);
    let result = build_d_ts(&env, &prog);

    insta::assert_snapshot!(result, @r###"
    export declare const foo = (a: number, b: number) => number;
    export declare const bar = "hello";
    "###);
}

#[test]
fn type_debug_trait() {
    let t = Type::from(TLam {
        // TODO: add From trait impls to go from Primitive to Type
        args: vec![Type::Prim(Primitive::Num)],
        ret: Box::new(Type::Prim(Primitive::Num)),
    });

    assert_eq!(format!("{}", t), "(number) => number");
}

#[test]
fn infer_if_else_without_widening() {
    let (_, env) = infer_prog("let x = if (true) { 5 } else { 5 }");
    let result = format!("{}", env.get("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
#[ignore]
fn infer_if_else_with_widening() {
    // TODO: implement type widening for number literals
    let (_, env) = infer_prog("let x = if (true) { 5 } else { 10 }");
    let result = format!("{}", env.get("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_let_rec_until() {
    let src = "let rec until = (p, f, x) => if (p(x)) { x } else { until(p, f, f(x)) }";
    let (_, env) = infer_prog(src);
    let result = format!("{}", env.get("until").unwrap());
    assert_eq!(result, "<T7>((T7) => boolean, (T7) => T7, T7) => T7");
}

#[test]
fn codegen_let_rec() {
    let src = "let rec f = () => f()";
    let (prog, env) = infer_prog(src);
    let js_tree = build_js(&prog);

    insta::assert_snapshot!(print_js(&js_tree), @r###"
    const f = () => f();

    export {f};
    "###);

    insta::assert_snapshot!(build_d_ts(&env, &prog), @"export declare const f = <T2>() => T2;");
}

#[test]
fn codegen_if_else() {
    let src = r#"
    let cond = true
    let result = if (cond) { 5 } else { 5 }
    "#;
    let (prog, env) = infer_prog(src);

    // TODO: enable this assertion once we can codegen if-else
    // let js_tree = build_js(&prog);
    // insta::assert_snapshot!(print_js(&js_tree), @"");

    insta::assert_snapshot!(build_d_ts(&env, &prog), @r###"
    export declare const cond = true;
    export declare const result = 5;
    "###);
}
