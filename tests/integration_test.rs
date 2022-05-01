use chumsky::prelude::*;
use std::collections::HashMap;

use crochet::context::{Context, Env};
use crochet::infer::infer_stmt;
use crochet::parser::token_parser;
use crochet::lexer::lexer;
use crochet::types::*;
use crochet::ts::convert::{extend_scheme};
use crochet::syntax::{Pattern, Program};

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
    let prog = token_parser(&spans).parse(tokens).unwrap();
    let env = crochet::infer::infer_prog(env, &prog);
    
    (prog, env)
}

fn build_d_ts(env: &Env, prog: &Program) -> String {
    let mut lines: Vec<_> = vec![];

    for (statement, _) in &prog.body {
        match statement {
            crochet::syntax::Statement::Decl { pattern: (pat, _), value: (expr, _) } => {
                let name = match pat {
                    Pattern::Ident { name } => name,
                };
                let scheme = env.get(name).unwrap();
                let result = extend_scheme(&scheme, Some(&expr));
                let line = format!("export declare const {name} = {result};");
                lines.push(line.to_owned());
            },
            _ => ()
        }
    };

    lines.join("\n")
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5"), "5");
}

#[test]
fn infer_lam() {
    assert_eq!(infer("(x) => x"), "<a1>(a1) => a1");
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
    assert_eq!(infer("(f, x) => f(x) + x"), "((number) => number, number) => number");
}

#[test]
fn infer_fn_param_used_with_multiple_other_params() {
    assert_eq!(infer("(f, x, y) => f(x) + f(y)"), "<a2>((a2) => number, a2, a2) => number");
}

#[test]
fn infer_i_combinator() {
    let (_, env) = infer_prog("let I = (x) => x");
    let result = format!("{}", env.get("I").unwrap());
    assert_eq!(result, "<a1>(a1) => a1");
}

#[test]
fn infer_k_combinator_not_curried() {
    let (_, env) = infer_prog("let K = (x, y) => x");
    let result = format!("{}", env.get("K").unwrap());
    assert_eq!(result, "<a1, a2>(a1, a2) => a1");
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, env) = infer_prog("let S = (f, g, x) => f(x, g(x))");
    let result = format!("{}", env.get("S").unwrap());
    assert_eq!(result, "<a3, a4, a5>((a3, a4) => a5, (a3) => a4, a3) => a5");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, env) = infer_prog("let K = (x) => (y) => x");
    let result = format!("{}", env.get("K").unwrap());
    assert_eq!(result, "<a1, a2>(a1) => (a2) => a1");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, env) = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x))");
    let result = format!("{}", env.get("S").unwrap());
    // "<a, b, c>((a) => (b) => c) => ((a) => b) => (a) => c"
    assert_eq!(result, "<a3, a5, a6>((a3) => (a5) => a6) => ((a3) => a5) => (a3) => a6");
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
    assert_eq!(result, "<a5>(a5) => a5");
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
