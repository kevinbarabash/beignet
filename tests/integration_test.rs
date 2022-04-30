use chumsky::prelude::*;
use std::collections::HashMap;

use cricket::context::Env;
use cricket::infer::{infer_stmt, infer_prog};
use cricket::parser::token_parser;
use cricket::lexer::lexer;
use cricket::types::*;
use cricket::ts::convert::{extend_scheme};
use cricket::syntax::{Pattern, Program};

fn infer(input: &str) -> String {
    let env: Env = HashMap::new();
    let result = lexer().parse(input).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(env, &stmt);
    format!("{}", result)
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

fn build_d_ts(env: &Env, prog: &Program) -> String {
    let mut lines: Vec<_> = vec![];

    for (statement, _) in &prog.body {
        match statement {
            cricket::syntax::Statement::Decl { pattern: (pat, _), value: (expr, _) } => {
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
fn infer_decl() {
    let env: Env = HashMap::new();
    let src = r#"
    let foo = (a, b) => a + b
    let bar = "hello"
    "#;
    let result = lexer().parse(src).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();
    let env = infer_prog(env, &prog);
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
