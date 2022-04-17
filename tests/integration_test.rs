use chumsky::prelude::*;
use std::collections::HashMap;

use nouveau_lib::context::Env;
use nouveau_lib::infer::infer_stmt;
use nouveau_lib::parser::token_parser;
use nouveau_lib::lexer::lexer;
use nouveau_lib::types::*;

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
fn type_debug_trait() {
    let t = Type::from(TLam {
        // TODO: add From trait impls to go from Primitive to Type
        args: vec![Type::Prim(Primitive::Num)],
        ret: Box::new(Type::Prim(Primitive::Num)),
    });

    assert_eq!(format!("{}", t), "(number) => number");
}
