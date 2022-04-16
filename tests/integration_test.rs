use chumsky::prelude::*;
use std::collections::HashMap;

use nouveau_lib::context::Env;
use nouveau_lib::infer::infer_stmt;
use nouveau_lib::parser::parser;
use nouveau_lib::types::*;

#[test]
fn infer_number_literal() {
    let env: Env = HashMap::new();
    let prog = parser().parse("5").unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(env, &stmt);

    assert_eq!(format!("{}", result), "5");
}

#[test]
fn infer_lam() {
    let env: Env = HashMap::new();
    let prog = parser().parse("(x) => x").unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(env, &stmt);

    assert_eq!(format!("{}", result), "<a1>(a1) => a1");
}

#[test]
fn infer_let() {
    let env: Env = HashMap::new();
    let prog = parser().parse("let x = 5 in x").unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(env, &stmt);

    assert_eq!(format!("{}", result), "5");
}

#[test]
fn infer_op() {
    let env: Env = HashMap::new();
    let prog = parser().parse("5 + 10").unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(env, &stmt);

    assert_eq!(format!("{}", result), "number");
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
