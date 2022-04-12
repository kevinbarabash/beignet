use chumsky::prelude::*;
use std::collections::HashMap;

use nouveau_lib::context::Env;
use nouveau_lib::infer::infer_expr;
use nouveau_lib::parser::parser;
use nouveau_lib::types::*;

#[test]
fn infer_number_literal() {
    let env: Env = HashMap::new();
    let expr = parser().parse("5").unwrap();
    let result = infer_expr(env, &expr);

    assert_eq!(format!("{}", result), "5");
}

#[test]
fn infer_lam() {
    let env: Env = HashMap::new();
    let expr = parser().parse("(x) => x").unwrap();
    let result = infer_expr(env, &expr);

    assert_eq!(format!("{}", result), "<a1>(a1) => a1");
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
