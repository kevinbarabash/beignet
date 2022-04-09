use std::collections::HashMap;

use nouveau_lib::context::*;
use nouveau_lib::infer::infer_expr;
use nouveau_lib::syntax::*;
use nouveau_lib::types::*;

#[test]
fn infer_number_literal() {
    let env: Env = HashMap::new();
    let expr: Expr = Expr::Lit(Literal::Num(String::from("5.0")));
    let result = infer_expr(env, &expr);

    assert_eq!(format!("{}", result), "5.0");
}

#[test]
fn infer_func() {
    let env: Env = HashMap::new();
    let expr: Expr = Expr::Lam(
        vec![BindingIdent::Ident(String::from("x"))],
        Box::from(Expr::Ident(String::from("x"))),
        false,
    );
    let result = infer_expr(env, &expr);

    assert_eq!(format!("{}", result), "<a1>(a1) => a1");
}

#[test]
fn type_debug_trait() {
    let t = Type::from(TFun {
        // TODO: add From trait impls to go from Primitive to Type
        args: vec![Type::Prim(Primitive::Num)],
        ret: Box::new(Type::Prim(Primitive::Num)),
    });

    assert_eq!(format!("{}", t), "(number) => number");
}
