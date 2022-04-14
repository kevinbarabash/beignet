use chumsky::prelude::*;
use std::collections::HashMap;

use nouveau_lib::parser::*;
use nouveau_lib::context::Env;
use nouveau_lib::infer::*;

fn main() {
    println!("Hello, world!");

    let env: Env = HashMap::new();
    let expr = parser().parse("5 + 10").unwrap();
    let result = infer_expr(env, &expr);

    assert_eq!(format!("{}", result), "number");

    let ast = parser().parse("let x = 5 in x").unwrap();
    println!("ast = {:?}", ast);
}
