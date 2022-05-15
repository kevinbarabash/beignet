use chumsky::prelude::*;
use std::collections::HashMap;

use crochet::parser::*;
use crochet::infer::*;

fn main() {
    println!("Hello, world!");

    let env: Env = HashMap::new();
    let ctx = Context::from(env);
    let prog = parser().parse("5 + 10").unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(&ctx, &stmt);

    assert_eq!(format!("{}", result), "number");

    let foo = "var add = (a, b => {\na + b\n};";
    println!("foo = {}", foo);
    println!("foo (debug) = {:?}", foo);
}
