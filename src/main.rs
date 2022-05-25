use chumsky::prelude::*;

use crochet::parser::*;
use crochet::infer::*;

fn main() {
    println!("Hello, world!");

    let ctx = Context::default();
    let prog = parser().parse("5 + 10").unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(&ctx, stmt);

    assert_eq!(format!("{}", result), "number");
}
