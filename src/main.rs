use chumsky::prelude::*;
use std::collections::HashMap;

use crochet::parser::*;
use crochet::lexer::*;
use crochet::context::Env;
use crochet::infer::*;

fn main() {
    println!("Hello, world!");

    let env: Env = HashMap::new();
    let result = lexer().parse("5 + 10").unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = token_parser(&spans).parse(tokens).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = infer_stmt(env, &stmt);

    assert_eq!(format!("{}", result), "number");

    // let result = lexer().parse("let x = 5 in x").unwrap();
    // let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    // let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    // let ast = token_parser(&spans).parse(tokens).unwrap();
    // println!("ast = {:?}", ast);

    let foo = "var add = (a, b => {\na + b\n};";
    println!("foo = {}", foo);
    println!("foo (debug) = {:?}", foo);
}
