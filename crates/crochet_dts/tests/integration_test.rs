use chumsky::prelude::*;
use std::fs;

use crochet_ast::Program;
use crochet_parser::parser;

use crochet_dts::parse_dts::*;

static LIB_ES5_D_TS: &str = "../../node_modules/typescript/lib/lib.es5.d.ts";

fn infer_prog(src: &str) -> (Program, crochet_infer::Context) {
    let lib = fs::read_to_string(LIB_ES5_D_TS).unwrap();
    let mut ctx = parse_dts(&lib).unwrap();

    let result = parser().parse(src);
    let prog = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };
    let ctx = crochet_infer::infer_prog(&prog, &mut ctx).unwrap();

    (prog, ctx)
}

#[test]
fn infer_adding_variables() {
    let src = r#"
    let msg = "Hello, world!"
    let len = msg.length.toString() // radix is optional
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value_scheme("len").unwrap());
    assert_eq!(result, "string");
}

#[test]
fn infer_method_on_array() {
    let src = r#"
    declare let arr: string[]
    let map = arr.map
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value_scheme("map").unwrap());
    assert_eq!(
        result,
        "<t0>(callbackfn: (value: string, index: number, array: string[]) => U, thisArg?: t0) => U[]"
    );
}

#[test]
fn infer_method_on_arrays_of_different_things() {
    let src = r#"
    declare let str_arr: string[]
    declare let num_arr: number[]
    let map1 = str_arr.map
    let map2 = num_arr.map
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value_scheme("map1").unwrap());
    assert_eq!(
        result,
        "<t0>(callbackfn: (value: string, index: number, array: string[]) => U, thisArg?: t0) => U[]"
    );
    let result = format!("{}", ctx.lookup_value_scheme("map2").unwrap());
    assert_eq!(
        result,
        "<t0>(callbackfn: (value: number, index: number, array: number[]) => U, thisArg?: t0) => U[]"
    );
}

#[test]
fn infer_array_method_on_tuple() {
    let src = r#"
    let tuple = [5, "hello", true]
    let map = tuple.map
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value_scheme("map").unwrap());
    assert_eq!(
        result,
        "<t0>(callbackfn: (value: \"hello\" | 5 | true, index: number, array: \"hello\" | 5 | true[]) => U, thisArg?: t0) => U[]"
    );
}

#[test]
fn infer_static_properties() {
    let src = r#"
    let max = Number.MAX_VALUE
    let parse = Date.parse
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value_scheme("max").unwrap());
    assert_eq!(result, "number");
    let result = format!("{}", ctx.lookup_value_scheme("parse").unwrap());
    assert_eq!(result, "(s: string) => number");
}
