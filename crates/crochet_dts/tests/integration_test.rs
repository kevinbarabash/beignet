use chumsky::prelude::*;

use crochet_ast::Program;
use crochet_infer::*;
use crochet_parser::parser;

use crochet_dts::parse_dts::*;

static LIB_ES5_D_TS: &str = "../../node_modules/typescript/lib/lib.es5.d.ts";

fn infer_prog(src: &str) -> (Program, crochet_infer::Context) {
    let dts = parse_dts(LIB_ES5_D_TS).unwrap();
    let mut ctx = Context::default();

    for name in dts.interfaces.keys() {
        let t = dts.get_interface(name);
        ctx.types.insert(name.to_owned(), types::Scheme::from(t));
    }

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
    let len = msg.length.toString(10) // radix is required b/c we don't support optional params yet
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("len").unwrap());
    assert_eq!(result, "string");
}
