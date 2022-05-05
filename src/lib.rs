pub mod constraint_solver;
pub mod context;
pub mod infer;
pub mod js;
pub mod lexer;
pub mod literal;
pub mod parser;
pub mod substitutable;
pub mod syntax;
pub mod ts;
pub mod types;

use chumsky::*;
use wasm_bindgen::prelude::*;
use std::collections::HashMap;

use context::Env;
use infer::infer_prog;
use syntax::{Pattern, Program};
use ts::convert::convert_scheme;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

fn greet(msg: &str) {
    alert(msg);
}

#[wasm_bindgen]
pub fn foo() {
    greet("Hello, world!");
}

#[wasm_bindgen]
pub struct CompileResult {
    js: String,
    dts: String,
}

#[wasm_bindgen]
impl CompileResult {
    // #[wasm_bindgen(constructor)]
    // pub fn new(field: i32) -> Baz {
    //     Baz { field }
    // }

    #[wasm_bindgen(getter)]
    pub fn js(&self) -> String {
        self.js.to_owned()
    }

    #[wasm_bindgen(getter)]
    pub fn dts(&self) -> String {
        self.dts.to_owned()
    }
}

fn build_d_ts(env: &Env, prog: &Program) -> String {
    let mut lines: Vec<_> = vec![];

    for (statement, _) in &prog.body {
        match statement {
            syntax::Statement::Decl { pattern: (pat, _), value: (expr, _) } => {
                let name = match pat {
                    Pattern::Ident { name } => name,
                };
                let scheme = env.get(name).unwrap();
                let result = convert_scheme(&scheme, Some(&expr));
                let line = format!("export declare const {name} = {result};");
                lines.push(line.to_owned());
            },
            _ => ()
        }
    };

    lines.join("\n")
}

#[wasm_bindgen]
pub fn compile(input: &str) -> CompileResult {
    let result = lexer::lexer().parse(input).unwrap();
    let spans: Vec<_> = result.iter().map(|(_, s)| s.to_owned()).collect();
    let tokens: Vec<_> = result.iter().map(|(t, _)| t.to_owned()).collect();
    let prog = parser::token_parser(&spans).parse(tokens).unwrap();

    let js_tree = js::builder::build_js(&prog);
    let js = js::printer::print_js(&js_tree);

    let env: Env = HashMap::new();
    let env = infer_prog(env, &prog);
    let dts = build_d_ts(&env, &prog);

    CompileResult { js, dts }
}
