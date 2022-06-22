use chumsky::prelude::*;
use crochet_infer::*;
use crochet_parser::parser;
use wasm_bindgen::prelude::*;

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

#[wasm_bindgen]
pub fn compile(input: &str) -> CompileResult {
    let program = parser().parse(input).unwrap();

    let js = crochet_codegen::js::codegen_js(&program);

    // TODO: return errors as part of CompileResult
    let ctx = infer_prog(&program).unwrap();
    let dts = crochet_codegen::d_ts::codegen_d_ts(&program, &ctx);

    CompileResult { js, dts }
}
