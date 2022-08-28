use chumsky::prelude::*;
use crochet_dts::parse_dts::parse_dts;
use crochet_infer::*;
use crochet_parser::parser;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct CompileResult {
    js: String,
    dts: String,
}

#[wasm_bindgen]
impl CompileResult {
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
pub fn compile(input: &str, lib: &str) -> Result<CompileResult, String> {
    let program = parser().parse(input).unwrap();

    let js = crochet_codegen::js::codegen_js(&program);

    // TODO: return errors as part of CompileResult
    let mut ctx = parse_dts(lib).unwrap();
    let ctx = infer_prog(&program, &mut ctx)?;
    let dts = crochet_codegen::d_ts::codegen_d_ts(&program, &ctx);

    Ok(CompileResult { js, dts })
}
