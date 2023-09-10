use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

use escalier_interop::parse::parse_dts;

pub mod compile_error;
pub mod diagnostics;

use crate::compile_error::CompileError;
use crate::diagnostics::get_diagnostics_from_compile_error;

#[derive(Serialize, Deserialize)]
pub struct CompileResult {
    js: String,
    srcmap: String,
    dts: String,
    ast: String,
    error: String,
}

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

fn _compile(input: &str, lib: &str) -> Result<(String, String, String, String), CompileError> {
    log(&format!("parsing input: {input}"));
    let mut program = escalier_parser::parse(input)?;
    let ast = format!("{program:#?}");

    let (js, srcmap) = escalier_codegen::js::codegen_js(input, &program);

    // TODO: return errors as part of CompileResult
    let (mut checker, mut ctx) = parse_dts(lib).unwrap();

    // TODO: get rid of panics and return errors instead
    match checker.infer_program(&mut program, &mut ctx) {
        Ok(_) => {
            if !checker.current_report.diagnostics.is_empty() {
                panic!("was expecting infer_prog() to return no errors");
            }
        }
        Err(error) => {
            let message = error.to_string();
            panic!("{message}");
        }
    }

    let dts = escalier_codegen::d_ts::codegen_d_ts(&program, &ctx, &checker)?;

    Ok((js, srcmap, dts, ast))
}

#[wasm_bindgen]
pub fn compile(input: &str, lib: &str) -> Result<JsValue, JsValue> {
    match _compile(input, lib) {
        Ok((js, srcmap, dts, ast)) => {
            let result = CompileResult {
                js,
                srcmap,
                dts,
                ast,
                error: "".to_string(),
            };
            Ok(serde_wasm_bindgen::to_value(&result)?)
        }
        Err(e) => {
            log("compile error");
            let diags = get_diagnostics_from_compile_error(e, input);
            Err(serde_wasm_bindgen::to_value(&diags)?)
        }
    }
}

#[wasm_bindgen]
pub fn parse(input: &str) -> Result<JsValue, JsValue> {
    match escalier_parser::parse(input) {
        Ok(program) => {
            // TODO: update the AST to implement Serialize/Deserialize
            let ast = format!("{program:#?}");
            Ok(serde_wasm_bindgen::to_value(&ast)?)
        }
        Err(e) => Err(serde_wasm_bindgen::to_value(&e.message)?),
    }
}
