use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_void};
use std::str;

use escalier_infer::*;
use escalier_interop::parse::parse_dts;

pub mod compile_error;
pub mod diagnostics;

use crate::compile_error::CompileError;
use crate::diagnostics::get_diagnostics;

#[repr(C)]
pub struct WasmString {
    pub offset: *const c_char,
    pub length: u32,
}

#[repr(C)]
pub struct CompileResult {
    js: WasmString,
    srcmap: WasmString,
    dts: WasmString,
    ast: WasmString,
    error: WasmString,
}

// A hacky way to allocate / deallocate memory in rust stable.
//
// Another way to do this would be to use heap::alloc, but that's still unstable
// You can check the progress on RFC #1974
// @ https://github.com/rust-lang/rust/issues/27389
//
#[no_mangle]
pub extern "C" fn allocate(length: usize) -> *mut c_void {
    let mut v = Vec::with_capacity(length);
    let ptr = v.as_mut_ptr();
    std::mem::forget(v);
    ptr
}

///
/// # Safety
///
#[no_mangle]
pub unsafe extern "C" fn deallocate(ptr: *mut c_void, length: usize) {
    std::mem::drop(Vec::from_raw_parts(ptr, 0, length));
}

fn _compile(input: &str, lib: &str) -> Result<(String, String, String, String), CompileError> {
    let mut program = match escalier_parser::parse(input) {
        Ok(program) => program,
        Err(error) => return Err(CompileError::ParseError(error)),
    };
    let ast = format!("{program:#?}");

    let (js, srcmap) = escalier_codegen::js::codegen_js(input, &program);

    // TODO: return errors as part of CompileResult
    let mut ctx = parse_dts(lib).unwrap();
    let ctx = match infer_prog(&mut program, &mut ctx) {
        Ok(ctx) => ctx,
        Err(error) => return Err(CompileError::TypeError(error)),
    };
    let dts = escalier_codegen::d_ts::codegen_d_ts(&program, &ctx);

    Ok((js, srcmap, dts, ast))
}

unsafe fn string_to_wasm_string(input: &str) -> WasmString {
    let length = input.len() as u32;
    let offset = CString::new(input).unwrap().into_raw();
    WasmString { offset, length }
}

///
/// # Safety
///
#[no_mangle]
pub unsafe extern "C" fn compile(input: *const c_char, lib: *const c_char) -> *const CompileResult {
    let input = CStr::from_ptr(input).to_str().unwrap();
    let lib = CStr::from_ptr(lib).to_str().unwrap();

    match _compile(input, lib) {
        Ok((js, srcmap, dts, ast)) => {
            let result = CompileResult {
                js: string_to_wasm_string(&js),
                srcmap: string_to_wasm_string(&srcmap),
                dts: string_to_wasm_string(&dts),
                ast: string_to_wasm_string(&ast),
                error: string_to_wasm_string(""),
            };
            Box::into_raw(Box::new(result))
        }
        Err(report) => {
            let diagnostics = get_diagnostics(report, input);
            let result = CompileResult {
                js: string_to_wasm_string(""),
                srcmap: string_to_wasm_string(""),
                dts: string_to_wasm_string(""),
                ast: string_to_wasm_string(""),
                // TODO: update report to exclude Escalier source code locations
                error: string_to_wasm_string(&diagnostics.join("\n")),
            };
            Box::into_raw(Box::new(result))
        }
    }
}

#[no_mangle]
///
/// # Safety
///
pub unsafe extern "C" fn parse(c_buf: *const c_char) -> *const WasmString {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_escalier::language())
        .expect("Error loading escalier language");

    let str_slice: &str = CStr::from_ptr(c_buf).to_str().unwrap();
    eprintln!("str_slice = {str_slice}");
    let tree = parser.parse(str_slice, None).unwrap();

    let root = tree.root_node();
    let result = format!("{root:#?}");

    Box::into_raw(Box::new(string_to_wasm_string(&result)))
}

#[no_mangle]
#[cfg(target_family = "wasm")]
pub extern "C" fn _start() {}
