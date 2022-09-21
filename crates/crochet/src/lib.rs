use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_void};
use std::str;

use crochet_dts::parse_dts::parse_dts;
use crochet_infer::*;

#[repr(C)]
pub struct WasmString {
    pub offset: *const c_char,
    pub length: u32,
}

#[repr(C)]
pub struct CompileResult {
    js: WasmString,
    dts: WasmString,
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

fn _compile(input: &str, lib: &str) -> Result<(String, String), String> {
    let program = crochet_tree_sitter_parser::parse(input).unwrap();

    let js = crochet_codegen::js::codegen_js(&program);

    // TODO: return errors as part of CompileResult
    let mut ctx = parse_dts(lib).unwrap();
    let ctx = infer_prog(&program, &mut ctx)?;
    let dts = crochet_codegen::d_ts::codegen_d_ts(&program, &ctx);

    Ok((js, dts))
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
        Ok((js, dts)) => {
            let result = CompileResult {
                js: string_to_wasm_string(&js),
                dts: string_to_wasm_string(&dts),
                error: string_to_wasm_string(""),
            };
            Box::into_raw(Box::new(result))
        }
        Err(error) => {
            let result = CompileResult {
                js: string_to_wasm_string(""),
                dts: string_to_wasm_string(""),
                error: string_to_wasm_string(&error),
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
        .set_language(tree_sitter_crochet::language())
        .expect("Error loading crochet language");

    let str_slice: &str = CStr::from_ptr(c_buf).to_str().unwrap();
    println!("str_slice = {str_slice}");
    let tree = parser.parse(str_slice, None).unwrap();

    let root = tree.root_node();
    let result = format!("{root:#?}");

    Box::into_raw(Box::new(string_to_wasm_string(&result)))
}

#[no_mangle]
#[cfg(target_family = "wasm")]
pub extern "C" fn _start() {}
