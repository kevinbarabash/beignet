use pretty_assertions::assert_eq;
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::str;

use crochet_infer::*;
use crochet_interop::parse::parse_dts;

use crochet::compile_error::CompileError;
use crochet::diagnostics::get_diagnostics;

enum Mode {
    Check,
    Write,
}

#[testing_macros::fixture("tests/pass/*.crochet")]
fn pass(in_path: PathBuf) {
    let mode = match env::var("UPDATE") {
        Ok(_) => Mode::Write,
        Err(_) => Mode::Check,
    };

    let mut js_path = in_path.clone();
    js_path.set_extension("js");
    let mut srcmap_path = in_path.clone();
    srcmap_path.set_extension("js.map");
    let mut d_ts_path = in_path.clone();
    d_ts_path.set_extension("d.ts");

    let input = fs::read_to_string(in_path).unwrap();
    let lib = fs::read_to_string(LIB_ES5_D_TS).unwrap();

    match compile(&input, &lib) {
        Ok((js_output, srcmap_output, d_ts_output)) => match mode {
            Mode::Check => {
                let js_fixture = fs::read_to_string(js_path).unwrap();
                assert_eq!(js_fixture, js_output);
                let srcmap_fixture = fs::read_to_string(srcmap_path).unwrap();
                assert_eq!(srcmap_fixture, srcmap_output);
                let d_ts_fixture = fs::read_to_string(d_ts_path).unwrap();
                assert_eq!(d_ts_fixture, d_ts_output);
            }
            Mode::Write => {
                let mut file = fs::File::create(js_path).unwrap();
                file.write_all(js_output.as_bytes())
                    .expect("unable to write data");
                let mut file = fs::File::create(srcmap_path).unwrap();
                file.write_all(srcmap_output.as_bytes())
                    .expect("unable to write data");
                let mut file = fs::File::create(d_ts_path).unwrap();
                file.write_all(d_ts_output.as_bytes())
                    .expect("unable to write data");
            }
        },
        Err(report) => {
            println!("{report:#?}");
            panic!("Unexpected error");
        }
    };
}

#[testing_macros::fixture("tests/errors/*.crochet")]
fn fail(in_path: PathBuf) {
    let lib = fs::read_to_string(LIB_ES5_D_TS).unwrap();

    let mode = match env::var("UPDATE") {
        Ok(_) => Mode::Write,
        Err(_) => Mode::Check,
    };

    let mut error_output_path = in_path.clone();
    error_output_path.set_extension("error");

    let input = fs::read_to_string(in_path).unwrap();

    match compile(&input, &lib) {
        Ok(_) => panic!("Expected an error"),
        Err(report) => {
            // let buf = strip_ansi_escapes::strip(format!("{report:#?}")).unwrap();
            // let error_output = str::from_utf8(&buf).unwrap();
            let diagnostics = get_diagnostics(report, &input);
            let error_output = diagnostics.join("\n");
            match mode {
                Mode::Check => {
                    let error_fixture = fs::read_to_string(error_output_path).unwrap();
                    assert_eq!(error_fixture, error_output);
                }
                Mode::Write => {
                    let mut file = fs::File::create(error_output_path).unwrap();
                    file.write_all(error_output.as_bytes())
                        .expect("unable to write data");
                }
            }
        }
    };
}

fn compile(input: &str, lib: &str) -> Result<(String, String, String), CompileError> {
    let mut program = match crochet_parser::parse(input) {
        Ok(program) => program,
        Err(error) => return Err(CompileError::ParseError(error)),
    };

    let (js, srcmap) = crochet_codegen::js::codegen_js(input, &program);

    // TODO: return errors as part of CompileResult
    let mut ctx = parse_dts(lib).unwrap();
    let ctx = match infer_prog(&mut program, &mut ctx) {
        Ok(ctx) => ctx,
        Err(error) => return Err(CompileError::TypeError(error)),
    };
    let dts = crochet_codegen::d_ts::codegen_d_ts(&program, &ctx);

    Ok((js, srcmap, dts))
}

static LIB_ES5_D_TS: &str = "../../node_modules/typescript/lib/lib.es5.d.ts";
