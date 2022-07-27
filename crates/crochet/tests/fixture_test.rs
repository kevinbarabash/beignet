use chumsky::prelude::*;
use pretty_assertions::assert_eq;

use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

use crochet_codegen::d_ts::*;
use crochet_codegen::js::*;
use crochet_infer::*;
use crochet_parser::parser;

enum Mode {
    Check,
    Write,
}

#[cfg(not(tarpaulin_include))]
#[testing_macros::fixture("tests/pass/*.crochet")]
fn pass(in_path: PathBuf) {
    let mode = match env::var("UPDATE") {
        Ok(_) => Mode::Write,
        Err(_) => Mode::Check,
    };

    let mut js_path = in_path.clone();
    js_path.set_extension("js");
    let mut d_ts_path = in_path.clone();
    d_ts_path.set_extension("d.ts");

    let input = fs::read_to_string(in_path).unwrap();

    let program = parser().parse(input).unwrap();
    let js_output = codegen_js(&program);
    match mode {
        Mode::Check => {
            let js_fixture = fs::read_to_string(js_path).unwrap();
            assert_eq!(js_fixture, js_output);
        }
        Mode::Write => {
            let mut file = fs::File::create(js_path).unwrap();
            file.write_all(js_output.as_bytes())
                .expect("unable to write data");
        }
    }

    let ctx = infer_prog(&program).unwrap();
    let d_ts_output = codegen_d_ts(&program, &ctx);
    match mode {
        Mode::Check => {
            let d_ts_fixture = fs::read_to_string(d_ts_path).unwrap();
            assert_eq!(d_ts_fixture, d_ts_output);
        }
        Mode::Write => {
            let mut file = fs::File::create(d_ts_path).unwrap();
            file.write_all(d_ts_output.as_bytes())
                .expect("unable to write data");
        }
    }
}
