use pretty_assertions::assert_eq;
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::str;

use escalier_infer::*;
use escalier_interop::parse::parse_dts;

use escalier::diagnostics::type_errors_to_string;

enum Mode {
    Check,
    Write,
}

#[testing_macros::fixture("tests/pass/*.esc")]
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

    let (js_output, srcmap_output, d_ts_output, errors) = compile(&input, &lib);
    if !errors.is_empty() {
        panic!("Unexpected error(s): {errors:#?}");
    }

    match mode {
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
    }
}

#[testing_macros::fixture("tests/errors/*.esc")]
fn fail(in_path: PathBuf) {
    let lib = fs::read_to_string(LIB_ES5_D_TS).unwrap();

    let mode = match env::var("UPDATE") {
        Ok(_) => Mode::Write,
        Err(_) => Mode::Check,
    };

    let mut error_output_path = in_path.clone();
    error_output_path.set_extension("error");

    let input = fs::read_to_string(in_path).unwrap();

    // TODO: write out js_output, srcmap_output, d_ts_output
    let (_, _, _, error_output) = compile(&input, &lib);
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

pub fn report_to_string(src: &str, report: &Report) -> String {
    report
        .iter()
        .map(|d| {
            let reasons = type_errors_to_string(&d.reasons, src);
            format!("{}:\n{}", d.message, reasons)
        })
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn all_reports_to_string(src: &str, checker: &Checker) -> String {
    let mut reports = vec![];
    if !checker.current_report.is_empty() {
        reports.push(report_to_string(src, &checker.current_report));
    }
    for report in &checker.parent_reports {
        if !report.is_empty() {
            reports.push(report_to_string(src, report));
        }
    }
    reports.join("\n")
}

fn compile(input: &str, lib: &str) -> (String, String, String, String) {
    let mut program = match escalier_old_parser::parse(input) {
        Ok(program) => program,
        Err(error) => {
            return (
                "".to_string(),
                "".to_string(),
                "".to_string(),
                format!("{error:#?}"),
            );
        }
    };

    let (js, srcmap) = escalier_codegen::js::codegen_js(input, &program);

    // TODO: return errors as part of CompileResult
    let mut checker = parse_dts(lib).unwrap();

    match infer_prog(&mut program, &mut checker) {
        Ok(_) => (),
        Err(errors) => {
            return (
                js,
                srcmap,
                "".to_string(),
                format!(
                    "{}{}",
                    all_reports_to_string(input, &checker),
                    type_errors_to_string(&errors, input),
                ),
            );
        }
    };

    let dts = match escalier_codegen::d_ts::codegen_d_ts(&program, &checker.current_scope) {
        Ok(value) => value,
        Err(errors) => {
            return (
                js,
                srcmap,
                "".to_string(),
                format!(
                    "{}{}",
                    report_to_string(input, &checker.current_report),
                    type_errors_to_string(&errors, input),
                ),
            );
        }
    };

    let errors = report_to_string(input, &checker.current_report);

    (js, srcmap, dts, errors)
}

static LIB_ES5_D_TS: &str = "../../node_modules/typescript/lib/lib.es5.d.ts";
