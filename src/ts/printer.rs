use std::rc::Rc;

use swc_common::source_map::SourceMap;
use swc_ecma_ast::*;
use swc_ecma_codegen::*;
use swc_ecma_codegen::text_writer::JsWriter;

pub fn print_ts(prog: &Program) -> String {
    let mut buf = vec![];
    let cm = Rc::new(SourceMap::default());

    let mut emitter = Emitter {
        cfg: swc_ecma_codegen::Config {
            ..Default::default()
        },
        cm: cm.clone(),
        comments: None,
        wr: JsWriter::new(cm.clone(), "\n", &mut buf, None),
    };

    emitter.emit_program(prog).unwrap();

    String::from_utf8_lossy(&buf).to_string()
}
