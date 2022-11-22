use ariadne::{Config, Label, Report as AriadneReport, ReportKind, Source};
use crochet_infer::TypeError;
use error_stack::Report;

use crate::compile_error::CompileError;

pub fn get_diagnostics(report: Report<CompileError>, src: &str) -> Vec<String> {
    let mut diagnostics = vec![];

    for frame in report.frames() {
        if let Some(context) = frame.downcast_ref::<TypeError>() {
            match context {
                TypeError::UnificationError(t1, t2) => {
                    let prov1 = t1.provenance.as_ref().unwrap();
                    let prov2 = t2.provenance.as_ref().unwrap();

                    let mut vec = vec![];
                    AriadneReport::build(ReportKind::Error, (), prov1.get_span().start)
                        .with_config(Config::default().with_color(false))
                        .with_message("Incompatible types")
                        .with_label(
                            Label::new(prov1.get_span().to_owned())
                                .with_message("This argument should be a number"),
                        )
                        .with_label(Label::new(prov2.get_span().to_owned()))
                        .finish()
                        .write(Source::from(src), &mut vec)
                        .unwrap();

                    let output = String::from_utf8(vec).unwrap();
                    diagnostics.push(output);
                }
                _ => todo!(),
            }
        }
    }

    diagnostics
}
