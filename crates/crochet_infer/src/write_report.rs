use ariadne::{Label, Report as AriadneReport, ReportKind, Source};
use error_stack::Report;

use crate::type_error::TypeError;

pub fn write_report(report: Report<TypeError>, src: &str) -> Vec<String> {
    let mut diagnostics = vec![];

    for frame in report.frames() {
        let error = frame.downcast_ref::<TypeError>();
        if let Some(context) = error {
            if let TypeError::UnificationError(t1, t2) = context {
                let prov1 = t1.provenance.as_ref().unwrap();
                let prov2 = t2.provenance.as_ref().unwrap();

                // println!("prov1 = {prov1:#?}");
                // println!("prov2 = {prov2:#?}");

                // TODO: figure out how we want to determine what the primary
                // source of the error is.  In this case it's the function call,
                // but in order to determine that we need look at its ancestors
                // in the AST.  In order to do that we need to use RefCells so
                // that .provenance is pointing to a node in the AST instead of
                // storing its own copy.
                // let output =

                let mut vec = vec![];
                AriadneReport::build(ReportKind::Error, (), prov1.get_span().start)
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
            };
        }
    }

    return diagnostics;
}
