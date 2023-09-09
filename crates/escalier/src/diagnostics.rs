use ariadne::{Config, Label, Report as AriadneReport, ReportKind, Source};

use escalier_ast::Span;
use escalier_hm::type_error::TypeError;
use escalier_hm::types::Type;

use crate::compile_error::CompileError;

fn get_provenance_spans(t1: &Type, t2: &Type) -> Option<(Span, Span)> {
    let prov1 = t1.provenance.as_ref()?;
    let prov2 = t2.provenance.as_ref()?;
    match (prov1.get_span(), prov2.get_span()) {
        (Some(span1), Some(span2)) => Some((span1, span2)),
        (_, _) => None,
    }
}

fn get_diagnostic_string(
    t1: &Type,
    t2: &Type,
    src: &str,
    message: &str,
    // TODO: make the labels optional
    label1: &str,
    label2: &str,
) -> Option<String> {
    if let Some((span1, span2)) = get_provenance_spans(t1, t2) {
        let mut vec = vec![];
        AriadneReport::build(ReportKind::Error, (), span1.start)
            .with_config(Config::default().with_color(false))
            .with_message(message)
            .with_label(Label::new(span1.start..span1.end).with_message(label1))
            .with_label(Label::new(span2.start..span2.end).with_message(label2))
            .finish()
            .write(Source::from(src), &mut vec)
            .unwrap();
        return Some(String::from_utf8(vec).unwrap());
    }
    None
}

pub fn type_errors_to_string(errors: &[TypeError], src: &str) -> String {
    errors
        .iter()
        .map(|error| error.message.to_owned())
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn get_diagnostics_from_compile_error(report: CompileError, src: &str) -> String {
    let diagnostics = match report {
        CompileError::TypeError(error) => error.message.to_owned(),
        CompileError::Diagnostic(diagnostics) => diagnostics
            .iter()
            .map(|diagnostic| {
                format!(
                    "{}: {}",
                    diagnostic.message,
                    type_errors_to_string(&diagnostic.reasons, src)
                )
            })
            .collect::<Vec<String>>()
            .join("\n"),
        CompileError::ParseError(error) => error.message.to_owned(),
    };

    diagnostics
}
