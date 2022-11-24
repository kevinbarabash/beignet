use ariadne::{Config, Label, Report as AriadneReport, ReportKind, Source};
use crochet_infer::TypeError;
use error_stack::Report;

use crate::compile_error::CompileError;
use crochet_ast::{
    types::{Type, TypeKind},
    values::{ExprKind, Span},
};

fn get_provenance_spans(t1: &Type, t2: &Type) -> Option<(Span, Span)> {
    let prov1 = t1.provenance.as_ref().unwrap();
    let prov2 = t2.provenance.as_ref().unwrap();
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
            .with_label(Label::new(span1).with_message(label1))
            .with_label(Label::new(span2).with_message(label2))
            .finish()
            .write(Source::from(src), &mut vec)
            .unwrap();
        return Some(String::from_utf8(vec).unwrap());
    }
    None
}

pub fn get_diagnostics(report: Report<CompileError>, src: &str) -> Vec<String> {
    let diagnostics: Vec<_> = report
        .frames()
        .filter_map(|frame| match frame.downcast_ref::<TypeError>() {
            Some(context) => match context {
                TypeError::UnificationError(t1, t2) => get_diagnostic_string(
                    t1,
                    t2,
                    src,
                    "Incompatible types",
                    &format!("{t1}"),
                    &format!("{t2}"),
                ),
                TypeError::TooFewArguments(app_t, lam_t) => {
                    let provided = if let TypeKind::App(app) = &app_t.kind {
                        app.args.len()
                    } else {
                        0
                    };
                    let expected = if let TypeKind::Lam(lam) = &lam_t.kind {
                        lam.params.len()
                    } else {
                        0
                    };
                    let expr = if let Some(prov) = &lam_t.provenance {
                        prov.get_expr()
                    } else {
                        None
                    };
                    if let Some(expr) = expr {
                        if let ExprKind::Lambda(lam) = &expr.kind {
                            // TODO: Add a span to lam.params so that we don't
                            // have to compute it here.
                            println!("params = {:#?}", lam.params)
                        }
                    }
                    // TODO: figure out how to get a span for just the function
                    // signature or just the param list.
                    get_diagnostic_string(
                        app_t,
                        lam_t,
                        src,
                        "Not enough args provided",
                        &format!("was passed {provided} args"),
                        &format!("expected {expected} args"),
                    )
                }
                TypeError::IndexOutOfBounds(obj_t, prop_t) => get_diagnostic_string(
                    obj_t,
                    prop_t,
                    src,
                    "Index out of bounds",
                    "for this tuple",
                    &format!("{prop_t} is out of bounds"),
                ),
                TypeError::InvalidIndex(obj_t, prop_t) => get_diagnostic_string(
                    obj_t,
                    prop_t,
                    src,
                    "Invalid Index",
                    "for tuples",
                    &format!("{prop_t} is not a valid index"),
                ),
                _ => None,
            },
            None => None,
        })
        .collect();

    diagnostics
}
