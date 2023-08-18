use generational_arena::Arena;
use std::fmt;
use std::mem;

use crate::diagnostic::Diagnostic;
use crate::types::Type;

#[derive(Default, Clone, Debug)]
pub struct Report {
    pub diagnostics: Vec<Diagnostic>,
}

impl fmt::Display for Report {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        for diagnostic in &self.diagnostics {
            writeln!(fmt, "{}", diagnostic)?;
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct Checker {
    pub arena: Arena<Type>,
    pub current_report: Report,
    pub parent_reports: Vec<Report>,
}

impl Checker {
    pub fn push_report(&mut self) {
        let mut report = Report::default();
        std::mem::swap(&mut report, &mut self.current_report);
        self.parent_reports.push(report);
    }

    pub fn pop_report(&mut self) {
        let mut report = self.parent_reports.pop().unwrap();
        mem::swap(&mut self.current_report, &mut report);

        self.current_report
            .diagnostics
            .append(&mut report.diagnostics);
    }
}
