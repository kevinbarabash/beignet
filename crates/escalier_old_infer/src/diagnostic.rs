// Roadmap:
// - start with type errors
// - once the parser has been rewritten add parse errors as well

use std::fmt;

use crate::type_error::TypeError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: u32,
    pub message: String,
    pub reasons: Vec<TypeError>,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(fmt, "ESC_{} - {}:", self.code, self.message)?;
        let len = self.reasons.len();
        for (i, reason) in self.reasons.iter().enumerate() {
            if i < len - 1 {
                write!(fmt, "\u{251C}")?
            } else {
                write!(fmt, "\u{2514}")?
            };
            writeln!(fmt, " {reason}")?;
        }
        Ok(())
    }
}
