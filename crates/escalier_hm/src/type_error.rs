use std::fmt;

// TODO: Replace with an enum so we can reference types and AST nodes
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
    pub message: String,
}

impl fmt::Display for TypeError {
    // TODO: print out the provenance chain for types referenced by errors
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "TypeError: {}", self.message)
    }
}
