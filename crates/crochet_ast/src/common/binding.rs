use std::fmt;

// TODO: add `span` field
// TODO: add an `ident` field so that we can have separate spans
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BindingIdent {
    pub name: String,
    pub mutable: bool,
}

impl fmt::Display for BindingIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { name, mutable } = self;
        if *mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{name}")
    }
}
