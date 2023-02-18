use derive_visitor::{Drive, DriveMut};
use std::fmt;

// TODO: have a separate struct for BindingIdents in types so that
// we don't have to create spans for things that don't need them.
// TODO: add an `ident` field so that we can have separate spans
#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BindingIdent {
    #[drive(skip)]
    pub name: String,
    #[drive(skip)]
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
