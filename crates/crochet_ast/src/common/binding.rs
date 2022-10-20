use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BindingIdent {
    pub name: String,
}

impl fmt::Display for BindingIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { name } = self;
        write!(f, "{name}")
    }
}
