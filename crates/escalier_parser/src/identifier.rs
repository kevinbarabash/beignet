use crate::source_location::SourceLocation;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub loc: SourceLocation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BindingIdent {
    pub name: String,
    pub loc: SourceLocation,
    pub mutable: bool,
}
