use derivative::*;
use std::fmt;
use swc_atoms::JsWord;
use swc_common::source_map::DUMMY_SP;
use swc_ecma_ast;

use crate::values::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

// TODO: add implementations of From for more nodes
impl From<&Ident> for swc_ecma_ast::Ident {
    fn from(ident: &Ident) -> Self {
        swc_ecma_ast::Ident {
            span: DUMMY_SP,
            sym: JsWord::from(ident.name.to_owned()),
            optional: false,
        }
    }
}

// TODO: have a separate struct for BindingIdents in types so that
// we don't have to create spans for things that don't need them.
// TODO: add an `ident` field so that we can have separate spans
#[derive(Derivative)]
#[derivative(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BindingIdent {
    pub name: String,
    pub mutable: bool,
    #[derivative(PartialOrd = "ignore")]
    #[derivative(Ord = "ignore")]
    // #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl fmt::Display for BindingIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            name,
            mutable,
            span: _,
        } = self;
        if *mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{name}")
    }
}
