use std::cmp::Ordering;
use std::fmt;
use swc_atoms::JsWord;
use swc_common::{self, BytePos, SyntaxContext};
use swc_ecma_ast;

use crate::values::common::{SourceLocation, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub loc: SourceLocation,
    pub span: Span,
    pub name: String,
}

// TODO: add implementations of From for more nodes
impl From<&Ident> for swc_ecma_ast::Ident {
    fn from(ident: &Ident) -> Self {
        let span = swc_common::Span {
            lo: BytePos(ident.span.start as u32 + 1),
            hi: BytePos(ident.span.end as u32 + 1),
            ctxt: SyntaxContext::empty(),
        };

        swc_ecma_ast::Ident {
            span,
            sym: JsWord::from(ident.name.to_owned()),
            optional: false,
        }
    }
}

impl From<&BindingIdent> for swc_ecma_ast::Ident {
    fn from(binding: &BindingIdent) -> Self {
        let span = swc_common::Span {
            lo: BytePos(binding.span.start as u32 + 1),
            hi: BytePos(binding.span.end as u32 + 1),
            ctxt: SyntaxContext::empty(),
        };

        swc_ecma_ast::Ident {
            span,
            sym: JsWord::from(binding.name.to_owned()),
            optional: false,
        }
    }
}

// TODO: have a separate struct for BindingIdents in types so that
// we don't have to create spans for things that don't need them.
// TODO: add an `ident` field so that we can have separate spans
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BindingIdent {
    pub name: String,
    pub mutable: bool,
    pub span: Span,
    pub loc: SourceLocation,
}

impl PartialOrd for BindingIdent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BindingIdent {
    fn cmp(&self, other: &Self) -> Ordering {
        let result = self.name.cmp(&other.name);

        if result == Ordering::Equal {
            self.mutable.cmp(&other.mutable)
        } else {
            result
        }
    }
}

impl fmt::Display for BindingIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { name, mutable, .. } = self;
        if *mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{name}")
    }
}
