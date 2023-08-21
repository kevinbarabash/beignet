use swc_atoms::JsWord;
use swc_common::{self, BytePos, SyntaxContext};
use swc_ecma_ast;

use crate::span::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BindingIdent {
    pub name: String,
    pub span: Span,
    pub mutable: bool,
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
