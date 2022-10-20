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
