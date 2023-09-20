use crate::decl::Decl;
use crate::span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    // todo
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Export {
    pub decl: Decl,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ModuleItemKind {
    Import(Import),
    Export(Export),
    Decl(Decl),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModuleItem {
    pub kind: ModuleItemKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    pub items: Vec<ModuleItem>,
}

// TODO:
// - parse_module()
// - infer_module()
