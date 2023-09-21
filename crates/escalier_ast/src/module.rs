use crate::decl::Decl;
use crate::span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportSpecifier {
    pub local: String,            // the local name of the imported symbol
    pub imported: Option<String>, // the symbol being imported
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub specifiers: Vec<ImportSpecifier>,
    pub source: String,
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
