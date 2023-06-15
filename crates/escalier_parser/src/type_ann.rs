// TODO:
// - keywords: number, string, null, undefined, etc.
// - literals: 5, "hello", true, etc.
// - tuples and objects
// - methods, callables, indexers, mapped types
// - function types
// - type references (with type params)
// - typeof, keyof, etc.
// - conditional types

use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectProp {
    pub name: String,
    pub optional: bool,
    pub mutable: bool,
    pub type_ann: TypeAnn,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeAnnKind {
    BoolLit(bool),
    Boolean,
    NumLit(String),
    Number,
    StrLit(String),
    String,
    Symbol,
    Null,
    Undefined,
    Object(Vec<ObjectProp>),
    Tuple(Vec<TypeAnn>),
    Array(Box<TypeAnn>),
    TypeRef(String, Option<Vec<TypeAnn>>),
    // TODO: Function(Vec<FuncParam>, Box<TypeAnn>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeAnn {
    pub kind: TypeAnnKind,
    pub loc: SourceLocation,
}
