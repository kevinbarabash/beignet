// TODO:
// - methods, callables, indexers, mapped types
// - typeof, keyof, etc.
// - conditional types

use crate::func_param::FuncParam;
use crate::span::*;

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
    Function(Vec<FuncParam>, Box<TypeAnn>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeAnn {
    pub kind: TypeAnnKind,
    pub span: Span,
}
