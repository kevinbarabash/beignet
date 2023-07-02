use generational_arena::Index;

use crate::expr::Expr;
use crate::func_param::FuncParam;
use crate::span::*;
use crate::type_param::TypeParam;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ObjectProp {
    Indexer(Indexer),
    Prop(Prop),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Prop {
    pub span: Span,
    pub name: String,
    pub optional: bool,
    pub mutable: bool,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexerKey {
    pub name: String, // TODO: change to Ident
    pub type_ann: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Indexer {
    pub span: Span,
    pub key: IndexerKey,
    pub mutable: bool,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionType {
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeAnn>,
}

// TODO: typeof, keyof, conditional types
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
    Function(FunctionType),
    Union(Vec<TypeAnn>),
    Intersection(Vec<TypeAnn>),
    IndexedAccess(Box<TypeAnn>, Box<TypeAnn>),
    KeyOf(Box<TypeAnn>),
    Query(Box<Expr>),
    Mutable(Box<TypeAnn>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeAnn {
    pub kind: TypeAnnKind,
    pub span: Span,
    pub inferred_type: Option<Index>,
}
