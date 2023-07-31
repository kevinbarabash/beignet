use generational_arena::Index;

use crate::expr::Expr;
use crate::func_param::FuncParam;
use crate::span::*;
use crate::type_param::TypeParam;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ObjectProp {
    Call(ObjCallable),
    Constructor(ObjCallable),
    Method(ObjMethod),
    Getter(ObjGetter),
    Setter(ObjSetter),
    Indexer(Indexer),
    Prop(Prop),
}

// TODO: dedupe with `FunctionType` below
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjCallable {
    pub span: Span,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjMethod {
    pub span: Span,
    pub name: String, // TODO: allow computed names, e.g. `Symbol.iterator`
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjGetter {
    pub span: Span,
    pub name: String, // TODO: allow computed names, e.g. `Symbol.iterator`
    // The first param must always be self and there should only be a single param
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjSetter {
    pub span: Span,
    pub name: String, // TODO: allow computed names, e.g. `Symbol.iterator`
    // The first param must always be self and there should only be two params
    pub params: Vec<FuncParam>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConditionType {
    pub check: Box<TypeAnn>,
    pub extends: Box<TypeAnn>,
    pub true_type: Box<TypeAnn>,
    pub false_type: Box<TypeAnn>,
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
    Unknown,
    Never,
    Object(Vec<ObjectProp>),
    Tuple(Vec<TypeAnn>),
    Array(Box<TypeAnn>),
    TypeRef(String, Option<Vec<TypeAnn>>),
    Function(FunctionType),
    Union(Vec<TypeAnn>),
    Intersection(Vec<TypeAnn>),
    IndexedAccess(Box<TypeAnn>, Box<TypeAnn>),
    KeyOf(Box<TypeAnn>),
    Rest(Box<TypeAnn>),
    TypeOf(Box<Expr>),
    Condition(ConditionType),
    Wildcard,
    Infer(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeAnn {
    pub kind: TypeAnnKind,
    pub span: Span,
    pub inferred_type: Option<Index>,
}
