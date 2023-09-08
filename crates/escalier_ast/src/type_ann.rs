use generational_arena::Index;

use crate::expr::BinaryOp;
use crate::func_param::FuncParam;
use crate::identifier::Ident;
use crate::span::*;
use crate::type_param::TypeParam;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ObjectProp {
    Call(ObjCallable),
    Constructor(ObjCallable),
    Prop(Prop),
    Mapped(Mapped),
}

// TODO: dedupe with `FunctionType` below
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjCallable {
    pub span: Span,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeAnn>,
}

// TODO: dedupe with TPropModifier
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PropModifier {
    Getter,
    Setter,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Prop {
    pub span: Span,
    pub name: String,
    pub modifier: Option<PropModifier>,
    pub optional: bool,
    pub readonly: bool,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MappedModifier {
    Add,
    Remove,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mapped {
    pub key: Box<TypeAnn>,
    pub value: Box<TypeAnn>,

    pub target: String,
    pub source: Box<TypeAnn>,
    pub optional: Option<MappedModifier>,

    // First half of a Conditional
    pub check: Option<Box<TypeAnn>>,
    pub extends: Option<Box<TypeAnn>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionType {
    // TODO
    // pub span: Span,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeAnn>,
    pub throws: Option<Box<TypeAnn>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConditionType {
    pub check: Box<TypeAnn>,
    pub extends: Box<TypeAnn>,
    pub true_type: Box<TypeAnn>,
    pub false_type: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchType {
    pub matchable: Box<TypeAnn>,
    pub cases: Vec<MatchTypeCase>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchTypeCase {
    pub extends: Box<TypeAnn>,
    pub true_type: Box<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryTypeAnn {
    pub left: Box<TypeAnn>,
    pub op: BinaryOp,
    pub right: Box<TypeAnn>,
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
    TypeOf(Ident),
    Condition(ConditionType),
    Match(MatchType),
    Wildcard,
    Infer(String),
    Binary(BinaryTypeAnn),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeAnn {
    pub kind: TypeAnnKind,
    pub span: Span,
    pub inferred_type: Option<Index>,
}
