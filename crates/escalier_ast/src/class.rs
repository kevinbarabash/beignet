use crate::block::Block;
use crate::expr::*;
use crate::func_param::FuncParam;
use crate::identifier::Ident;
use crate::span::Span;
use crate::type_ann::TypeAnn;
use crate::type_param::TypeParam;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Class {
    pub span: Span,
    // pub name: Option<Ident>,
    pub type_params: Option<Vec<TypeParam>>,
    pub super_class: Option<Ident>,
    pub super_type_args: Option<Vec<TypeAnn>>,
    pub body: Vec<ClassMember>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Method {
    pub span: Span,
    pub name: PropName,
    pub is_public: bool, // TODO: change to is_private
    pub is_async: bool,
    pub is_gen: bool,
    pub is_mutating: bool,
    pub is_static: bool,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub body: Block,
    pub type_ann: Option<TypeAnn>, // return type
                                   // TODO: handle throws
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Getter {
    pub span: Span,
    pub name: PropName,
    pub is_public: bool,
    pub type_ann: Option<TypeAnn>,
    pub params: Vec<FuncParam>, // should only contain `self` param
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Setter {
    pub span: Span,
    pub name: PropName,
    pub is_public: bool,
    pub type_ann: Option<TypeAnn>, // should always be `void`
    pub params: Vec<FuncParam>,    // should only contain `self`, `value` params
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PropName {
    Ident(Ident),
    // Str(Str),
    // Num(Num),
    Computed(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constructor {
    pub span: Span,
    pub is_public: bool,
    pub params: Vec<FuncParam>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub span: Span,
    pub name: Ident,
    pub is_public: bool,
    pub is_static: bool,
    pub type_ann: Option<TypeAnn>,
    pub init: Option<Box<Expr>>,
    // TODO: add `is_static` and `is_optional` fields
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ClassMember {
    Method(Method),
    Getter(Getter),
    Setter(Setter),
    Field(Field), // TODO: rename to property?
}
