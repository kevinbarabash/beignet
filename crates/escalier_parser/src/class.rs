use crate::block::Block;
use crate::expr::Expr;
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
    pub name: Ident,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub body: Block,
    pub type_ann: Option<TypeAnn>, // return type
    pub is_async: bool,
    pub is_gen: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constructor {
    pub span: Span,
    pub params: Vec<FuncParam>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub span: Span,
    pub name: Ident,
    pub type_ann: Option<TypeAnn>,
    pub init: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ClassMember {
    Method(Method),
    Constructor(Constructor),
    Field(Field), // TODO: rename to property?
}

/*
let Foo = class {
    field: number

    constructor(self) {}

    foo(self) {}
    bar(mut self) {}
}
 */
