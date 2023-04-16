use crate::ast::block::Block;
use crate::ast::expr::{EFnParam, Expr, Lambda};
use crate::ast::ident::Ident;
use crate::ast::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Class {
    pub ident: Ident, // Why do have `ident` here an in `ClassDecl`?
    pub body: Vec<ClassMember>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassMember {
    Constructor(Constructor),
    Method(ClassMethod),
    Prop(ClassProp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constructor {
    pub params: Vec<EFnParam>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassMethod {
    pub key: Ident,
    pub kind: MethodKind,
    pub lambda: Lambda,
    pub is_static: bool,
    pub is_mutating: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MethodKind {
    // TODO: differentiate mutating methods from non-mutating ones
    Method,
    Getter,
    Setter,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassProp {
    pub key: Ident,
    pub value: Option<Box<Expr>>,
    pub type_ann: Option<Box<TypeAnn>>,
    pub is_static: bool,
    pub is_optional: bool,
    pub is_mutable: bool,
}
