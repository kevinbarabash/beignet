use derive_visitor::{Drive, DriveMut};

use crate::values::block::Block;
use crate::values::expr::{EFnParam, Expr, Lambda};
use crate::values::ident::Ident;
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Class {
    #[drive(skip)]
    pub ident: Ident, // Why do have `ident` here an in `ClassDecl`?
    pub body: Vec<ClassMember>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum ClassMember {
    Constructor(Constructor),
    Method(ClassMethod),
    Prop(ClassProp),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Constructor {
    pub params: Vec<EFnParam>,
    pub body: Block,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ClassMethod {
    #[drive(skip)]
    pub key: Ident,
    pub kind: MethodKind,
    pub lambda: Lambda,
    #[drive(skip)]
    pub is_static: bool,
    #[drive(skip)]
    pub is_mutating: bool,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum MethodKind {
    // TODO: differentiate mutating methods from non-mutating ones
    Method,
    Getter,
    Setter,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ClassProp {
    #[drive(skip)]
    pub key: Ident,
    pub value: Option<Box<Expr>>,
    pub type_ann: Option<Box<TypeAnn>>,
    #[drive(skip)]
    pub is_static: bool,
    #[drive(skip)]
    pub is_optional: bool,
    #[drive(skip)]
    pub is_mutable: bool,
}
