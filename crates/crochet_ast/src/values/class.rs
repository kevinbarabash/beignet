use crate::values::expr::{EFnParam, Expr, Lambda};
use crate::values::ident::Ident;
use crate::values::type_ann::TypeAnn;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub ident: Ident,
    pub body: Vec<ClassMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassMember {
    Constructor(Constructor),
    Method(ClassMethod),
    Prop(ClassProp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constructor {
    pub params: Vec<EFnParam>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMethod {
    pub key: Ident,
    pub kind: MethodKind,
    pub lambda: Lambda,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodKind {
    // TODO: differentiate mutating methods from non-mutating ones
    Method,
    Getter,
    Setter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassProp {
    pub key: Ident,
    pub value: Option<Box<Expr>>,
    pub type_ann: Option<Box<TypeAnn>>,
    pub is_static: bool,
    pub is_optional: bool,
}
