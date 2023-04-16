use derive_visitor::{Drive, DriveMut};

use crate::values::block::Block;
use crate::values::expr::{EFnParam, Expr, Lambda};
use crate::values::ident::Ident;
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Class<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub ident: Ident, // Why do have `ident` here an in `ClassDecl`?
    pub body: Vec<ClassMember<T>>,
    pub type_params: Option<Vec<TypeParam<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum ClassMember<T: 'static>
where
    T: Drive + DriveMut,
{
    Constructor(Constructor<T>),
    Method(ClassMethod<T>),
    Prop(ClassProp<T>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Constructor<T: 'static>
where
    T: Drive + DriveMut,
{
    pub params: Vec<EFnParam<T>>,
    pub body: Block<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ClassMethod<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub key: Ident,
    pub kind: MethodKind,
    pub lambda: Lambda<T>,
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
pub struct ClassProp<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub key: Ident,
    pub value: Option<Box<Expr<T>>>,
    pub type_ann: Option<Box<TypeAnn<T>>>,
    #[drive(skip)]
    pub is_static: bool,
    #[drive(skip)]
    pub is_optional: bool,
    #[drive(skip)]
    pub is_mutable: bool,
}
