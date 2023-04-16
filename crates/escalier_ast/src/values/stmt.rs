use derive_visitor::{Drive, DriveMut};

use crate::values::block::Block;
use crate::values::class::Class;
use crate::values::common::{SourceLocation, Span};
use crate::values::expr::Expr;
use crate::values::ident::Ident;
use crate::values::pattern::Pattern;
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ClassDecl<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub ident: Ident, // Why do have `ident` here an in `Class`?
    pub class: Box<Class<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct VarDecl<T: 'static>
where
    T: Drive + DriveMut,
{
    pub pattern: Pattern<T>,
    pub type_ann: Option<TypeAnn<T>>,
    pub init: Option<Box<Expr<T>>>,
    #[drive(skip)]
    pub declare: bool,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TypeDecl<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub declare: bool,
    #[drive(skip)]
    pub id: Ident,
    pub type_ann: TypeAnn<T>,
    pub type_params: Option<Vec<TypeParam<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ForStmt<T: 'static>
where
    T: Drive + DriveMut,
{
    pub pattern: Box<Pattern<T>>,
    pub expr: Box<Expr<T>>,
    pub body: Block<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ReturnStmt<T: 'static>
where
    T: Drive + DriveMut,
{
    pub arg: Option<Box<Expr<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum StmtKind<T: 'static>
where
    T: Drive + DriveMut,
{
    // Declarations
    ClassDecl(ClassDecl<T>),
    VarDecl(VarDecl<T>),
    TypeDecl(TypeDecl<T>),

    // Statements
    ExprStmt(Expr<T>),
    ForStmt(ForStmt<T>),
    ReturnStmt(ReturnStmt<T>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Statement<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub kind: StmtKind<T>,
    // pub inferred_type: Option<T>,
}
