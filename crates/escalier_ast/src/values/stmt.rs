use derive_visitor::{Drive, DriveMut};

use crate::values::class::Class;
use crate::values::common::{SourceLocation, Span};
use crate::values::expr::Expr;
use crate::values::ident::Ident;
use crate::values::pattern::Pattern;
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ClassDecl {
    #[drive(skip)]
    pub ident: Ident, // Why do have `ident` here an in `Class`?
    pub class: Box<Class>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct VarDecl {
    pub pattern: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub init: Option<Box<Expr>>,
    #[drive(skip)]
    pub declare: bool,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TypeDecl {
    #[drive(skip)]
    pub declare: bool,
    #[drive(skip)]
    pub id: Ident,
    pub type_ann: TypeAnn,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum StmtKind {
    ClassDecl(ClassDecl),
    VarDecl(VarDecl),
    TypeDecl(TypeDecl),
    ExprStmt(Expr),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Statement {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub kind: StmtKind,
    // pub inferred_type: Option<Type>,
}
