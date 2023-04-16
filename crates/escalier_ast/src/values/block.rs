use derive_visitor::{Drive, DriveMut};

use crate::values::common::Span;
use crate::values::stmt::Statement;

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Block<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub span: Span,
    // #[drive(skip)]
    // pub loc: SourceLocation,
    pub stmts: Vec<Statement<T>>,
}
