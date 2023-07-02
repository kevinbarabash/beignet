use crate::stmt::Stmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}
