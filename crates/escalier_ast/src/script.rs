use crate::stmt::Stmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Script {
    pub stmts: Vec<Stmt>,
}
