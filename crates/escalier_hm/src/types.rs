// Types and type constructors

pub type ArenaType = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub instance: Option<ArenaType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub name: String,
    pub types: Vec<ArenaType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub params: Vec<ArenaType>,
    pub ret: ArenaType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Variable(Variable),
    Constructor(Constructor),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub id: ArenaType,
    pub kind: TypeKind,
}
