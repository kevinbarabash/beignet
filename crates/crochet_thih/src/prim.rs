use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim {
    Num,
    Bool,
    Str,
    // Undefined,
    // Null,
}
