use generational_arena::Arena;

use crate::types::Type;

pub struct Checker {
    pub arena: Arena<Type>,
}
