use std::collections::HashMap;
use std::collections::HashSet;

use super::types::{TVar, Type};

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    fn ftv(&self) -> HashSet<TVar>;
}
