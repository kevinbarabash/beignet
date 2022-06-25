use std::collections::HashSet;

use super::id::*;
use super::r#type::*;

// type Subst  = [(Tyvar, Type)]
pub type Subst = Vec<(ID, Type)>;

// class Types t where
//   apply :: Subst -> t -> t
//   tv    :: t -> [Tyvar]
pub trait Types {
    fn apply(&self, subs: &Subst) -> Self;
    fn tv(&self) -> HashSet<ID>;
}

// infixr 4 @@
// (@@)       :: Subst -> Subst -> Subst
// s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1
pub fn at_at(s1: &Subst, s2: &Subst) -> Subst {
    s2.iter()
        .map(|(u, t)| (u.to_owned(), t.apply(s1)))
        .into_iter()
        .chain(s1.iter().cloned())
        .collect()
}
