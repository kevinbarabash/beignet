use std::collections::HashSet;

use super::id::*;
use super::scheme::*;
use super::subst::*;

pub type Assump = (ID, Scheme);

// instance Types Assump where
//   apply s (i :>: sc) = i :>: (apply s sc)
//   tv (i :>: sc)      = tv sc
impl Types for Assump {
    fn apply(&self, subs: &Subst) -> Self {
        let (id, sc) = self;
        (id.to_owned(), sc.apply(subs))
    }
    fn tv(&self) -> HashSet<ID> {
        let (_id, sc) = self;
        sc.tv()
    }
}