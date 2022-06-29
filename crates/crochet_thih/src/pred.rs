use std::collections::HashSet;

use super::id::*;
use super::subst::*;
use super::r#type::*;

// data Pred   = IsIn Id Type
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Pred {
    IsIn(ID, Type)
}

// data Qual t = [Pred] :=> t
pub type Qual<T> = (Vec<Pred>, T);

// instance Types t => Types (Qual t) where
//   apply s (ps :=> t) = apply s ps :=> apply s t
//   tv (ps :=> t)      = tv ps `union` tv t
impl<I> Types for Qual<I>
where
    I: Types,
{
    fn apply(&self, subs: &Subst) -> Self {
        let (ps, t) = self;
        (ps.apply(subs), t.apply(subs))
    }
    fn tv(&self) -> HashSet<ID> {
        let (ps, t) = self;
        ps.tv().union(&t.tv()).cloned().collect()
    }
}

// instance Types Pred where
//   apply s (IsIn i t) = IsIn i (apply s t)
//   tv (IsIn i t)      = tv t
impl Types for Pred {
    fn apply(&self, subs: &Subst) -> Self {
        let Pred::IsIn(id, t) = self;
        Pred::IsIn(id.to_owned(), t.apply(subs))
    }
    fn tv(&self) -> HashSet<ID> {
        let Pred::IsIn(_, t) = self;
        t.tv()
    }
}

// bySuper :: ClassEnv -> Pred -> [Pred]
// bySuper ce p@(IsIn i t)
//  = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

// byInst                   :: ClassEnv -> Pred -> Maybe [Pred]
// byInst ce p@(IsIn i t)    = msum [ tryInst it | it <- insts ce i ]
//  where tryInst (ps :=> h) = do u <- matchPred h p
//                                Just (map (apply u) ps)

// entail        :: ClassEnv -> [Pred] -> Pred -> Bool
// entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
//                  case byInst ce p of
//                    Nothing -> False
//                    Just qs -> all (entail ce ps) qs
pub fn entail(_ps: &[Pred], _p: &Pred) -> bool {
    // We return false because we're not suporting type classes so we
    // don't have to worry about checking whether there is an instance
    // of a particular type class for a given value.
    false
}
