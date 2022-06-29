use std::collections::*;

use super::id::*;
use super::kind::*;
use super::pred::*;
use super::subst::*;
use super::r#type::*;

// data Scheme = Forall [Kind] (Qual Type)
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Scheme {
    Forall(Vec<Kind>, Qual<Type>)
}

// instance Types Scheme where
//   apply s (Forall ks qt) = Forall ks (apply s qt)
//   tv (Forall ks qt)      = tv qt
impl Types for Scheme {
    fn apply(&self, subs: &Subst) -> Self {
        let Scheme::Forall(ks, qt) = self;
        Scheme::Forall(ks.to_owned(), qt.apply(subs))
    }
    fn tv(&self) -> HashSet<ID> {
        let Scheme::Forall(_ks, qt) = self;
        qt.tv()
    }
}

// quantify      :: [Tyvar] -> Qual Type -> Scheme
// quantify vs qt = Forall ks (apply s qt)
//  where vs' = [ v | v <- tv qt, v `elem` vs ]
//        ks  = map kind vs'
//        s   = zip vs' (map TGen [0..])
pub fn quantify(vs: &[ID], qt: &Qual<Type>) -> Scheme {
    let vs_1: Vec<_> = qt.tv().iter().filter(|v| vs.contains(v)).cloned().collect();
    let ks: Vec<_> = vs_1.iter().map(|_| Kind::Star).collect();

    // TODO: add Variant::Gen(u32) to type.rs
    // let s: Subst = vs_1.iter().enumerate(|v, index| (v.to_owned(), )).collect();

    todo!()
}

// toScheme      :: Type -> Scheme
// toScheme t     = Forall [] ([] :=> t)
pub fn to_scheme(t: &Type) -> Scheme {
    Scheme::Forall(vec![], (vec![], t.to_owned()))
}
