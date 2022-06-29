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
pub fn quantify(vs: &[ID], qt: &Qual<Type>) -> Scheme {
    //  where vs' = [ v | v <- tv qt, v `elem` vs ]
    let vs_1: Vec<_> = qt.tv().iter().filter(|v| vs.contains(v)).cloned().collect();
    //        ks  = map kind vs'
    let ks: Vec<_> = vs_1.iter().map(|_| Kind::Star).collect();
    //        s   = zip vs' (map TGen [0..])
    let s: Subst = vs_1.iter().enumerate().map(|(index, value)| {
        let t = Type {
            usage: None,
            variant: Variant::Gen(index as u32)
        };
        (value.to_owned(), t)
    }).collect();

    // quantify vs qt = Forall ks (apply s qt)
    Scheme::Forall(ks, qt.apply(&s))
}

// toScheme      :: Type -> Scheme
// toScheme t     = Forall [] ([] :=> t)
pub fn to_scheme(t: &Type) -> Scheme {
    Scheme::Forall(vec![], (vec![], t.to_owned()))
}
