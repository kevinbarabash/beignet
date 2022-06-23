use super::id::*;

// data Tyvar = Tyvar Id Kind
//              deriving Eq

// data Tycon = Tycon Id Kind
//              deriving Eq

// data Type  = TVar Tyvar | TCon Tycon | TAp  Type Type | TGen Int
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Var(ID),
    Con(ID),
    App(Box<Type>, Box<Type>),
    Lam(Box<Type>, Box<Type>), // TODO: support n-ary args in the future
}

// NOTES:
// - we can't use Kind because it's incompatible with crochet's type system
// - Lam is introduced as a replacement for Kind
// - additional types will be added into the future to fill other gaps due
//   to the absence of Kind
