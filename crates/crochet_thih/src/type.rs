use std::collections::HashSet;

use super::id::*;
use super::lit::*;
use super::prim::*;
use super::subst::*;

// data Tyvar = Tyvar Id Kind
//              deriving Eq

// data Tycon = Tycon Id Kind
//              deriving Eq

// data Type  = TVar Tyvar | TCon Tycon | TAp  Type Type | TGen Int
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant {
    Var(ID),
    // Replaces TCon and TAp
    Lam(Vec<Type>, Box<Type>), // TODO: support n-ary args in the future
    Lit(Lit),
    Prim(Prim),
    // TODO: Gen(u32)
    // TODO: support more data types
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Usage {
    Alt,
    FnDecl,
    FnCall,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type {
    pub usage: Option<Usage>,
    pub variant: Variant,
}

// NOTES ON FLAGS:
// - flags can be used to indicate additional properties of the type
//   e.g. decl vs. call or arg vs. param
// - these flags can be used to determine when sub-typing is allowed
//   e.g. passing a number literal to as an arg for a param that's
//   a number primitive
// - QUESTINON: when creating (or applying) a substitution, should the
//   flag be copied from the type variable to the target type?

// NOTES:
// - we can't use Kind because it's incompatible with crochet's type system
// - Lam is a replacement for TCon and TAp since we can't use Kind
// - additional types will be added into the future to fill other gaps due
//   to the absence of Kind

fn lookup(u: &ID, s: &Subst) -> Option<Type> {
    s.iter().find(|(v, _)| u == v).map(|(_, t)| t.to_owned())
}

impl Types for Type {
    fn apply(&self, s: &Subst) -> Self {
        match &self.variant {
            Variant::Var(u) => {
                match lookup(u, s) {
                    Some(t) => t,
                    None => Type {
                        usage: None,
                        variant: Variant::Var(u.to_owned())
                    },
                }
            },
            Variant::Lam(args, ret) => {
                Type {
                    usage: None,
                    variant: Variant::Lam(
                        args.to_owned().apply(s),
                        Box::from(ret.as_ref().apply(s)),
                    ),
                }
            },
            _ => self.to_owned(),
        }
    }
    fn tv(&self) -> HashSet<ID> {
        match &self.variant {
            Variant::Var(u) => {
                let mut set = HashSet::new();
                set.insert(u.to_owned());
                set
            },
            Variant::Lam(arg, ret) => {
                arg.tv().union(&ret.tv()).cloned().collect()
            },
            _ => HashSet::new(),
        }
    }
}

impl<I> Types for Vec<I>
where
    I: Types,
{
    fn apply(&self, subs: &Subst) -> Self {
        self.iter().map(|c| c.apply(subs)).collect()
    }
    fn tv(&self) -> HashSet<ID> {
        self.iter().flat_map(|c| c.tv()).collect()
    }
}
