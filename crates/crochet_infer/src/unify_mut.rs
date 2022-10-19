use std::cmp;
use std::collections::HashSet;

use crochet_types::{self as types, TGeneric, TLam, TObjElem, TObject, TVar, Type};
use types::TKeyword;

use crate::context::Context;
use crate::key_of::key_of;
use crate::substitutable::{Subst, Substitutable};
use crate::util::*;

pub fn unify_mut(t1: &Type, t2: &Type, ctx: &Context) -> Result<Subst, String> {
    if t1 == t2 {
        Ok(Subst::new())
    } else {
        Err(format!("Couldn't unify {t1} and {t2}"))
    }
}
