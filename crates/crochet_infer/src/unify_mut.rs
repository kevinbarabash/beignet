use crochet_ast::types::Type;

use crate::context::Context;
use crate::substitutable::Subst;

pub fn unify_mut(t1: &Type, t2: &Type, _ctx: &Context) -> Result<Subst, String> {
    if t1 == t2 {
        Ok(Subst::new())
    } else {
        Err(format!("Couldn't unify {t1} and {t2}"))
    }
}
