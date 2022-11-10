use crochet_ast::types::Type;

use crate::context::Context;
use crate::substitutable::Subst;
use crate::type_error::TypeError;

pub fn unify_mut(t1: &Type, t2: &Type, _ctx: &Context) -> Result<Subst, TypeError> {
    if t1 == t2 {
        println!("unify_mut: {t1} == {t2}");
        Ok(Subst::new())
    } else {
        Err(TypeError::from(format!("Couldn't unify {t1} and {t2}")))
    }
}
