use crochet_ast::types::Type;
use error_stack::{Report, Result};

use crate::context::Context;
use crate::substitutable::Subst;
use crate::type_error::TypeError;

pub fn unify_mut(t1: &Type, t2: &Type, _ctx: &Context) -> Result<Subst, TypeError> {
    if t1 == t2 {
        println!("unify_mut: {t1} == {t2}");
        Ok(Subst::new())
    } else {
        Err(Report::new(TypeError::UnificationError(
            Box::from(t1.to_owned()),
            Box::from(t2.to_owned()),
        ))
        .attach_printable(format!("Couldn't unify {t1} and {t2}")))
    }
}
