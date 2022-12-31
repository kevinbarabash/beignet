use crochet_ast::types::Type;

use crate::context::Context;
use crate::substitutable::Subst;
use crate::type_error::TypeError;

pub fn unify_mut(t1: &Type, t2: &Type, _ctx: &Context) -> Result<Subst, Vec<TypeError>> {
    if t1 == t2 {
        eprintln!("unify_mut: {t1} == {t2}");
        Ok(Subst::new())
    } else {
        Err(vec![TypeError::UnificationError(
            Box::from(t1.to_owned()),
            Box::from(t2.to_owned()),
        )])
    }
}
