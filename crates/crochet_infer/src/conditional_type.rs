use crochet_ast::types::{TConditionalType, Type, TypeKind};
use error_stack::{Report, Result};

use crate::context::Context;
use crate::type_error::TypeError;
use crate::unify::unify;

pub fn compute_conditional_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Ref(alias) => {
            // When conditional types act on a generic type, they become
            // distributive when given a union type.
            ctx.lookup_ref_and_instantiate(alias)
        }
        TypeKind::ConditionalType(TConditionalType {
            check_type,
            extends_type,
            true_type,
            false_type,
        }) => {
            let t = match unify(check_type, extends_type, ctx) {
                Ok(_) => true_type,
                Err(_) => false_type,
            };

            Ok(t.as_ref().to_owned())
        }
        _ => Err(Report::new(TypeError::Unhandled)),
    }
}
