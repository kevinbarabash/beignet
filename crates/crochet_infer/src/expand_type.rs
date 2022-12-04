use crochet_ast::types::*;
use error_stack::Result;

use crate::context::Context;
use crate::key_of::key_of;
use crate::mapped_type::compute_mapped_type;
use crate::type_error::TypeError;
use crate::unify::unify;

// Question: When should we expand a type
pub fn expand_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Var(_) => todo!(),
        TypeKind::App(_) => Ok(t.to_owned()),
        TypeKind::Lam(_) => Ok(t.to_owned()),
        TypeKind::Lit(_) => Ok(t.to_owned()),
        TypeKind::Keyword(_) => Ok(t.to_owned()),
        // TODO: run expand_type on each child and take the union of the result
        TypeKind::Union(_) => Ok(t.to_owned()),
        // run expand_type on each child and take the intersection of the result
        TypeKind::Intersection(_) => todo!(),
        TypeKind::Object(_) => Ok(t.to_owned()),
        TypeKind::Ref(alias) => {
            let alias = TRef {
                name: alias.name.to_owned(),
                type_args: match &alias.type_args {
                    Some(args) => {
                        let args: Result<Vec<_>, TypeError> =
                            args.iter().map(|arg| expand_type(arg, ctx)).collect();
                        Some(args?)
                    }
                    None => None,
                },
            };
            let t = ctx.lookup_ref_and_instantiate(&alias)?;
            expand_type(&t, ctx)
        }
        TypeKind::Tuple(_) => todo!(),
        TypeKind::Array(_) => todo!(),
        TypeKind::Rest(_) => todo!(),
        TypeKind::This => todo!(),
        // for keyof we want to lookup the keys on literals and keywords
        TypeKind::KeyOf(_) => key_of(t, ctx),
        TypeKind::IndexAccess(_) => todo!(),
        // should only be used to process object types and arrays/tuples
        // all other types should be passed through
        TypeKind::MappedType(_) => {
            let t = compute_mapped_type(t, ctx)?;
            expand_type(&t, ctx)
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
            expand_type(t.as_ref(), ctx)
        }
        TypeKind::Generic(_) => todo!(),
    }
}
