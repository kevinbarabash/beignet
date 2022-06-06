use std::collections::HashMap;

use crate::ast::*;
use crate::types::{Scheme, Type};

use super::constraint_solver::is_subtype;
use super::context::Context;
use super::infer_expr::{infer_expr, type_to_scheme};
use super::infer_type_ann::*;

// TODO: We need multiple Envs so that we can control things at differen scopes
// e.g. global, module, function, ...
pub fn infer_prog(prog: &Program) -> Result<Context, String> {
    let mut ctx: Context = Context::default();

    // TODO: figure out how report multiple errors
    for stmt in &prog.body {
        match stmt {
            Statement::VarDecl {
                declare,
                init,
                pattern,
                ..
            } => {
                match declare {
                    true => {
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                match type_ann {
                                    Some(type_ann) => {
                                        let scheme = infer_scheme(type_ann, &ctx);
                                        ctx.values.insert(id.name.to_owned(), scheme);
                                    }
                                    None => {
                                        // A type annotation should always be provided when using `declare`
                                        return Err(String::from(
                                            "missing type annotation in declare statement",
                                        ))
                                    },
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    false => {
                        // An initial value should always be used when using a normal `let` statement
                        let init = init.as_ref().unwrap();
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                let inferred_scheme = infer_expr(&ctx, init)?;
                                let scheme = match type_ann {
                                    Some(type_ann) => {
                                        let type_ann_ty = infer_type_ann(type_ann, &ctx);
                                        match is_subtype(&inferred_scheme.ty, &type_ann_ty, &ctx)? {
                                            true => Ok(type_to_scheme(&type_ann_ty)),
                                            false => Err(String::from(
                                                "value is not a subtype of decl's declared type",
                                            )),
                                        }
                                    }
                                    None => Ok(inferred_scheme),
                                };
                                ctx.values.insert(id.name.to_owned(), scheme?);
                            }
                            _ => todo!(),
                        }
                    }
                };
            }
            // TODO: dedupe with infer_type_ann.rs
            Statement::TypeDecl {
                id,
                type_ann,
                type_params,
                ..
            } => {
                let type_param_map: HashMap<String, Type> = match type_params {
                    Some(params) => params
                        .iter()
                        .map(|param| (param.name.name.to_owned(), ctx.fresh_var()))
                        .collect(),
                    None => HashMap::default(),
                };

                // Infers the type from type annotation and replaces all type references whose names
                // appear in `mapping` with a type variable whose `id` is the value in the mapping.
                let type_ann_ty = infer_type_ann_with_params(type_ann, &ctx, &type_param_map);

                // Creates a Scheme with the correct qualifiers for the type references that were
                // replaced with type variables.
                let scheme = Scheme {
                    qualifiers: type_param_map.values().map(|tv| tv.id).collect(),
                    ty: type_ann_ty.clone(),
                };

                ctx.types.insert(id.name.to_owned(), scheme);
            }
            Statement::Expr { expr, .. } => {
                // We ignore the type that was inferred, we only care that
                // it succeeds since we aren't assigning it to variable.
                infer_expr(&ctx, expr)?;
            }
        };
    }

    Ok(ctx)
}
