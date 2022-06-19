use std::collections::HashMap;

use crate::ast::*;
use crate::types::{freeze_scheme, set_flag, Flag};

use super::constraint_solver::{run_solve, Constraint};
use super::context::Context;
use super::infer_expr::{infer, infer_expr};
use super::infer_pattern::infer_pattern;
use super::infer_type_ann::*;
use super::substitutable::*;
use super::util::{normalize, is_subtype};

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
                                        ctx.values.insert(id.name.to_owned(), freeze_scheme(scheme));
                                    }
                                    None => {
                                        // A type annotation should always be provided when using `declare`
                                        return Err(String::from(
                                            "missing type annotation in declare statement",
                                        ));
                                    }
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    false => {
                        // An initial value should always be used when using a normal `let` statement
                        let init = init.as_ref().unwrap();

                        let mut constraints: Vec<Constraint> = vec![];

                        let (pat_type, new_vars) = infer_pattern(
                            pattern,
                            &mut ctx,
                            &mut constraints,
                            &HashMap::new(),
                        )?;
                        let pat_type = set_flag(pat_type, &Flag::AssignPattern);
                      
                        let init_type = infer(init, &ctx, &mut constraints)?;

                        constraints.push(Constraint::from((init_type.clone(), pat_type.clone())));
                        let subs = run_solve(&constraints, &ctx)?;

                        // We need to apply the substitutions from the constraint solver to the
                        // pattern and initializer types before checking if they're subtypes since
                        // since is_subtype() ignores type variables.
                        let pat_type = pat_type.apply(&subs);
                        let init_type = init_type.apply(&subs);

                        // If the initializer is not a subtype of what we're assigning it
                        // to, return an error.
                        if !is_subtype(&init_type, &pat_type, &ctx)? {
                            return Err(String::from(
                                "value is not a subtype of decl's declared type",
                            ));
                        }

                        // Inserts the new variables from infer_pattern() into the
                        // current context.
                        for (name, scheme) in new_vars {
                            let scheme = normalize(&scheme.apply(&subs), &ctx);
                            ctx.values.insert(name, freeze_scheme(scheme));
                        }
                    }
                };
            }
            Statement::TypeDecl {
                id,
                type_ann,
                type_params,
                ..
            } => {
                let scheme = infer_scheme_with_type_params(type_ann, type_params, &ctx);
                ctx.types.insert(id.name.to_owned(), freeze_scheme(scheme));
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
