use std::collections::HashMap;

use crate::ast::*;
use crate::types::Scheme;

use super::constraint_solver::{Constraint, run_solve, is_subtype};
use super::context::Context;
use super::substitutable::*;
use super::infer_expr::{infer_expr, infer};
use super::infer_pattern::infer_pattern;
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
                    
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                let inferred_scheme = infer_expr(&ctx, init)?;
                                let scheme = match type_ann {
                                    Some(type_ann) => {
                                        let type_ann_ty = infer_type_ann(type_ann, &ctx);
                                        match is_subtype(&inferred_scheme.ty, &type_ann_ty, &ctx)? {
                                            true => Ok(Scheme::from(type_ann_ty)),
                                            false => Err(String::from(
                                                "value is not a subtype of decl's declared type",
                                            )),
                                        }
                                    }
                                    None => Ok(inferred_scheme),
                                };
                                ctx.values.insert(id.name.to_owned(), scheme?);
                            }
                            _ => {
                                let (pat_type, mut pat_cs, new_vars) =
                                    infer_pattern(pattern, &mut ctx, &HashMap::new())?;
                                println!("pattern_type: {}", pat_type);
                                let (init_ty, mut cs) = infer(init, &ctx)?;
                                cs.append(&mut pat_cs);
                                cs.push(Constraint { types: (init_ty, pat_type.clone()) });
                                let subs = run_solve(&cs, &ctx)?;
                                // TODO: call close_over() so that we can get a scheme out of this
                                let pat_type = pat_type.apply(&subs);
                                println!("pattern_type: {}", pat_type);
                                println!("new_vars = {:#?}", new_vars);

                                for (name, scheme) in new_vars {
                                    println!("{name} = {scheme}");
                                    let scheme = scheme.apply(&subs);
                                    println!("{name} = {scheme}");
                                    ctx.values.insert(name, scheme);
                                }
                                
                                // TODO: 
                            }
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
