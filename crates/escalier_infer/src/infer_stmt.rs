use escalier_ast::types::{TRef, Type, TypeKind};
use escalier_ast::values::*;

use crate::context::{Context, Env};
use crate::infer_class::infer_class;
use crate::infer_expr::infer_expr as infer_expr_rec;
use crate::infer_pattern::*;
use crate::infer_type_ann::*;
use crate::scheme::generalize;
use crate::substitutable::Substitutable;
use crate::type_error::TypeError;
use crate::update::*;
use crate::util::close_over;

pub fn infer_stmt(stmt: &mut Statement, ctx: &mut Context) -> Result<(), Vec<TypeError>> {
    match stmt {
        Statement::VarDecl {
            declare,
            init,
            pattern,
            type_ann,
            ..
        } => {
            match declare {
                true => {
                    match &mut pattern.kind {
                        PatternKind::Ident(BindingIdent { name, .. }) => {
                            match type_ann {
                                Some(type_ann) => {
                                    let (s, t) = infer_type_ann(type_ann, ctx, &mut None)?;

                                    let t = close_over(&s, &t, ctx);
                                    ctx.insert_value(name.to_owned(), t);

                                    update_type_ann(type_ann, &s);
                                    update_pattern(pattern, &s);
                                }
                                None => {
                                    // A type annotation should always be provided when using `declare`
                                    return Err(vec![TypeError::MissingTypeAnnotation(Box::from(
                                        stmt.to_owned(),
                                    ))]);
                                }
                            }
                        }
                        _ => todo!(),
                    }
                }
                false => {
                    // An initial value should always be used when using a normal
                    // `let` statement
                    let init = init.as_mut().unwrap();

                    let (pa, s) = infer_pattern_and_init(
                        pattern,
                        type_ann,
                        init,
                        ctx,
                        &PatternUsage::Assign,
                    )?;

                    // Inserts the new variables from infer_pattern() into the
                    // current context.
                    for (name, mut binding) in pa {
                        binding.t = close_over(&s, &binding.t, ctx);
                        ctx.insert_binding(name, binding);
                    }

                    update_expr(init, &s);
                    update_pattern(pattern, &s);
                }
            };
        }
        Statement::TypeDecl {
            id: Ident { name, .. },
            type_ann,
            type_params,
            ..
        } => {
            let (s, t) = infer_type_ann(type_ann, ctx, type_params)?;

            let t = t.apply(&s);

            let empty_env = Env::default();
            let scheme = generalize(&empty_env, &t);

            ctx.insert_scheme(name.to_owned(), scheme);

            update_type_ann(type_ann, &s);
        }
        Statement::Expr { expr, .. } => {
            let (s, _) = infer_expr_rec(ctx, expr, false)?;
            // We ignore the type that was inferred, we only care that
            // it succeeds since we aren't assigning it to variable.
            update_expr(expr, &s);
        }
        Statement::ClassDecl {
            loc: _,
            span: _,
            ident,
            class,
        } => {
            let (s, t) = infer_class(ctx, class)?;

            let t = t.apply(&s);

            // This follows the same pattern found in lib.es5.d.ts.
            let name = ident.name.to_owned();
            eprintln!("inserting {name}Constructor = {t}");
            ctx.insert_type(format!("{name}Constructor"), t);
            ctx.insert_value(
                name.to_owned(),
                Type::from(TypeKind::Ref(TRef {
                    name: format!("{name}Constructor"),
                    type_args: None,
                })),
            );
        }
    };

    Ok(())
}
