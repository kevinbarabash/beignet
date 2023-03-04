use escalier_ast::types::{TKeyword, TRef, Type, TypeKind};
use escalier_ast::values::*;

use crate::context::{Context, Env};
use crate::infer_class::infer_class;
use crate::infer_expr::infer_expr;
use crate::infer_pattern::*;
use crate::infer_type_ann::*;
use crate::scheme::generalize;
use crate::substitutable::Subst;
use crate::substitutable::Substitutable;
use crate::type_error::TypeError;
use crate::unify::unify;
use crate::update::*;
use crate::util::*;

pub fn infer_stmt(
    stmt: &mut Statement,
    ctx: &mut Context,
    top_level: bool,
) -> Result<(Subst, Type), Vec<TypeError>> {
    match &mut stmt.kind {
        StmtKind::VarDecl(VarDecl {
            declare,
            init,
            pattern,
            type_ann,
            ..
        }) => {
            match declare {
                false => {
                    // An initial value should always be used when using a normal
                    // `let` statement
                    let init = init.as_mut().unwrap();

                    let s = infer_pattern_and_init(
                        pattern,
                        type_ann.as_mut(),
                        &infer_expr(ctx, init, false)?,
                        ctx,
                        &PatternUsage::Assign,
                        top_level,
                    )?;

                    // TODO: Update `infer_pattern_and_init` to do this for us.
                    update_expr(init, &s);

                    let t = Type::from(TypeKind::Keyword(TKeyword::Undefined));

                    Ok((s, t))
                }
                true => {
                    match &mut pattern.kind {
                        PatternKind::Ident(BindingIdent { name, .. }) => {
                            match type_ann {
                                Some(type_ann) => {
                                    let (s, t) = infer_type_ann(type_ann, ctx, &mut None)?;

                                    let t = if top_level {
                                        close_over(&s, &t, ctx)
                                    } else {
                                        t
                                    };
                                    ctx.insert_value(name.to_owned(), t.to_owned());

                                    update_type_ann(type_ann, &s);
                                    update_pattern(pattern, &s);

                                    Ok((s, t))
                                }
                                None => {
                                    // A type annotation should always be provided when using `declare`
                                    Err(vec![TypeError::MissingTypeAnnotation(Box::from(
                                        stmt.to_owned(),
                                    ))])
                                }
                            }
                        }
                        _ => todo!(),
                    }
                }
            }
        }
        StmtKind::TypeDecl(TypeDecl {
            id: Ident { name, .. },
            type_ann,
            type_params,
            ..
        }) => {
            let (s, t) = infer_type_ann(type_ann, ctx, type_params)?;

            let t = t.apply(&s);

            let empty_env = Env::default();
            let scheme = generalize(&empty_env, &t);

            ctx.insert_scheme(name.to_owned(), scheme);

            update_type_ann(type_ann, &s);

            Ok((s, t))
        }
        StmtKind::ClassDecl(ClassDecl { ident, class }) => {
            let (s, t) = infer_class(ctx, class)?;

            let t = t.apply(&s);

            // This follows the same pattern found in lib.es5.d.ts.
            let name = ident.name.to_owned();
            eprintln!("inserting {name}Constructor = {t}");
            ctx.insert_type(format!("{name}Constructor"), t.to_owned());
            ctx.insert_value(
                name.to_owned(),
                Type::from(TypeKind::Ref(TRef {
                    name: format!("{name}Constructor"),
                    type_args: None,
                })),
            );

            Ok((s, t))
        }

        StmtKind::ExprStmt(expr) => {
            let (s, t) = infer_expr(ctx, expr, false)?;
            // We ignore the type that was inferred, we only care that
            // it succeeds since we aren't assigning it to variable.
            update_expr(expr, &s);

            Ok((s, t))
        }
        StmtKind::ForStmt(ForStmt {
            pattern,
            expr,
            body,
        }) => {
            let elem_t = ctx.fresh_var();
            let array_t = Type::from(TypeKind::Array(Box::from(elem_t.clone())));

            let (_expr_s, expr_t) = infer_expr(ctx, expr, false)?;

            let s1 = unify(&expr_t, &array_t, ctx)?;
            let elem_t = elem_t.apply(&s1);

            let mut new_ctx = ctx.clone();
            let s2 = infer_pattern_and_init(
                pattern,
                None,
                &(Subst::new(), elem_t),
                &mut new_ctx,
                &PatternUsage::Assign,
                top_level,
            )?;

            let (s3, _) = infer_block(body, &mut new_ctx)?;

            ctx.count = new_ctx.count;

            let s = compose_many_subs(&[s3, s2, s1]);
            let t = Type::from(TypeKind::Keyword(TKeyword::Undefined));

            Ok((s, t))
        }
        StmtKind::ReturnStmt(ReturnStmt { arg }) => match arg {
            Some(arg) => infer_expr(ctx, arg.as_mut(), false),
            None => {
                let s = Subst::default();
                let t = Type::from(TypeKind::Keyword(TKeyword::Undefined));

                Ok((s, t))
            }
        },
    }
}

pub fn infer_block(body: &mut Block, ctx: &mut Context) -> Result<(Subst, Type), Vec<TypeError>> {
    let mut new_ctx = ctx.clone();
    let mut t = Type::from(TypeKind::Keyword(TKeyword::Undefined));
    let mut s = Subst::new();

    for stmt in &mut body.stmts {
        new_ctx = new_ctx.clone();
        let (new_s, new_t) = infer_stmt(stmt, &mut new_ctx, false)?;

        t = new_t.apply(&s);
        s = compose_subs(&new_s, &s);
    }

    ctx.count = new_ctx.count;

    Ok((s, t))
}
