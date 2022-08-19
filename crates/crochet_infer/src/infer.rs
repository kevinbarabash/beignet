use crochet_ast::*;

use super::context::{Context, Env};
use super::infer_expr::infer_expr as infer_expr_rec;
use super::infer_pattern::*;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
use super::types::{Scheme, TProp, Type};
use super::util::*;

pub fn infer_prog(prog: &Program, ctx: &mut Context) -> Result<Context, String> {
    // let mut ctx: Context = Context::default();
    // TODO: replace with Class type once it exists
    // We use {_name: "Promise"} to differentiate it from other
    // object types.
    let promise_scheme = Scheme::from(Type::Object(vec![TProp {
        name: String::from("_name"),
        optional: false,
        mutable: false,
        ty: ctx.lit(Lit::str(String::from("Promise"), 0..0)),
    }]));
    ctx.types.insert(String::from("Promise"), promise_scheme);
    // TODO: replace with Class type once it exists
    // We use {_name: "JSXElement"} to differentiate it from other
    // object types.
    let jsx_element_scheme = Scheme::from(Type::Object(vec![TProp {
        name: String::from("_name"),
        optional: false,
        mutable: false,
        ty: ctx.lit(Lit::str(String::from("JSXElement"), 0..0)),
    }]));
    ctx.types
        .insert(String::from("JSXElement"), jsx_element_scheme);

    // TODO: figure out how report multiple errors
    for stmt in &prog.body {
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
                        match pattern {
                            Pattern::Ident(BindingIdent { id, .. }) => {
                                match type_ann {
                                    Some(type_ann) => {
                                        let scheme = infer_scheme(type_ann, ctx);
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
                        // An initial value should always be used when using a normal
                        // `let` statement
                        let init = init.as_ref().unwrap();

                        let (pa, s) = infer_pattern_and_init(
                            pattern,
                            type_ann,
                            init,
                            ctx,
                            &PatternUsage::Assign,
                        )?;

                        // Inserts the new variables from infer_pattern() into the
                        // current context.
                        for (name, scheme) in pa {
                            let scheme = normalize(&scheme.apply(&s), ctx);
                            ctx.values.insert(name, scheme);
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
                let scheme = infer_scheme_with_type_params(type_ann, type_params, ctx);
                ctx.types.insert(id.name.to_owned(), scheme);
            }
            Statement::Expr { expr, .. } => {
                // We ignore the type that was inferred, we only care that
                // it succeeds since we aren't assigning it to variable.
                infer_expr(ctx, expr)?;
            }
        };
    }

    Ok(ctx.to_owned())
}

pub fn infer_expr(ctx: &mut Context, expr: &Expr) -> Result<Scheme, String> {
    let (s, t) = infer_expr_rec(ctx, expr)?;
    Ok(close_over(&s, &t, ctx))
}

// closeOver :: (Map.Map TVar Type, Type) -> Scheme
// closeOver (sub, ty) = normalize sc
//   where sc = generalize emptyTyenv (apply sub ty)
fn close_over(s: &Subst, t: &Type, ctx: &Context) -> Scheme {
    let empty_env = Env::default();
    normalize(&generalize(&empty_env, &t.to_owned().apply(s)), ctx)
}
