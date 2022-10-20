use crochet_ast::common::*;
use crochet_ast::types::{TObjElem, TObject, TProp, Type, TypeKind};
use crochet_ast::values::*;

use crate::context::Context;
use crate::infer_expr::infer_expr as infer_expr_rec;
use crate::infer_pattern::*;
use crate::infer_type_ann::*;
use crate::update::*;
use crate::util::*;

pub fn infer_prog(prog: &mut Program, ctx: &mut Context) -> Result<Context, String> {
    // TODO: replace with Class type once it exists
    // We use {_name: "Promise"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: String::from("_name"),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("Promise"), 0..0)),
    })];
    let promise_type = Type {
        kind: TypeKind::Object(TObject { elems }),
    };
    ctx.insert_type(String::from("Promise"), promise_type);
    // TODO: replace with Class type once it exists
    // We use {_name: "JSXElement"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: String::from("_name"),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("JSXElement"), 0..0)),
    })];
    let jsx_element_type = Type {
        kind: TypeKind::Object(TObject { elems }),
    };
    ctx.insert_type(String::from("JSXElement"), jsx_element_type);

    // We push a scope here so that it's easy to differentiate globals from
    // module definitions.
    ctx.push_scope(false);

    // TODO: figure out how report multiple errors
    for stmt in &mut prog.body {
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
                            PatternKind::Ident(BindingIdent { name }) => {
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
                        for (name, t) in pa {
                            let t = close_over(&s, &t, ctx);
                            ctx.insert_value(name, t);
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
                let t = close_over(&s, &t, ctx);
                ctx.insert_type(name.to_owned(), t);

                update_type_ann(type_ann, &s);
            }
            Statement::Expr { expr, .. } => {
                // We ignore the type that was inferred, we only care that
                // it succeeds since we aren't assigning it to variable.
                let (s, _) = infer_expr_rec(ctx, expr)?;
                update_expr(expr, &s);
            }
        };
    }

    Ok(ctx.to_owned())
}

pub fn infer_expr(ctx: &mut Context, expr: &mut Expr) -> Result<Type, String> {
    let (s, t) = infer_expr_rec(ctx, expr)?;
    Ok(close_over(&s, &t, ctx))
}
