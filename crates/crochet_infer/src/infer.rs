use crochet_ast::types::{TObjElem, TObject, TProp, TPropKey, TRef, Type, TypeKind};
use crochet_ast::values::*;

use crate::context::{Context, Env};
use crate::infer_class::infer_class;
use crate::infer_expr::infer_expr as infer_expr_rec;
use crate::infer_type_ann::*;
use crate::scheme::generalize;
use crate::substitutable::Substitutable;
use crate::type_error::TypeError;
use crate::update::*;
use crate::util::close_over;
use crate::{infer_pattern::*, normalize};

pub fn infer_prog(prog: &mut Program, ctx: &mut Context) -> Result<Context, Vec<TypeError>> {
    // TODO: replace with Class type once it exists
    // We use {_name: "Promise"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: TPropKey::StringKey(String::from("_name")),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("Promise"), 0..0, DUMMY_LOC)),
    })];
    let promise_type = Type::from(TypeKind::Object(TObject { elems }));
    ctx.insert_type(String::from("Promise"), promise_type);
    // TODO: replace with Class type once it exists
    // We use {_name: "JSXElement"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: TPropKey::StringKey(String::from("_name")),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("JSXElement"), 0..0, DUMMY_LOC)),
    })];
    let jsx_element_type = Type::from(TypeKind::Object(TObject { elems }));
    ctx.insert_type(String::from("JSXElement"), jsx_element_type);

    // We push a scope here so that it's easy to differentiate globals from
    // module definitions.
    ctx.push_scope(false);

    let mut reports: Vec<TypeError> = vec![];

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
                            PatternKind::Ident(BindingIdent { name, .. }) => {
                                match type_ann {
                                    Some(type_ann) => {
                                        match infer_type_ann(type_ann, ctx, &mut None) {
                                            Ok((s, t)) => {
                                                let t = close_over(&s, &t, ctx);
                                                ctx.insert_value(name.to_owned(), t);

                                                update_type_ann(type_ann, &s);
                                                update_pattern(pattern, &s);
                                            }
                                            Err(mut report) => reports.append(&mut report),
                                        }
                                    }
                                    None => {
                                        // A type annotation should always be provided when using `declare`
                                        return Err(vec![TypeError::MissingTypeAnnotation(
                                            Box::from(stmt.to_owned()),
                                        )]);
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

                        match infer_pattern_and_init(
                            pattern,
                            type_ann,
                            init,
                            ctx,
                            &PatternUsage::Assign,
                        ) {
                            Ok((pa, s)) => {
                                // Inserts the new variables from infer_pattern() into the
                                // current context.
                                for (name, mut binding) in pa {
                                    binding.t = close_over(&s, &binding.t, ctx);
                                    ctx.insert_binding(name, binding);
                                }

                                update_expr(init, &s);
                                update_pattern(pattern, &s);
                            }
                            Err(mut report) => reports.append(&mut report),
                        }
                    }
                };
            }
            Statement::TypeDecl {
                id: Ident { name, .. },
                type_ann,
                type_params,
                ..
            } => match infer_type_ann(type_ann, ctx, type_params) {
                Ok((s, t)) => {
                    let mut t = t.clone();
                    t.apply(&s);

                    let empty_env = Env::default();
                    let scheme = generalize(&empty_env, &t);

                    ctx.insert_scheme(name.to_owned(), scheme);

                    update_type_ann(type_ann, &s);
                }
                Err(mut report) => reports.append(&mut report),
            },
            Statement::Expr { expr, .. } => {
                match infer_expr_rec(ctx, expr) {
                    // We ignore the type that was inferred, we only care that
                    // it succeeds since we aren't assigning it to variable.
                    Ok((s, _)) => update_expr(expr, &s),
                    Err(mut report) => reports.append(&mut report),
                }
            }
            Statement::ClassDecl {
                loc: _,
                span: _,
                ident,
                class,
            } => {
                let (s, t) = infer_class(ctx, class)?;

                let mut t = t.clone();
                t.apply(&s);

                // This follows the same pattern found in lib.es5.d.ts.
                let name = ident.name.to_owned();
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
    }

    if reports.is_empty() {
        Ok(ctx.to_owned())
    } else {
        Err(reports)
    }
}

pub fn infer_expr(ctx: &mut Context, expr: &mut Expr) -> Result<Type, Vec<TypeError>> {
    let (s, t) = infer_expr_rec(ctx, expr)?;
    Ok(close_over(&s, &t, ctx))
}
