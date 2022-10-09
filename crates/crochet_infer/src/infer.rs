use crochet_ast::*;
use crochet_types::{TObjElem, TObject, TProp, Type};

use super::context::{Context, Env};
use super::infer_expr::infer_expr as infer_expr_rec;
use super::infer_pattern::*;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
use super::update::update_program;
use super::util::*;

pub fn infer_prog(prog: &mut Program, ctx: &mut Context) -> Result<Context, String> {
    // let mut ctx: Context = Context::default();
    // TODO: replace with Class type once it exists
    // We use {_name: "Promise"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: String::from("_name"),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("Promise"), 0..0)),
    })];
    let promise_type = Type::Object(TObject { elems });
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
    let jsx_element_type = Type::Object(TObject { elems });
    ctx.insert_type(String::from("JSXElement"), jsx_element_type);

    // We push a scope here so that it's easy to differentiate globals from
    // module definitions.
    ctx.push_scope(false);

    let mut ss: Vec<Subst> = vec![];

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
                            PatternKind::Ident(BindingIdent { id, .. }) => {
                                match type_ann {
                                    Some(type_ann) => {
                                        let (s, t) = infer_scheme(type_ann, ctx)?;
                                        ctx.insert_value(id.name.to_owned(), t.apply(&s));

                                        ss.push(s);
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
                            let t = normalize(&t.apply(&s), ctx);
                            ctx.insert_value(name, t);
                        }

                        ss.push(s);
                    }
                };
            }
            Statement::TypeDecl {
                id,
                type_ann,
                type_params,
                ..
            } => {
                let (s, t) = infer_qualified_type_ann(type_ann, type_params, ctx)?;
                ctx.insert_type(id.name.to_owned(), t.apply(&s));

                ss.push(s);
            }
            Statement::Expr { expr, .. } => {
                // We ignore the type that was inferred, we only care that
                // it succeeds since we aren't assigning it to variable.
                let (s, _) = infer_expr_rec(ctx, expr)?;

                ss.push(s);
            }
        };
    }

    let s = compose_many_subs(&ss);
    update_program(prog, &s);

    Ok(ctx.to_owned())
}

pub fn infer_expr(ctx: &mut Context, expr: &mut Expr) -> Result<Type, String> {
    let (s, t) = infer_expr_rec(ctx, expr)?;
    Ok(close_over(&s, &t, ctx))
}

// closeOver :: (Map.Map TVar Type, Type) -> Scheme
// closeOver (sub, ty) = normalize sc
//   where sc = generalize emptyTyenv (apply sub ty)
pub fn close_over(s: &Subst, t: &Type, ctx: &Context) -> Type {
    let empty_env = Env::default();
    normalize(&generalize_type(&empty_env, &t.to_owned().apply(s)), ctx)
}
