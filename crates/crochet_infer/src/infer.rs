use crochet_ast::*;
use crochet_types::{TObjElem, TObject, TProp, Type};

use super::context::{Context, Env};
use super::infer_expr::infer_expr as infer_expr_rec;
use super::infer_pattern::*;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
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
    let promise_type = Type::Object(TObject {
        elems,
        type_params: vec![],
    });
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
    let jsx_element_type = Type::Object(TObject {
        elems,
        type_params: vec![],
    });
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
                        match pattern {
                            Pattern::Ident(BindingIdent { id, .. }) => {
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

fn update_program(prog: &mut Program, s: &Subst) {
    for stmt in prog.body.iter_mut() {
        match stmt {
            Statement::VarDecl {
                span: _,
                pattern,
                type_ann,
                init,
                declare: _,
            } => {
                update_pattern(pattern, s);
                if let Some(type_ann) = type_ann {
                    update_type_ann(type_ann, s);
                }
                if let Some(init) = init.as_mut() {
                    update_expr(init, s);
                }
            }
            Statement::TypeDecl {
                span: _,
                declare: _,
                id: _,
                type_ann,
                type_params: _,
            } => {
                update_type_ann(type_ann, s);
            }
            Statement::Expr { span: _, expr } => {
                update_expr(expr, s);
            }
        }
    }
}

fn update_pattern(pattern: &mut Pattern, _s: &Subst) {
    match pattern {
        Pattern::Ident(_) => (),
        Pattern::Rest(_) => (),
        Pattern::Object(_) => (),
        Pattern::Array(_) => (),
        Pattern::Lit(_) => (),
        Pattern::Is(_) => (),
        Pattern::Wildcard(_) => (),
    }
}

fn update_expr(expr: &mut Expr, s: &Subst) {
    // Since we process the node first, if `expr` is a leaf-node we
    // ignore it in the match statement below.
    match &expr.inferred_type {
        Some(t) => {
            if let Type::Var(_) = t {
                expr.inferred_type = Some(t.apply(s));
            }
        }
        None => (),
    };

    match &mut expr.kind {
        ExprKind::App(App { lam, args }) => {
            update_expr(lam, s);
            args.iter_mut().for_each(|arg_or_spread| {
                // TODO: rework args to be less awkward
                update_expr(&mut arg_or_spread.expr, s);
            });
        }
        ExprKind::Fix(Fix { expr }) => {
            update_expr(expr, s);
        }
        ExprKind::Ident(_) => (), // leaf node
        ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            update_expr(cond, s);
            update_expr(consequent, s);
            if let Some(alternate) = alternate {
                update_expr(alternate, s);
            }
        }
        ExprKind::JSXElement(JSXElement {
            span: _,
            name: _,
            attrs,
            children,
        }) => {
            attrs.iter_mut().for_each(|attr| {
                match &mut attr.value {
                    JSXAttrValue::Lit(_) => (), // leaf-node
                    JSXAttrValue::JSXExprContainer(JSXExprContainer { expr, span: _ }) => {
                        update_expr(expr, s);
                    }
                }
            });
            children.iter_mut().for_each(|child| {
                update_jsx_element_child(child, s);
            })
        }
        ExprKind::Lambda(Lambda {
            params,
            body,
            is_async: _,
            return_type: _,
            type_params: _,
        }) => {
            params.iter_mut().for_each(|param| {
                update_fn_param(param, s);
            });
            update_expr(body, s);
        }
        ExprKind::Let(Let {
            pattern,
            type_ann,
            init,
            body,
        }) => {
            if let Some(pattern) = pattern {
                update_pattern(pattern, s);
            }
            if let Some(type_ann) = type_ann {
                update_type_ann(type_ann, s);
            }
            update_expr(init, s);
            update_expr(body, s);
        }
        ExprKind::LetExpr(LetExpr { pat, expr }) => {
            update_pattern(pat, s);
            update_expr(expr, s);
        }
        ExprKind::Lit(_) => (), // leaf node
        ExprKind::BinaryExpr(BinaryExpr { left, right, op: _ }) => {
            update_expr(left, s);
            update_expr(right, s);
        }
        ExprKind::UnaryExpr(UnaryExpr { arg, op: _ }) => {
            update_expr(arg, s);
        }
        ExprKind::Obj(Obj { props }) => {
            // TODO: handle props
            props
                .iter_mut()
                .for_each(|prop_or_spread| match prop_or_spread {
                    PropOrSpread::Spread(spread) => {
                        update_expr(&mut spread.expr, s);
                    }
                    // TODO: figure out how to attach an inferred_type to props
                    PropOrSpread::Prop(prop) => match prop.as_mut() {
                        Prop::Shorthand(_) => (),
                        Prop::KeyValue(_) => (),
                    },
                });
        }
        ExprKind::Await(Await { expr }) => update_expr(expr, s),
        ExprKind::Tuple(Tuple { elems }) => {
            elems
                .iter_mut()
                .for_each(|elem| update_expr(&mut elem.expr, s));
        }
        ExprKind::Member(Member { obj, prop }) => {
            update_expr(obj, s);
            update_member_prop(prop, s);
        }
        ExprKind::Empty => (), // leaf node
        ExprKind::TemplateLiteral(TemplateLiteral { exprs, quasis: _ }) => {
            // NOTE: we don't bother with quasis because they're just another
            // flavor of string literal.
            exprs.iter_mut().for_each(|expr| {
                update_expr(expr, s);
            });
        }
        ExprKind::TaggedTemplateLiteral(_) => (),
        ExprKind::Match(Match { expr, arms }) => {
            update_expr(expr, s);
            arms.iter_mut().for_each(|arm| {
                let Arm {
                    span: _,
                    pattern,
                    guard,
                    body,
                } = arm;
                update_pattern(pattern, s);
                if let Some(guard) = guard {
                    update_expr(guard, s);
                }
                update_expr(body, s);
            })
        }
    }
}

fn update_fn_param(_param: &mut EFnParam, _s: &Subst) {}

fn update_type_ann(_type_ann: &mut TypeAnn, _s: &Subst) {}

fn update_member_prop(_prop: &mut MemberProp, _s: &Subst) {}

fn update_jsx_element_child(_prop: &mut JSXElementChild, _s: &Subst) {}
