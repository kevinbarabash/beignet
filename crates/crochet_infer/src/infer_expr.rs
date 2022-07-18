use std::collections::HashMap;
use std::iter::Iterator;

use crate::types::{self, freeze, unfreeze, Flag, Primitive, Scheme, Type};
use crochet_ast::*;

use super::constraint_solver::{run_solve, Constraint};
use super::context::{Context, Env};
use super::infer_lambda::infer_lambda;
use super::infer_mem::infer_mem;
use super::infer_pattern::infer_pattern;
use super::substitutable::Substitutable;
use super::util::*;

pub fn infer_expr(ctx: &Context, expr: &Expr) -> Result<Scheme, String> {
    let mut constraints = vec![];
    let ty = infer(expr, ctx, &mut constraints)?;
    let subs = run_solve(&constraints, ctx)?;

    Ok(close_over(&ty.apply(&subs), ctx))
}

fn close_over(ty: &Type, ctx: &Context) -> Scheme {
    let empty_env = Env::new();
    normalize(&generalize(&empty_env, ty), ctx)
}

pub fn infer(
    expr: &Expr,
    ctx: &Context,
    constraints: &mut Vec<Constraint>,
) -> Result<Type, String> {
    // For each arm that adds constraints, we should instead be calling unify
    // on them just before we return the resulting type
    // `run_solve` maps to `mgu` which returns `Subst`
    // unify      :: Type -> Type -> TI ()
    // unify t1 t2 = do s <- getSubst
    //                  u <- mgu (apply s t1) (apply s t2)
    //                  extSubst u
    // Changes to make:
    // - keep track of all of the substitutions that have currently been created,
    //   as we gain more substitutions, these will be applied to other types that
    //   we encounter
    // Additional Notes:
    // - each call to runTI uses a fresh `Subst`
    // - `unify` only updates the substitutions in the monad with the result of `mgu`
    match expr {
        Expr::Ident(Ident { name, .. }) => ctx.lookup_value(name),
        Expr::Lambda(lambda) => infer_lambda(infer, lambda, ctx, constraints),
        Expr::Lit(literal) => Ok(ctx.lit(literal.to_owned())),
        Expr::Member(member) => infer_mem(infer, member, ctx, constraints),
        Expr::App(App { lam, args, .. }) => {
            let fn_type = infer(lam, ctx, constraints)?;

            let args_types = infer_many(args, ctx, constraints)?;
            let args_types = args_types
                .iter()
                .map(|arg| {
                    let mut arg = arg.to_owned();
                    arg.flag = Some(Flag::Argument);
                    arg
                })
                .collect();

            let ret_type = ctx.fresh_var();

            constraints.push(Constraint::from((
                ctx.lam(args_types, Box::new(ret_type.clone())),
                fn_type,
            )));

            Ok(ret_type)
        }

        Expr::Fix(Fix { expr, .. }) => {
            let expr_type = infer(expr, ctx, constraints)?;

            let fix_type = ctx.fresh_var();
            constraints.push(Constraint::from((
                ctx.lam(vec![fix_type.clone()], Box::new(fix_type.clone())),
                expr_type,
            )));

            Ok(fix_type)
        }

        Expr::IfElse(IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            let result_type = ctx.fresh_var();

            match alternate {
                Some(alternate) => {
                    let cond_type = infer(cond, ctx, constraints)?;
                    constraints.push(Constraint::from((cond_type, ctx.prim(Primitive::Bool))));

                    let cons_type = infer(consequent, ctx, constraints)?;
                    let alt_type = infer(alternate, ctx, constraints)?;
                    // We use unfrozen copies of cons_type and alt_type so that they can
                    // be widened if need be.  This does not affect the original frozen
                    // type.
                    constraints.push(Constraint::from((unfreeze(cons_type), result_type.clone())));
                    constraints.push(Constraint::from((unfreeze(alt_type), result_type.clone())));

                    Ok(result_type)
                }
                None => {
                    let (cond_type, cons_type) = match cond.as_ref() {
                        Expr::LetExpr(LetExpr { pat, expr, .. }) => {
                            let mut new_ctx = ctx.clone();

                            let mut init_cs = vec![];
                            let init_type = infer(expr, ctx, &mut init_cs)?;
                            constraints.append(&mut init_cs);

                            let subs = run_solve(&init_cs, ctx)?;

                            let (pat_type, new_vars) =
                                infer_pattern(pat, &mut new_ctx, constraints, &HashMap::new())?;
                            // let pat_type = set_flag(pat_type, &Flag::MatchPattern);

                            // Add bindings for the new variables inside the consequent block.
                            for (name, scheme) in new_vars {
                                new_ctx.values.insert(name, scheme);
                            }

                            // Order matters here: value_type appearing first indicates that
                            // it should be treated as a sub-type of pattern_type.
                            constraints.push(Constraint::from((init_type, pat_type)));

                            let mut cons_cs = vec![];
                            let cons_type = infer(consequent, &new_ctx, &mut cons_cs)?;
                            let mut cons_cs = cons_cs.apply(&subs);
                            constraints.append(&mut cons_cs);

                            // Ensures that type variable ids are unique.
                            ctx.state.count.set(new_ctx.state.count.get());

                            let cond_type = ctx.prim(Primitive::Bool);

                            (cond_type, cons_type)
                        }
                        _ => {
                            let cond_type = infer(cond, ctx, constraints)?;
                            let cons_type = infer(consequent, ctx, constraints)?;

                            (cond_type, cons_type)
                        }
                    };
                    constraints.push(Constraint::from((cond_type, ctx.prim(Primitive::Bool))));

                    // The alternative must be undefined
                    let alt_type = ctx.prim(Primitive::Undefined);
                    // We freeze `cons_type` and `alt_type` to make sure that they're unified
                    // with "undefined" exactly.
                    constraints.push(Constraint::from((freeze(cons_type), result_type.clone())));
                    constraints.push(Constraint::from((freeze(alt_type), result_type.clone())));

                    Ok(result_type)
                }
            }
        }

        Expr::Let(Let {
            pattern,
            init,
            body,
            ..
        }) => {
            let mut init_cs = vec![];
            let init_type = infer(init, ctx, &mut init_cs)?;
            constraints.append(&mut init_cs);

            let subs = run_solve(&init_cs, ctx)?;

            let body_type = match pattern {
                Some(pattern) => {
                    let mut new_ctx = ctx.clone();

                    let (pat_type, new_vars) =
                        infer_pattern(pattern, &mut new_ctx, constraints, &HashMap::new())?;
                    // let pat_type = set_flag(pat_type, &Flag::AssignPattern);

                    // Add bindings for the new variables inside the body.
                    for (name, scheme) in new_vars {
                        new_ctx.values.insert(name, scheme);
                    }

                    // Order matters here: value_type appearing first indicates that
                    // it should be treated as a sub-type of pattern_type.
                    constraints.push(Constraint::from((init_type, pat_type)));

                    let mut body_cs = vec![];
                    let body_type = infer(body, &new_ctx, &mut body_cs)?;
                    let mut body_cs = body_cs.apply(&subs);
                    constraints.append(&mut body_cs);

                    // Ensures that type variable ids are unique.
                    ctx.state.count.set(new_ctx.state.count.get());

                    body_type
                }
                // handles: let _ = ...
                None => {
                    let mut body_cs = vec![];
                    let body_type = infer(body, ctx, &mut body_cs)?;
                    let mut body_cs = body_cs.apply(&subs);
                    constraints.append(&mut body_cs);

                    body_type
                }
            };

            Ok(body_type.apply(&subs))
        }

        // TODO: consider introduce functions for each operator and rewrite Ops as Apps
        Expr::Op(Op {
            left, right, op, ..
        }) => {
            let left = Box::as_ref(left);
            let right = Box::as_ref(right);

            let args_types = infer_many(&[left.clone(), right.clone()], ctx, constraints)?;
            let ret_type = ctx.fresh_var();

            let c = match op {
                BinOp::EqEq | BinOp::NotEq => {
                    let arg_tv = ctx.fresh_var();
                    Constraint {
                        types: (
                            ctx.lam(args_types, Box::from(ret_type.clone())),
                            // equivalent to <T>(arg0: T, arg1: T) => bool
                            ctx.lam(
                                vec![arg_tv.clone(), arg_tv],
                                Box::from(ctx.prim(Primitive::Bool)),
                            ),
                        ),
                    }
                }
                // For now, only numbers can be ordered, but in the future we should allow
                // strings and potentially user defined types.
                BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => Constraint {
                    types: (
                        ctx.lam(args_types, Box::from(ret_type.clone())),
                        ctx.lam(
                            vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Num)],
                            Box::from(ctx.prim(Primitive::Bool)),
                        ),
                    ),
                },
                // This is essential the same as a call or application
                BinOp::Add | BinOp::Sub | BinOp::Div | BinOp::Mul => {
                    let inf_type = ctx.lam(args_types, Box::from(ret_type.clone()));
                    let def_type = ctx.lam(
                        vec![
                            ctx.prim_with_flag(Primitive::Num, Flag::Argument),
                            ctx.prim_with_flag(Primitive::Num, Flag::Argument),
                        ],
                        Box::from(ctx.prim(Primitive::Num)),
                    );

                    Constraint::from((inf_type, def_type))
                }
            };
            constraints.push(c);

            Ok(ret_type)
        }

        Expr::Obj(Obj { props, .. }) => {
            let props: Result<Vec<types::TProp>, String> = props
                .iter()
                .map(|p| {
                    match p {
                        PropOrSpread::Prop(p) => match p.as_ref() {
                            Prop::Shorthand(Ident { name, .. }) => {
                                let prop_type = ctx.lookup_value(name)?;

                                // The property is not optional in the type we infer from
                                // an object literal, because the property has a value.
                                Ok(ctx.prop(name, prop_type, false))
                            }
                            Prop::KeyValue(KeyValueProp { name, value, .. }) => {
                                let prop_type = infer(value, ctx, constraints)?;

                                // The property is not optional in the type we infer from
                                // an object literal, because the property has a value.
                                Ok(ctx.prop(name, prop_type, false))
                            }
                        },
                        PropOrSpread::Spread(_) => todo!(),
                    }
                })
                .collect();

            Ok(ctx.object(props?))
        }

        Expr::Await(Await { expr, .. }) => {
            if !ctx.is_async {
                return Err(String::from("Can't use `await` inside non-async lambda"));
            }

            let inferred_type = infer(expr, ctx, constraints)?;
            let type_param_type = ctx.fresh_var();
            let promise_type = ctx.alias("Promise", Some(vec![type_param_type.clone()]));

            constraints.push(Constraint::from((inferred_type, promise_type)));

            Ok(type_param_type)
        }

        Expr::JSXElement(JSXElement {
            span: _,
            name: _,
            attrs: _,
            children: _,
        }) => {
            // TODO: check that the `attrs` match the props of the component/tag with
            // the given `name`.  If there are any `children`, check that they matches
            // props['children'].

            // TODO: replace this with JSX.Element once we have support for Type::Mem
            Ok(ctx.alias("JSXElement", None))
        }

        Expr::Tuple(Tuple { elems, .. }) => Ok(ctx.tuple(infer_many(elems, ctx, constraints)?)),
        Expr::Empty(_) => Ok(ctx.prim(Primitive::Undefined)),
        Expr::LetExpr(_) => todo!(), // should this be a literal maybe?
    }
}

fn infer_many(
    exprs: &[Expr],
    ctx: &Context,
    constraints: &mut Vec<Constraint>,
) -> Result<Vec<Type>, String> {
    exprs
        .iter()
        .map(|expr| infer(expr, ctx, constraints))
        .collect()
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;
    use crochet_parser::expr::expr_parser;

    use super::*;

    fn parse_and_infer_expr(input: &str) -> String {
        let ctx: Context = Context::default();
        let expr = expr_parser().then_ignore(end()).parse(input).unwrap();
        format!("{}", infer_expr(&ctx, &expr).unwrap())
    }

    #[test]
    fn infer_let_with_type_ann() {
        assert_eq!(parse_and_infer_expr("{let x: number = 5; x}"), "number");
    }

    #[test]
    #[should_panic = "unification failed"]
    fn infer_let_with_incorrect_type_ann() {
        parse_and_infer_expr("{let x: string = 5; x}");
    }
}
