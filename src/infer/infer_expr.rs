use defaultmap::*;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, Flag, Primitive, Scheme, Type, Variant};

use super::constraint_solver::{run_solve, Constraint};
use super::context::{Context, Env};
use super::infer_lambda::infer_lambda;
use super::infer_mem::infer_mem;
use super::infer_pattern::infer_pattern;
use super::substitutable::Substitutable;

pub fn infer_expr(ctx: &Context, expr: &Expr) -> Result<Scheme, String> {
    let (ty, cs) = infer(expr, ctx)?;
    let subs = run_solve(&cs, ctx)?;

    Ok(close_over(&ty.apply(&subs), ctx))
}

fn close_over(ty: &Type, ctx: &Context) -> Scheme {
    let empty_env = Env::new();
    normalize(&generalize(&empty_env, ty), ctx)
}

pub fn normalize(sc: &Scheme, ctx: &Context) -> Scheme {
    let body = &sc.ty;
    let keys = body.ftv();
    let mut keys: Vec<_> = keys.iter().cloned().collect();
    keys.sort_unstable();
    let mapping: HashMap<i32, Type> = keys
        .iter()
        .enumerate()
        .map(|(index, key)| {
            (
                key.to_owned(),
                Type {
                    id: index as i32,
                    frozen: false,
                    variant: Variant::Var,
                    flag: None,
                },
            )
        })
        .collect();

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    fn norm_type(ty: &Type, mapping: &HashMap<i32, Type>, ctx: &Context) -> Type {
        match &ty.variant {
            Variant::Var => mapping.get(&ty.id).unwrap().to_owned(),
            Variant::Lam(types::LamType { params, ret }) => {
                let params: Vec<_> = params
                    .iter()
                    .map(|param| norm_type(param, mapping, ctx))
                    .collect();
                Type {
                    variant: Variant::Lam(types::LamType {
                        params,
                        ret: Box::from(norm_type(ret, mapping, ctx)),
                    }),
                    ..ty.to_owned()
                }
            }
            Variant::Prim(_) => ty.to_owned(),
            Variant::Lit(_) => ty.to_owned(),
            Variant::Union(types) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type {
                    variant: Variant::Union(types),
                    ..ty.to_owned()
                }
            }
            Variant::Intersection(types) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types: Vec<_> = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                simplify_intersection(&types, ctx)
            }
            Variant::Object(props) => {
                let props = props
                    .iter()
                    .map(|prop| types::TProp {
                        name: prop.name.clone(),
                        optional: prop.optional,
                        ty: norm_type(&prop.ty, mapping, ctx),
                    })
                    .collect();
                Type {
                    variant: Variant::Object(props),
                    ..ty.to_owned()
                }
            }
            Variant::Alias(types::AliasType { name, type_params }) => {
                let type_params = type_params.clone().map(|params| {
                    params
                        .iter()
                        .map(|ty| norm_type(ty, mapping, ctx))
                        .collect()
                });
                Type {
                    variant: Variant::Alias(types::AliasType {
                        name: name.to_owned(),
                        type_params,
                    }),
                    ..ty.to_owned()
                }
            }
            Variant::Tuple(types) => {
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type {
                    variant: Variant::Tuple(types),
                    ..ty.to_owned()
                }
            }
            Variant::Rest(arg) => Type {
                variant: Variant::Rest(Box::from(norm_type(arg, mapping, ctx))),
                ..ty.to_owned()
            },
            Variant::Member(types::MemberType { obj, prop }) => Type {
                variant: Variant::Member(types::MemberType {
                    obj: Box::from(norm_type(obj, mapping, ctx)),
                    prop: prop.to_owned(),
                }),
                ..ty.to_owned()
            },
        }
    }

    Scheme {
        qualifiers: (0..keys.len()).map(|x| x as i32).collect(),
        ty: norm_type(body, &mapping, ctx),
    }
}

fn generalize(env: &Env, ty: &Type) -> Scheme {
    // ftv() returns a Set which is not ordered
    // TODO: switch to an ordered set
    let mut qualifiers: Vec<_> = ty.ftv().difference(&env.ftv()).cloned().collect();
    qualifiers.sort_unstable();
    Scheme {
        qualifiers,
        ty: ty.clone(),
    }
}

pub fn infer(expr: &Expr, ctx: &Context) -> Result<(Type, Vec<Constraint>), String> {
    let mut constraints = Vec::new();

    match expr {
        Expr::Ident(Ident { name, .. }) => {
            let ty = ctx.lookup_value(name);
            Ok((ty, vec![]))
        }
        Expr::App(App { lam, args, .. }) => {
            let (fn_type, mut fn_cs) = infer(lam, ctx)?;
            constraints.append(&mut fn_cs);

            let (args_types, mut args_cs) = infer_many(args, ctx)?;
            constraints.append(&mut args_cs);

            let tv = ctx.fresh_var();

            constraints.push(Constraint::from((
                ctx.lam(args_types, Box::new(tv.clone())),
                fn_type,
            )));

            Ok((tv, constraints))
        }
        Expr::Fix(Fix { expr, .. }) => {
            let (expr_type, mut expr_cs) = infer(expr, ctx)?;
            constraints.append(&mut expr_cs);

            let tv = ctx.fresh_var();
            constraints.push(Constraint::from((
                ctx.lam(vec![tv.clone()], Box::new(tv.clone())),
                expr_type,
            )));

            Ok((tv, constraints))
        }
        Expr::IfElse(IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            let (cond_type, mut cond_cs) = infer(cond, ctx)?;
            constraints.append(&mut cond_cs);

            let (cons_type, mut cons_cs) = infer(consequent, ctx)?;
            constraints.append(&mut cons_cs);

            let (alt_type, mut alt_cs) = infer(alternate, ctx)?;
            constraints.append(&mut alt_cs);

            constraints.push(Constraint::from((cond_type, ctx.prim(Primitive::Bool))));
            constraints.push(Constraint::from((cons_type.clone(), alt_type)));

            Ok((cons_type, constraints))
        }
        Expr::Lambda(lambda) => infer_lambda(lambda, ctx, infer),
        Expr::Let(Let {
            pattern,
            value,
            body,
            ..
        }) => {
            let (init_type, mut init_cs) = infer(value, ctx)?;
            constraints.append(&mut init_cs);

            let subs = run_solve(&init_cs, ctx)?;

            let body_type = match pattern {
                Some(pattern) => {
                    let mut new_ctx = ctx.clone();

                    let (mut pat_type, mut pat_cs, new_vars) =
                        infer_pattern(pattern, &mut new_ctx, &HashMap::new())?;
                    pat_type.flag = Some(Flag::SupertypeWins);

                    for (name, scheme) in new_vars {
                        new_ctx.values.insert(name, scheme);
                    }

                    constraints.append(&mut pat_cs);
                    // Order matters here: value_type appearing first indicates that
                    // it should be treated as a sub-type of pattern_type.
                    constraints.push(Constraint::from((init_type, pat_type)));

                    let (body_type, body_cs) = infer(body, &new_ctx)?;
                    let mut body_cs = body_cs.apply(&subs);
                    constraints.append(&mut body_cs);

                    // Ensures that type variable ids are unique.
                    ctx.state.count.set(new_ctx.state.count.get());

                    body_type
                }
                // handles: let _ = ...
                None => {
                    let (body_type, body_cs) = infer(body, ctx)?;
                    let mut body_cs = body_cs.apply(&subs);
                    constraints.append(&mut body_cs);

                    body_type
                }
            };

            Ok((body_type.apply(&subs), constraints))
        }
        Expr::Lit(literal) => Ok((ctx.lit(literal.to_owned()), vec![])),
        // TODO: consider introduce functions for each operator and rewrite Ops as Apps
        Expr::Op(Op {
            left, right, op, ..
        }) => {
            let left = Box::as_ref(left);
            let right = Box::as_ref(right);

            let (args_types, mut args_cs) = infer_many(&[left.clone(), right.clone()], ctx)?;
            constraints.append(&mut args_cs);

            let tv = ctx.fresh_var();

            let c = match op {
                BinOp::EqEq | BinOp::NotEq => {
                    let arg_tv = ctx.fresh_var();
                    Constraint {
                        types: (
                            ctx.lam(args_types, Box::from(tv.clone())),
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
                        ctx.lam(args_types, Box::from(tv.clone())),
                        ctx.lam(
                            vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Num)],
                            Box::from(ctx.prim(Primitive::Bool)),
                        ),
                    ),
                },
                BinOp::Add | BinOp::Sub | BinOp::Div | BinOp::Mul => {
                    let inf_type = ctx.lam(args_types, Box::from(tv.clone()));
                    let def_type = ctx.lam(
                        vec![
                            ctx.prim_with_flag(Primitive::Num, Flag::SubtypeWins),
                            ctx.prim_with_flag(Primitive::Num, Flag::SubtypeWins),
                        ],
                        Box::from(ctx.prim(Primitive::Num)),
                    );

                    Constraint::from((inf_type, def_type))
                }
            };
            constraints.push(c);

            Ok((tv, constraints))
        }
        Expr::Obj(Obj { props, .. }) => {
            let props: Result<Vec<types::TProp>, String> = props
                .iter()
                .map(|p| {
                    let (prop_type, mut prop_cs) = infer(&p.value, ctx)?;
                    constraints.append(&mut prop_cs);

                    // The property is not optional in the type we infer from
                    // an object literal, because the property has a value.
                    Ok(ctx.prop(&p.name, prop_type, false))
                })
                .collect();

            let obj_type = ctx.object(&props?);

            Ok((obj_type, constraints))
        }
        Expr::Await(Await { expr, .. }) => {
            if !ctx.is_async {
                return Err(String::from("Can't use `await` inside non-async lambda"));
            }

            let (promise_ty, mut promise_cs) = infer(expr, ctx)?;
            let tv = ctx.fresh_var();

            constraints.append(&mut promise_cs);
            constraints.push(Constraint::from((
                promise_ty,
                ctx.alias("Promise", Some(vec![tv.clone()])),
            )));

            Ok((tv, constraints))
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
            let ty = ctx.alias("JSXElement", None);

            Ok((ty, vec![]))
        }
        Expr::Tuple(Tuple { elems, .. }) => {
            let mut types: Vec<Type> = vec![];

            for elem in elems {
                let (elem_type, mut elem_cs) = infer(elem, ctx)?;
                constraints.append(&mut elem_cs);
                types.push(elem_type);
            }

            Ok((ctx.tuple(types), constraints))
        }
        Expr::Member(member) => infer_mem(infer, member, ctx),
    }
}

fn infer_many(exprs: &[Expr], ctx: &Context) -> Result<(Vec<Type>, Vec<Constraint>), String> {
    let mut types: Vec<Type> = Vec::new();
    let mut constraints: Vec<Constraint> = Vec::new();

    for expr in exprs {
        let (expr_type, expr_cs) = infer(expr, ctx)?;
        constraints.extend(expr_cs);
        types.push(expr_type);
    }

    Ok((types, constraints))
}

// TODO: make this recursive
fn simplify_intersection(in_types: &[types::Type], ctx: &Context) -> Type {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|ty| match &ty.variant {
            Variant::Object(props) => Some(props),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, HashSet<Type>> = defaulthashmap!();
    for props in obj_types {
        for prop in props {
            props_map[prop.name.clone()].insert(prop.ty.clone());
        }
    }

    let mut props: Vec<types::TProp> = props_map
        .iter()
        .map(|(name, types)| {
            let types: Vec<_> = types.iter().cloned().collect();
            let ty: Type = if types.len() == 1 {
                types[0].clone()
            } else {
                ctx.intersection(types)
            };
            types::TProp {
                name: name.to_owned(),
                // TODO: determine this field from all of the TProps with
                // the same name.  This should only be optional if all of
                // the TProps with the current name are optional.
                optional: false,
                ty,
            }
        })
        .collect();
    props.sort_by_key(|prop| prop.name.clone()); // ensure a stable order

    let obj_type = ctx.object(&props);

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|ty| !matches!(ty.variant, Variant::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    out_types.push(obj_type);
    out_types.sort_by_key(|ty| ty.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0].clone()
    } else {
        ctx.intersection(out_types)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expr::expr_parser;
    use chumsky::prelude::*;

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
