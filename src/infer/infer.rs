use std::collections::HashMap;
use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, freeze, Primitive, Scheme, Type};

use super::constraint_solver::{is_subtype, run_solve, Constraint};
use super::context::{Context, Env};
use super::substitutable::Substitutable;

// TODO: We need multiple Envs so that we can control things at differen scopes
// e.g. global, module, function, ...
pub fn infer_prog(env: Env, prog: &Program) -> Env {
    let mut ctx: Context = Context::from(env);

    for stmt in &prog.body {
        match stmt {
            Statement::Decl { declare, init, pattern, .. } => {
                match declare {
                    true => {
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                // A type annotation should always be provided when using `declare`
                                let type_ann = type_ann.as_ref().unwrap();
                                let type_ann_ty = type_ann_to_type(type_ann, &ctx);
                                let scheme = type_to_scheme(&type_ann_ty);
                                ctx.env.insert(id.name.to_owned(), scheme);
                            },
                            _ => todo!(),
                        }
                    },
                    false => {
                        // An initial value should always be used when using a normal `let` statement
                        let init = init.as_ref().unwrap();
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                let inferred_scheme = infer_expr(&ctx, init);
                                let scheme = match type_ann {
                                    Some(type_ann) => {
                                        let type_ann_ty = type_ann_to_type(type_ann, &ctx);
                                        match is_subtype(&inferred_scheme.ty, &type_ann_ty) {
                                            true => type_to_scheme(&type_ann_ty),
                                            false => panic!("value is not a subtype of decl's declared type"),
                                        }
                                    }
                                    None => inferred_scheme,
                                };
                                ctx.env.insert(id.name.to_owned(), scheme);
                            }
                            _ => todo!(),
                        }
                    },
                };
            },
            Statement::Expr { expr, .. } => {
                // We ignore the type that was inferred, we only care that
                // it succeeds since we aren't assigning it to variable.
                infer_expr(&ctx, expr);
            }
        };
    }

    ctx.env
}

pub fn infer_stmt(ctx: &Context, stmt: &Statement) -> Scheme {
    match stmt {
        Statement::Expr { expr, .. } => infer_expr(ctx, expr),
        _ => panic!("We can't infer decls yet"),
    }
}

pub fn infer_expr(ctx: &Context, expr: &Expr) -> Scheme {
    let (ty, cs) = infer(expr, ctx);
    let subs = run_solve(&cs, ctx);

    close_over(&ty.apply(&subs))
}

fn close_over(ty: &Type) -> Scheme {
    let empty_env = Env::new();
    normalize(&generalize(&empty_env, ty))
}

fn normalize(sc: &Scheme) -> Scheme {
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
                Type::Var(types::VarType {
                    id: index as i32,
                    frozen: false,
                }),
            )
        })
        .collect();

    fn norm_type(ty: &Type, mapping: &HashMap<i32, Type>) -> Type {
        // let id = ty.id;
        // let frozen = ty.frozen;
        match ty {
            Type::Var(types::VarType { id, .. }) => mapping.get(id).unwrap().to_owned(),
            Type::Lam(types::LamType {
                id,
                frozen,
                args,
                ret,
            }) => {
                let args: Vec<_> = args.iter().map(|arg| norm_type(arg, mapping)).collect();
                let ret = Box::from(norm_type(ret, mapping));
                Type::Lam(types::LamType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    args,
                    ret,
                })
            }
            Type::Prim(_) => ty.to_owned(),
            Type::Lit(_) => ty.to_owned(),
            Type::Union(types::UnionType { id, frozen, types }) => {
                let types = types.iter().map(|ty| norm_type(ty, mapping)).collect();
                Type::Union(types::UnionType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    types,
                })
            }
            Type::Object(types::ObjectType { id, frozen, props }) => {
                let props = props
                    .iter()
                    .map(|prop| types::TProp {
                        name: prop.name.clone(),
                        ty: norm_type(&prop.ty, mapping),
                    })
                    .collect();
                Type::Object(types::ObjectType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    props,
                })
            }
            Type::Alias(types::AliasType {
                id,
                frozen,
                name,
                type_params,
            }) => {
                let type_params = type_params
                    .clone()
                    .map(|params| params.iter().map(|ty| norm_type(ty, mapping)).collect());
                Type::Alias(types::AliasType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    name: name.to_owned(),
                    type_params,
                })
            }
        }
    }

    Scheme {
        qualifiers: (0..keys.len()).map(|x| x as i32).collect(),
        ty: norm_type(body, &mapping),
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

type InferResult = (Type, Vec<Constraint>);

fn is_promise(ty: &Type) -> bool {
    matches!(ty, Type::Alias(types::AliasType { name, .. }) if name == "Promise")
}

fn infer(expr: &Expr, ctx: &Context) -> InferResult {
    match expr {
        Expr::Ident(Ident { name, .. }) => {
            let ty = ctx.lookup_env(name);
            (ty, vec![])
        }
        Expr::App(App { lam, args, .. }) => {
            let (t_fn, cs_fn) = infer(lam, ctx);
            let (t_args, cs_args) = infer_many(args, ctx);
            let tv = ctx.fresh_var();

            let mut constraints = Vec::new();
            constraints.extend(cs_fn);
            constraints.extend(cs_args);
            constraints.push(Constraint {
                types: (ctx.lam(t_args, Box::new(tv.clone())), t_fn),
            });

            (tv, constraints)
        }
        Expr::Fix(Fix { expr, .. }) => {
            let (t, cs) = infer(expr, ctx);
            let tv = ctx.fresh_var();
            let mut constraints = Vec::new();
            constraints.extend(cs);
            constraints.push(Constraint {
                types: (ctx.lam(vec![tv.clone()], Box::new(tv.clone())), t),
            });

            (tv, constraints)
        }
        Expr::IfElse(IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            let (t1, cs1) = infer(cond, ctx);
            let (t2, cs2) = infer(consequent, ctx);
            let (t3, cs3) = infer(alternate, ctx);
            let bool = ctx.prim(Primitive::Bool);

            let result_type = t2.clone();
            let mut constraints = Vec::new();
            constraints.extend(cs1);
            constraints.extend(cs2);
            constraints.extend(cs3);
            constraints.push(Constraint { types: (t1, bool) });
            constraints.push(Constraint { types: (t2, t3) });

            (result_type, constraints)
        }
        Expr::Lambda(Lambda {
            args,
            body,
            is_async,
            ..
        }) => {
            // Creates a new type variable for each arg
            let arg_tvs: Vec<_> = args
                .iter()
                .map(|arg| match arg {
                    Pattern::Ident(BindingIdent { type_ann, .. }) => match type_ann {
                        Some(type_ann) => type_ann_to_type(type_ann, ctx),
                        None => ctx.fresh_var(),
                    },
                    Pattern::Rest(_) => todo!(),
                })
                .collect();
            let mut new_ctx = ctx.clone();
            for (arg, tv) in args.iter().zip(arg_tvs.clone().into_iter()) {
                let scheme = Scheme {
                    qualifiers: vec![],
                    ty: tv.clone(),
                };
                match arg {
                    Pattern::Ident(BindingIdent { id, .. }) => {
                        new_ctx.env.insert(id.name.to_string(), scheme)
                    }
                    Pattern::Rest(_) => todo!(),
                };
            }
            new_ctx.is_async = is_async.to_owned();
            let (ret, cs) = infer(body, &new_ctx);
            ctx.state.count.set(new_ctx.state.count.get());

            let ret = if !is_async || is_promise(&ret) {
                ret
            } else {
                ctx.alias("Promise", Some(vec![ret]))
            };

            let lam_ty = ctx.lam(arg_tvs, Box::new(ret));

            (lam_ty, cs)
        }
        Expr::Let(Let {
            pattern,
            value,
            body,
            ..
        }) => {
            let (t1, cs1) = infer(value, ctx);
            let subs = run_solve(&cs1, ctx);
            let (new_ctx, new_cs) = infer_pattern(pattern, &t1, ctx);
            let (t2, cs2) = infer(body, &new_ctx);
            ctx.state.count.set(new_ctx.state.count.get());
            let mut cs: Vec<Constraint> = Vec::new();
            cs.extend(cs1);
            cs.extend(new_cs);
            cs.extend(cs2.apply(&subs));

            (t2.apply(&subs), cs)
        }
        Expr::Lit(literal) => (ctx.lit(literal.to_owned()), vec![]),
        // TODO: consider introduce functions for each operator and rewrite Ops as Apps
        Expr::Op(Op {
            left, right, op, ..
        }) => {
            let left = Box::as_ref(left);
            let right = Box::as_ref(right);
            let (ts, cs) = infer_many(&[left.clone(), right.clone()], ctx);
            let tv = ctx.fresh_var();

            let mut cs = cs;

            let c = match op {
                BinOp::EqEq | BinOp::NotEq => {
                    let arg_tv = ctx.fresh_var();
                    Constraint {
                        types: (
                            ctx.lam(ts, Box::from(tv.clone())),
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
                        ctx.lam(ts, Box::from(tv.clone())),
                        ctx.lam(
                            vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Num)],
                            Box::from(ctx.prim(Primitive::Bool)),
                        ),
                    ),
                },
                BinOp::Add | BinOp::Sub | BinOp::Div | BinOp::Mul => Constraint {
                    types: (
                        ctx.lam(ts, Box::from(tv.clone())),
                        ctx.lam(
                            vec![ctx.prim(Primitive::Num), ctx.prim(Primitive::Num)],
                            Box::from(ctx.prim(Primitive::Num)),
                        ),
                    ),
                },
            };
            cs.push(c);

            (tv, cs)
        }
        Expr::Obj(Obj { properties, .. }) => {
            let mut all_cs: Vec<Constraint> = Vec::new();
            let properties: Vec<_> = properties
                .iter()
                .map(|p| {
                    let (ty, cs) = infer(&p.value, ctx);
                    all_cs.extend(cs);
                    ctx.prop(&p.name, ty)
                })
                .collect();

            let obj_ty = ctx.object(&properties);

            (obj_ty, all_cs)
        }
        Expr::Await(Await { expr, .. }) => {
            if !ctx.is_async {
                panic!("Can't use `await` inside non-async lambda")
            }

            let (promise_ty, promise_cs) = infer(expr, ctx);
            let tv = ctx.fresh_var();

            let c = Constraint {
                types: (promise_ty, ctx.alias("Promise", Some(vec![tv.clone()]))),
            };

            let mut cs: Vec<Constraint> = Vec::new();
            cs.extend(promise_cs);
            cs.push(c);

            (tv, cs)
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

            (ty, vec![])
        }
    }
}

fn type_to_scheme(ty: &Type) -> Scheme {
    Scheme {
        qualifiers: vec![],
        ty: ty.clone(),
    }
}

fn infer_pattern(pattern: &Pattern, ty: &Type, ctx: &Context) -> (Context, Vec<Constraint>) {
    match pattern {
        Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
            let mut new_ctx = ctx.clone();

            let cs = match type_ann {
                // If the pattern has a type annotation then we use that when inserting a
                // type definition for the identifier into new_ctx, othewise we use the
                // type that has been passed to us (which should be the type inferred from
                // the `value` in the `let-in` node).
                Some(type_ann) => {
                    let type_ann_ty = type_ann_to_type(type_ann, ctx);
                    new_ctx
                        .env
                        .insert(id.name.to_owned(), type_to_scheme(&type_ann_ty));
                    // This constraint is so that the type of the `value` in the `let-in`
                    // node conforms to the type annotation that was provided by the developer.
                    vec![Constraint {
                        types: (ty.to_owned(), type_ann_ty),
                    }]
                }
                None => {
                    new_ctx.env.insert(id.name.to_owned(), type_to_scheme(ty));
                    vec![]
                }
            };

            (new_ctx, cs)
        }
        Pattern::Rest(_) => todo!(),
    }
}

fn infer_many(exprs: &[Expr], ctx: &Context) -> (Vec<Type>, Vec<Constraint>) {
    let mut ts: Vec<Type> = Vec::new();
    let mut all_cs: Vec<Constraint> = Vec::new();

    for elem in exprs {
        let (ty, cs) = infer(elem, ctx);
        ts.push(ty);
        all_cs.extend(cs);
    }

    (ts, all_cs)
}

fn type_ann_to_type(type_ann: &TypeAnn, ctx: &Context) -> Type {
    freeze(_type_ann_to_type(type_ann, ctx))
}

fn _type_ann_to_type(type_ann: &TypeAnn, ctx: &Context) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params: args, ret, .. }) => {
            let args: Vec<_> = args.iter().map(|arg| _type_ann_to_type(arg, ctx)).collect();
            let ret = Box::from(_type_ann_to_type(ret.as_ref(), ctx));
            ctx.lam(args, ret)
        }
        TypeAnn::Lit(LitType { lit, .. }) => ctx.lit(lit.to_owned()),
        TypeAnn::Prim(PrimType { prim, .. }) => ctx.prim(prim.to_owned()),
        TypeAnn::Object(ObjectType { props, .. }) => {
            let props: Vec<_> = props
                .iter()
                .map(|prop| types::TProp {
                    name: prop.name.to_owned(),
                    ty: _type_ann_to_type(prop.type_ann.as_ref(), ctx),
                })
                .collect();
            ctx.object(&props)
        }
        TypeAnn::TypeRef(TypeRef {
            name, type_params, ..
        }) => {
            let type_params = type_params.clone().map(|params| {
                params
                    .iter()
                    .map(|param| _type_ann_to_type(param, ctx))
                    .collect()
            });
            ctx.alias(name, type_params)
        }
        TypeAnn::Union(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expr::expr_parser;
    use chumsky::prelude::*;

    use super::*;

    fn parse_and_infer_expr(input: &str) -> String {
        let env: Env = HashMap::new();
        let ctx: Context = Context::from(env);
        let expr = expr_parser().then_ignore(end()).parse(input).unwrap();
        format!("{}", infer_expr(&ctx, &expr))
    }

    #[test]
    fn infer_let_with_type_ann() {
        assert_eq!(parse_and_infer_expr("let x: number = 5 in x"), "number");
    }

    #[test]
    #[should_panic = "unification failed"]
    fn infer_let_with_incorrect_type_ann() {
        parse_and_infer_expr("let x: string = 5 in x");
    }
}