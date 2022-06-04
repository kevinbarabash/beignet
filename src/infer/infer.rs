use defaultmap::*;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, freeze, Primitive, Scheme, Type, Variant};

use super::constraint_solver::{is_subtype, run_solve, Constraint};
use super::context::{Context, Env};
use super::infer_mem::infer_mem;
use super::infer_pattern::infer_pattern as infer_pattern2;
use super::substitutable::Substitutable;

// TODO: We need multiple Envs so that we can control things at differen scopes
// e.g. global, module, function, ...
pub fn infer_prog(prog: &Program) -> Result<Context, String> {
    let mut ctx: Context = Context::default();

    // TODO: figure out how report multiple errors
    for stmt in &prog.body {
        match stmt {
            Statement::VarDecl {
                declare,
                init,
                pattern,
                ..
            } => {
                match declare {
                    true => {
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                // A type annotation should always be provided when using `declare`
                                let type_ann = type_ann.as_ref().unwrap();
                                let type_ann_ty = type_ann_to_type(type_ann, &ctx);
                                let scheme = type_to_scheme(&type_ann_ty);
                                ctx.values.insert(id.name.to_owned(), scheme);
                            }
                            _ => todo!(),
                        }
                    }
                    false => {
                        // An initial value should always be used when using a normal `let` statement
                        let init = init.as_ref().unwrap();
                        match pattern {
                            Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
                                let inferred_scheme = infer_expr(&ctx, init)?;
                                let scheme = match type_ann {
                                    Some(type_ann) => {
                                        let type_ann_ty = type_ann_to_type(type_ann, &ctx);
                                        match is_subtype(&inferred_scheme.ty, &type_ann_ty, &ctx) {
                                            true => Ok(type_to_scheme(&type_ann_ty)),
                                            false => Err(String::from(
                                                "value is not a subtype of decl's declared type",
                                            )),
                                        }
                                    }
                                    None => Ok(inferred_scheme),
                                };
                                ctx.values.insert(id.name.to_owned(), scheme?);
                            }
                            _ => todo!(),
                        }
                    }
                };
            }
            Statement::TypeDecl { id, type_ann, .. } => {
                let type_ann_ty = type_ann_to_type(type_ann, &ctx);
                let scheme = type_to_scheme(&type_ann_ty);
                ctx.types.insert(id.name.to_owned(), scheme);
            }
            Statement::Expr { expr, .. } => {
                // We ignore the type that was inferred, we only care that
                // it succeeds since we aren't assigning it to variable.
                infer_expr(&ctx, expr)?;
            }
        };
    }

    Ok(ctx)
}

pub fn infer_stmt(ctx: &Context, stmt: &Statement) -> Result<Scheme, String> {
    match stmt {
        Statement::Expr { expr, .. } => infer_expr(ctx, expr),
        _ => Err(String::from("We can't infer decls yet")),
    }
}

pub fn infer_expr(ctx: &Context, expr: &Expr) -> Result<Scheme, String> {
    let (ty, cs) = infer(expr, ctx)?;
    let subs = run_solve(&cs, ctx)?;

    Ok(close_over(&ty.apply(&subs), ctx))
}

fn close_over(ty: &Type, ctx: &Context) -> Scheme {
    let empty_env = Env::new();
    normalize(&generalize(&empty_env, ty), ctx)
}

fn normalize(sc: &Scheme, ctx: &Context) -> Scheme {
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
                },
            )
        })
        .collect();

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    fn norm_type(ty: &Type, mapping: &HashMap<i32, Type>, ctx: &Context) -> Type {
        // let id = ty.id;
        // let frozen = ty.frozen;
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
            Variant::Union(union) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = union
                    .types
                    .iter()
                    .map(|ty| norm_type(ty, mapping, ctx))
                    .collect();
                Type {
                    variant: Variant::Union(types::UnionType { types }),
                    ..ty.to_owned()
                }
            }
            Variant::Intersection(intersection) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = intersection
                    .types
                    .iter()
                    .map(|ty| norm_type(ty, mapping, ctx))
                    .collect();
                simplify_intersection(
                    types::IntersectionType {
                        types,
                    },
                    ctx,
                )
            }
            Variant::Object(object) => {
                let props = object
                    .props
                    .iter()
                    .map(|prop| types::TProp {
                        name: prop.name.clone(),
                        optional: prop.optional,
                        ty: norm_type(&prop.ty, mapping, ctx),
                    })
                    .collect();
                Type {
                    variant: Variant::Object(types::ObjectType {
                        props,
                        widen_flag: object.widen_flag.to_owned(),
                    }),
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
            Variant::Tuple(types::TupleType { types }) => {
                let types = types.iter().map(|ty| norm_type(ty, mapping, ctx)).collect();
                Type {
                    variant: Variant::Tuple(types::TupleType { types }),
                    ..ty.to_owned()
                }
            }
            Variant::Rest(rest) => Type {
                variant: Variant::Rest(types::RestType {
                    ty: Box::from(norm_type(&rest.ty, mapping, ctx)),
                }),
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

pub type InferResult = (Type, Vec<Constraint>);

fn is_promise(ty: &Type) -> bool {
    matches!(&ty.variant, Variant::Alias(types::AliasType { name, .. }) if name == "Promise")
}

fn infer(expr: &Expr, ctx: &Context) -> Result<InferResult, String> {
    match expr {
        Expr::Ident(Ident { name, .. }) => {
            let ty = ctx.lookup_value(name);
            Ok((ty, vec![]))
        }
        Expr::App(App { lam, args, .. }) => {
            let (t_fn, cs_fn) = infer(lam, ctx)?;
            let (t_args, cs_args) = infer_many(args, ctx)?;
            let tv = ctx.fresh_var();

            let mut constraints = Vec::new();
            constraints.extend(cs_fn);
            constraints.extend(cs_args);
            constraints.push(Constraint {
                types: (ctx.lam(t_args, Box::new(tv.clone())), t_fn),
            });

            Ok((tv, constraints))
        }
        Expr::Fix(Fix { expr, .. }) => {
            let (t, cs) = infer(expr, ctx)?;
            let tv = ctx.fresh_var();
            let mut constraints = Vec::new();
            constraints.extend(cs);
            constraints.push(Constraint {
                types: (ctx.lam(vec![tv.clone()], Box::new(tv.clone())), t),
            });

            Ok((tv, constraints))
        }
        Expr::IfElse(IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            let (t1, cs1) = infer(cond, ctx)?;
            let (t2, cs2) = infer(consequent, ctx)?;
            let (t3, cs3) = infer(alternate, ctx)?;
            let bool = ctx.prim(Primitive::Bool);

            let result_type = t2.clone();
            let mut constraints = Vec::new();
            constraints.extend(cs1);
            constraints.extend(cs2);
            constraints.extend(cs3);
            constraints.push(Constraint { types: (t1, bool) });
            constraints.push(Constraint { types: (t2, t3) });

            Ok((result_type, constraints))
        }
        Expr::Lambda(Lambda {
            params,
            body,
            is_async,
            ..
        }) => {
            // Creates a new type variable for each arg
            let param_tvs: Vec<_> = params
                .iter()
                .map(|param| match param {
                    Pattern::Ident(BindingIdent { type_ann, .. }) => match type_ann {
                        Some(type_ann) => type_ann_to_type(type_ann, ctx),
                        None => ctx.fresh_var(),
                    },
                    Pattern::Rest(RestPat { type_ann, .. }) => {
                        match type_ann {
                            Some(type_ann) => {
                                // TODO: check that type_ann is an array
                                type_ann_to_type(type_ann, ctx)
                            }
                            None => ctx.alias("Array", Some(vec![ctx.fresh_var()])),
                        }
                    }
                    Pattern::Object(_) => todo!(),
                    Pattern::Array(_) => todo!(),
                })
                .collect();
            let mut new_ctx = ctx.clone();
            // TODO: try to do this without iterating over `params` twice.
            for (arg, tv) in params.iter().zip(param_tvs.clone().into_iter()) {
                let scheme = Scheme {
                    // When would qualifiers not be an empty vector here?
                    qualifiers: vec![],
                    ty: tv.clone(),
                };
                match arg {
                    Pattern::Ident(BindingIdent { id, .. }) => {
                        new_ctx.values.insert(id.name.to_string(), scheme)
                    }
                    Pattern::Rest(RestPat { arg, .. }) => match arg.as_ref() {
                        Pattern::Ident(BindingIdent { id, .. }) => {
                            new_ctx.values.insert(id.name.to_string(), scheme)
                        }
                        _ => todo!(),
                    },
                    Pattern::Object(_) => todo!(),
                    Pattern::Array(_) => todo!(),
                };
            }
            new_ctx.is_async = is_async.to_owned();
            let (ret, cs) = infer(body, &new_ctx)?;
            ctx.state.count.set(new_ctx.state.count.get());

            let ret = if !is_async || is_promise(&ret) {
                ret
            } else {
                ctx.alias("Promise", Some(vec![ret]))
            };

            let lam_ty = ctx.lam(param_tvs, Box::new(ret));

            Ok((lam_ty, cs))
        }
        Expr::Let(Let {
            pattern,
            value,
            body,
            ..
        }) => {
            let mut cs: Vec<Constraint> = Vec::new();
            let (t1, cs1) = infer(value, ctx)?;
            let subs = run_solve(&cs1, ctx)?;
            cs.extend(cs1);

            let t2 = match pattern {
                Some(pattern) => {
                    // let mut new_ctx = ctx.clone();
                    // let pat_ty = infer_pattern2(pattern, &mut new_ctx, &mut cs);
                    // println!("pat_ty = {:?}", pat_ty);
                    // println!("t1 = {:?}", t1);
                    // cs.push(Constraint {types: (pat_ty, t1)});
                    let (new_ctx, new_cs) = infer_pattern(pattern, &t1, ctx)?;
                    cs.extend(new_cs);
                    println!("cs = {:?}", cs);
                    let (t2, cs2) = infer(body, &new_ctx)?;
                    ctx.state.count.set(new_ctx.state.count.get());
                    cs.extend(cs2.apply(&subs));
                    t2
                }
                // handles: let _ = ...
                None => {
                    let (t2, cs2) = infer(body, ctx)?;
                    cs.extend(cs2.apply(&subs));
                    t2
                }
            };

            Ok((t2.apply(&subs), cs))
        }
        Expr::Lit(literal) => Ok((ctx.lit(literal.to_owned()), vec![])),
        // TODO: consider introduce functions for each operator and rewrite Ops as Apps
        Expr::Op(Op {
            left, right, op, ..
        }) => {
            let left = Box::as_ref(left);
            let right = Box::as_ref(right);
            let (ts, cs) = infer_many(&[left.clone(), right.clone()], ctx)?;
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

            Ok((tv, cs))
        }
        Expr::Obj(Obj { props, .. }) => {
            let mut all_cs: Vec<Constraint> = Vec::new();
            let props: Result<Vec<types::TProp>, String> = props
                .iter()
                .map(|p| {
                    let (ty, cs) = infer(&p.value, ctx)?;
                    all_cs.extend(cs);
                    // The property is not optional in the type we infer from
                    // an object literal, because the property has a value.
                    Ok(ctx.prop(&p.name, ty, false))
                })
                .collect();

            let obj_ty = ctx.object(&props?, None);

            Ok((obj_ty, all_cs))
        }
        Expr::Await(Await { expr, .. }) => {
            if !ctx.is_async {
                return Err(String::from("Can't use `await` inside non-async lambda"));
            }

            let (promise_ty, promise_cs) = infer(expr, ctx)?;
            let tv = ctx.fresh_var();

            let c = Constraint {
                types: (promise_ty, ctx.alias("Promise", Some(vec![tv.clone()]))),
            };

            let mut cs: Vec<Constraint> = Vec::new();
            cs.extend(promise_cs);
            cs.push(c);

            Ok((tv, cs))
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
            let mut all_cs: Vec<Constraint> = Vec::new();
            for elem in elems {
                let (ty, cs) = infer(elem, ctx)?;
                types.push(ty);
                all_cs.extend(cs);
            }

            Ok((ctx.tuple(types), all_cs))
        }
        Expr::Member(member) => infer_mem(infer, member, ctx),
    }
}

fn type_to_scheme(ty: &Type) -> Scheme {
    Scheme {
        qualifiers: vec![],
        ty: ty.clone(),
    }
}

// TODO: make this recursive, only the top-level needs `ty`
fn infer_pattern(
    pattern: &Pattern,
    ty: &Type,
    ctx: &Context,
) -> Result<(Context, Vec<Constraint>), String> {
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
                        .values
                        .insert(id.name.to_owned(), type_to_scheme(&type_ann_ty));
                    // This constraint is so that the type of the `value` in the `let-in`
                    // node conforms to the type annotation that was provided by the developer.
                    vec![Constraint {
                        types: (ty.to_owned(), type_ann_ty),
                    }]
                }
                None => {
                    new_ctx
                        .values
                        .insert(id.name.to_owned(), type_to_scheme(ty));
                    vec![]
                }
            };

            Ok((new_ctx, cs))
        }
        Pattern::Rest(_) => {
            todo!()
        }
        Pattern::Object(_) => todo!(),
        Pattern::Array(_) => todo!(),
    }
}

fn infer_many(exprs: &[Expr], ctx: &Context) -> Result<(Vec<Type>, Vec<Constraint>), String> {
    let mut ts: Vec<Type> = Vec::new();
    let mut all_cs: Vec<Constraint> = Vec::new();

    for elem in exprs {
        let (ty, cs) = infer(elem, ctx)?;
        ts.push(ty);
        all_cs.extend(cs);
    }

    Ok((ts, all_cs))
}

pub fn type_ann_to_type(type_ann: &TypeAnn, ctx: &Context) -> Type {
    freeze(_type_ann_to_type(type_ann, ctx))
}

fn _type_ann_to_type(type_ann: &TypeAnn, ctx: &Context) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params, ret, .. }) => {
            let params: Vec<_> = params
                .iter()
                .map(|arg| _type_ann_to_type(arg, ctx))
                .collect();
            let ret = Box::from(_type_ann_to_type(ret.as_ref(), ctx));
            ctx.lam(params, ret)
        }
        TypeAnn::Lit(LitType { lit, .. }) => ctx.lit(lit.to_owned()),
        TypeAnn::Prim(PrimType { prim, .. }) => ctx.prim(prim.to_owned()),
        TypeAnn::Object(ObjectType { props, .. }) => {
            let props: Vec<_> = props
                .iter()
                .map(|prop| types::TProp {
                    name: prop.name.to_owned(),
                    optional: prop.optional,
                    ty: _type_ann_to_type(prop.type_ann.as_ref(), ctx),
                })
                .collect();
            ctx.object(&props, None)
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
        TypeAnn::Union(UnionType { types, .. }) => {
            ctx.union(types.iter().map(|ty| _type_ann_to_type(ty, ctx)).collect())
        }
        TypeAnn::Intersection(IntersectionType { types, .. }) => {
            ctx.intersection(types.iter().map(|ty| _type_ann_to_type(ty, ctx)).collect())
        }
        TypeAnn::Tuple(TupleType { types, .. }) => {
            ctx.tuple(types.iter().map(|ty| _type_ann_to_type(ty, ctx)).collect())
        }
    }
}

// TODO: make this recursive
fn simplify_intersection(intersection: types::IntersectionType, ctx: &Context) -> Type {
    let obj_types: Vec<_> = intersection
        .types
        .iter()
        .filter_map(|ty| match &ty.variant {
            Variant::Object(object) => Some(object),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, HashSet<Type>> = defaulthashmap!();
    for obj_type in obj_types {
        for prop in &obj_type.props {
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

    let obj_type = ctx.object(&props, None);

    let mut not_obj_types: Vec<_> = intersection
        .types
        .iter()
        .filter(|ty| !matches!(ty.variant, Variant::Object(_)))
        .cloned()
        .collect();

    let mut types = vec![];
    types.append(&mut not_obj_types);
    types.push(obj_type);
    types.sort_by_key(|ty| ty.id); // ensure a stable order

    if types.len() == 1 {
        types[0].clone()
    } else {
        ctx.intersection(types)
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
