use crochet_ast::common::*;
use crochet_ast::types::{self as types, TFnParam, TKeyword, TObject, TPat, TVar, Type, TypeKind};
use crochet_ast::values::*;

use types::TObjElem;

use crate::assump::Assump;
use crate::context::Context;
use crate::infer_fn_param::infer_fn_param;
use crate::infer_pattern::*;
use crate::infer_type_ann::*;
use crate::substitutable::{Subst, Substitutable};
use crate::unify::unify;
use crate::util::*;

pub fn infer_expr(ctx: &mut Context, expr: &mut Expr) -> Result<(Subst, Type), String> {
    let result = match &mut expr.kind {
        ExprKind::App(App { lam, args, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut arg_types: Vec<Type> = vec![];

            let (s1, lam_type) = infer_expr(ctx, lam)?;
            ss.push(s1);

            for arg in args {
                let (arg_s, arg_t) = infer_expr(ctx, &mut arg.expr)?;
                ss.push(arg_s);
                if arg.spread.is_some() {
                    match &arg_t.kind {
                        TypeKind::Tuple(types) => arg_types.extend(types.to_owned()),
                        _ => arg_types.push(Type {
                            kind: TypeKind::Rest(Box::from(arg_t)),
                            provenance: None,
                        }),
                    }
                } else {
                    arg_types.push(arg_t);
                }
            }

            let ret_type = ctx.fresh_var();
            // Are we missing an `apply()` call here?
            // Maybe, I could see us needing an apply to handle generic functions properly
            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            let call_type = Type {
                kind: TypeKind::App(types::TApp {
                    args: arg_types,
                    ret: Box::from(ret_type.clone()),
                }),
                provenance: None,
            };
            let s3 = unify(&call_type, &lam_type, ctx)?;

            ss.push(s3);

            let s = compose_many_subs(&ss);
            let t = ret_type.apply(&s);
            expr.inferred_type = Some(t.clone());
            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
            Ok((s, t))
        }
        ExprKind::Fix(Fix { expr, .. }) => {
            let (s1, t) = infer_expr(ctx, expr)?;
            let tv = ctx.fresh_var();
            let param = TFnParam {
                pat: TPat::Ident(BindingIdent {
                    name: String::from("fix_param"),
                }),
                t: tv.clone(),
                optional: false,
            };
            let s2 = unify(
                &Type {
                    kind: TypeKind::Lam(types::TLam {
                        params: vec![param],
                        ret: Box::from(tv),
                    }),
                    provenance: None,
                },
                &t,
                ctx,
            )?;

            // This leaves the function param names intact and returns a TLam
            // instead of a TApp.
            let t = match t.kind {
                TypeKind::Lam(types::TLam { ret, .. }) => Ok(ret.as_ref().to_owned()),
                _ => Err(String::from("Expr::Fix should always infer a lambda")),
            }?;
            expr.inferred_type = Some(t.clone());
            Ok((compose_subs(&s2, &s1), t))
        }
        ExprKind::Ident(Ident { name, .. }) => {
            let s = Subst::default();
            let t = ctx.lookup_value_and_instantiate(name)?;
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => match alternate {
            Some(alternate) => {
                match &mut cond.kind {
                    ExprKind::LetExpr(LetExpr { pat, expr, .. }) => {
                        // TODO: warn if the pattern isn't refutable
                        let (s1, t1) =
                            infer_let(pat, &mut None, expr, consequent, ctx, &PatternUsage::Match)?;
                        let (s2, t2) = infer_expr(ctx, alternate)?;

                        let s = compose_many_subs(&[s1, s2]);
                        let t = union_types(&t1, &t2);
                        Ok((s, t))
                    }
                    _ => {
                        let (s1, t1) = infer_expr(ctx, cond)?;
                        let (s2, t2) = infer_expr(ctx, consequent)?;
                        let (s3, t3) = infer_expr(ctx, alternate)?;
                        let s4 = unify(
                            &t1,
                            &Type {
                                kind: TypeKind::Keyword(TKeyword::Boolean),
                                provenance: None,
                            },
                            ctx,
                        )?;

                        let s = compose_many_subs(&[s1, s2, s3, s4]);
                        let t = union_types(&t2, &t3);
                        expr.inferred_type = Some(t.clone());
                        Ok((s, t))
                    }
                }
            }
            None => match &mut cond.kind {
                ExprKind::LetExpr(LetExpr { pat, expr, .. }) => {
                    let (s1, t1) =
                        infer_let(pat, &mut None, expr, consequent, ctx, &PatternUsage::Match)?;
                    let s2 = match unify(
                        &t1,
                        &Type {
                            kind: TypeKind::Keyword(TKeyword::Undefined),
                            provenance: None,
                        },
                        ctx,
                    ) {
                        Ok(s) => Ok(s),
                        Err(_) => Err(String::from(
                            "Consequent for 'if' without 'else' must not return a value",
                        )),
                    }?;

                    let s = compose_subs(&s2, &s1);
                    let t = t1;
                    Ok((s, t))
                }
                _ => {
                    let (s1, t1) = infer_expr(ctx, cond)?;
                    let (s2, t2) = infer_expr(ctx, consequent)?;
                    let s3 = unify(
                        &t1,
                        &Type {
                            kind: TypeKind::Keyword(TKeyword::Boolean),
                            provenance: None,
                        },
                        ctx,
                    )?;
                    let s4 = match unify(
                        &t2,
                        &Type {
                            kind: TypeKind::Keyword(TKeyword::Undefined),
                            provenance: None,
                        },
                        ctx,
                    ) {
                        Ok(s) => Ok(s),
                        Err(_) => Err(String::from(
                            "Consequent for 'if' without 'else' must not return a value",
                        )),
                    }?;

                    let s = compose_many_subs(&[s1, s2, s3, s4]);
                    let t = t2;
                    expr.inferred_type = Some(t.clone());
                    Ok((s, t))
                }
            },
        },
        ExprKind::JSXElement(JSXElement {
            name,
            attrs,
            children: _,
            ..
        }) => {
            let first_char = name.chars().next().unwrap();
            // JSXElement's starting with an uppercase char are user defined.
            if first_char.is_uppercase() {
                let t = ctx.lookup_value_and_instantiate(name)?;
                match &t.kind {
                    TypeKind::Lam(_) => {
                        let mut ss: Vec<_> = vec![];
                        let mut elems: Vec<_> = vec![];
                        for attr in attrs {
                            let (s, t) = match &mut attr.value {
                                JSXAttrValue::Lit(lit) => {
                                    let kind = ExprKind::Lit(lit.to_owned());
                                    let mut expr = Expr {
                                        span: lit.span(),
                                        kind,
                                        inferred_type: None,
                                    };
                                    infer_expr(ctx, &mut expr)?
                                }
                                JSXAttrValue::JSXExprContainer(JSXExprContainer {
                                    expr, ..
                                }) => infer_expr(ctx, expr)?,
                            };
                            ss.push(s);

                            let prop = types::TProp {
                                name: attr.ident.name.to_owned(),
                                optional: false,
                                mutable: false,
                                t,
                            };
                            elems.push(types::TObjElem::Prop(prop));
                        }

                        let ret_type = Type {
                            kind: TypeKind::Ref(types::TRef {
                                name: String::from("JSXElement"),
                                type_args: None,
                            }),
                            provenance: None,
                        };

                        let call_type = Type {
                            kind: TypeKind::App(types::TApp {
                                args: vec![Type {
                                    kind: TypeKind::Object(TObject { elems }),
                                    provenance: None,
                                }],
                                ret: Box::from(ret_type.clone()),
                            }),
                            provenance: None,
                        };

                        let s1 = compose_many_subs(&ss);
                        let s2 = unify(&call_type, &t, ctx)?;

                        let s = compose_subs(&s2, &s1);
                        expr.inferred_type = Some(ret_type.clone());
                        return Ok((s, ret_type));
                    }
                    _ => return Err(String::from("Component must be a function")),
                }
            }

            let s = Subst::default();
            // TODO: check props on JSXInstrinsics
            let t = Type {
                kind: TypeKind::Ref(types::TRef {
                    name: String::from("JSXElement"),
                    type_args: None,
                }),
                provenance: None,
            };
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::Lambda(Lambda {
            params,
            body,
            is_async,
            return_type: rt_type_ann,
            type_params,
            ..
        }) => {
            ctx.push_scope(is_async.to_owned());

            let type_params_map: Assump = match type_params {
                Some(params) => params
                    .iter_mut()
                    .map(|param| {
                        let tv = match &mut param.constraint {
                            Some(type_ann) => {
                                // TODO: push `s` on to `ss`
                                let (_s, t) = infer_type_ann(type_ann, ctx, &mut None)?;
                                Type {
                                    kind: TypeKind::Var(TVar {
                                        id: ctx.fresh_id(),
                                        constraint: Some(Box::from(t)),
                                    }),
                                    provenance: None,
                                }
                            }
                            None => ctx.fresh_var(),
                        };
                        ctx.insert_type(param.name.name.clone(), tv.clone());
                        Ok((param.name.name.to_owned(), tv))
                    })
                    .collect::<Result<Assump, String>>()?,
                None => Assump::default(),
            };

            let params: Result<Vec<(Subst, TFnParam)>, String> = params
                .iter_mut()
                .map(|e_param| {
                    let (ps, pa, t_param) = infer_fn_param(e_param, ctx, &type_params_map)?;

                    // Inserts any new variables introduced by infer_fn_param() into
                    // the current context.
                    for (name, t) in pa {
                        ctx.insert_value(name, t);
                    }

                    Ok((ps, t_param))
                })
                .collect();

            let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

            let (rs_1, rt_1) = infer_expr(ctx, body)?;
            ss.push(rs_1);

            ctx.pop_scope();

            let rt_1 = if *is_async && !is_promise(&rt_1) {
                Type {
                    kind: TypeKind::Ref(types::TRef {
                        name: String::from("Promise"),
                        type_args: Some(vec![rt_1]),
                    }),
                    provenance: None,
                }
            } else {
                rt_1
            };

            let s = match rt_type_ann {
                Some(rt_type_ann) => {
                    let (rs_2, rt_2) =
                        infer_type_ann_with_params(rt_type_ann, ctx, &type_params_map)?;
                    ss.push(rs_2);

                    unify(&rt_1, &rt_2, ctx)?
                }
                None => Subst::default(),
            };
            ss.push(s);
            let t = Type {
                kind: TypeKind::Lam(types::TLam {
                    params: t_params,
                    ret: Box::from(rt_1),
                }),
                provenance: None,
            };

            let s = compose_many_subs(&ss);
            let t = t.apply(&s);
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::Let(Let {
            pattern,
            type_ann,
            init,
            body,
            ..
        }) => match pattern {
            Some(pat) => infer_let(pat, type_ann, init, body, ctx, &PatternUsage::Assign),
            None => {
                let (init_s, init_t) = infer_expr(ctx, init)?;

                if init_t.kind != TypeKind::Keyword(TKeyword::Undefined) {
                    println!("WARNING: {init_t} was not assigned");
                }

                let (body_s, body_t) = infer_expr(ctx, body)?;

                let t = body_t.apply(&init_s);
                let s = compose_subs(&body_s, &init_s);

                Ok((s, t))
            }
        },
        ExprKind::LetExpr(_) => {
            panic!("Unexpected LetExpr.  All LetExprs should be handled by IfElse arm.")
        }
        ExprKind::Lit(lit) => {
            let s = Subst::new();
            let t = Type::from(lit.to_owned());
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::BinaryExpr(BinaryExpr {
            op, left, right, ..
        }) => {
            // TODO: check what `op` is and handle comparison operators
            // differently from arithmetic operators
            // TODO: if both are literals, compute the result at compile
            // time and set the result to be appropriate number literal.
            let (s1, t1) = infer_expr(ctx, left)?;
            let (s2, t2) = infer_expr(ctx, right)?;
            let s3 = unify(
                &t1,
                &Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                ctx,
            )?;
            let s4 = unify(
                &t2,
                &Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                ctx,
            )?;
            let t = match op {
                BinOp::Add => Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                BinOp::Sub => Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                BinOp::Mul => Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                BinOp::Div => Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                BinOp::EqEq => Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                },
                BinOp::NotEq => Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                },
                BinOp::Gt => Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                },
                BinOp::GtEq => Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                },
                BinOp::Lt => Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                },
                BinOp::LtEq => Type {
                    kind: TypeKind::Keyword(TKeyword::Boolean),
                    provenance: None,
                },
            };
            expr.inferred_type = Some(t.clone());
            Ok((compose_many_subs(&[s1, s2, s3, s4]), t))
        }
        ExprKind::UnaryExpr(UnaryExpr { op, arg, .. }) => {
            let (s1, t1) = infer_expr(ctx, arg)?;
            let s2 = unify(
                &t1,
                &Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
                ctx,
            )?;
            let t = match op {
                UnaryOp::Minus => Type {
                    kind: TypeKind::Keyword(TKeyword::Number),
                    provenance: None,
                },
            };
            expr.inferred_type = Some(t.clone());
            Ok((compose_many_subs(&[s1, s2]), t))
        }
        ExprKind::Obj(Obj { props, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut elems: Vec<types::TObjElem> = vec![];
            let mut spread_types: Vec<_> = vec![];
            for p in props {
                match p {
                    PropOrSpread::Prop(p) => {
                        match p.as_mut() {
                            Prop::Shorthand(Ident { name, .. }) => {
                                let t = ctx.lookup_value_and_instantiate(name)?;
                                elems.push(types::TObjElem::Prop(types::TProp {
                                    name: name.to_owned(),
                                    optional: false,
                                    mutable: false,
                                    t,
                                }));
                            }
                            Prop::KeyValue(KeyValueProp { name, value, .. }) => {
                                let (s, t) = infer_expr(ctx, value)?;
                                ss.push(s);
                                // TODO: check if the inferred type is T | undefined and use that
                                // determine the value of optional
                                elems.push(types::TObjElem::Prop(types::TProp {
                                    name: name.to_owned(),
                                    optional: false,
                                    mutable: false,
                                    t,
                                }));
                            }
                        }
                    }
                    PropOrSpread::Spread(SpreadElement { expr, .. }) => {
                        let (s, t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        spread_types.push(t);
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = if spread_types.is_empty() {
                Type {
                    kind: TypeKind::Object(TObject { elems }),
                    provenance: None,
                }
            } else {
                let mut all_types = spread_types;
                all_types.push(Type {
                    kind: TypeKind::Object(TObject { elems }),
                    provenance: None,
                });
                simplify_intersection(&all_types)
            };
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::Await(Await { expr, .. }) => {
            if !ctx.is_async() {
                return Err(String::from("Can't use `await` inside non-async lambda"));
            }

            let (s1, t1) = infer_expr(ctx, expr)?;
            let wrapped_type = ctx.fresh_var();
            let promise_type = Type {
                kind: TypeKind::Ref(types::TRef {
                    name: String::from("Promise"),
                    type_args: Some(vec![wrapped_type.clone()]),
                }),
                provenance: None,
            };

            let s2 = unify(&t1, &promise_type, ctx)?;

            let s = compose_subs(&s2, &s1);
            expr.inferred_type = Some(wrapped_type.clone());
            Ok((s, wrapped_type))
        }
        ExprKind::Tuple(Tuple { elems, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut ts: Vec<Type> = vec![];

            for elem in elems {
                let expr = elem.expr.as_mut();
                match elem.spread {
                    Some(_) => {
                        let (s, t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        match &t.kind {
                            TypeKind::Tuple(types) => {
                                ts.extend(types.to_owned());
                            }
                            _ => {
                                return Err(String::from(
                                    "Can only spread tuple types inside a tuple",
                                ))
                            }
                        }
                    }
                    None => {
                        let (s, t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        ts.push(t);
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = Type {
                kind: TypeKind::Tuple(ts),
                provenance: None,
            };
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::Member(Member { obj, prop, .. }) => {
            let (obj_s, obj_t) = infer_expr(ctx, obj)?;
            let (prop_s, prop_t) = infer_property_type(&obj_t, prop, ctx)?;

            let s = compose_subs(&prop_s, &obj_s);
            let t = prop_t;
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::Empty => {
            let t = Type {
                kind: TypeKind::Keyword(TKeyword::Undefined),
                provenance: None,
            };
            let s = Subst::default();
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::TemplateLiteral(TemplateLiteral {
            exprs, quasis: _, ..
        }) => {
            let t = Type {
                kind: TypeKind::Keyword(TKeyword::String),
                provenance: None,
            };
            let result: Result<Vec<(Subst, Type)>, String> =
                exprs.iter_mut().map(|expr| infer_expr(ctx, expr)).collect();
            // We ignore the types of expressions if there are any because any expression
            // in JavaScript has a string representation.
            let (ss, _): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();
            let s = compose_many_subs(&ss);
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::TaggedTemplateLiteral(_) => {
            // TODO: treat this like a call/application
            // NOTE: requires:
            // - arrays
            // - rest params
            todo!()
        }
        ExprKind::Match(Match { expr, arms, .. }) => {
            // TODO: warn if the pattern isn't refutable
            let mut ss: Vec<Subst> = vec![];
            let mut ts: Vec<Type> = vec![];
            for arm in arms {
                let (s, t) = infer_let(
                    &mut arm.pattern,
                    &mut None,
                    expr,
                    &mut arm.body,
                    ctx,
                    &PatternUsage::Match,
                )?;
                ss.push(s);
                ts.push(t);
            }

            let s = compose_many_subs(&ss);
            let t = union_many_types(&ts);
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
    };

    let (s, t) = result?;

    ctx.apply(&s);

    Ok((s, t))
}

fn infer_let(
    pat: &mut Pattern,
    type_ann: &mut Option<TypeAnn>,
    init: &mut Expr,
    body: &mut Expr,
    ctx: &mut Context,
    pu: &PatternUsage,
) -> Result<(Subst, Type), String> {
    ctx.push_scope(ctx.is_async());
    let (pa, s1) = infer_pattern_and_init(pat, type_ann, init, ctx, pu)?;

    // Inserts the new variables from infer_pattern_and_init() into the
    // current context.
    // TODO: have infer_pattern_and_init do this
    for (name, t) in pa {
        ctx.insert_value(name.to_owned(), t.to_owned());
    }

    let (s2, t2) = infer_expr(ctx, body)?;

    ctx.pop_scope();

    let s = compose_subs(&s2, &s1);
    let t = t2;
    Ok((s, t))
}

fn is_promise(t: &Type) -> bool {
    matches!(&t, Type {kind: TypeKind::Ref(types::TRef { name, .. }), ..} if name == "Promise")
}

// TODO: try to dedupe with key_of()
fn infer_property_type(
    obj_t: &Type,
    prop: &mut MemberProp,
    ctx: &mut Context,
) -> Result<(Subst, Type), String> {
    match &obj_t.kind {
        TypeKind::Generic(_) => {
            // TODO: Improve performance by getting the property type first and
            // then instantiating it instead of instantiating the whole object type.
            let t = ctx.instantiate(obj_t);
            infer_property_type(&t, prop, ctx)
        }
        TypeKind::Var(TVar { constraint, .. }) => match constraint {
            Some(constraint) => infer_property_type(constraint, prop, ctx),
            None => Err("Cannot read property on unconstrained type param".to_owned()),
        },
        TypeKind::Object(obj) => get_prop_value(obj, prop, ctx),
        TypeKind::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            infer_property_type(&t, prop, ctx)
        }
        TypeKind::Lit(lit) => match lit {
            types::TLit::Num(_) => {
                let t = ctx.lookup_type_and_instantiate("Number")?;
                infer_property_type(&t, prop, ctx)
            }
            types::TLit::Bool(_) => {
                let t = ctx.lookup_type_and_instantiate("Boolean")?;
                infer_property_type(&t, prop, ctx)
            }
            types::TLit::Str(_) => {
                let t = ctx.lookup_type_and_instantiate("String")?;
                infer_property_type(&t, prop, ctx)
            }
        },
        TypeKind::Keyword(keyword) => match keyword {
            TKeyword::Number => {
                let t = ctx.lookup_type_and_instantiate("Number")?;
                infer_property_type(&t, prop, ctx)
            }
            TKeyword::Boolean => {
                let t = ctx.lookup_type_and_instantiate("Boolean")?;
                infer_property_type(&t, prop, ctx)
            }
            TKeyword::String => {
                let t = ctx.lookup_type_and_instantiate("String")?;
                infer_property_type(&t, prop, ctx)
            }
            TKeyword::Symbol => {
                let t = ctx.lookup_type_and_instantiate("Symbol")?;
                infer_property_type(&t, prop, ctx)
            }
            TKeyword::Null => Err("Cannot read property on 'null'".to_owned()),
            TKeyword::Undefined => Err("Cannot read property on 'undefined'".to_owned()),
            TKeyword::Never => Err("Cannot read property on 'never'".to_owned()),
        },
        TypeKind::Array(type_param) => {
            // TODO: Do this for all interfaces that we lookup
            let t = ctx.lookup_type("ReadonlyArray")?;
            let type_params = get_type_params(&t);
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            let s: Subst =
                Subst::from([(type_params[0].id.to_owned(), type_param.as_ref().to_owned())]);
            let t = t.apply(&s);
            infer_property_type(&t, prop, ctx)
        }
        TypeKind::Tuple(elem_types) => {
            match prop {
                // TODO: lookup methods on Array.prototype
                MemberProp::Ident(_) => {
                    // TODO: Do this for all interfaces that we lookup
                    let t = ctx.lookup_type("ReadonlyArray")?;
                    println!("ReadonlyArray = {t}");
                    // TODO: Instead of instantiating the whole interface for one method, do
                    // the lookup call first and then instantiate the method.
                    // TODO: remove duplicate types
                    let type_param = Type {
                        kind: TypeKind::Union(elem_types.to_owned()),
                        provenance: None,
                    };
                    let type_params = get_type_params(&t); // ReadonlyArray type params

                    let s: Subst = Subst::from([(type_params[0].id.to_owned(), type_param)]);
                    let t = t.apply(&s);
                    infer_property_type(&t, prop, ctx)
                }
                MemberProp::Computed(ComputedPropName { expr, .. }) => {
                    let (prop_s, prop_t) = infer_expr(ctx, expr)?;

                    match &prop_t.kind {
                        TypeKind::Keyword(keyword) => match keyword {
                            TKeyword::Number => {
                                // TODO: remove duplicate types
                                let mut elem_types = elem_types.to_owned();
                                elem_types.push(Type {
                                    kind: TypeKind::Keyword(TKeyword::Undefined),
                                    provenance: None,
                                });
                                let t = Type {
                                    kind: TypeKind::Union(elem_types),
                                    provenance: None,
                                };
                                Ok((prop_s, t))
                            }
                            _ => Err(format!("{keyword} is an invalid indexer for tuple types")),
                        },
                        TypeKind::Lit(lit) => match lit {
                            types::TLit::Num(index) => {
                                let index: usize = index.parse().unwrap();
                                match elem_types.get(index) {
                                    Some(t) => Ok((prop_s, t.to_owned())),
                                    None => Err(format!("{index} is out of bounds for {obj_t}")),
                                }
                            }
                            _ => Err(format!("{lit} is an invalid indexer for tuple types")),
                        },
                        _ => Err(format!("{prop_t} is an invalid indexer for tuple types")),
                    }
                }
            }
        }
        _ => {
            todo!("Unhandled {obj_t:#?} in infer_property_type")
        }
    }
}

fn get_prop_value(
    obj: &TObject,
    prop: &mut MemberProp,
    ctx: &mut Context,
) -> Result<(Subst, Type), String> {
    let elems = &obj.elems;
    match prop {
        MemberProp::Ident(Ident { name, .. }) => {
            let prop = elems.iter().find_map(|elem| match elem {
                types::TObjElem::Call(_) => None,
                types::TObjElem::Constructor(_) => None,
                types::TObjElem::Index(_) => None,
                types::TObjElem::Prop(prop) => {
                    if prop.name == *name {
                        Some(prop)
                    } else {
                        None
                    }
                }
            });

            match prop {
                Some(prop) => {
                    let t = get_property_type(prop);
                    Ok((Subst::default(), t))
                }
                None => Err(format!("Object type doesn't contain key {name}.")),
            }
        }
        MemberProp::Computed(ComputedPropName { expr, .. }) => {
            let (prop_s, prop_t) = infer_expr(ctx, expr)?;

            let prop_t_clone = prop_t.clone();
            let prop_s_clone = prop_s.clone();

            let result = match &prop_t.kind {
                TypeKind::Keyword(keyword) => match keyword {
                    TKeyword::String => {
                        let mut value_types: Vec<Type> = elems
                            .iter()
                            .filter_map(|elem| match elem {
                                // TODO: include index types in the future
                                // Call signatures aren't included because they can't be accessed
                                // as members.  What about .constructor?
                                types::TObjElem::Call(_) => None,
                                types::TObjElem::Constructor(_) => None,
                                types::TObjElem::Index(_) => None,
                                types::TObjElem::Prop(prop) => {
                                    // TODO: handle generic object properties
                                    Some(prop.t.to_owned())
                                }
                            })
                            .collect();

                        // We can't tell if the property is in the object or not because the
                        // key is a string whose exact value is unknown at compile time.
                        value_types.push(Type {
                            kind: TypeKind::Keyword(TKeyword::Undefined),
                            provenance: None,
                        });
                        let t = Type {
                            kind: TypeKind::Union(value_types),
                            provenance: None,
                        };

                        Ok((prop_s, t))
                    }
                    _ => Err(format!("{keyword} is an invalid key for object types")),
                },
                TypeKind::Lit(lit) => match lit {
                    types::TLit::Str(key) => {
                        let prop = elems.iter().find_map(|elem| match elem {
                            types::TObjElem::Call(_) => None,
                            types::TObjElem::Constructor(_) => None,
                            types::TObjElem::Index(_) => None,
                            types::TObjElem::Prop(prop) => {
                                if &prop.name == key {
                                    Some(prop)
                                } else {
                                    None
                                }
                            }
                        });

                        match prop {
                            Some(prop) => {
                                // TODO: handle generic object properties
                                Ok((Subst::default(), prop.t.to_owned()))
                            }
                            None => Err(format!("Object type doesn't contain key {key}.")),
                        }
                    }
                    _ => Err(format!("{lit} is an invalid key for object types")),
                },
                _ => Err(format!("{prop_t} is an invalid key for object types")),
            };

            match result {
                Ok((s, t)) => Ok((s, t)),
                Err(err) => {
                    let indexers: Vec<_> = elems
                        .iter()
                        .filter_map(|elem| match elem {
                            TObjElem::Index(indexer) => Some(indexer),
                            _ => None,
                        })
                        .collect();

                    if indexers.is_empty() {
                        Err(err)
                    } else {
                        for indexer in indexers {
                            let result = unify(&prop_t_clone, &indexer.key.t, ctx);
                            if result.is_ok() {
                                let key_s = result?;
                                let s = compose_subs(&key_s, &prop_s_clone);
                                // TODO: handle generic indexers
                                // NOTE: Since access any indexer could result in an `undefined`
                                // we include `| undefined` in the return type here.
                                let undefined = Type {
                                    kind: TypeKind::Keyword(TKeyword::Undefined),
                                    provenance: None,
                                };
                                let t = union_types(&indexer.t, &undefined);
                                return Ok((s, t));
                            }
                        }
                        Err(format!("{prop_t_clone} is an invalid key for object types"))
                    }
                }
            }
        }
    }
}
