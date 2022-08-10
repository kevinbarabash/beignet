use std::collections::HashMap;

use crochet_ast::*;

use super::context::{lookup_alias, Context};
use super::infer_pattern::*;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
use super::types::{self, Type, Variant};
use super::unify::unify;
use super::util::*;

pub fn infer_expr(ctx: &mut Context, expr: &Expr) -> Result<(Subst, Type), String> {
    let result = match expr {
        Expr::App(App { lam, args, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut arg_types: Vec<Type> = vec![];

            let (s1, lam_type) = infer_expr(ctx, lam)?;
            ss.push(s1);

            for arg in args {
                let (arg_s, arg_t) = infer_expr(ctx, arg.expr.as_ref())?;
                ss.push(arg_s);
                if arg.spread.is_some() {
                    match arg_t.variant {
                        Variant::Tuple(types) => arg_types.extend(types.to_owned()),
                        _ => arg_types.push(ctx.rest(arg_t))
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
                id: ctx.fresh_id(),
                variant: Variant::Lam(types::LamType {
                    params: arg_types,
                    ret: Box::from(ret_type.clone()),
                    is_call: true,
                }),
            };
            let s3 = unify(&call_type, &lam_type, ctx)?;

            ss.push(s3);

            let s = compose_many_subs(&ss);
            let t = ret_type.apply(&s);

            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
            Ok((s, t))
        }
        Expr::Fix(Fix { expr, .. }) => {
            let (s1, t) = infer_expr(ctx, expr)?;
            let tv = ctx.fresh_var();
            let s2 = unify(&ctx.lam(vec![tv.clone()], Box::from(tv.clone())), &t, ctx)?;
            Ok((compose_subs(&s2, &s1), tv.apply(&s2)))
        }
        Expr::Ident(Ident { name, .. }) => {
            let s = Subst::default();
            let t = ctx.lookup_value(name)?;
            Ok((s, t))
        }
        Expr::IfElse(IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => match alternate {
            Some(alternate) => {
                match cond.as_ref() {
                    Expr::LetExpr(LetExpr { pat, expr, .. }) => {
                        // TODO: warn if the pattern isn't refutable
                        let (s1, t1) = infer_let(pat, expr, consequent, ctx, &PatternUsage::Match)?;
                        let (s2, t2) = infer_expr(ctx, alternate)?;

                        let s = compose_many_subs(&[s1, s2]);
                        let t = union_types(&t1, &t2, ctx);
                        Ok((s, t))
                    }
                    _ => {
                        let (s1, t1) = infer_expr(ctx, cond)?;
                        let (s2, t2) = infer_expr(ctx, consequent)?;
                        let (s3, t3) = infer_expr(ctx, alternate)?;
                        let s4 = unify(&t1, &ctx.prim(Primitive::Bool), ctx)?;

                        let s = compose_many_subs(&[s1, s2, s3, s4]);
                        let t = union_types(&t2, &t3, ctx);
                        Ok((s, t))
                    }
                }
            }
            None => match cond.as_ref() {
                Expr::LetExpr(LetExpr { pat, expr, .. }) => {
                    let (s1, t1) = infer_let(pat, expr, consequent, ctx, &PatternUsage::Match)?;
                    let s2 = match unify(&t1, &ctx.prim(Primitive::Undefined), ctx) {
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
                    let s3 = unify(&t1, &ctx.prim(Primitive::Bool), ctx)?;
                    let s4 = match unify(&t2, &ctx.prim(Primitive::Undefined), ctx) {
                        Ok(s) => Ok(s),
                        Err(_) => Err(String::from(
                            "Consequent for 'if' without 'else' must not return a value",
                        )),
                    }?;

                    let s = compose_many_subs(&[s1, s2, s3, s4]);
                    let t = t2;
                    Ok((s, t))
                }
            },
        },
        Expr::JSXElement(JSXElement {
            name,
            attrs,
            children: _,
            ..
        }) => {
            let first_char = name.chars().next().unwrap();
            // JSXElement's starting with an uppercase char are user defined.
            if first_char.is_uppercase() {
                match ctx.values.get(name) {
                    Some(scheme) => {
                        let ct = ctx.instantiate(scheme);
                        match &ct.variant {
                            Variant::Lam(_) => {
                                let mut ss: Vec<_> = vec![];
                                let mut props: Vec<_> = vec![];
                                for attr in attrs {
                                    let (s, t) = match &attr.value {
                                        JSXAttrValue::Lit(lit) => {
                                            infer_expr(ctx, &Expr::Lit(lit.to_owned()))?
                                        }
                                        JSXAttrValue::JSXExprContainer(JSXExprContainer {
                                            expr,
                                            ..
                                        }) => infer_expr(ctx, expr)?,
                                    };
                                    ss.push(s);

                                    let prop = types::TProp {
                                        name: attr.ident.name.to_owned(),
                                        optional: false,
                                        mutable: false,
                                        ty: t,
                                    };
                                    props.push(prop);
                                }

                                let ret_type = ctx.alias("JSXElement", None);

                                let call_type = Type {
                                    id: ctx.fresh_id(),
                                    variant: Variant::Lam(types::LamType {
                                        params: vec![ctx.object(props)],
                                        ret: Box::from(ret_type.clone()),
                                        is_call: true,
                                    }),
                                };

                                let s1 = compose_many_subs(&ss);
                                let s2 = unify(&call_type, &ct, ctx)?;

                                let s = compose_subs(&s2, &s1);

                                return Ok((s, ret_type));
                            }
                            _ => return Err(String::from("Component must be a function")),
                        }
                    }
                    None => return Err(format!("Component '{name}' is not in scope")),
                }
            }

            let s = Subst::default();
            // TODO: check props on JSXInstrinsics
            let t = ctx.alias("JSXElement", None);

            Ok((s, t))
        }
        Expr::Lambda(Lambda {
            params,
            body,
            is_async,
            return_type: rt_type_ann,
            type_params,
            ..
        }) => {
            let mut new_ctx = ctx.clone();
            new_ctx.is_async = is_async.to_owned();

            let type_params_map: HashMap<String, Type> = match type_params {
                Some(params) => params
                    .iter()
                    .map(|param| (param.name.name.to_owned(), new_ctx.fresh_var()))
                    .collect(),
                None => HashMap::default(),
            };

            let params: Result<Vec<(Subst, Type)>, String> = params
                .iter()
                .map(|param| {
                    let (ps, pa, pt) = infer_pattern(param, &new_ctx, &type_params_map)?;

                    // Inserts any new variables introduced by infer_pattern() into
                    // the current context.
                    for (name, scheme) in pa {
                        new_ctx.values.insert(name, scheme);
                    }

                    Ok((ps, pt))
                })
                .collect();

            let (ss, ts): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

            let (rs, rt) = infer_expr(&mut new_ctx, body)?;

            // Copies over the count from new_ctx so that it's unique across Contexts.
            ctx.state.count.set(new_ctx.state.count.get());

            let rt = if *is_async && !is_promise(&rt) {
                ctx.alias("Promise", Some(vec![rt]))
            } else {
                rt
            };

            let s = match rt_type_ann {
                Some(rt_type_ann) => unify(
                    &rt,
                    &infer_type_ann_with_params(rt_type_ann, ctx, &type_params_map),
                    ctx,
                )?,
                None => Subst::default(),
            };
            let t = ctx.lam(ts, Box::from(rt));
            let s = compose_subs(&s, &compose_subs(&rs, &compose_many_subs(&ss)));
            let t = t.apply(&s);

            Ok((s, t))
        }
        Expr::Let(Let {
            pattern,
            init,
            body,
            ..
        }) => {
            match pattern {
                Some(pat) => infer_let(pat, init, body, ctx, &PatternUsage::Assign),
                None => {
                    // TODO: warn about unused values
                    infer_expr(ctx, body)
                }
            }
        }
        Expr::LetExpr(_) => {
            panic!("Unexpected LetExpr.  All LetExprs should be handled by IfElse arm.")
        }
        Expr::Lit(lit) => {
            let s = Subst::new();
            let t = ctx.lit(lit.to_owned());
            Ok((s, t))
        }
        Expr::Op(Op {
            op, left, right, ..
        }) => {
            // TODO: check what `op` is and handle comparison operators
            // differently from arithmetic operators
            // TODO: if both are literals, compute the result at compile
            // time and set the result to be appropriate number literal.
            let (s1, t1) = infer_expr(ctx, left)?;
            let (s2, t2) = infer_expr(ctx, right)?;
            let s3 = unify(&t1, &ctx.prim(Primitive::Num), ctx)?;
            let s4 = unify(&t2, &ctx.prim(Primitive::Num), ctx)?;
            let t = match op {
                BinOp::Add => ctx.prim(Primitive::Num),
                BinOp::Sub => ctx.prim(Primitive::Num),
                BinOp::Mul => ctx.prim(Primitive::Num),
                BinOp::Div => ctx.prim(Primitive::Num),
                BinOp::EqEq => ctx.prim(Primitive::Bool),
                BinOp::NotEq => ctx.prim(Primitive::Bool),
                BinOp::Gt => ctx.prim(Primitive::Bool),
                BinOp::GtEq => ctx.prim(Primitive::Bool),
                BinOp::Lt => ctx.prim(Primitive::Bool),
                BinOp::LtEq => ctx.prim(Primitive::Bool),
            };
            Ok((compose_many_subs(&[s1, s2, s3, s4]), t))
        }
        Expr::UnaryExpr(UnaryExpr { op, arg, .. }) => {
            let (s1, t1) = infer_expr(ctx, arg)?;
            let s2 = unify(&t1, &ctx.prim(Primitive::Num), ctx)?;
            let t = match op {
                UnaryOp::Minus => ctx.prim(Primitive::Num),
            };
            Ok((compose_many_subs(&[s1, s2]), t))
        }
        Expr::Obj(Obj { props, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut ps: Vec<types::TProp> = vec![];
            let mut spread_types: Vec<_> = vec![];
            for p in props {
                match p {
                    PropOrSpread::Prop(p) => {
                        match p.as_ref() {
                            Prop::Shorthand(Ident { name, .. }) => {
                                let t = ctx.lookup_value(name)?;
                                ps.push(ctx.prop(name, t, false));
                            }
                            Prop::KeyValue(KeyValueProp { name, value, .. }) => {
                                let (s, t) = infer_expr(ctx, value)?;
                                ss.push(s);
                                // TODO: check if the inferred type is T | undefined and use that
                                // determine the value of optional
                                ps.push(ctx.prop(name, t, false));
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
            if spread_types.is_empty() {
                let t = ctx.object(ps);
                Ok((s, t))
            } else {
                let mut all_types = spread_types;
                all_types.push(ctx.object(ps));
                let t = simplify_intersection(&all_types, ctx);
                Ok((s, t))
            }
        }
        Expr::Await(Await { expr, .. }) => {
            if !ctx.is_async {
                return Err(String::from("Can't use `await` inside non-async lambda"));
            }

            let (s1, t1) = infer_expr(ctx, expr)?;
            let wrapped_type = ctx.fresh_var();
            let promise_type = ctx.alias("Promise", Some(vec![wrapped_type.clone()]));

            let s2 = unify(&t1, &promise_type, ctx)?;

            let s = compose_subs(&s2, &s1);

            Ok((s, wrapped_type))
        }
        Expr::Tuple(Tuple { elems, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut ts: Vec<Type> = vec![];

            for elem in elems {
                let expr = elem.expr.as_ref();
                match elem.spread {
                    Some(_) => {
                        let (s, t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        match &t.variant {
                            Variant::Tuple(types) => {
                                ts.extend(types.to_owned());
                            },
                            _ => {
                                return Err(String::from("Can only spread tuple types inside a tuple"))
                            }
                        }
                    },
                    None => {
                        let (s, t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        ts.push(t);
                    },
                }
            }

            let s = compose_many_subs(&ss);
            let t = ctx.tuple(ts);
            Ok((s, t))
        }
        Expr::Member(Member { obj, prop, .. }) => {
            let (obj_s, obj_t) = infer_expr(ctx, obj)?;
            let (prop_s, prop_t) = infer_property_type(&obj_t, prop, ctx)?;

            let s = compose_subs(&prop_s, &obj_s);
            let t = unwrap_member_type(&prop_t, ctx);

            Ok((s, t))
        }
        Expr::Empty(_) => {
            let t = ctx.prim(Primitive::Undefined);
            let s = Subst::default();
            Ok((s, t))
        }
        Expr::TemplateLiteral(TemplateLiteral { exprs, quasis: _, .. }) => {
            let t = ctx.prim(Primitive::Str);
            let result: Result<Vec<(Subst, Type)>, String> = exprs.iter().map(|expr| {
                infer_expr(ctx, expr)
            }).collect();
            // We ignore the types of expressions if there are any because any expression
            // in JavaScript has a string representation.
            let (ss, _): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();
            let s = compose_many_subs(&ss);
            Ok((s, t))
        }
        Expr::TaggedTemplateLiteral(_) => {
            // TODO: treat this like a call/application
            // NOTE: requires:
            // - arrays
            // - rest params
            todo!()
        }
        Expr::Match(Match {expr, arms, ..}) => {
            // TODO: warn if the pattern isn't refutable
            let mut ss: Vec<Subst> = vec![];
            let mut ts: Vec<Type> = vec![];
            for arm in arms {
                let (s, t) = infer_let(&arm.pattern, expr, &arm.expr, ctx, &PatternUsage::Match)?;
                ss.push(s);
                ts.push(t);
            }
            
            let s = compose_many_subs(&ss);
            let t = union_many_types(&ts, ctx);

            Ok((s, t))
        }
    };

    let (s, t) = result?;

    // TODO: apply `s` to `ctx.values`
    for (k, v) in ctx.values.clone() {
        ctx.values.insert(k.to_owned(), v.apply(&s));
    }

    Ok((s, t))
}

fn infer_let(
    pat: &Pattern,
    init: &Expr,
    body: &Expr,
    ctx: &Context,
    pu: &PatternUsage,
) -> Result<(Subst, Type), String> {
    let mut new_ctx = ctx.clone();
    let (pa, s1) = infer_pattern_and_init(pat, init, &mut new_ctx, pu)?;

    // Inserts the new variables from infer_pattern() into the
    // current context.
    for (name, scheme) in pa {
        new_ctx.values.insert(name.to_owned(), scheme.to_owned());
    }

    let (s2, t2) = infer_expr(&mut new_ctx, body)?;

    // Copies over the count from new_ctx so that it's unique across Contexts.
    ctx.state.count.set(new_ctx.state.count.get());

    let s = compose_subs(&s2, &s1);
    let t = t2;
    Ok((s, t))
}

fn is_promise(ty: &Type) -> bool {
    matches!(&ty.variant, Variant::Alias(types::AliasType { name, .. }) if name == "Promise")
}

fn infer_property_type(
    obj_t: &Type,
    prop: &MemberProp,
    ctx: &Context,
) -> Result<(Subst, Type), String> {
    match &obj_t.variant {
        Variant::Object(props) => {
            let mem_t = ctx.mem(obj_t.clone(), &prop.name());
            match props.iter().find(|p| p.name == prop.name()) {
                Some(_) => Ok((Subst::default(), mem_t)),
                None => Err(String::from("Record literal doesn't contain property")),
            }
        }
        Variant::Alias(alias) => {
            let t = lookup_alias(ctx, alias)?;
            infer_property_type(&t, prop, ctx)
        }
        Variant::Lit(lit) => {
            match lit {
                crate::Lit::Num(_) => {
                    let t = ctx.lookup_type("Number")?;
                    infer_property_type(&t, prop, ctx)
                },
                crate::Lit::Bool(_) => {
                    let t = ctx.lookup_type("Boolean")?;
                    infer_property_type(&t, prop, ctx)
                },
                crate::Lit::Str(_) => {
                    let t = ctx.lookup_type("String")?;
                    infer_property_type(&t, prop, ctx)
                },
                crate::Lit::Null => todo!(),
                crate::Lit::Undefined => todo!(),
            }
        }
        Variant::Prim(prim) => {
            match prim {
                Primitive::Num => {
                    let t = ctx.lookup_type("Number")?;
                    infer_property_type(&t, prop, ctx)
                },
                Primitive::Bool => {
                    let t = ctx.lookup_type("Boolean")?;
                    infer_property_type(&t, prop, ctx)
                },
                Primitive::Str => {
                    let t = ctx.lookup_type("String")?;
                    infer_property_type(&t, prop, ctx)
                },
                Primitive::Undefined => todo!(),
                Primitive::Null => todo!(),
            }
        }
        _ => {
            todo!("Unhandled {obj_t:#?} in infer_property_type")
        },
    }
}

fn unwrap_member_type(t: &Type, ctx: &Context) -> Type {
    if let Variant::Member(member) = &t.variant {
        if let Variant::Object(props) = &member.obj.as_ref().variant {
            let prop = props.iter().find(|prop| prop.name == member.prop);
            if let Some(prop) = prop {
                return prop.get_type(ctx);
            }
        }
    }
    t.to_owned()
}
