use std::collections::HashMap;

use crochet_ast::*;
use crochet_types::{self as types, TFnParam, TKeyword, TObject, TPat, TPrim, Type};
use types::TObjElem;

use super::context::Context;
use super::infer_fn_param::infer_fn_param;
use super::infer_pattern::*;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
use super::unify::unify;
use super::util::*;

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
                    match arg_t {
                        Type::Tuple(types) => arg_types.extend(types.to_owned()),
                        _ => arg_types.push(Type::Rest(Box::from(arg_t))),
                    }
                } else {
                    arg_types.push(arg_t);
                }
            }

            let ret_type = ctx.fresh_var();
            // Are we missing an `apply()` call here?
            // Maybe, I could see us needing an apply to handle generic functions properly
            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            let call_type = Type::App(types::TApp {
                args: arg_types,
                ret: Box::from(ret_type.clone()),
            });
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
                pat: TPat::Ident(types::BindingIdent {
                    name: String::from("fix_param"),
                    mutable: false,
                }),
                t: tv.clone(),
                optional: false,
            };
            let s2 = unify(
                &Type::Lam(types::TLam {
                    params: vec![param],
                    ret: Box::from(tv),
                    type_params: vec![],
                }),
                &t,
                ctx,
            )?;

            // This leaves the function param names intact and returns a TLam
            // instead of a TApp.
            let t = match t {
                Type::Lam(types::TLam { ret, .. }) => Ok(ret.as_ref().to_owned()),
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
                        let s4 = unify(&t1, &Type::Prim(TPrim::Bool), ctx)?;

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
                    let s2 = match unify(&t1, &Type::Keyword(TKeyword::Undefined), ctx) {
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
                    let s3 = unify(&t1, &Type::Prim(TPrim::Bool), ctx)?;
                    let s4 = match unify(&t2, &Type::Keyword(TKeyword::Undefined), ctx) {
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
                match t {
                    Type::Lam(_) => {
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

                        let ret_type = Type::Ref(types::TRef {
                            name: String::from("JSXElement"),
                            type_args: None,
                        });

                        let call_type = Type::App(types::TApp {
                            args: vec![Type::Object(TObject {
                                elems,
                                type_params: vec![],
                            })],
                            ret: Box::from(ret_type.clone()),
                        });

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
            let t = Type::Ref(types::TRef {
                name: String::from("JSXElement"),
                type_args: None,
            });
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

            let type_params_map: HashMap<String, Type> = match type_params {
                Some(params) => params
                    .iter()
                    .map(|param| (param.name.name.to_owned(), ctx.fresh_var()))
                    .collect(),
                None => HashMap::default(),
            };

            let params: Result<Vec<(Subst, TFnParam)>, String> = params
                .iter_mut()
                .map(|e_param| {
                    let (ps, pa, t_param) = infer_fn_param(e_param, ctx, &type_params_map)?;

                    // Inserts any new variables introduced by infer_fn_param() into
                    // the current context.
                    for (name, scheme) in pa {
                        ctx.insert_value(name, scheme);
                    }

                    Ok((ps, t_param))
                })
                .collect();

            let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

            let (rs_1, rt_1) = infer_expr(ctx, body)?;
            ss.push(rs_1);

            ctx.pop_scope();

            let rt_1 = if *is_async && !is_promise(&rt_1) {
                Type::Ref(types::TRef {
                    name: String::from("Promise"),
                    type_args: Some(vec![rt_1]),
                })
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
            let t = Type::Lam(types::TLam {
                params: t_params,
                ret: Box::from(rt_1),
                type_params: vec![],
            });

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
        }) => {
            match pattern {
                Some(pat) => infer_let(pat, type_ann, init, body, ctx, &PatternUsage::Assign),
                None => {
                    // TODO: warn about unused values
                    infer_expr(ctx, body)
                }
            }
        }
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
            let s3 = unify(&t1, &Type::Prim(TPrim::Num), ctx)?;
            let s4 = unify(&t2, &Type::Prim(TPrim::Num), ctx)?;
            let t = match op {
                BinOp::Add => Type::Prim(TPrim::Num),
                BinOp::Sub => Type::Prim(TPrim::Num),
                BinOp::Mul => Type::Prim(TPrim::Num),
                BinOp::Div => Type::Prim(TPrim::Num),
                BinOp::EqEq => Type::Prim(TPrim::Bool),
                BinOp::NotEq => Type::Prim(TPrim::Bool),
                BinOp::Gt => Type::Prim(TPrim::Bool),
                BinOp::GtEq => Type::Prim(TPrim::Bool),
                BinOp::Lt => Type::Prim(TPrim::Bool),
                BinOp::LtEq => Type::Prim(TPrim::Bool),
            };
            expr.inferred_type = Some(t.clone());
            Ok((compose_many_subs(&[s1, s2, s3, s4]), t))
        }
        ExprKind::UnaryExpr(UnaryExpr { op, arg, .. }) => {
            let (s1, t1) = infer_expr(ctx, arg)?;
            let s2 = unify(&t1, &Type::Prim(TPrim::Num), ctx)?;
            let t = match op {
                UnaryOp::Minus => Type::Prim(TPrim::Num),
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
                Type::Object(TObject {
                    elems,
                    type_params: vec![],
                })
            } else {
                let mut all_types = spread_types;
                all_types.push(Type::Object(TObject {
                    elems,
                    type_params: vec![],
                }));
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
            let promise_type = Type::Ref(types::TRef {
                name: String::from("Promise"),
                type_args: Some(vec![wrapped_type.clone()]),
            });

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
                        match &t {
                            Type::Tuple(types) => {
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
            let t = Type::Tuple(ts);
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
            let t = Type::Keyword(TKeyword::Undefined);
            let s = Subst::default();
            expr.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        ExprKind::TemplateLiteral(TemplateLiteral {
            exprs, quasis: _, ..
        }) => {
            let t = Type::Prim(TPrim::Str);
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
                    &arm.pattern,
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
    pat: &Pattern,
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
    for (name, scheme) in pa {
        ctx.insert_value(name.to_owned(), scheme.to_owned());
    }

    let (s2, t2) = infer_expr(ctx, body)?;

    ctx.pop_scope();

    let s = compose_subs(&s2, &s1);
    let t = t2;
    Ok((s, t))
}

fn is_promise(t: &Type) -> bool {
    matches!(&t, Type::Ref(types::TRef { name, .. }) if name == "Promise")
}

// TODO: try to dedupe with key_of()
fn infer_property_type(
    obj_t: &Type,
    prop: &mut MemberProp,
    ctx: &mut Context,
) -> Result<(Subst, Type), String> {
    match &obj_t {
        Type::Object(obj) => get_prop_value(obj, prop, ctx),
        Type::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            infer_property_type(&t, prop, ctx)
        }
        Type::Lit(lit) => match lit {
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
        Type::Prim(prim) => match prim {
            TPrim::Num => {
                let t = ctx.lookup_type_and_instantiate("Number")?;
                infer_property_type(&t, prop, ctx)
            }
            TPrim::Bool => {
                let t = ctx.lookup_type_and_instantiate("Boolean")?;
                infer_property_type(&t, prop, ctx)
            }
            TPrim::Str => {
                let t = ctx.lookup_type_and_instantiate("String")?;
                infer_property_type(&t, prop, ctx)
            }
        },
        Type::Keyword(keyword) => match keyword {
            TKeyword::Symbol => {
                let t = ctx.lookup_type_and_instantiate("Symbol")?;
                infer_property_type(&t, prop, ctx)
            }
            TKeyword::Null => Err("Cannot read property on 'null'".to_owned()),
            TKeyword::Undefined => Err("Cannot read property on 'undefined'".to_owned()),
            TKeyword::Never => Err("Cannot read property on 'never'".to_owned()),
        },
        Type::Array(type_param) => {
            // TODO: Do this for all interfaces that we lookup
            let t = ctx.lookup_type("ReadonlyArray")?;
            let type_params = get_type_params(&t);
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            let s: Subst = Subst::from([(type_params[0], type_param.as_ref().to_owned())]);
            let t = t.apply(&s);
            infer_property_type(&t, prop, ctx)
        }
        Type::Tuple(elem_types) => {
            match prop {
                // TODO: lookup methods on Array.prototype
                MemberProp::Ident(_) => {
                    // TODO: Do this for all interfaces that we lookup
                    let t = ctx.lookup_type("ReadonlyArray")?;
                    println!("ReadonlyArray = {t}");
                    // TODO: Instead of instantiating the whole interface for one method, do
                    // the lookup call first and then instantiate the method.
                    // TODO: remove duplicate types
                    let type_param = Type::Union(elem_types.to_owned());
                    let type_params = get_type_params(&t); // ReadonlyArray type params

                    let s: Subst = Subst::from([(type_params[0], type_param)]);
                    let t = t.apply(&s);
                    infer_property_type(&t, prop, ctx)
                }
                MemberProp::Computed(ComputedPropName { expr, .. }) => {
                    let (prop_s, prop_t) = infer_expr(ctx, expr)?;

                    match prop_t {
                        Type::Prim(prim) => match prim {
                            TPrim::Num => {
                                // TODO: remove duplicate types
                                let mut elem_types = elem_types.to_owned();
                                elem_types.push(Type::Keyword(TKeyword::Undefined));
                                let t = Type::Union(elem_types);
                                Ok((prop_s, t))
                            }
                            _ => Err(format!("{prim} is an invalid indexer for tuple types")),
                        },
                        Type::Lit(lit) => match lit {
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

            let result = match prop_t {
                Type::Prim(prim) => match prim {
                    TPrim::Str => {
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
                        value_types.push(Type::Keyword(TKeyword::Undefined));
                        let t = Type::Union(value_types);

                        Ok((prop_s, t))
                    }
                    _ => Err(format!("{prim} is an invalid key for object types")),
                },
                Type::Lit(lit) => match lit {
                    types::TLit::Str(key) => {
                        let prop = elems.iter().find_map(|elem| match elem {
                            types::TObjElem::Call(_) => None,
                            types::TObjElem::Constructor(_) => None,
                            types::TObjElem::Index(_) => None,
                            types::TObjElem::Prop(prop) => {
                                if prop.name == key {
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
                    // TODO:
                    // - look for indexers
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
                                return Ok((s, indexer.t.to_owned()));
                            }
                        }
                        Err(format!("{prop_t_clone} is an invalid key for object types"))
                    }
                }
            }
        }
    }
}
