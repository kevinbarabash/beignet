use std::collections::HashMap;

use crochet_ast::*;
use crochet_types::{self as types, Scheme, TFnParam, TPat, TPrim, Type};

use super::context::Context;
use super::infer_fn_param::infer_fn_param;
use super::infer_pattern::*;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
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

            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
            Ok((s, t))
        }
        Expr::Fix(Fix { expr, .. }) => {
            let (s1, t) = infer_expr(ctx, expr)?;
            let tv = ctx.fresh_var();
            let param = TFnParam {
                pat: TPat::Ident(types::BindingIdent {
                    name: String::from("fix_param"),
                    mutable: false,
                }),
                ty: tv.clone(),
                optional: false,
            };
            let s2 = unify(
                &Type::Lam(types::TLam {
                    params: vec![param],
                    ret: Box::from(tv),
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

            Ok((compose_subs(&s2, &s1), t))
        }
        Expr::Ident(Ident { name, .. }) => {
            let s = Subst::default();
            let t = if name == "_" {
                Type::Wildcard
            } else {
                ctx.lookup_value(name)?
            };
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
                        let (s1, t1) =
                            infer_let(pat, &None, expr, consequent, ctx, &PatternUsage::Match)?;
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
                        Ok((s, t))
                    }
                }
            }
            None => match cond.as_ref() {
                Expr::LetExpr(LetExpr { pat, expr, .. }) => {
                    let (s1, t1) =
                        infer_let(pat, &None, expr, consequent, ctx, &PatternUsage::Match)?;
                    let s2 = match unify(&t1, &Type::Prim(TPrim::Undefined), ctx) {
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
                    let s4 = match unify(&t2, &Type::Prim(TPrim::Undefined), ctx) {
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
                let t = ctx.lookup_value(name)?;
                match t {
                    Type::Lam(_) => {
                        let mut ss: Vec<_> = vec![];
                        let mut props: Vec<_> = vec![];
                        for attr in attrs {
                            let (s, t) = match &attr.value {
                                JSXAttrValue::Lit(lit) => {
                                    infer_expr(ctx, &Expr::Lit(lit.to_owned()))?
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
                                scheme: Scheme::from(t),
                            };
                            props.push(prop);
                        }

                        let ret_type = Type::Alias(types::TAlias {
                            name: String::from("JSXElement"),
                            type_params: None,
                        });

                        let call_type = Type::App(types::TApp {
                            args: vec![Type::Object(props)],
                            ret: Box::from(ret_type.clone()),
                        });

                        let s1 = compose_many_subs(&ss);
                        let s2 = unify(&call_type, &t, ctx)?;

                        let s = compose_subs(&s2, &s1);

                        return Ok((s, ret_type));
                    }
                    _ => return Err(String::from("Component must be a function")),
                }
            }

            let s = Subst::default();
            // TODO: check props on JSXInstrinsics
            let t = Type::Alias(types::TAlias {
                name: String::from("JSXElement"),
                type_params: None,
            });

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
            ctx.push_scope(is_async.to_owned());

            let type_params_map: HashMap<String, Type> = match type_params {
                Some(params) => params
                    .iter()
                    .map(|param| (param.name.name.to_owned(), ctx.fresh_var()))
                    .collect(),
                None => HashMap::default(),
            };

            let params: Result<Vec<(Subst, TFnParam)>, String> = params
                .iter()
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

            let (ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

            let (rs, rt) = infer_expr(ctx, body)?;

            ctx.pop_scope();

            let rt = if *is_async && !is_promise(&rt) {
                Type::Alias(types::TAlias {
                    name: String::from("Promise"),
                    type_params: Some(vec![rt]),
                })
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
            let t = Type::Lam(types::TLam {
                params: t_params,
                ret: Box::from(rt),
            });
            let s = compose_subs(&s, &compose_subs(&rs, &compose_many_subs(&ss)));
            let t = t.apply(&s);

            Ok((s, t))
        }
        Expr::Let(Let {
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
        Expr::LetExpr(_) => {
            panic!("Unexpected LetExpr.  All LetExprs should be handled by IfElse arm.")
        }
        Expr::Lit(lit) => {
            let s = Subst::new();
            let t = Type::from(lit.to_owned());
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
            Ok((compose_many_subs(&[s1, s2, s3, s4]), t))
        }
        Expr::UnaryExpr(UnaryExpr { op, arg, .. }) => {
            let (s1, t1) = infer_expr(ctx, arg)?;
            let s2 = unify(&t1, &Type::Prim(TPrim::Num), ctx)?;
            let t = match op {
                UnaryOp::Minus => Type::Prim(TPrim::Num),
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
                                ps.push(types::TProp {
                                    name: name.to_owned(),
                                    optional: false,
                                    mutable: false,
                                    scheme: Scheme::from(t),
                                });
                            }
                            Prop::KeyValue(KeyValueProp { name, value, .. }) => {
                                let (s, t) = infer_expr(ctx, value)?;
                                ss.push(s);
                                // TODO: check if the inferred type is T | undefined and use that
                                // determine the value of optional
                                ps.push(types::TProp {
                                    name: name.to_owned(),
                                    optional: false,
                                    mutable: false,
                                    scheme: Scheme::from(t),
                                });
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
                let t = Type::Object(ps);
                Ok((s, t))
            } else {
                let mut all_types = spread_types;
                all_types.push(Type::Object(ps));
                let t = simplify_intersection(&all_types);
                Ok((s, t))
            }
        }
        Expr::Await(Await { expr, .. }) => {
            if !ctx.is_async() {
                return Err(String::from("Can't use `await` inside non-async lambda"));
            }

            let (s1, t1) = infer_expr(ctx, expr)?;
            let wrapped_type = ctx.fresh_var();
            let promise_type = Type::Alias(types::TAlias {
                name: String::from("Promise"),
                type_params: Some(vec![wrapped_type.clone()]),
            });

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
            Ok((s, t))
        }
        Expr::Member(Member { obj, prop, .. }) => {
            let (obj_s, obj_t) = infer_expr(ctx, obj)?;
            let (prop_s, prop_t) = infer_property_type(&obj_t, prop, ctx)?;

            let s = compose_subs(&prop_s, &obj_s);
            let t = prop_t;

            Ok((s, t))
        }
        Expr::Empty(_) => {
            let t = Type::Prim(TPrim::Undefined);
            let s = Subst::default();
            Ok((s, t))
        }
        Expr::TemplateLiteral(TemplateLiteral {
            exprs, quasis: _, ..
        }) => {
            let t = Type::Prim(TPrim::Str);
            let result: Result<Vec<(Subst, Type)>, String> =
                exprs.iter().map(|expr| infer_expr(ctx, expr)).collect();
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
        Expr::Match(Match { expr, arms, .. }) => {
            // TODO: warn if the pattern isn't refutable
            let mut ss: Vec<Subst> = vec![];
            let mut ts: Vec<Type> = vec![];
            for arm in arms {
                let (s, t) = infer_let(
                    &arm.pattern,
                    &None,
                    expr,
                    &arm.expr,
                    ctx,
                    &PatternUsage::Match,
                )?;
                ss.push(s);
                ts.push(t);
            }

            let s = compose_many_subs(&ss);
            let t = union_many_types(&ts);

            Ok((s, t))
        }
    };

    let (s, t) = result?;

    ctx.apply(&s);

    Ok((s, t))
}

fn infer_let(
    pat: &Pattern,
    type_ann: &Option<TypeAnn>,
    init: &Expr,
    body: &Expr,
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

fn is_promise(ty: &Type) -> bool {
    matches!(&ty, Type::Alias(types::TAlias { name, .. }) if name == "Promise")
}

fn infer_property_type(
    obj_t: &Type,
    prop: &MemberProp,
    ctx: &mut Context,
) -> Result<(Subst, Type), String> {
    match &obj_t {
        Type::Object(props) => match prop {
            MemberProp::Ident(Ident { name, .. }) => {
                let prop = props.iter().find(|prop| prop.name == *name);
                match prop {
                    Some(prop) => {
                        let prop = ctx.instantiate(&prop.get_scheme());
                        Ok((Subst::default(), prop))
                    }
                    None => Err(format!("Object type doesn't contain key {name}.")),
                }
            }
            MemberProp::Computed(ComputedPropName { expr, .. }) => {
                let (prop_s, prop_t) = infer_expr(ctx, expr)?;

                match prop_t {
                    Type::Prim(prim) => match prim {
                        TPrim::Str => {
                            let mut value_types: Vec<Type> = props
                                .iter()
                                .map(|prop| ctx.instantiate(&prop.scheme))
                                .collect();
                            value_types.push(Type::Prim(TPrim::Undefined));
                            let t = Type::Union(value_types);
                            Ok((prop_s, t))
                        }
                        _ => Err(format!("{prim} is an invalid key for object types")),
                    },
                    Type::Lit(lit) => match lit {
                        types::TLit::Str(key) => {
                            let prop = props.iter().find(|prop| prop.name == key);
                            match prop {
                                Some(prop) => {
                                    let prop = ctx.instantiate(&prop.get_scheme());
                                    Ok((Subst::default(), prop))
                                }
                                None => Err(format!("Object type doesn't contain key {key}.")),
                            }
                        }
                        _ => Err(format!("{lit} is an invalid key for object types")),
                    },
                    _ => Err(format!("{prop_t} is an invalid key for object types")),
                }
            }
        },
        Type::Alias(alias) => {
            let t = ctx.lookup_alias(alias)?;
            infer_property_type(&t, prop, ctx)
        }
        Type::Lit(lit) => match lit {
            types::TLit::Num(_) => {
                let t = ctx.lookup_type("Number")?;
                infer_property_type(&t, prop, ctx)
            }
            types::TLit::Bool(_) => {
                let t = ctx.lookup_type("Boolean")?;
                infer_property_type(&t, prop, ctx)
            }
            types::TLit::Str(_) => {
                let t = ctx.lookup_type("String")?;
                infer_property_type(&t, prop, ctx)
            }
            types::TLit::Null => todo!(),
            types::TLit::Undefined => todo!(),
        },
        Type::Prim(prim) => match prim {
            TPrim::Num => {
                let t = ctx.lookup_type("Number")?;
                infer_property_type(&t, prop, ctx)
            }
            TPrim::Bool => {
                let t = ctx.lookup_type("Boolean")?;
                infer_property_type(&t, prop, ctx)
            }
            TPrim::Str => {
                let t = ctx.lookup_type("String")?;
                infer_property_type(&t, prop, ctx)
            }
            TPrim::Undefined => todo!(),
            TPrim::Null => todo!(),
        },
        Type::Tuple(elem_types) => {
            match prop {
                // TODO: lookup methods on Array.prototype
                MemberProp::Ident(_) => todo!(),
                MemberProp::Computed(ComputedPropName { expr, .. }) => {
                    let (prop_s, prop_t) = infer_expr(ctx, expr)?;

                    match prop_t {
                        Type::Prim(prim) => match prim {
                            TPrim::Num => {
                                // TODO: remove duplicate types
                                let mut elem_types = elem_types.to_owned();
                                elem_types.push(Type::Prim(TPrim::Undefined));
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
