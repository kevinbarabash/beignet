use std::collections::HashMap;

use crochet_ast::types::{
    self as types, Provenance, TCallable, TFnParam, TKeyword, TLam, TObjElem, TObject, TPat,
    TPropKey, TVar, Type, TypeKind,
};
use crochet_ast::values::*;

use crate::context::Context;
use crate::expand_type::get_obj_type;
use crate::infer_fn_param::infer_fn_param;
use crate::infer_pattern::*;
use crate::infer_type_ann::*;
use crate::scheme::get_type_param_map;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::unify::unify;
use crate::update::update_pattern;
use crate::util::*;
use crate::visitor::Visitor;

pub fn infer_expr(
    ctx: &mut Context,
    expr: &mut Expr,
    is_lvalue: bool,
) -> Result<(Subst, Type), Vec<TypeError>> {
    let result = match &mut expr.kind {
        ExprKind::App(App {
            lam,
            args,
            type_args,
        }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut arg_types: Vec<Type> = vec![];

            let (s1, mut lam_type) = infer_expr(ctx, lam, false)?;
            ss.push(s1);

            for arg in args {
                let (arg_s, mut arg_t) = infer_expr(ctx, &mut arg.expr, false)?;
                ss.push(arg_s);
                if arg.spread.is_some() {
                    match &mut arg_t.kind {
                        TypeKind::Tuple(types) => arg_types.append(types),
                        _ => arg_types.push(Type::from(TypeKind::Rest(Box::from(arg_t)))),
                    }
                } else {
                    arg_types.push(arg_t);
                }
            }

            let mut ret_type = ctx.fresh_var();
            let type_args = match type_args {
                Some(type_args) => {
                    let tuples = type_args
                        .iter_mut()
                        .map(|type_arg| infer_type_ann(type_arg, ctx, &mut None))
                        .collect::<Result<Vec<_>, _>>()?;
                    let (mut subs, types): (Vec<_>, Vec<_>) = tuples.iter().cloned().unzip();
                    ss.append(&mut subs);
                    Some(types)
                }
                None => None,
            };

            // Are we missing an `apply()` call here?
            // Maybe, I could see us needing an apply to handle generic functions properly
            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            let mut call_type = Type::from(TypeKind::App(types::TApp {
                args: arg_types,
                ret: Box::from(ret_type.clone()),
                type_args,
            }));
            call_type.provenance = Some(Box::from(Provenance::Expr(Box::from(expr.to_owned()))));

            let s3 = unify(&mut call_type, &mut lam_type, ctx)?;

            ss.push(s3);

            let s = compose_many_subs(&ss);
            ret_type.apply(&s);

            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
            Ok((s, ret_type))
        }
        ExprKind::New(New {
            expr,
            args,
            type_args,
        }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut arg_types: Vec<Type> = vec![];

            let (s1, t) = infer_expr(ctx, expr, false)?;
            ss.push(s1);
            let t = get_obj_type(&t, ctx)?;

            for arg in args {
                let (arg_s, mut arg_t) = infer_expr(ctx, &mut arg.expr, false)?;
                ss.push(arg_s);
                if arg.spread.is_some() {
                    match &mut arg_t.kind {
                        TypeKind::Tuple(types) => arg_types.append(types),
                        _ => arg_types.push(Type::from(TypeKind::Rest(Box::from(arg_t)))),
                    }
                } else {
                    arg_types.push(arg_t);
                }
            }

            let mut results = vec![];
            if let TypeKind::Object(TObject {
                elems,
                is_interface: _,
            }) = t.kind
            {
                for elem in elems {
                    if let TObjElem::Constructor(callable) = &elem {
                        // TODO: replace type
                        let mut ret_type = ctx.fresh_var();
                        let mut call_type = Type::from(TypeKind::App(types::TApp {
                            args: arg_types.clone(),
                            ret: Box::from(ret_type.clone()),
                            type_args: None,
                        }));

                        let TCallable {
                            type_params,
                            params,
                            ret,
                        } = callable;

                        // TODO: Check to make sure that we're passing type args
                        // if and only if we need to.  In some cases it's okay
                        // not to pass type args, e.g. new Array(1, 2, 3);
                        let type_param_map: HashMap<String, Type> = match type_params {
                            Some(type_params) => {
                                if let Some(type_args) = type_args {
                                    let mut type_param_map = HashMap::new();
                                    for (type_param, type_arg) in type_params.iter().zip(type_args)
                                    {
                                        let (s, t) = infer_type_ann(type_arg, ctx, &mut None)?;
                                        ss.push(s);
                                        type_param_map.insert(type_param.name.to_string(), t);
                                    }
                                    type_param_map
                                } else {
                                    get_type_param_map(ctx, type_params)
                                }
                            }
                            None => HashMap::new(),
                        };

                        let lam_type = Type::from(TypeKind::Lam(TLam {
                            params: params.to_owned(),
                            ret: ret.to_owned(),
                        }));

                        let mut lam_type = replace_aliases_rec(&lam_type, &type_param_map);

                        // let t = generalize(&Env::default(), &lam_type);
                        // let mut lam_type = ctx.instantiate(&t);
                        lam_type.provenance =
                            Some(Box::from(Provenance::TObjElem(Box::from(elem.to_owned()))));

                        if let Ok(s3) = unify(&mut call_type, &mut lam_type, ctx) {
                            ss.push(s3);

                            let s = compose_many_subs(&ss.clone());
                            ret_type.apply(&s);
                            // TODO: figure out how we want to differentiate
                            // new-ing up mutable objects vs. immutable ones.
                            ret_type.mutable = true;

                            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
                            results.push((s, ret_type));
                        }
                    }
                }
            }

            // Sorts the results based on number of free type variables in
            // ascending order.
            results.sort_by(|a, b| {
                let a_len = a.1.ftv().len();
                let b_len = b.1.ftv().len();
                a_len.cmp(&b_len)
            });

            // Pick the result with the lowest number of of free type variables
            match results.get(0) {
                Some(result) => Ok(result.to_owned()),
                None => {
                    // TODO: update this to communicate that we couldn't find a
                    // valid constructor for the given arguments
                    Err(vec![TypeError::Unspecified])
                }
            }
        }
        ExprKind::Fix(Fix { expr, .. }) => {
            let (s1, mut t) = infer_expr(ctx, expr, false)?;
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
                &mut Type::from(TypeKind::Lam(types::TLam {
                    params: vec![param],
                    ret: Box::from(tv),
                })),
                &mut t,
                ctx,
            )?;

            let s = compose_subs(&s2, &s1);
            // This leaves the function param names intact and returns a TLam
            // instead of a TApp.
            let t = match t.kind {
                TypeKind::Lam(types::TLam { ret, .. }) => Ok(ret.as_ref().to_owned()),
                _ => Err(vec![TypeError::InvalidFix]),
            }?;

            Ok((s, t))
        }
        ExprKind::Ident(Ident { name, .. }) => {
            let s = Subst::default();
            let t = ctx.lookup_value_and_instantiate(name)?;

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
                        let (s2, t2) = infer_expr(ctx, alternate, false)?;

                        let s = compose_many_subs(&[s1, s2]);
                        let t = union_types(&t1, &t2);
                        Ok((s, t))
                    }
                    _ => {
                        let (s1, mut t1) = infer_expr(ctx, cond, false)?;
                        let (s2, t2) = infer_expr(ctx, consequent, false)?;
                        let (s3, t3) = infer_expr(ctx, alternate, false)?;
                        let s4 = unify(
                            &mut t1,
                            &mut Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                            ctx,
                        )?;

                        let s = compose_many_subs(&[s1, s2, s3, s4]);
                        let t = union_types(&t2, &t3);

                        Ok((s, t))
                    }
                }
            }
            None => match &mut cond.kind {
                ExprKind::LetExpr(LetExpr { pat, expr, .. }) => {
                    let (s, t) =
                        infer_let(pat, &mut None, expr, consequent, ctx, &PatternUsage::Match)?;

                    let undefined = Type::from(TypeKind::Keyword(TKeyword::Undefined));
                    let t = union_types(&t, &undefined);

                    Ok((s, t))
                }
                _ => {
                    let (s1, mut t1) = infer_expr(ctx, cond, false)?;
                    let (s2, t2) = infer_expr(ctx, consequent, false)?;
                    let s3 = unify(
                        &mut t1,
                        &mut Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                        ctx,
                    )?;

                    let s = compose_many_subs(&[s1, s2, s3]);

                    let undefined = Type::from(TypeKind::Keyword(TKeyword::Undefined));
                    let t = union_types(&t2, &undefined);

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
                let mut t = ctx.lookup_value_and_instantiate(name)?;
                match &t.kind {
                    TypeKind::Lam(_) => {
                        let mut ss: Vec<_> = vec![];
                        let mut elems: Vec<_> = vec![];
                        for attr in attrs {
                            let (s, t) = match &mut attr.value {
                                JSXAttrValue::Lit(lit) => {
                                    let kind = ExprKind::Lit(lit.to_owned());
                                    let mut expr = Expr {
                                        loc: lit.loc(),
                                        span: lit.span(),
                                        kind,
                                        inferred_type: None,
                                    };
                                    infer_expr(ctx, &mut expr, false)?
                                }
                                JSXAttrValue::JSXExprContainer(JSXExprContainer {
                                    expr, ..
                                }) => infer_expr(ctx, expr, false)?,
                            };
                            ss.push(s);

                            let prop = types::TProp {
                                name: TPropKey::StringKey(attr.ident.name.to_owned()),
                                optional: false,
                                mutable: false,
                                t,
                            };
                            elems.push(types::TObjElem::Prop(prop));
                        }

                        let ret_type = Type::from(TypeKind::Ref(types::TRef {
                            name: String::from("JSXElement"),
                            type_args: None,
                        }));

                        let mut call_type = Type::from(TypeKind::App(types::TApp {
                            args: vec![Type::from(TypeKind::Object(TObject {
                                elems,
                                is_interface: false,
                            }))],
                            ret: Box::from(ret_type.clone()),
                            type_args: None,
                        }));

                        let s1 = compose_many_subs(&ss);
                        let s2 = unify(&mut call_type, &mut t, ctx)?;

                        let s = compose_subs(&s2, &s1);
                        let t = ret_type;

                        return Ok((s, t));
                    }
                    _ => return Err(vec![TypeError::InvalidComponent]),
                }
            }

            let s = Subst::default();
            // TODO: check props on JSXInstrinsics
            let t = Type::from(TypeKind::Ref(types::TRef {
                name: String::from("JSXElement"),
                type_args: None,
            }));

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
                Some(type_params) => type_params
                    .iter_mut()
                    .map(|type_param| {
                        let tv = match &mut type_param.constraint {
                            Some(type_ann) => {
                                // TODO: push `s` on to `ss`
                                let (_s, t) = infer_type_ann(type_ann, ctx, &mut None)?;
                                Type::from(TypeKind::Var(TVar {
                                    id: ctx.fresh_id(),
                                    constraint: Some(Box::from(t)),
                                }))
                            }
                            None => ctx.fresh_var(),
                        };
                        ctx.insert_type(type_param.name.name.clone(), tv.clone());
                        Ok((type_param.name.name.to_owned(), tv))
                    })
                    .collect::<Result<HashMap<String, Type>, Vec<TypeError>>>()?,
                None => HashMap::default(),
            };

            let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = params
                .iter_mut()
                .map(|e_param| {
                    let (ps, pa, t_param) = infer_fn_param(e_param, ctx, &type_params_map)?;

                    // Inserts any new variables introduced by infer_fn_param() into
                    // the current context.
                    for (name, binding) in pa {
                        ctx.insert_binding(name, binding);
                    }

                    Ok((ps, t_param))
                })
                .collect();

            let (mut ss, mut t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

            let (body_s, mut body_t) = infer_expr(ctx, body, false)?;
            ss.push(body_s);

            ctx.pop_scope();

            if *is_async && !is_promise(&body_t) {
                body_t = Type::from(TypeKind::Ref(types::TRef {
                    name: String::from("Promise"),
                    type_args: Some(vec![body_t]),
                }))
            }

            if let Some(rt_type_ann) = rt_type_ann {
                let (ret_s, mut ret_t) =
                    infer_type_ann_with_params(rt_type_ann, ctx, &type_params_map)?;
                ss.push(ret_s);
                ss.push(unify(&mut body_t, &mut ret_t, ctx)?);
            }

            let mut t = Type::from(TypeKind::Lam(types::TLam {
                params: t_params.clone(),
                ret: Box::from(body_t.clone()),
            }));

            let s = compose_many_subs(&ss);

            t_params.apply(&s); // Do we need to do this since t_params is part of t?
            t.apply(&s);

            // TODO: Update the inferred_type on each param to equal the
            // corresponding type from t_params.

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
                let (init_s, init_t) = infer_expr(ctx, init, false)?;

                if init_t.kind != TypeKind::Keyword(TKeyword::Undefined) {
                    eprintln!("WARNING: {init_t} was not assigned");
                }

                let (body_s, mut body_t) = infer_expr(ctx, body, false)?;

                body_t.apply(&init_s);
                let s = compose_subs(&body_s, &init_s);

                Ok((s, body_t))
            }
        },
        ExprKind::Assign(assign) => {
            // TODO:
            // - if left is an identifier look it up to see if it exists in the context
            // - if it does, check if its mutable or not
            if let ExprKind::Ident(id) = &assign.left.kind {
                let name = &id.name;
                let binding = ctx.lookup_binding(name)?;
                if !binding.mutable {
                    return Err(vec![TypeError::NonMutableBindingAssignment(Box::from(
                        assign.to_owned(),
                    ))]);
                }
            }

            // This is similar to infer let, but without the type annotation and
            // with pat being an expression instead of a pattern.
            let (rs, mut rt) = infer_expr(ctx, &mut assign.right, false)?;
            // TODO: figure out how to get the type of a setter
            let (ls, mut lt) = infer_expr(ctx, &mut assign.left, true)?;

            if assign.op != AssignOp::Eq {
                todo!("handle update assignment operators");
            }

            let s = unify(&mut rt, &mut lt, ctx)?;

            let s = compose_many_subs(&[rs, ls, s]);
            let t = rt; // This is JavaScript's behavior

            Ok((s, t))
        }
        ExprKind::LetExpr(_) => {
            panic!("Unexpected LetExpr.  All LetExprs should be handled by IfElse arm.")
        }
        ExprKind::Lit(lit) => {
            let s = Subst::new();
            let t = Type::from(lit.to_owned());

            Ok((s, t))
        }
        ExprKind::Keyword(keyword) => {
            let s = Subst::new();
            let t = Type::from(keyword.to_owned());

            Ok((s, t))
        }
        ExprKind::BinaryExpr(BinaryExpr {
            op, left, right, ..
        }) => {
            // TODO: check what `op` is and handle comparison operators
            // differently from arithmetic operators
            // TODO: if both are literals, compute the result at compile
            // time and set the result to be appropriate number literal.
            let (s1, mut t1) = infer_expr(ctx, left, false)?;
            let (s2, mut t2) = infer_expr(ctx, right, false)?;
            let s3 = unify(
                &mut t1,
                &mut Type::from(TypeKind::Keyword(TKeyword::Number)),
                ctx,
            )?;
            let s4 = unify(
                &mut t2,
                &mut Type::from(TypeKind::Keyword(TKeyword::Number)),
                ctx,
            )?;

            let s = compose_many_subs(&[s1, s2, s3, s4]);
            let t = match op {
                BinOp::Add => Type::from(TypeKind::Keyword(TKeyword::Number)),
                BinOp::Sub => Type::from(TypeKind::Keyword(TKeyword::Number)),
                BinOp::Mul => Type::from(TypeKind::Keyword(TKeyword::Number)),
                BinOp::Div => Type::from(TypeKind::Keyword(TKeyword::Number)),
                BinOp::EqEq => Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                BinOp::NotEq => Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                BinOp::Gt => Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                BinOp::GtEq => Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                BinOp::Lt => Type::from(TypeKind::Keyword(TKeyword::Boolean)),
                BinOp::LtEq => Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            };

            Ok((s, t))
        }
        ExprKind::UnaryExpr(UnaryExpr { op, arg, .. }) => {
            let (s1, mut t1) = infer_expr(ctx, arg, false)?;
            let s2 = unify(
                &mut t1,
                &mut Type::from(TypeKind::Keyword(TKeyword::Number)),
                ctx,
            )?;

            let s = compose_many_subs(&[s1, s2]);
            let t = match op {
                UnaryOp::Minus => Type::from(TypeKind::Keyword(TKeyword::Number)),
            };

            Ok((s, t))
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
                                    name: TPropKey::StringKey(name.to_owned()),
                                    optional: false,
                                    mutable: false,
                                    t,
                                }));
                            }
                            Prop::KeyValue(KeyValueProp { key, value, .. }) => {
                                let (s, t) = infer_expr(ctx, value, false)?;
                                ss.push(s);
                                // TODO: check if the inferred type is T | undefined and use that
                                // determine the value of optional
                                elems.push(types::TObjElem::Prop(types::TProp {
                                    name: TPropKey::StringKey(key.name.to_owned()),
                                    optional: false,
                                    mutable: false,
                                    t,
                                }));
                            }
                        }
                    }
                    PropOrSpread::Spread(SpreadElement { expr, .. }) => {
                        let (s, t) = infer_expr(ctx, expr, false)?;
                        ss.push(s);
                        spread_types.push(t);
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = if spread_types.is_empty() {
                Type::from(TypeKind::Object(TObject {
                    elems,
                    is_interface: false,
                }))
            } else {
                let mut all_types = spread_types;
                all_types.push(Type::from(TypeKind::Object(TObject {
                    elems,
                    is_interface: false,
                })));
                simplify_intersection(&all_types)
            };

            Ok((s, t))
        }
        ExprKind::Await(Await { expr, .. }) => {
            if !ctx.is_async() {
                return Err(vec![TypeError::AwaitOutsideOfAsync]);
            }

            let (s1, mut t1) = infer_expr(ctx, expr, false)?;
            let inner_t = ctx.fresh_var();
            let mut promise_t = Type::from(TypeKind::Ref(types::TRef {
                name: String::from("Promise"),
                type_args: Some(vec![inner_t.clone()]),
            }));

            let s2 = unify(&mut t1, &mut promise_t, ctx)?;
            let s = compose_subs(&s2, &s1);

            Ok((s, inner_t))
        }
        ExprKind::Tuple(Tuple { elems, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut ts: Vec<Type> = vec![];

            for elem in elems {
                let expr = elem.expr.as_mut();
                match elem.spread {
                    Some(_) => {
                        let (s, mut t) = infer_expr(ctx, expr, false)?;
                        ss.push(s);
                        match &mut t.kind {
                            TypeKind::Tuple(types) => ts.append(types),
                            _ => {
                                return Err(vec![TypeError::TupleSpreadOutsideTuple]);
                            }
                        }
                    }
                    None => {
                        let (s, t) = infer_expr(ctx, expr, false)?;
                        ss.push(s);
                        ts.push(t);
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = Type::from(TypeKind::Tuple(ts));

            Ok((s, t))
        }
        ExprKind::Member(Member { obj, prop, .. }) => {
            let (obj_s, mut obj_t) = infer_expr(ctx, obj, false)?;
            let (prop_s, prop_t) = infer_property_type(&mut obj_t, prop, ctx, is_lvalue)?;

            let s = compose_subs(&prop_s, &obj_s);
            let t = prop_t;

            Ok((s, t))
        }
        ExprKind::Empty => {
            let t = Type::from(TypeKind::Keyword(TKeyword::Undefined));
            let s = Subst::default();

            Ok((s, t))
        }
        ExprKind::TemplateLiteral(TemplateLiteral {
            exprs, quasis: _, ..
        }) => {
            let t = Type::from(TypeKind::Keyword(TKeyword::String));
            let result: Result<Vec<(Subst, Type)>, Vec<TypeError>> = exprs
                .iter_mut()
                .map(|expr| infer_expr(ctx, expr, false))
                .collect();
            // We ignore the types of expressions if there are any because any expression
            // in JavaScript has a string representation.
            let (ss, _): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();
            let s = compose_many_subs(&ss);

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

            Ok((s, t))
        }
        ExprKind::Class(_) => todo!(),
    };

    let (s, mut t) = result?;

    ctx.apply(&s);

    expr.inferred_type = Some(t.clone());
    t.provenance = Some(Box::from(Provenance::from(expr)));

    Ok((s, t))
}

fn infer_let(
    pat: &mut Pattern,
    type_ann: &mut Option<TypeAnn>,
    init: &mut Expr,
    body: &mut Expr,
    ctx: &mut Context,
    pu: &PatternUsage,
) -> Result<(Subst, Type), Vec<TypeError>> {
    ctx.push_scope(ctx.is_async());
    let (pa, s1) = infer_pattern_and_init(pat, type_ann, init, ctx, pu)?;

    // Inserts the new variables from infer_pattern_and_init() into the
    // current context.
    // TODO: have infer_pattern_and_init do this
    for (name, binding) in pa {
        ctx.insert_binding(name.to_owned(), binding.to_owned());
    }

    let (s2, t2) = infer_expr(ctx, body, false)?;

    ctx.pop_scope();

    let s = compose_subs(&s2, &s1);
    let t = t2;

    update_pattern(pat, &s);

    Ok((s, t))
}

fn is_promise(t: &Type) -> bool {
    matches!(&t, Type {kind: TypeKind::Ref(types::TRef { name, .. }), ..} if name == "Promise")
}

// TODO: update this function to make use of get_obj_type
fn infer_property_type(
    obj_t: &mut Type,
    prop: &mut MemberProp,
    ctx: &mut Context,
    is_lvalue: bool,
) -> Result<(Subst, Type), Vec<TypeError>> {
    // TODO: figure out when we have to copy .mutable from `obj_t` to the `t`
    // being returned.
    match &mut obj_t.kind {
        TypeKind::Var(TVar { constraint, .. }) => match constraint {
            Some(constraint) => infer_property_type(constraint, prop, ctx, is_lvalue),
            None => Err(vec![TypeError::PossiblyNotAnObject(Box::from(
                obj_t.to_owned(),
            ))]),
        },
        TypeKind::Object(obj) => get_prop_value(obj, prop, ctx, is_lvalue, obj_t.mutable),
        TypeKind::Ref(_) => {
            let mut t = get_obj_type(obj_t, ctx)?;
            t.mutable = obj_t.mutable;
            infer_property_type(&mut t, prop, ctx, is_lvalue)
        }
        TypeKind::Lit(_) => {
            let mut t = get_obj_type(obj_t, ctx)?;
            infer_property_type(&mut t, prop, ctx, is_lvalue)
        }
        TypeKind::Keyword(_) => {
            let mut t = get_obj_type(obj_t, ctx)?;
            infer_property_type(&mut t, prop, ctx, is_lvalue)
        }
        TypeKind::Array(type_param) => {
            let type_param = type_param.clone();

            let mut t = get_obj_type(obj_t, ctx)?;
            t.mutable = obj_t.mutable;

            let (s, mut t) = infer_property_type(&mut t, prop, ctx, is_lvalue)?;

            // Replaces `this` with `mut <type_param>[]`
            let rep_t = Type {
                kind: TypeKind::Array(type_param),
                provenance: None,
                mutable: obj_t.mutable,
            };
            replace_this(&mut t, &rep_t);

            Ok((s, t))
        }
        TypeKind::Tuple(elem_types) => {
            // QUESTION: Why don't we need to call `replace_this` here as well?

            // If `prop` is a number literal then look up the index entry, if
            // not, treat it the same as a regular property look up on Array.
            match prop {
                // TODO: lookup methods on Array.prototype
                MemberProp::Ident(_) => {
                    let scheme = ctx.lookup_scheme("Array")?;

                    let mut type_param_map: HashMap<String, Type> = HashMap::new();
                    let type_param = Type::from(TypeKind::Union(elem_types.to_owned()));
                    if let Some(type_params) = scheme.type_params {
                        type_param_map.insert(type_params[0].name.to_owned(), type_param);
                    }

                    let mut t = replace_aliases_rec(&scheme.t, &type_param_map);
                    t.mutable = obj_t.mutable;

                    infer_property_type(&mut t, prop, ctx, is_lvalue)
                }
                MemberProp::Computed(ComputedPropName { expr, .. }) => {
                    let (prop_s, prop_t) = infer_expr(ctx, expr, false)?;

                    match &prop_t.kind {
                        TypeKind::Keyword(TKeyword::Number) => {
                            // TODO: remove duplicate types
                            let mut elem_types = elem_types.to_owned();
                            elem_types.push(Type::from(TypeKind::Keyword(TKeyword::Undefined)));
                            let t = Type::from(TypeKind::Union(elem_types));
                            Ok((prop_s, t))
                        }
                        TypeKind::Lit(types::TLit::Num(index)) => {
                            let index: usize = index.parse().unwrap();
                            match elem_types.get(index) {
                                Some(t) => Ok((prop_s, t.to_owned())),
                                None => Err(vec![TypeError::IndexOutOfBounds(
                                    Box::from(obj_t.to_owned()),
                                    Box::from(prop_t.to_owned()),
                                )]),
                            }
                        }
                        _ => Err(vec![TypeError::InvalidIndex(
                            Box::from(obj_t.to_owned()),
                            Box::from(prop_t.to_owned()),
                        )]),
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
    is_lvalue: bool,
    obj_is_mutable: bool,
) -> Result<(Subst, Type), Vec<TypeError>> {
    let elems = &obj.elems;

    match prop {
        MemberProp::Ident(Ident { name, .. }) => {
            for elem in elems {
                match elem {
                    types::TObjElem::Prop(prop) => {
                        // TODO: if `is_lvalue` is true and `obj_is_mutable`is false raise and error
                        if prop.name == TPropKey::StringKey(name.to_owned()) {
                            let t = get_property_type(prop);
                            return Ok((Subst::default(), t));
                        }
                    }
                    TObjElem::Getter(getter) if !is_lvalue => {
                        if getter.name == TPropKey::StringKey(name.to_owned()) {
                            return Ok((Subst::default(), getter.ret.as_ref().to_owned()));
                        }
                    }
                    TObjElem::Setter(setter) if is_lvalue && obj_is_mutable => {
                        if setter.name == TPropKey::StringKey(name.to_owned()) {
                            return Ok((Subst::default(), setter.param.t.to_owned()));
                        }
                    }
                    TObjElem::Method(method) if !is_lvalue => {
                        if method.is_mutating && !obj_is_mutable {
                            return Err(vec![TypeError::MissingKey(name.to_owned())]);
                        }

                        if method.name == TPropKey::StringKey(name.to_owned()) {
                            let t = if method.type_params.is_some() {
                                Type::from(TypeKind::GenLam(types::TGenLam {
                                    lam: Box::from(types::TLam {
                                        params: method.params.to_owned(),
                                        ret: method.ret.to_owned(),
                                    }),
                                    type_params: method.type_params.to_owned(),
                                }))
                            } else {
                                Type::from(TypeKind::Lam(types::TLam {
                                    params: method.params.to_owned(),
                                    ret: method.ret.to_owned(),
                                }))
                            };
                            return Ok((Subst::default(), t));
                        }
                    }
                    _ => (),
                }
            }

            Err(vec![TypeError::MissingKey(name.to_owned())])
        }
        MemberProp::Computed(ComputedPropName { expr, .. }) => {
            let (prop_s, prop_t) = infer_expr(ctx, expr, false)?;

            let mut prop_t_clone = prop_t.clone();
            let prop_s_clone = prop_s.clone();

            let result = match &prop_t.kind {
                TypeKind::Keyword(TKeyword::String) => {
                    let mut value_types: Vec<Type> = elems
                        .iter()
                        .filter_map(|elem| match elem {
                            // TODO: include index types in the future
                            types::TObjElem::Index(_) => todo!(),
                            types::TObjElem::Prop(prop) => {
                                // TODO: handle generic object properties
                                Some(prop.t.to_owned())
                            }
                            types::TObjElem::Getter(_) => todo!(),
                            types::TObjElem::Method(_) => todo!(),
                            _ => None,
                        })
                        .collect();

                    // We can't tell if the property is in the object or not because the
                    // key is a string whose exact value is unknown at compile time.
                    value_types.push(Type::from(TypeKind::Keyword(TKeyword::Undefined)));
                    let t = Type::from(TypeKind::Union(value_types));

                    Ok((prop_s, t))
                }
                // TODO: handle index types where the computed property key is
                // a number instead of string.
                TypeKind::Lit(types::TLit::Str(key)) => {
                    let prop = elems.iter().find_map(|elem| match elem {
                        // TODO: include index types in the future
                        types::TObjElem::Index(_) => None,
                        types::TObjElem::Prop(prop) => {
                            if prop.name == TPropKey::StringKey(key.to_owned()) {
                                Some(prop)
                            } else {
                                None
                            }
                        }
                        types::TObjElem::Getter(_) => todo!(),
                        types::TObjElem::Method(_) => todo!(),
                        _ => None,
                    });

                    match prop {
                        Some(prop) => {
                            // TODO: handle generic object properties
                            Ok((Subst::default(), prop.t.to_owned()))
                        }
                        None => Err(TypeError::MissingKey(key.to_owned())),
                    }
                }
                _ => Err(TypeError::InvalidKey(Box::from(prop_t.to_owned()))),
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
                        Err(vec![err])
                    } else {
                        for indexer in indexers {
                            let mut key_clone = indexer.key.t.clone();
                            let result = unify(&mut prop_t_clone, &mut key_clone, ctx);
                            if result.is_ok() {
                                let key_s = result?;
                                let s = compose_subs(&key_s, &prop_s_clone);
                                // TODO: handle generic indexers
                                // NOTE: Since access any indexer could result in an `undefined`
                                // we include `| undefined` in the return type here.
                                let undefined = Type::from(TypeKind::Keyword(TKeyword::Undefined));
                                let t = union_types(&indexer.t, &undefined);
                                return Ok((s, t));
                            }
                        }
                        Err(vec![TypeError::InvalidKey(Box::from(prop_t))])
                    }
                }
            }
        }
    }
}

struct ReplaceVisitor {
    rep: Type,
}

impl ReplaceVisitor {
    fn new(t: &Type) -> Self {
        ReplaceVisitor { rep: t.to_owned() }
    }
}

impl Visitor for ReplaceVisitor {
    fn visit_type(&mut self, t: &mut Type) {
        if let TypeKind::This = t.kind {
            t.kind = self.rep.kind.to_owned();
            t.mutable = self.rep.mutable;
            // TODO: set t.provenance to the original type's kind
        }
    }
}

fn replace_this(t: &mut Type, rep: &Type) {
    let mut rep_visitor = ReplaceVisitor::new(rep);
    rep_visitor.visit_children(t);
}
