use std::collections::HashMap;

use crochet_ast::types::{
    self as types, Provenance, TFnParam, TKeyword, TObjElem, TObject, TPat, TPropKey, TVar, Type,
    TypeKind,
};
use crochet_ast::values::*;

use crate::context::Context;
use crate::expand_type::get_obj_type;
use crate::infer_fn_param::infer_fn_param;
use crate::infer_pattern::*;
use crate::infer_type_ann::*;
use crate::scheme::instantiate_callable;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::unify::unify;
use crate::update::update_pattern;
use crate::util::*;
use crate::visitor::Visitor;

pub fn infer_expr(ctx: &mut Context, expr: &mut Expr) -> Result<(Subst, Type), Vec<TypeError>> {
    let result = match &mut expr.kind {
        ExprKind::App(App { lam, args, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut arg_types: Vec<Type> = vec![];

            let (s1, mut lam_type) = infer_expr(ctx, lam)?;
            ss.push(s1);

            for arg in args {
                let (arg_s, mut arg_t) = infer_expr(ctx, &mut arg.expr)?;
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
            // Are we missing an `apply()` call here?
            // Maybe, I could see us needing an apply to handle generic functions properly
            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            let mut call_type = Type::from(TypeKind::App(types::TApp {
                args: arg_types,
                ret: Box::from(ret_type.clone()),
            }));
            call_type.provenance = Some(Box::from(Provenance::Expr(Box::from(expr.to_owned()))));

            let s3 = unify(&mut call_type, &mut lam_type, ctx)?;

            ss.push(s3);

            let s = compose_many_subs(&ss);
            ret_type.apply(&s);

            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
            Ok((s, ret_type))
        }
        ExprKind::New(New { expr, args }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut arg_types: Vec<Type> = vec![];

            let (s1, t) = infer_expr(ctx, expr)?;
            ss.push(s1);
            let t = get_obj_type(&t, ctx)?;

            for arg in args {
                let (arg_s, mut arg_t) = infer_expr(ctx, &mut arg.expr)?;
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
            if let TypeKind::Object(TObject { elems }) = t.kind {
                for elem in elems {
                    match &elem {
                        TObjElem::Call(_) => (),
                        TObjElem::Constructor(callable) => {
                            let mut ret_type = ctx.fresh_var();
                            let mut call_type = Type::from(TypeKind::App(types::TApp {
                                args: arg_types.clone(),
                                ret: Box::from(ret_type.clone()),
                            }));

                            // We generalize first b/c lam_type isn't generic and
                            // we need it to be before we can instantiate it.
                            let mut lam_type = instantiate_callable(ctx, callable);
                            // let t = generalize(&Env::default(), &lam_type);
                            // let mut lam_type = ctx.instantiate(&t);
                            lam_type.provenance =
                                Some(Box::from(Provenance::TObjElem(Box::from(elem.to_owned()))));

                            if let Ok(s3) = unify(&mut call_type, &mut lam_type, ctx) {
                                ss.push(s3);

                                let s = compose_many_subs(&ss.clone());
                                ret_type.apply(&s);

                                // return (s3 `compose` s2 `compose` s1, apply s3 tv)
                                results.push((s, ret_type));
                            }
                        }
                        TObjElem::Index(_) => (),
                        TObjElem::Prop(_) => (),
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
            let (s1, mut t) = infer_expr(ctx, expr)?;
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
                        let (s2, t2) = infer_expr(ctx, alternate)?;

                        let s = compose_many_subs(&[s1, s2]);
                        let t = union_types(&t1, &t2);
                        Ok((s, t))
                    }
                    _ => {
                        let (s1, mut t1) = infer_expr(ctx, cond)?;
                        let (s2, t2) = infer_expr(ctx, consequent)?;
                        let (s3, t3) = infer_expr(ctx, alternate)?;
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
                    let (s1, mut t1) = infer_expr(ctx, cond)?;
                    let (s2, t2) = infer_expr(ctx, consequent)?;
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
                                    infer_expr(ctx, &mut expr)?
                                }
                                JSXAttrValue::JSXExprContainer(JSXExprContainer {
                                    expr, ..
                                }) => infer_expr(ctx, expr)?,
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
                            args: vec![Type::from(TypeKind::Object(TObject { elems }))],
                            ret: Box::from(ret_type.clone()),
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
                Some(params) => params
                    .iter_mut()
                    .map(|param| {
                        let tv = match &mut param.constraint {
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
                        ctx.insert_type(param.name.name.clone(), tv.clone());
                        Ok((param.name.name.to_owned(), tv))
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

            let (body_s, mut body_t) = infer_expr(ctx, body)?;
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

            t_params.apply(&s);
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
                let (init_s, init_t) = infer_expr(ctx, init)?;

                if init_t.kind != TypeKind::Keyword(TKeyword::Undefined) {
                    eprintln!("WARNING: {init_t} was not assigned");
                }

                let (body_s, mut body_t) = infer_expr(ctx, body)?;

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
            let (rs, mut rt) = infer_expr(ctx, &mut assign.right)?;
            let (ls, mut lt) = infer_expr(ctx, &mut assign.left)?;

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
            let (s1, mut t1) = infer_expr(ctx, left)?;
            let (s2, mut t2) = infer_expr(ctx, right)?;
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
            let (s1, mut t1) = infer_expr(ctx, arg)?;
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
                                let (s, t) = infer_expr(ctx, value)?;
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
                        let (s, t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        spread_types.push(t);
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = if spread_types.is_empty() {
                Type::from(TypeKind::Object(TObject { elems }))
            } else {
                let mut all_types = spread_types;
                all_types.push(Type::from(TypeKind::Object(TObject { elems })));
                simplify_intersection(&all_types)
            };

            Ok((s, t))
        }
        ExprKind::Await(Await { expr, .. }) => {
            if !ctx.is_async() {
                return Err(vec![TypeError::AwaitOutsideOfAsync]);
            }

            let (s1, mut t1) = infer_expr(ctx, expr)?;
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
                        let (s, mut t) = infer_expr(ctx, expr)?;
                        ss.push(s);
                        match &mut t.kind {
                            TypeKind::Tuple(types) => ts.append(types),
                            _ => {
                                return Err(vec![TypeError::TupleSpreadOutsideTuple]);
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
            let t = Type::from(TypeKind::Tuple(ts));

            Ok((s, t))
        }
        ExprKind::Member(Member { obj, prop, .. }) => {
            let (obj_s, mut obj_t) = infer_expr(ctx, obj)?;
            let (prop_s, prop_t) = infer_property_type(&mut obj_t, prop, ctx)?;

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
            let result: Result<Vec<(Subst, Type)>, Vec<TypeError>> =
                exprs.iter_mut().map(|expr| infer_expr(ctx, expr)).collect();
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

    update_pattern(pat, &s1);

    let (s2, t2) = infer_expr(ctx, body)?;

    ctx.pop_scope();

    let s = compose_subs(&s2, &s1);
    let t = t2;
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
) -> Result<(Subst, Type), Vec<TypeError>> {
    match &mut obj_t.kind {
        TypeKind::Var(TVar { constraint, .. }) => match constraint {
            Some(constraint) => infer_property_type(constraint, prop, ctx),
            None => Err(vec![TypeError::PossiblyNotAnObject(Box::from(
                obj_t.to_owned(),
            ))]),
        },
        TypeKind::Object(obj) => get_prop_value(obj, prop, ctx),
        TypeKind::Ref(_) => {
            let mut t = get_obj_type(obj_t, ctx)?;
            infer_property_type(&mut t, prop, ctx)
        }
        TypeKind::Lit(_) => {
            let mut t = get_obj_type(obj_t, ctx)?;
            infer_property_type(&mut t, prop, ctx)
        }
        TypeKind::Keyword(_) => {
            let mut t = get_obj_type(obj_t, ctx)?;
            infer_property_type(&mut t, prop, ctx)
        }
        TypeKind::Array(type_param) => {
            let type_param = type_param.clone();

            let mut t = get_obj_type(obj_t, ctx)?;
            let (s, mut t) = infer_property_type(&mut t, prop, ctx)?;

            // Replaces `this` with `mut <type_param>[]`
            let rep_t = Type {
                kind: TypeKind::Array(type_param),
                provenance: None,
                mutable: true,
            };
            replace_this(&mut t, &rep_t);

            Ok((s, t))
        }
        TypeKind::Tuple(elem_types) => {
            // If `prop` is a number literal then look up the index entry, if
            // not, treat it the same as a regular property look up on Array.
            match prop {
                // TODO: lookup methods on Array.prototype
                MemberProp::Ident(_) => {
                    let name = if obj_t.mutable {
                        "Array"
                    } else {
                        "ReadonlyArray"
                    };
                    let scheme = ctx.lookup_scheme(name)?;

                    let mut type_param_map: HashMap<String, Type> = HashMap::new();
                    let type_param = Type::from(TypeKind::Union(elem_types.to_owned()));
                    type_param_map.insert(scheme.type_params[0].name.to_owned(), type_param);

                    let mut t = replace_aliases_rec(&scheme.t, &type_param_map);

                    infer_property_type(&mut t, prop, ctx)
                }
                MemberProp::Computed(ComputedPropName { expr, .. }) => {
                    let (prop_s, prop_t) = infer_expr(ctx, expr)?;

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
) -> Result<(Subst, Type), Vec<TypeError>> {
    let elems = &obj.elems;
    match prop {
        MemberProp::Ident(Ident { name, .. }) => {
            let prop = elems.iter().find_map(|elem| match elem {
                types::TObjElem::Call(_) => None,
                types::TObjElem::Constructor(_) => None,
                types::TObjElem::Index(_) => None,
                types::TObjElem::Prop(prop) => {
                    if prop.name == TPropKey::StringKey(name.to_owned()) {
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
                None => Err(vec![TypeError::MissingKey(name.to_owned())]),
            }
        }
        MemberProp::Computed(ComputedPropName { expr, .. }) => {
            let (prop_s, prop_t) = infer_expr(ctx, expr)?;

            let mut prop_t_clone = prop_t.clone();
            let prop_s_clone = prop_s.clone();

            let result = match &prop_t.kind {
                TypeKind::Keyword(TKeyword::String) => {
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
                    value_types.push(Type::from(TypeKind::Keyword(TKeyword::Undefined)));
                    let t = Type::from(TypeKind::Union(value_types));

                    Ok((prop_s, t))
                }
                TypeKind::Lit(types::TLit::Str(key)) => {
                    let prop = elems.iter().find_map(|elem| match elem {
                        types::TObjElem::Call(_) => None,
                        types::TObjElem::Constructor(_) => None,
                        types::TObjElem::Index(_) => None,
                        types::TObjElem::Prop(prop) => {
                            if prop.name == TPropKey::StringKey(key.to_owned()) {
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
