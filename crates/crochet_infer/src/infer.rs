use std::collections::{HashMap, HashSet};
use std::convert::identity;

use crochet_ast::*;

use crate::substitutable::Substitutable;

use super::context::{Context, Env};
use super::infer_type_ann::*;
use super::substitutable::Subst;
use super::types::{self, freeze_scheme, Scheme, Type, Variant};
use super::util::{generalize, normalize};

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
                                match type_ann {
                                    Some(type_ann) => {
                                        let scheme = infer_scheme(type_ann, &ctx);
                                        ctx.values
                                            .insert(id.name.to_owned(), freeze_scheme(scheme));
                                    }
                                    None => {
                                        // A type annotation should always be provided when using `declare`
                                        return Err(String::from(
                                            "missing type annotation in declare statement",
                                        ));
                                    }
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    false => {
                        // An initial value should always be used when using a normal
                        // `let` statement
                        let init = init.as_ref().unwrap();

                        let (pa, s) =
                            infer_pattern_and_init(pattern, init, &ctx, &PatternUsage::Assign)?;

                        // Inserts the new variables from infer_pattern() into the
                        // current context.
                        for (name, scheme) in pa {
                            let scheme = normalize(&scheme.apply(&s), &ctx);
                            ctx.values.insert(name, freeze_scheme(scheme));
                        }
                    }
                };
            }
            Statement::TypeDecl {
                id,
                type_ann,
                type_params,
                ..
            } => {
                let scheme = infer_scheme_with_type_params(type_ann, type_params, &ctx);
                ctx.types.insert(id.name.to_owned(), freeze_scheme(scheme));
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

pub fn infer_expr(ctx: &Context, expr: &Expr) -> Result<Scheme, String> {
    let (s, t) = infer(ctx, expr)?;
    Ok(close_over(&s, &t, ctx))
}

// closeOver :: (Map.Map TVar Type, Type) -> Scheme
// closeOver (sub, ty) = normalize sc
//   where sc = generalize emptyTyenv (apply sub ty)
fn close_over(s: &Subst, t: &Type, ctx: &Context) -> Scheme {
    let empty_env = Env::default();
    normalize(&generalize(&empty_env, &t.to_owned().apply(s)), ctx)
}

enum PatternUsage {
    Assign,
    Match,
}

fn infer_pattern_and_init(
    pat: &Pattern,
    init: &Expr,
    ctx: &Context,
    pu: &PatternUsage,
) -> Result<(Assump, Subst), String> {
    let type_param_map = HashMap::new();
    let (ps, pa, pt) = infer_pattern(pat, ctx, &type_param_map)?;

    let (is, it) = infer(ctx, init)?;

    // Unifies initializer and pattern.
    let s = match pu {
        // Assign: The inferred type of the init value must be a sub-type
        // of the pattern it's being assigned to.
        PatternUsage::Assign => unify(&it, &pt, ctx, Some(Rel::Subtype))?,
        // Matching: The pattern must be a sub-type of the expression
        // it's being matched against
        PatternUsage::Match => unify(&pt, &it, ctx, Some(Rel::Subtype))?,
    };

    // infer_pattern can generate a non-empty Subst when the pattern includes
    // a type annotation.
    let s = compose_many_subs(&[is, ps, s]);

    Ok((pa.apply(&s), s))
}

fn infer_let(
    pat: &Pattern,
    init: &Expr,
    body: &Expr,
    ctx: &Context,
    pu: &PatternUsage,
) -> Result<(Subst, Type), String> {
    let mut new_ctx = ctx.clone();
    let (pa, s1) = infer_pattern_and_init(pat, init, &new_ctx, pu)?;

    // Inserts the new variables from infer_pattern() into the
    // current context.
    for (name, scheme) in pa {
        new_ctx.values.insert(name.to_owned(), scheme.to_owned());
    }

    let (s2, t2) = infer(&new_ctx, body)?;

    // Copies over the count from new_ctx so that it's unique across Contexts.
    ctx.state.count.set(new_ctx.state.count.get());

    let s = compose_subs(&s2, &s1);
    let t = t2;
    Ok((s, t))
}

fn infer(ctx: &Context, expr: &Expr) -> Result<(Subst, Type), String> {
    match expr {
        Expr::App(App { lam, args, .. }) => {
            let (s1, t1) = infer(ctx, lam)?;
            let (mut args_ss, args_ts): (Vec<_>, Vec<_>) =
                args.iter().filter_map(|arg| infer(ctx, arg).ok()).unzip();

            let tv = ctx.fresh_var();
            // Are we missing an `apply()` call here?
            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            let s3 = unify(&t1, &ctx.lam(args_ts, Box::from(tv.clone())), ctx, None)?;

            // ss = [s3, ...args_ss, s1]
            let mut ss = vec![s3.clone()];
            ss.append(&mut args_ss);
            ss.push(s1);

            // return (s3 `compose` s2 `compose` s1, apply s3 tv)
            Ok((compose_many_subs(&ss), tv.apply(&s3)))
            // TODO:
            // - partial application
            // - subtyping
        }
        Expr::Fix(Fix { expr, .. }) => {
            let (s1, t) = infer(ctx, expr)?;
            let tv = ctx.fresh_var();
            let s2 = unify(
                &ctx.lam(vec![tv.clone()], Box::from(tv.clone())),
                &t,
                ctx,
                None,
            )?;
            Ok((compose_subs(&s2, &s1), tv.apply(&s2)))
        }
        Expr::Ident(Ident { name, .. }) => {
            // TODO: return an error if the lookup fails
            let s = Subst::new();
            let t = ctx.lookup_value(name);
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
                        let (s2, t2) = infer(ctx, alternate)?;

                        let s = compose_many_subs(&[s1, s2]);
                        let t = union_types(&t1, &t2, ctx);
                        Ok((s, t))
                    }
                    _ => {
                        let (s1, t1) = infer(ctx, cond)?;
                        let (s2, t2) = infer(ctx, consequent)?;
                        let (s3, t3) = infer(ctx, alternate)?;
                        let s4 = unify(&t1, &ctx.prim(Primitive::Bool), ctx, Some(Rel::Subtype))?;

                        let s = compose_many_subs(&[s1, s2, s3, s4]);
                        let t = union_types(&t2, &t3, ctx);
                        Ok((s, t))
                    }
                }
            }
            None => match cond.as_ref() {
                Expr::LetExpr(LetExpr { pat, expr, .. }) => {
                    let (s1, t1) = infer_let(pat, expr, consequent, ctx, &PatternUsage::Match)?;
                    let s2 = match unify(&t1, &ctx.prim(Primitive::Undefined), ctx, None) {
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
                    let (s1, t1) = infer(ctx, cond)?;
                    let (s2, t2) = infer(ctx, consequent)?;
                    let s3 = unify(&t1, &ctx.prim(Primitive::Bool), ctx, Some(Rel::Subtype))?;
                    let s4 = match unify(&t2, &ctx.prim(Primitive::Undefined), ctx, None) {
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
        Expr::JSXElement(_) => todo!(),
        Expr::Lambda(Lambda {
            params,
            body,
            is_async: _,
            return_type,
            type_params,
            ..
        }) => {
            let mut new_ctx = ctx.clone();

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

            let (rs, rt) = infer(&new_ctx, body)?;

            // Copies over the count from new_ctx so that it's unique across Contexts.
            ctx.state.count.set(new_ctx.state.count.get());

            let s = match return_type {
                Some(return_type) => unify(
                    &rt,
                    &infer_type_ann_with_params(return_type, ctx, &type_params_map),
                    ctx,
                    None,
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
                    infer(ctx, body)
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
            let (s1, t1) = infer(ctx, left)?;
            let (s2, t2) = infer(ctx, right)?;
            let s3 = unify(&t1, &ctx.prim(Primitive::Num), ctx, Some(Rel::Subtype))?;
            let s4 = unify(&t2, &ctx.prim(Primitive::Num), ctx, Some(Rel::Subtype))?;
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
        Expr::Obj(Obj { props, .. }) => {
            let mut ss: Vec<Subst> = vec![];
            let mut ps: Vec<types::TProp> = vec![];
            for p in props {
                match p {
                    Prop::Shorthand(_) => {
                        // TODO: lookup the value in the current ctx
                        todo!()
                    }
                    Prop::KeyValue(KeyValueProp { name, value, .. }) => {
                        let (s, t) = infer(ctx, value)?;
                        ss.push(s);
                        // TODO: check if the inferred type is T | undefined and use that
                        // determine the value of optional
                        ps.push(ctx.prop(name, t, false));
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = ctx.object(ps);
            Ok((s, t))
        }
        Expr::Await(_) => todo!(),
        Expr::Tuple(Tuple { elems, .. }) => {
            let result: Result<Vec<(Subst, Type)>, String> =
                elems.iter().map(|elem| infer(ctx, elem)).collect();
            let (ss, ts): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();

            let s = compose_many_subs(&ss);
            let t = ctx.tuple(ts);
            Ok((s, t))
        }
        Expr::Member(_) => todo!(),
        Expr::Empty(_) => {
            let t = ctx.prim(Primitive::Undefined);
            let s = Subst::default();
            Ok((s, t))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Rel {
    Subtype,
    Supertype,
}

fn unify(t1: &Type, t2: &Type, ctx: &Context, rel: Option<Rel>) -> Result<Subst, String> {
    if rel == Some(Rel::Supertype) {
        return unify(t2, t1, ctx, Some(Rel::Subtype));
    }
    match (&t1.variant, &t2.variant) {
        (Variant::Var, _) => bind(&t1.id, t2),
        (_, Variant::Var) => bind(&t2.id, t1),
        (Variant::Lam(lam1), Variant::Lam(lam2)) => {
            let mut s = Subst::new();
            for (p1, p2) in lam1.params.iter().zip(&lam2.params) {
                let s1 = unify(&p1.apply(&s), &p2.apply(&s), ctx, None)?;
                s = compose_subs(&s, &s1);
            }
            let s1 = unify(&lam1.ret.apply(&s), &lam2.ret.apply(&s), ctx, None)?;
            Ok(compose_subs(&s, &s1))
        }
        (Variant::Prim(prim1), Variant::Prim(prim2)) => {
            if prim1 == prim2 {
                Ok(Subst::default())
            } else {
                Err(format!("{prim1} and {prim2} do not unify"))
            }
        }
        (Variant::Lit(lit1), Variant::Lit(lit2)) => {
            if lit1 == lit2 {
                Ok(Subst::default())
            } else {
                Err(format!("{lit1} and {lit2} do not unify"))
            }
        }
        (Variant::Tuple(tuple1), Variant::Tuple(tuple2)) => {
            if rel == Some(Rel::Subtype) && tuple1.len() < tuple2.len() {
                return Err(String::from("too many elements to unpack"));
            } else if rel == None && tuple1.len() != tuple2.len() {
                return Err(String::from("lengths of tuples do not match"));
            };
            let mut s = Subst::new();
            for (t1, t2) in tuple1.iter().zip(tuple2) {
                // TODO: report all errors instead of just the first error
                let s1 = unify(&t1.apply(&s), &t2.apply(&s), ctx, rel.clone())?;
                s = compose_subs(&s, &s1);
            }
            Ok(s)
        }
        (Variant::Object(obj1), Variant::Object(obj2)) => {
            let map1: HashMap<_, _> = obj1
                .iter()
                // TODO: handle optional properties
                .map(|p| (p.name.to_owned(), p.get_type(ctx)))
                .collect();
            let map2: HashMap<_, _> = obj2
                .iter()
                // TODO: handle optional properties
                .map(|p| (p.name.to_owned(), p.get_type(ctx)))
                .collect();

            let mut s = Subst::new();
            for (k, p2) in map2.iter() {
                match map1.get(k) {
                    Some(p1) => {
                        let s1 = unify(p1, p2, ctx, rel.clone())?;
                        s = compose_subs(&s, &s1);
                    }
                    None => return Err(format!("Property '{k}' missing in {t1}")),
                }
            }

            Ok(s)
        }
        (_, _) => {
            if rel == Some(Rel::Subtype) {
                let (b, s) = is_subtype(t1, t2, ctx)?;
                if b {
                    return Ok(s);
                }
            }
            println!("Unification failure: {t1} != {t2}");
            println!("rel = {:?}", rel);
            Err(String::from("Unification failure"))
        }
    }
}

fn bind(id: &i32, t: &Type) -> Result<Subst, String> {
    // | t == TVar a     = return nullSubst
    // | occursCheck a t = throwError $ InfiniteType a t
    // | otherwise       = return $ Map.singleton a t
    if &t.id == id {
        Ok(Subst::default())
    } else if occurs_check(id, t) {
        Err(String::from("InfiniteType"))
    } else {
        Ok(Subst::from([(id.to_owned(), t.to_owned())]))
    }
}

fn occurs_check(id: &i32, t: &Type) -> bool {
    t.ftv().contains(id)
}

fn compose_subs(s1: &Subst, s2: &Subst) -> Subst {
    let mut result: Subst = s2.iter().map(|(id, tv)| (*id, tv.apply(s1))).collect();
    result.extend(s1.to_owned());
    result
}

// subs are composed from left to right with ones to the right
// being applied to all of the ones to the left.
fn compose_many_subs(subs: &[Subst]) -> Subst {
    subs.iter()
        .fold(Subst::new(), |accum, next| compose_subs(&accum, next))
}

// If are multiple entries for the same type variable, this function merges
// them into a union type (simplifying the type if possible).
fn compose_subs_with_context(s1: &Subst, s2: &Subst, ctx: &Context) -> Subst {
    let mut result: Subst = s2.iter().map(|(i, t)| (*i, t.apply(s1))).collect();
    for (i, t) in s1 {
        match result.get(i) {
            Some(t1) => {
                let t = union_types(t, t1, ctx);
                result.insert(*i, t)
            }
            None => result.insert(*i, t.to_owned()),
        };
    }
    result
}

fn compose_many_subs_with_context(subs: &[Subst], ctx: &Context) -> Subst {
    subs.iter().fold(Subst::new(), |accum, next| {
        compose_subs_with_context(&accum, next, ctx)
    })
}

type Assump = HashMap<String, Scheme>;

// Everywhere we see contraints.push() we need to replace that we a call to `unify()`
// and we need to collect the Substs produced as part of the return value

// NOTE: The caller is responsible for inserting any new variables introduced
// into the appropriate context.
fn infer_pattern(
    pat: &Pattern,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Assump, Type), String> {
    // Keeps track of all of the variables the need to be introduced by this pattern.
    let mut new_vars: HashMap<String, Scheme> = HashMap::new();

    let pat_type = infer_pattern_rec(pat, ctx, &mut new_vars)?;

    // If the pattern had a type annotation associated with it, we infer type of the
    // type annotation and add a constraint between the types of the pattern and its
    // type annotation.
    match get_type_ann(pat) {
        Some(type_ann) => {
            let type_ann_ty = infer_type_ann_with_params(&type_ann, ctx, type_param_map);
            // Allowing type_ann_ty to be a subtype of pat_type because
            // only non-refutable patterns can have type annotations.
            let s = unify(&type_ann_ty, &pat_type, ctx, Some(Rel::Subtype))?;

            // Substs are applied to any new variables introduced.  This handles
            // the situation where explicit types have be provided for function
            // parameters.
            Ok((s.clone(), new_vars.apply(&s), type_ann_ty))
        }
        None => Ok((Subst::new(), new_vars, pat_type)),
    }
}

fn get_type_ann(pat: &Pattern) -> Option<TypeAnn> {
    match pat {
        Pattern::Ident(BindingIdent { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Rest(RestPat { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Object(ObjectPat { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Array(ArrayPat { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Lit(_) => None,
        Pattern::Is(_) => None,
    }
}

fn infer_pattern_rec(pat: &Pattern, ctx: &Context, assump: &mut Assump) -> Result<Type, String> {
    match pat {
        Pattern::Ident(BindingIdent { id, .. }) => {
            let tv = ctx.fresh_var();
            let scheme = Scheme::from(&tv);
            if assump.insert(id.name.to_owned(), scheme).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(tv)
        }
        Pattern::Lit(LitPat { lit, .. }) => Ok(ctx.lit(lit.to_owned())),
        Pattern::Is(IsPat { id, is_id, .. }) => {
            let ty = match is_id.name.as_str() {
                "string" => ctx.prim(types::Primitive::Str),
                "number" => ctx.prim(types::Primitive::Num),
                "boolean" => ctx.prim(types::Primitive::Bool),
                // The alias type will be used for `instanceof` of checks, but
                // only if the definition of the alias is an object type with a
                // `constructor` method.
                name => ctx.alias(name, None),
            };
            let scheme = generalize(&ctx.types, &ty);
            if assump.insert(id.name.to_owned(), scheme).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(ty)
        }
        Pattern::Rest(RestPat { arg, .. }) => infer_pattern_rec(arg.as_ref(), ctx, assump),
        Pattern::Array(ArrayPat { elems, .. }) => {
            let elems: Result<Vec<Type>, String> = elems
                .iter()
                .map(|elem| {
                    match elem {
                        Some(elem) => match elem {
                            Pattern::Rest(rest) => {
                                let rest_ty = infer_pattern_rec(rest.arg.as_ref(), ctx, assump)?;
                                Ok(ctx.rest(rest_ty))
                            }
                            _ => infer_pattern_rec(elem, ctx, assump),
                        },
                        None => {
                            // TODO: figure how to ignore gaps in the array
                            todo!()
                        }
                    }
                })
                .collect();

            Ok(ctx.tuple(elems?))
        }
        Pattern::Object(ObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let props: Vec<types::TProp> = props
                .iter()
                .filter_map(|prop| {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        ObjectPatProp::KeyValue(KeyValuePatProp { key, value }) => {
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            let value_type = infer_pattern_rec(value, ctx, assump).unwrap();

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: value_type,
                            })
                        }
                        ObjectPatProp::Assign(AssignPatProp { key, value: _, .. }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.
                            // TODO: handle default values

                            let tv = ctx.fresh_var();
                            let scheme = Scheme::from(&tv);
                            if assump.insert(key.name.to_owned(), scheme).is_some() {
                                todo!("return an error");
                            }

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: tv,
                            })
                        }
                        ObjectPatProp::Rest(rest) => {
                            // TypeScript doesn't support spreading/rest in types so instead we
                            // need to turn:
                            // {x, y, ...rest}
                            // into:
                            // {x: A, y: B} & C
                            // we also need some way to specify that C is an object type of some
                            // sort... maybe it'll just fall out when we trying to unify some other
                            // object type with an intersection type.
                            // essentially C = other_object_type - {x: A, y: B}
                            // TODO: panic if rest_opt_ty is not None, it means that the parser has
                            // failed to ensure that there's only one rest pattern in an object pattern

                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            rest_opt_ty = infer_pattern_rec(rest.arg.as_ref(), ctx, assump).ok();
                            None
                        }
                    }
                })
                .collect();

            let obj_type = ctx.object(props);

            match rest_opt_ty {
                Some(rest_ty) => Ok(ctx.intersection(vec![obj_type, rest_ty])),
                None => Ok(obj_type),
            }
        }
    }
}

// dedupe with constraint_solver.rs
fn flatten_types(ty: &Type) -> Vec<Type> {
    match &ty.variant {
        Variant::Union(types) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![ty.to_owned()],
    }
}

// dedupe with constraint_solver.rs
fn union_types(t1: &Type, t2: &Type, ctx: &Context) -> Type {
    let mut types: Vec<Type> = vec![];
    types.extend(flatten_types(t1));
    types.extend(flatten_types(t2));

    let types_set: HashSet<_> = types.iter().cloned().collect();

    let prim_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| matches!(ty.variant, Variant::Prim(_)))
        .collect();
    let lit_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| match &ty.variant {
            // Primitive types subsume corresponding literal types
            Variant::Lit(lit) => match lit {
                types::Lit::Num(_) => !prim_types.contains(&ctx.prim(Primitive::Num)),
                types::Lit::Bool(_) => !prim_types.contains(&ctx.prim(Primitive::Bool)),
                types::Lit::Str(_) => !prim_types.contains(&ctx.prim(Primitive::Str)),
                types::Lit::Null => !prim_types.contains(&ctx.prim(Primitive::Null)),
                types::Lit::Undefined => !prim_types.contains(&ctx.prim(Primitive::Undefined)),
            },
            _ => false,
        })
        .collect();
    let rest_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| !matches!(ty.variant, Variant::Prim(_) | Variant::Lit(_)))
        .collect();

    let mut types: Vec<_> = prim_types
        .iter()
        .chain(lit_types.iter())
        .chain(rest_types.iter())
        .cloned()
        .collect();
    types.sort_by_key(|k| k.id);

    if types.len() > 1 {
        ctx.union(types)
    } else {
        types[0].clone()
    }
}

// Returns true if t2 admits all values from t1.
fn is_subtype(t1: &Type, t2: &Type, ctx: &Context) -> Result<(bool, Subst), String> {
    match (&t1.variant, &t2.variant) {
        (Variant::Lit(lit), Variant::Prim(prim)) => {
            let b = matches!(
                (lit, prim),
                (types::Lit::Num(_), Primitive::Num)
                    | (types::Lit::Str(_), Primitive::Str)
                    | (types::Lit::Bool(_), Primitive::Bool)
            );
            let s = Subst::default();
            Ok((b, s))
        }
        (Variant::Object(props1), Variant::Object(props2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            let result: Result<Vec<_>, String> = props2
                .iter()
                .map(|prop2| {
                    let inner_result: Result<Vec<_>, String> = props1
                        .iter()
                        .map(|prop1| {
                            if prop1.name == prop2.name {
                                is_subtype(&prop1.get_type(ctx), &prop2.get_type(ctx), ctx)
                            } else {
                                Ok((false, Subst::default()))
                            }
                        })
                        .collect();
                    let mut b = false;
                    let mut ss = vec![];
                    for (ib, is) in inner_result? {
                        if ib {
                            ss.push(is);
                        }
                        b |= ib; // any
                    }
                    let s = compose_many_subs(&ss);
                    // TODO: we need to make sure that there are no properties in props1
                    // with this name
                    if !b && prop2.optional {
                        match props1.iter().find(|prop1| prop1.name == prop2.name) {
                            Some(_) => Ok((false, s)),
                            None => Ok((true, s)),
                        }
                    } else {
                        Ok((b, s))
                    }
                })
                .collect();
            let (bs, ss): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();
            Ok((bs.into_iter().all(identity), compose_many_subs(&ss)))
        }
        (Variant::Tuple(types1), Variant::Tuple(types2)) => {
            // It's okay if t1 has extra properties, but it has to have all of t2's properties.
            if types1.len() < types2.len() {
                panic!("t1 contain at least the same number of elements as t2");
            }
            let result: Result<Vec<_>, _> = types1
                .iter()
                .zip(types2.iter())
                .map(|(t1, t2)| is_subtype(t1, t2, ctx))
                .collect();
            let (bs, ss): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();
            Ok((bs.into_iter().all(identity), compose_many_subs(&ss)))
        }
        (Variant::Union(types), _) => {
            let result: Result<Vec<_>, _> =
                types.iter().map(|t1| is_subtype(t1, t2, ctx)).collect();
            let (bs, ss): (Vec<_>, Vec<_>) = result?.iter().cloned().unzip();
            Ok((
                bs.into_iter().all(identity),
                compose_many_subs_with_context(&ss, ctx),
            ))
        }
        (_, Variant::Union(types)) => {
            let result: Result<Vec<_>, _> =
                types.iter().map(|t2| is_subtype(t1, t2, ctx)).collect();

            let mut b = false;
            let mut ss = vec![];
            for (ib, is) in result? {
                if ib {
                    ss.push(is);
                }
                b |= ib; // any
            }
            let s = compose_many_subs(&ss);
            Ok((b, s))
        }
        (_, Variant::Alias(types::AliasType { name, .. })) => {
            match ctx.types.get(name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    is_subtype(t1, &scheme.ty, ctx)
                }
                None => panic!("Can't find alias '{name}' in context"),
            }
        }
        (Variant::Alias(types::AliasType { name, .. }), _) => {
            match ctx.types.get(name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    is_subtype(&scheme.ty, t2, ctx)
                }
                None => panic!("Can't find alias '{name}' in context"),
            }
        }
        (Variant::Var, _) => {
            let s = Subst::from([(t1.id, t2.to_owned())]);
            Ok((true, s))
        }
        (_, Variant::Var) => {
            let s = Subst::from([(t2.id, t1.to_owned())]);
            Ok((true, s))
        }
        (v1, v2) => {
            // TODO: add t1 => t2 to the Subst that's returned
            Ok((v1 == v2, Subst::new()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn num(val: &str) -> Lit {
        Lit::num(val.to_owned(), 0..0)
    }

    fn str(val: &str) -> Lit {
        Lit::str(val.to_owned(), 0..0)
    }

    fn bool(val: &bool) -> Lit {
        Lit::bool(val.to_owned(), 0..0)
    }

    #[test]
    fn literals_are_subtypes_of_corresponding_primitives() {
        let ctx = Context::default();

        let result = is_subtype(&ctx.lit(num("5")), &ctx.prim(Primitive::Num), &ctx);
        assert_eq!(result, Ok((true, Subst::default())));

        let result = is_subtype(&ctx.lit(str("hello")), &ctx.prim(Primitive::Str), &ctx);
        assert_eq!(result, Ok((true, Subst::default())));

        let result = is_subtype(&ctx.lit(bool(&true)), &ctx.prim(Primitive::Bool), &ctx);
        assert_eq!(result, Ok((true, Subst::default())));
    }

    #[test]
    fn object_subtypes() {
        let ctx = Context::default();

        let t1 = ctx.object(vec![
            types::TProp {
                name: String::from("foo"),
                optional: false,
                ty: ctx.lit(num("5")),
            },
            types::TProp {
                name: String::from("bar"),
                optional: false,
                ty: ctx.lit(bool(&true)),
            },
            // Having extra properties is okay
            types::TProp {
                name: String::from("baz"),
                optional: false,
                ty: ctx.prim(Primitive::Str),
            },
            // It's okay for qux to not appear in the subtype since
            // it's an optional property.
        ]);
        let t2 = ctx.object(vec![
            types::TProp {
                name: String::from("foo"),
                optional: false,
                ty: ctx.prim(Primitive::Num),
            },
            types::TProp {
                name: String::from("bar"),
                optional: true,
                ty: ctx.prim(Primitive::Bool),
            },
            types::TProp {
                name: String::from("qux"),
                optional: true,
                ty: ctx.prim(Primitive::Str),
            },
        ]);

        let result = is_subtype(&t1, &t2, &ctx);
        assert_eq!(result, Ok((true, Subst::default())));
    }

    // TODO: object subtype failure cases

    #[test]
    fn false_cases() {
        let ctx = Context::default();

        let result = is_subtype(&ctx.prim(Primitive::Num), &ctx.lit(num("5")), &ctx);

        assert_eq!(result, Ok((false, Subst::default())))
    }
}
