use super::constraint_solver::{run_solve, Constraint};
use super::types::*;

use std::collections::HashSet;
use std::iter::Iterator;

// TODO: adopt immutable collections: https://github.com/bodil/im-rs
// this will save on copying data when cloning of collections is required

// TODO: make an autoconvert trait to convert TVar to Type when need be

impl Substitutable<Constraint> for Constraint {
    fn apply(&self, sub: &Subst) -> Constraint {
        Constraint {
            types: (self.types.0.apply(sub), self.types.1.apply(sub)),
        }
    }
    fn ftv(&self) -> HashSet<TVar> {
        let mut result = HashSet::new();
        result.extend(self.types.0.ftv());
        result.extend(self.types.1.ftv());
        result
    }
}

impl Substitutable<Vec<Constraint>> for Vec<Constraint> {
    fn apply(&self, sub: &Subst) -> Vec<Constraint> {
        self.iter().map(|c| c.apply(sub)).collect()
    }
    fn ftv(&self) -> HashSet<TVar> {
        let mut result = HashSet::new();
        for c in self {
            result.extend(c.ftv());
        }
        result
    }
}

impl Substitutable<Type> for Type {
    fn apply(&self, sub: &Subst) -> Type {
        // TODO: sub.get(self.id) ?? self.clone();
        match self {
            Type::Var(TVar { .. }) => self.clone(),
            Type::Fun(TFun { args, ret }) => Type::Fun(TFun {
                args: args.iter().map(|arg| arg.apply(sub)).collect(),
                ret: Box::from(ret.apply(sub)),
            }),
            Type::Prim(_) => self.clone(),
            Type::Lit(_) => self.clone(),
        }
    }
    fn ftv(&self) -> HashSet<TVar> {
        match self {
            Type::Var(tv) => HashSet::from([tv.clone()]),
            Type::Fun(TFun { args, ret }) => {
                let mut result = ret.ftv();
                for arg in args {
                    result.extend(arg.ftv());
                }
                result
            }
            Type::Prim(_) => HashSet::new(),
            Type::Lit(_) => HashSet::new(),
        }
    }
}

impl Substitutable<Vec<Type>> for Vec<Type> {
    fn apply(&self, sub: &Subst) -> Vec<Type> {
        self.iter().map(|c| c.apply(sub)).collect()
    }
    fn ftv(&self) -> HashSet<TVar> {
        let mut result = HashSet::new();
        for c in self {
            result.extend(c.ftv());
        }
        result
    }
}

impl Substitutable<Scheme> for Scheme {
    fn apply(&self, sub: &Subst) -> Scheme {
        Scheme {
            qualifiers: self.qualifiers.clone(),
            ty: self.ty.apply(sub),
        }
    }
    fn ftv(&self) -> HashSet<TVar> {
        let qualifiers: HashSet<_> = self.qualifiers.iter().cloned().collect();
        self.ty.ftv().difference(&qualifiers).cloned().collect()
    }
}

impl Substitutable<Env> for Env {
    fn apply(&self, sub: &Subst) -> Env {
        self.iter()
            .map(|(a, b)| (a.clone(), b.apply(sub)))
            .collect()
    }
    fn ftv(&self) -> HashSet<TVar> {
        // we can't use iter_values() here because it's a consuming iterator
        self.iter().flat_map(|(_, b)| b.ftv()).collect()
    }
}

use crate::context::{Context, Env};
use crate::syntax::{BindingIdent, Expr};

pub fn infer_expr(env: Env, expr: &Expr) -> Scheme {
    let init_ctx = Context::from(env);
    let (ty, cs) = infer(expr, &init_ctx);
    let subs = run_solve(&cs, &init_ctx);

    close_over(&ty.apply(&subs))
}

fn close_over(ty: &Type) -> Scheme {
    let empty_env = Env::new();
    generalize(&empty_env, ty)
}

fn generalize(env: &Env, ty: &Type) -> Scheme {
    let quals: HashSet<_> = ty.ftv().difference(&env.ftv()).cloned().collect();
    Scheme {
        qualifiers: quals.into_iter().collect(),
        ty: ty.clone(),
    }
}

type InferResult = (Type, Vec<Constraint>);

fn infer(expr: &Expr, ctx: &Context) -> InferResult {
    match expr {
        Expr::Ident(name) => {
            let ty = ctx.lookup_env(name);
            (ty, vec![])
        }
        Expr::App(func, args) => {
            let (t_fn, cs_fn) = infer(func, ctx);
            let (t_args, cs_args) = infer_many(args, ctx);
            let tv = ctx.fresh();

            let mut constraints = Vec::new();
            constraints.extend(cs_fn);
            constraints.extend(cs_args);
            constraints.push(Constraint {
                types: (
                    Type::from(TFun {
                        args: t_args,
                        ret: Box::new(Type::from(tv.clone())),
                    }),
                    t_fn,
                ),
            });

            (Type::from(tv), constraints)
        }
        Expr::Lam(args, body, _ /* isAsync */) => {
            // Creates a new type variable for each arg
            let arg_tvs: Vec<_> = args.iter().map(|_| Type::Var(ctx.fresh())).collect();
            let mut new_ctx = ctx.clone();
            for (arg, tv) in args.iter().zip(arg_tvs.clone().into_iter()) {
                let scheme = Scheme {
                    qualifiers: vec![],
                    ty: tv.clone(),
                };
                match arg {
                    BindingIdent::Ident(name) => new_ctx.env.insert(name.to_string(), scheme),
                    BindingIdent::Rest(name) => new_ctx.env.insert(name.to_string(), scheme),
                };
            }
            let (ret_ty, cs) = infer(body, &new_ctx);
            let fun_ty = Type::Fun(TFun {
                args: arg_tvs,
                ret: Box::new(ret_ty),
            });

            (fun_ty, cs)
        }
        Expr::Lit(lit) => (Type::from(lit), vec![]),
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
