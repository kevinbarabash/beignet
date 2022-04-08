use super::context::Context;
use super::types::*;

#[derive(Clone)]
pub struct Constraint {
    pub types: (Type, Type),
}

type Unifier = (Subst, Vec<Constraint>);

pub fn run_solve(cs: &Vec<Constraint>, ctx: &Context) -> Subst {
    let empty_subst = Subst::new();

    solver((empty_subst, cs.clone()), ctx)
}

fn solver(u: Unifier, ctx: &Context) -> Subst {
    let (su, cs) = u;

    if cs.len() == 0 {
        return su;
    }

    let rest = &cs[1..]; // const [_ , ...rest] = cs;
    rest.iter();

    match cs.get(0) {
        Some(c) => {
            let su1 = unifies(c, ctx);
            // TODO: can we create an impl of Substitutable for slices (or iterators)?
            // TODO: implement ftv() and apply() on Vec<Constraint>
            let unifier: Unifier = (compose_subs(&su1, &su), Vec::from(rest).apply(&su));
            solver(unifier, ctx)
        }
        None => su,
    }
}
fn compose_subs(s1: &Subst, s2: &Subst) -> Subst {
    s2.iter()
        .map(|(id, tv)| (id.clone(), tv.apply(s1)))
        .collect()
}

fn unifies(c: &Constraint, ctx: &Context) -> Subst {
    let (t1, t2) = c.types.clone();

    match (t1, t2) {
        (Type::Var(tv), ty) => bind(&tv, &ty, ctx),
        (ty, Type::Var(tv)) => bind(&tv, &ty, ctx),
        (Type::Prim(prim1), Type::Prim(prim2)) if prim1 == prim2 => Subst::new(),
        (Type::Lit(lit1), Type::Lit(lit2)) if lit1 == lit2 => Subst::new(),
        (Type::Fun(fun1), Type::Fun(fun2)) => unify_funcs(&fun1, &fun2, ctx),

        _ => panic!("unification failed"),
    }
}

fn unify_funcs(t1: &TFun, t2: &TFun, ctx: &Context) -> Subst {
    // TODO: 
    // - varargs
    // - subtyping

    // Partial application
    if t1.args.len() < t2.args.len() {
        let t2_partial = TFun {
            args: t2.args[..t1.args.len()].to_vec(),
            ret: Box::from(Type::from(TFun {
                args: t2.args[t1.args.len()..].to_vec(),
                ret: t2.ret.clone(),
            })),
        };
        let mut cs: Vec<_> = t1
            .args
            .iter()
            .zip(&t2_partial.args)
            .map(|(a, b)| Constraint {
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {types: (*t1.ret.clone(), *t2_partial.ret)});
        unify_many(&cs, ctx)
    } else if t1.args.len() != t2.args.len() {
        panic!("Unification mismatch: arity mismatch");
    } else {
        let mut cs: Vec<_> = t1.args
            .iter()
            .zip(&t2.args)
            .map(|(a, b)| Constraint{
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {types: (*t1.ret.clone(), *t2.ret.clone())});
        unify_many(&cs, ctx)
    }
}

fn unify_many(cs: &[Constraint], ctx: &Context) -> Subst {
    match cs {
        [head, tail @ ..] => {
            let su_1 = unifies(head, ctx);
            let su_2 = unify_many(&tail.to_vec().apply(&su_1), ctx);
            compose_subs(&su_2, &su_1)
        }
        _ => Subst::new(),
    }
}

fn bind(tv: &TVar, ty: &Type, _: &Context) -> Subst {
    // TODO: Handle Type::Mem once it's added
    // NOTE: This will require the use of the &Context

    if let Type::Var(TVar { id, .. }) = ty {
        if id == &tv.id {
            return Subst::new();
        } else if ty.ftv().contains(tv) {
            panic!("type var appears in type");
        } else {
            let mut subst = Subst::new();
            subst.insert(tv.id, ty.clone());
            return subst;
        }
    } else if ty.ftv().contains(tv) {
        panic!("type var appears in type");
    } else {
        let mut subst = Subst::new();
        subst.insert(tv.id, ty.clone());
        return subst;
    }
}
