use super::context::Context;
use super::literal::Literal;
use super::substitutable::*;
use super::types::*;

#[derive(Clone, Debug)]
pub struct Constraint {
    pub types: (Type, Type),
}

type Unifier = (Subst, Vec<Constraint>);

pub fn run_solve(cs: &[Constraint], ctx: &Context) -> Subst {
    let empty_subst = Subst::new();

    // TODO: normalize the result so that type params start at a1
    solver((empty_subst, cs.to_vec()), ctx)
}

fn solver(u: Unifier, ctx: &Context) -> Subst {
    let (su, cs) = u;

    if cs.is_empty() {
        return su;
    }

    let rest = &cs[1..]; // const [_ , ...rest] = cs;

    match cs.get(0) {
        Some(c) => {
            // su1 represents new substitutions from unifying the first constraint in cs.
            let su1 = unifies(c, ctx);
            // This is applied to the remaining constraints (rest).  compose_subs is used
            // to apply su1 to the HashMap of subsitutions, su.
            let unifier: Unifier = (compose_subs(&su1, &su), Vec::from(rest).apply(&su1));
            solver(unifier, ctx)
        }
        None => su,
    }
}
fn compose_subs(s1: &Subst, s2: &Subst) -> Subst {
    let mut result: Subst = s2.iter().map(|(id, tv)| (*id, tv.apply(s1))).collect();
    result.extend(s1.to_owned());
    result
}

fn unifies(c: &Constraint, ctx: &Context) -> Subst {
    let (t1, t2) = c.types.clone();

    // TODO: use references for t1 and t2 here
    match (t1.clone(), t2.clone()) {
        (Type::Var(tv), ty) => bind(&tv, &ty, ctx),
        (ty, Type::Var(tv)) => bind(&tv, &ty, ctx),
        (Type::Prim(prim1), Type::Prim(prim2)) if prim1 == prim2 => Subst::new(),
        (Type::Lit(lit1), Type::Lit(lit2)) if lit1 == lit2 => Subst::new(),
        (Type::Lam(lam1), Type::Lam(lam2)) => unify_lams(&lam1, &lam2, ctx),

        _ => {
            if is_subtype(&t1, &t2) {
                return Subst::new()
            }

            panic!("unification failed")   
        },
    }
}

fn unify_lams(t1: &TLam, t2: &TLam, ctx: &Context) -> Subst {
    // println!("unify_lams");
    // println!("t1 = {t1}, t2 = {t2}");
    // TODO:
    // - varargs
    // - subtyping

    // Partial application
    if t1.args.len() < t2.args.len() {
        let t2_partial = TLam {
            args: t2.args[..t1.args.len()].to_vec(),
            ret: Box::from(Type::from(TLam {
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
        cs.push(Constraint {
            types: (*t1.ret.clone(), *t2_partial.ret),
        });
        unify_many(&cs, ctx)
    } else if t1.args.len() != t2.args.len() {
        panic!("Unification mismatch: arity mismatch");
    } else {
        let mut cs: Vec<_> = t1
            .args
            .iter()
            .zip(&t2.args)
            .map(|(a, b)| Constraint {
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {
            types: (*t1.ret.clone(), *t2.ret.clone()),
        });
        unify_many(&cs, ctx)
    }
}

fn unify_many(cs: &[Constraint], ctx: &Context) -> Subst {
    // println!("rununify_many_solve:");
    // for Constraint { types: (left, right) } in cs {
    //     println!("{left} = {right}");
    // }
    match cs {
        [head, tail @ ..] => {
            let su_1 = unifies(head, ctx);
            let su_2 = unify_many(&tail.to_vec().apply(&su_1), ctx);
            compose_subs(&su_2, &su_1)
        }
        _ => Subst::new(),
    }
}

fn is_subtype(t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Lit(Literal::Num(_)), Type::Prim(Primitive::Num)) => true,
        (Type::Lit(Literal::Str(_)), Type::Prim(Primitive::Str)) => true,
        (Type::Lit(Literal::Bool(_)), Type::Prim(Primitive::Bool)) => true,
        _ => false
    }
}

fn bind(tv: &TVar, ty: &Type, _: &Context) -> Subst {
    // TODO: Handle Type::Mem once it's added
    // NOTE: This will require the use of the &Context

    match ty {
        Type::Var(TVar { id, .. }) if id == &tv.id => Subst::new(),
        ty if ty.ftv().contains(tv) => panic!("type var appears in type"),
        ty => {
            let mut subst = Subst::new();
            subst.insert(tv.id, ty.clone());
            subst
        }
    }
}
