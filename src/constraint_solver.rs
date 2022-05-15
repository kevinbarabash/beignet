use super::context::Context;
use super::substitutable::*;
use super::types::*;

#[derive(Clone, Debug)]
pub struct Constraint {
    pub types: (Type, Type),
}

type Unifier = (Subst, Vec<Constraint>);

pub fn run_solve(cs: &[Constraint], ctx: &Context) -> Subst {
    let empty_subst = Subst::new();

    solver((empty_subst, cs.to_vec()), ctx)
}

fn solver(u: Unifier, ctx: &Context) -> Subst {
    let (su, cs) = u;

    if cs.is_empty() {
        return su;
    }

    let rest = &cs[1..]; // const [_ , ...rest] = cs;

    // println!("-----------");
    // println!("constraints:");
    // for Constraint {types: (left, right)} in cs.iter() {
    //     println!("{left} = {right}")
    // }

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

    match (&t1.kind, &t2.kind) {
        (TypeKind::Var, _) => bind(&t1.id, &t2, ctx),
        (_, TypeKind::Var) => bind(&t2.id, &t1, ctx),
        (TypeKind::Prim(prim1), TypeKind::Prim(prim2)) if prim1 == prim2 => Subst::new(),
        (TypeKind::Lit(lit1), TypeKind::Lit(lit2)) if lit1 == lit2 => Subst::new(),
        (TypeKind::Lam(lam1), TypeKind::Lam(lam2)) => unify_lams(&lam1, &lam2, ctx),
        // TODO: copy tests case from `compiler` project for this
        (
            TypeKind::Alias {
                name: name1,
                type_params: vars1,
            },
            TypeKind::Alias {
                name: name2,
                type_params: vars2,
            },
        ) if name1 == name2 => {
            // TODO: throw if vars1 and vars2 have different lengths
            let cs: Vec<_> = vars1
                .iter()
                .zip(vars2)
                .map(|(a, b)| Constraint {
                    types: (a.clone(), b.clone()),
                })
                .collect();
            unify_many(&cs, ctx)
        }

        _ => {
            if is_subtype(&t1, &t2) {
                return Subst::new();
            }

            if !t1.frozen && !t2.frozen {
                return widen_types(&t1, &t2, ctx);
            }

            panic!("unification failed")
        }
    }
}

fn unify_lams(t1: &TLam, t2: &TLam, ctx: &Context) -> Subst {
    // TODO:
    // - varargs
    // - subtyping

    // Partial application
    if t1.args.len() < t2.args.len() {
        let t2_partial = TLam {
            args: t2.args[..t1.args.len()].to_vec(),
            ret: Box::from(ctx.from_lam(TLam {
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
    match (&t1.kind, &t2.kind) {
        (TypeKind::Lit(Lit::Num(_)), TypeKind::Prim(Primitive::Num)) => true,
        (TypeKind::Lit(Lit::Str(_)), TypeKind::Prim(Primitive::Str)) => true,
        (TypeKind::Lit(Lit::Bool(_)), TypeKind::Prim(Primitive::Bool)) => true,
        _ => false,
    }
}

fn bind(tv_id: &i32, ty: &Type, _: &Context) -> Subst {
    // TODO: Handle Type::Mem once it's added
    // NOTE: This will require the use of the &Context

    match ty {
        Type {
            id,
            kind: TypeKind::Var,
            ..
        } if id == tv_id => Subst::new(),
        ty if ty.ftv().contains(tv_id) => panic!("type var appears in type"),
        ty => {
            let mut subst = Subst::new();
            subst.insert(tv_id.to_owned(), ty.clone());
            subst
        }
    }
}

fn widen_types(t1: &Type, t2: &Type, ctx: &Context) -> Subst {
    let mut result = Subst::new();

    let mut types: Vec<Type> = vec![];
    match &t1.kind {
        TypeKind::Union(t1_types) => types.extend(t1_types.to_owned()),
        _ => types.push(t1.to_owned()),
    }
    match &t2.kind {
        TypeKind::Union(t2_types) => types.extend(t2_types.to_owned()),
        _ => types.push(t2.to_owned()),
    }
    let union = ctx.union(&types);

    result.insert(t1.id, union.clone());
    result.insert(t2.id, union.clone());

    result
}
