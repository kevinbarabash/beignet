use super::id::ID;
use super::lit::Lit;
use super::prim::Prim;
use super::r#type::*;
use super::subst::*;

#[derive(PartialEq, Eq)]
pub enum Rel {
    Sub,
    Sup,
    Exact,
}

// mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
//                                s2 <- mgu (apply s1 r) (apply s1 r')
//                                return (s2 @@ s1)
// mgu (TVar u) t        = varBind u t
// mgu t (TVar u)        = varBind u t
// mgu (TCon tc1) (TCon tc2)
//            | tc1==tc2 = return nullSubst
// mgu t1 t2             = fail "types do not unify"
pub fn mgu(t1: &Type, t2: &Type, rel: Rel) -> Subst {
    if rel == Rel::Sup {
        return mgu(t2, t1, Rel::Sub);
    }

    match (t1, t2) {
        (Type::Var(u), t) | (t, Type::Var(u)) => var_bind(u, t),
        (Type::Lam(arg1, ret1), Type::Lam(arg2, ret2)) => {
            // TODO: tag the types so that we can differentiate between a
            // Lam that's coming from the decl vs. one that's coming from
            // a call (application).
            let s1 = mgu(arg1, arg2, Rel::Exact);
            let s2 = mgu(&ret1.apply(&s1), &ret2.apply(&s1), Rel::Exact);
            at_at(&s2, &s1)
        }
        (Type::Lit(lit1), Type::Lit(lit2)) if lit1 == lit2 => Subst::new(),
        (Type::Prim(prim1), Type::Prim(prim2)) if prim1 == prim2 => Subst::new(),
        (Type::Lit(lit), Type::Prim(prim)) if rel == Rel::Sub => {
            match (lit, prim) {
                (Lit::Num(_), Prim::Num) => Subst::new(),
                (Lit::Str(_), Prim::Str) => Subst::new(),
                (Lit::Bool(_), Prim::Bool) => Subst::new(),
                _ => panic!("types do not unify"),
            }
        }
        // TODO: support more data types
        _ => panic!("types do not unify"),
    }
}

// varBind u t | t == TVar u      = return nullSubst
//             | u `elem` tv t    = fail "occurs check fails"
//             | kind u /= kind t = fail "kinds do not match"
//             | otherwise        = return (u +-> t)
// NOTE: the `kind` check is omitted since Kind is incompatible with Crochet's type system
fn var_bind(id: &ID, t: &Type) -> Subst {
    if *t == Type::Var(id.to_owned()) {
        Subst::new()
    } else if t.tv().contains(id) {
        panic!("occurs check fails")
    } else {
        vec![(id.to_owned(), t.to_owned())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lit::Lit;
    use crate::prim::Prim;

    fn num(val: &str) -> Type {
        Type::Lit(Lit::Num(val.to_owned()))
    }

    fn str(val: &str) -> Type {
        Type::Lit(Lit::Str(val.to_owned()))
    }

    fn bool(val: &bool) -> Type {
        Type::Lit(Lit::Bool(val.to_owned()))
    }

    fn prim(prim: Prim) -> Type {
        Type::Prim(prim)
    }

    fn lam(arg: &Type, ret: &Type) -> Type {
        Type::Lam(Box::from(arg.to_owned()), Box::from(ret.to_owned()))
    }

    #[test]
    fn equal_lits() {
        let result = mgu(&num("5"), &num("5"), Rel::Exact);
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_lits() {
        mgu(&num("5"), &num("15"), Rel::Exact);
    }

    #[test]
    fn equal_prims() {
        let result = mgu(&prim(Prim::Num), &prim(Prim::Num), Rel::Exact);
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_prims() {
        mgu(&prim(Prim::Str), &prim(Prim::Num), Rel::Exact);
    }

    #[test]
    fn lit_and_prim_subtyping() {
        assert_eq!(mgu(&num("5"), &prim(Prim::Num), Rel::Sub), Subst::new());
        assert_eq!(mgu(&str("hello"), &prim(Prim::Str), Rel::Sub), Subst::new());
        assert_eq!(mgu(&bool(&true), &prim(Prim::Bool), Rel::Sub), Subst::new());
    }

    #[test]
    fn lit_and_prim_supertyping() {
        assert_eq!(mgu(&prim(Prim::Num), &num("5"), Rel::Sup), Subst::new());
        assert_eq!(mgu(&prim(Prim::Str), &str("hello"), Rel::Sup), Subst::new());
        assert_eq!(mgu(&prim(Prim::Bool), &bool(&true), Rel::Sup), Subst::new());
    }

    #[test]
    fn equal_lams() {
        let result = mgu(
            &lam(&prim(Prim::Num), &prim(Prim::Num)),
            &lam(&prim(Prim::Num), &prim(Prim::Num)), 
            Rel::Exact,
        );
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_lams() {
        mgu(
            &lam(&prim(Prim::Num), &prim(Prim::Num)),
            &lam(&prim(Prim::Str), &prim(Prim::Str)), 
            Rel::Exact,
        );
    }
}
