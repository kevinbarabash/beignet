use super::id::ID;
use super::lit::Lit;
use super::prim::Prim;
use super::r#type::*;
use super::subst::*;

#[derive(PartialEq, Eq)]
pub enum Rel {
    Sub,
    Sup,
    Eql,
}

// mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
//                                s2 <- mgu (apply s1 r) (apply s1 r')
//                                return (s2 @@ s1)
// mgu (TVar u) t        = varBind u t
// mgu t (TVar u)        = varBind u t
// mgu (TCon tc1) (TCon tc2)
//            | tc1==tc2 = return nullSubst
// mgu t1 t2             = fail "types do not unify"
pub fn mgu(t1: &Type, t2: &Type, rel: &Rel) -> Subst {
    if rel == &Rel::Sup {
        return mgu(t2, t1, &Rel::Sub);
    }

    match (&t1.variant, &t2.variant) {
        (Variant::Var(u), _) => var_bind(u, t2),
        (_, Variant::Var(u)) => var_bind(u, t1),
        (Variant::Lam(arg1, ret1), Variant::Lam(arg2, ret2)) => {
            let rel = match (&t1.usage, &t2.usage) {
                (Some(Usage::FnCall), Some(Usage::FnDecl)) => Rel::Sub,
                (Some(Usage::FnDecl), Some(Usage::FnCall)) => Rel::Sup,
                (_, _) => Rel::Eql,
            };

            let s1 = mgu(arg1.as_ref(), arg2.as_ref(), &rel);
            let s2 = mgu(&ret1.apply(&s1), &ret2.apply(&s1), &rel);
            at_at(&s2, &s1)
        }
        (Variant::Lit(lit1), Variant::Lit(lit2)) if lit1 == lit2 => Subst::new(),
        (Variant::Prim(prim1), Variant::Prim(prim2)) if prim1 == prim2 => Subst::new(),
        (v1, v2) if is_subtype(v1, v2) => Subst::new(),
        // TODO: support more data types
        _ => panic!("types do not unify"),
    }
}

fn is_subtype(v1: &Variant, v2: &Variant) -> bool {
    if let (Variant::Lit(lit), Variant::Prim(prim)) = (v1, v2) {
        matches!(
            (lit, prim),
            (Lit::Num(_), Prim::Num) | (Lit::Str(_), Prim::Str) | (Lit::Bool(_), Prim::Bool)
        )
    } else {
        false
    }
}

// varBind u t | t == TVar u      = return nullSubst
//             | u `elem` tv t    = fail "occurs check fails"
//             | kind u /= kind t = fail "kinds do not match"
//             | otherwise        = return (u +-> t)
// NOTE: the `kind` check is omitted since Kind is incompatible with Crochet's type system
fn var_bind(id: &ID, t: &Type) -> Subst {
    if t.variant == Variant::Var(id.to_owned()) {
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

    fn var(id: &str) -> Type {
        Type {
            usage: None,
            variant: Variant::Var(id.to_owned()),
        }
    }

    fn num(val: &str) -> Type {
        Type {
            usage: None,
            variant: Variant::Lit(Lit::Num(val.to_owned())),
        }
    }

    fn str(val: &str) -> Type {
        Type {
            usage: None,
            variant: Variant::Lit(Lit::Str(val.to_owned())),
        }
    }

    fn bool(val: &bool) -> Type {
        Type {
            usage: None,
            variant: Variant::Lit(Lit::Bool(val.to_owned())),
        }
    }

    fn prim(prim: Prim) -> Type {
        Type {
            usage: None,
            variant: Variant::Prim(prim),
        }
    }

    fn lam(arg: &Type, ret: &Type) -> Type {
        Type {
            usage: None,
            variant: Variant::Lam(Box::from(arg.to_owned()), Box::from(ret.to_owned())),
        }
    }

    #[test]
    fn bindings() {
        let result = mgu(&num("5"), &var("v1"), &Rel::Eql);
        assert_eq!(result, vec![(String::from("v1"), num("5"))]);

        let result = mgu(
            &lam(&prim(Prim::Str), &var("v1")),
            &lam(&var("v2"), &prim(Prim::Bool)),
            &Rel::Eql,
        );
        assert_eq!(
            result,
            vec![
                (String::from("v2"), prim(Prim::Str)),
                (String::from("v1"), prim(Prim::Bool)),
            ]
        );
    }

    #[test]
    fn equal_lits() {
        let result = mgu(&num("5"), &num("5"), &Rel::Eql);
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_lits() {
        mgu(&num("5"), &num("15"), &Rel::Eql);
    }

    #[test]
    fn equal_prims() {
        let result = mgu(&prim(Prim::Num), &prim(Prim::Num), &Rel::Eql);
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_prims() {
        mgu(&prim(Prim::Str), &prim(Prim::Num), &Rel::Eql);
    }

    #[test]
    fn lit_and_prim_subtyping() {
        assert_eq!(mgu(&num("5"), &prim(Prim::Num), &Rel::Sub), Subst::new());
        assert_eq!(
            mgu(&str("hello"), &prim(Prim::Str), &Rel::Sub),
            Subst::new()
        );
        assert_eq!(
            mgu(&bool(&true), &prim(Prim::Bool), &Rel::Sub),
            Subst::new()
        );
    }

    #[test]
    fn lit_and_prim_supertyping() {
        assert_eq!(mgu(&prim(Prim::Num), &num("5"), &Rel::Sup), Subst::new());
        assert_eq!(
            mgu(&prim(Prim::Str), &str("hello"), &Rel::Sup),
            Subst::new()
        );
        assert_eq!(
            mgu(&prim(Prim::Bool), &bool(&true), &Rel::Sup),
            Subst::new()
        );
    }

    #[test]
    fn equal_lams() {
        let result = mgu(
            &lam(&prim(Prim::Num), &prim(Prim::Num)),
            &lam(&prim(Prim::Num), &prim(Prim::Num)),
            &Rel::Eql,
        );
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_lams() {
        mgu(
            &lam(&prim(Prim::Num), &prim(Prim::Num)),
            &lam(&prim(Prim::Str), &prim(Prim::Str)),
            &Rel::Eql,
        );
    }

    #[test]
    fn call_and_decl_with_subtypes() {
        let mut call = lam(&num("5"), &num("10"));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&prim(Prim::Num), &prim(Prim::Num));
        decl.usage = Some(Usage::FnDecl);

        assert_eq!(mgu(&call, &decl, &Rel::Eql), Subst::new());
    }

    #[test]
    fn call_and_decl() {
        let mut call = lam(&prim(Prim::Num), &prim(Prim::Num));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&prim(Prim::Num), &prim(Prim::Num));
        decl.usage = Some(Usage::FnDecl);

        assert_eq!(mgu(&call, &decl, &Rel::Eql), Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn call_and_decl_supertype_failure() {
        let mut call = lam(&prim(Prim::Num), &prim(Prim::Num));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&num("5"), &num("10"));
        decl.usage = Some(Usage::FnDecl);

        mgu(&call, &decl, &Rel::Eql);
    }
}
