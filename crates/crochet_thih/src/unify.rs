use super::id::ID;
use super::r#type::*;
use super::subst::*;
use super::subtyping::*;

// mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
//                                s2 <- mgu (apply s1 r) (apply s1 r')
//                                return (s2 @@ s1)
// mgu (TVar u) t        = varBind u t
// mgu t (TVar u)        = varBind u t
// mgu (TCon tc1) (TCon tc2)
//            | tc1==tc2 = return nullSubst
// mgu t1 t2             = fail "types do not unify"
pub fn mgu(t1: &Type, t2: &Type, rel: &Option<Rel>) -> Subst {
    if rel == &Some(Rel::Supertype) {
        return mgu(t2, t1, &Some(Rel::Subtype));
    }

    match (&t1.variant, &t2.variant) {
        (Variant::Var(u), _) => var_bind(u, t2),
        (_, Variant::Var(u)) => var_bind(u, t1),
        (Variant::Lam(args1, ret1), Variant::Lam(args2, ret2)) => {
            let rel = match (&t1.usage, &t2.usage) {
                (Some(Usage::FnCall), Some(Usage::FnDecl)) => {
                    if args1.len() < args2.len() {
                        // TODO: implement partial application
                        panic!("not enough args in function call")
                    }
                    Some(Rel::Subtype)
                },
                (Some(Usage::FnDecl), Some(Usage::FnCall)) => {
                    if args1.len() > args2.len() {
                        // TODO: implement partial application
                        panic!("not enough args in function call")
                    }
                    Some(Rel::Supertype)
                },
                (_, _) => {
                    if args1.len() != args2.len() {
                        panic!("arg counts don't match")
                    }
                    None
                },
            };

            let mut constraints: Vec<(Type, Type)> =
                args1.iter().cloned().zip(args2.iter().cloned()).collect();
            constraints.push((ret1.as_ref().to_owned(), ret2.as_ref().to_owned()));
            mgu_many(&constraints, &rel)
        }
        (Variant::Lit(lit1), Variant::Lit(lit2)) if lit1 == lit2 => Subst::new(),
        (Variant::Prim(prim1), Variant::Prim(prim2)) if prim1 == prim2 => Subst::new(),
        (_, _) if is_subtype(t1, t2) => Subst::new(),
        // TODO: support more data types
        _ => panic!("types do not unify"),
    }
}

fn mgu_many(constraints: &[(Type, Type)], rel: &Option<Rel>) -> Subst {
    match constraints {
        [] => Subst::new(),
        [(t1, t2), tail @ ..] => {
            let s1 = mgu(t1, t2, rel);
            let tail: Vec<_> = tail
                .iter()
                .map(|(t1, t2)| (t1.apply(&s1), t2.apply(&s1)))
                .collect();
            let s2 = mgu_many(&tail, rel);
            at_at(&s1, &s2)
        }
    }
}

// varBind u t | t == TVar u      = return nullSubst
//             | u `elem` tv t    = fail "occurs check fails"
//             | kind u /= kind t = fail "kinds do not match"
//             | otherwise        = return (u +-> t)
// NOTE: the `kind` check is omitted since Kind is incompatible with Crochet's type system
fn var_bind(id: &ID, t: &Type) -> Subst {
    // TODO: figure out how to handle .usage when it appears on type variables
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

    fn lam(args: &[Type], ret: &Type) -> Type {
        Type {
            usage: None,
            variant: Variant::Lam(args.to_owned(), Box::from(ret.to_owned())),
        }
    }

    #[test]
    fn bindings() {
        let result = mgu(&num("5"), &var("v1"), &None);
        assert_eq!(result, vec![(String::from("v1"), num("5"))]);

        let result = mgu(
            &lam(&[prim(Prim::Str)], &var("v1")),
            &lam(&[var("v2")], &prim(Prim::Bool)),
            &None,
        );
        assert_eq!(
            result,
            vec![
                (String::from("v1"), prim(Prim::Bool)),
                (String::from("v2"), prim(Prim::Str)),
            ]
        );
    }

    #[test]
    fn equal_lits() {
        let result = mgu(&num("5"), &num("5"), &None);
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_lits() {
        mgu(&num("5"), &num("15"), &None);
    }

    #[test]
    fn equal_prims() {
        let result = mgu(&prim(Prim::Num), &prim(Prim::Num), &None);
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_prims() {
        mgu(&prim(Prim::Str), &prim(Prim::Num), &None);
    }

    #[test]
    fn lit_and_prim_subtyping() {
        assert_eq!(mgu(&num("5"), &prim(Prim::Num), &Some(Rel::Subtype)), Subst::new());
        assert_eq!(
            mgu(&str("hello"), &prim(Prim::Str), &Some(Rel::Subtype)),
            Subst::new()
        );
        assert_eq!(
            mgu(&bool(&true), &prim(Prim::Bool), &Some(Rel::Subtype)),
            Subst::new()
        );
    }

    #[test]
    fn lit_and_prim_supertyping() {
        assert_eq!(mgu(&prim(Prim::Num), &num("5"), &Some(Rel::Supertype)), Subst::new());
        assert_eq!(
            mgu(&prim(Prim::Str), &str("hello"), &Some(Rel::Supertype)),
            Subst::new()
        );
        assert_eq!(
            mgu(&prim(Prim::Bool), &bool(&true), &Some(Rel::Supertype)),
            Subst::new()
        );
    }

    #[test]
    fn equal_lams() {
        let result = mgu(
            &lam(&[prim(Prim::Num)], &prim(Prim::Num)),
            &lam(&[prim(Prim::Num)], &prim(Prim::Num)),
            &None,
        );
        assert_eq!(result, Subst::new());
    }

    #[test]
    fn equal_lams_multiple_args() {
        let result = mgu(
            &lam(&[prim(Prim::Num), prim(Prim::Str)], &prim(Prim::Bool)),
            &lam(&[prim(Prim::Num), prim(Prim::Str)], &prim(Prim::Bool)),
            &None,
        );
        assert_eq!(result, Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn unequal_lams() {
        mgu(
            &lam(&[prim(Prim::Num)], &prim(Prim::Num)),
            &lam(&[prim(Prim::Str)], &prim(Prim::Str)),
            &None,
        );
    }

    #[test]
    fn call_and_decl_with_subtypes() {
        let mut call = lam(&[num("5")], &num("10"));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&[prim(Prim::Num)], &prim(Prim::Num));
        decl.usage = Some(Usage::FnDecl);

        assert_eq!(mgu(&call, &decl, &None), Subst::new());
    }

    #[test]
    fn call_and_decl() {
        let mut call = lam(&[prim(Prim::Num)], &prim(Prim::Num));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&[prim(Prim::Num)], &prim(Prim::Num));
        decl.usage = Some(Usage::FnDecl);

        assert_eq!(mgu(&call, &decl, &None), Subst::new());
    }

    #[test]
    #[should_panic = "types do not unify"]
    fn call_and_decl_supertype_failure() {
        let mut call = lam(&[prim(Prim::Num)], &prim(Prim::Num));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&[num("5")], &num("10"));
        decl.usage = Some(Usage::FnDecl);

        mgu(&call, &decl, &None);
    }

    #[test]
    #[should_panic = "not enough args in function call"]
    fn call_and_decl_insufficient_arg_count() {
        let mut call = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&[prim(Prim::Num), prim(Prim::Str)], &prim(Prim::Bool));
        decl.usage = Some(Usage::FnDecl);

        mgu(&call, &decl, &None);
    }

    #[test]
    fn call_and_decl_extra_call_args_allowed() {
        let mut call = lam(&[prim(Prim::Num), prim(Prim::Str)], &prim(Prim::Bool));
        call.usage = Some(Usage::FnCall);
        let mut decl = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        decl.usage = Some(Usage::FnDecl);

        mgu(&call, &decl, &None);
    }
}
