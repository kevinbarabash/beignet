use super::lit::Lit;
use super::prim::Prim;
use super::r#type::*;

#[derive(PartialEq, Eq)]
pub enum Rel {
    Subtype,
    Supertype,
}

pub fn is_subtype(t1: &Type, t2: &Type) -> bool {
    match (&t1.variant, &t2.variant) {
        (Variant::Lit(lit), Variant::Prim(prim)) => {
            matches!(
                (lit, prim),
                (Lit::Num(_), Prim::Num) | (Lit::Str(_), Prim::Str) | (Lit::Bool(_), Prim::Bool)
            )
        }
        (Variant::Lam(args1, ret1), Variant::Lam(args2, ret2)) => {
            // t1 must have at fewer number of args than t2 in order to called with the
            // same args as t2.  This is because excess args are ignored my functions.
            if args1.len() > args2.len() {
                return false;
            }

            // If any of the t2's args are not subtypes of the corresponding t1 arg, then
            // t1 cannot be safely called in place of t2.
            if args1.iter().zip(args2).any(|(a1, a2)| !is_subtype(a2, a1)) {
                return false;
            }

            // t1's return type must be a subtype of t2's return type.
            is_subtype(ret1, ret2)
        }
        // If the types are the same then they are subtypes of each other.
        (v1, v2) => v1 == v2,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lit::Lit;
    use crate::prim::Prim;

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
    fn lit_is_subtype_of_matching_prim() {
        assert!(is_subtype(&num("5"), &prim(Prim::Num)));
        assert!(is_subtype(&bool(&true), &prim(Prim::Bool)));
        assert!(is_subtype(&str("hello"), &prim(Prim::Str)));
    }

    #[test]
    fn lam_with_fewer_args_is_a_subtype() {
        let lam1 = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        let lam2 = lam(&[prim(Prim::Num), prim(Prim::Str)], &prim(Prim::Bool));
        assert!(is_subtype(&lam1, &lam2));
    }

    #[test]
    fn lam_with_supertype_args_is_a_subtype() {
        let lam1 = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        let lam2 = lam(&[num("5")], &prim(Prim::Bool));
        assert!(is_subtype(&lam1, &lam2));
    }

    #[test]
    fn lam_with_subtype_return_is_a_subtype() {
        let lam1 = lam(&[prim(Prim::Num)], &bool(&true));
        let lam2 = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        assert!(is_subtype(&lam1, &lam2));
    }

    #[test]
    fn lam_with_more_args_is_not_a_subtype() {
        let lam1 = lam(&[prim(Prim::Num), prim(Prim::Str)], &prim(Prim::Bool));
        let lam2 = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        assert!(!is_subtype(&lam1, &lam2));
    }

    #[test]
    fn lam_with_subtype_args_is_not_a_subtype() {
        let lam1 = lam(&[num("5")], &prim(Prim::Bool));
        let lam2 = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        assert!(!is_subtype(&lam1, &lam2));
    }

    #[test]
    fn lam_with_supertype_return_is_not_a_subtype() {
        let lam1 = lam(&[prim(Prim::Num)], &prim(Prim::Bool));
        let lam2 = lam(&[prim(Prim::Num)], &bool(&true));
        assert!(!is_subtype(&lam1, &lam2));
    }
}
