pub mod assump;
pub mod id;
pub mod pred;
pub mod scheme;
pub mod subst;

pub mod lit;
pub mod prim;
pub mod r#type;

pub mod subtyping;
pub mod unify;

// Definitions:
// - Pred == InIn(ID, Type)
// - Qual<T> == (Vec<Pred>, T>)
// - Assump == (ID, Scheme)
// - Subst == Vec<(Tyvar, Type)>

// Kind is used by Haskell as an optimization.  If the kinds
// don't match then further checking isn't required.  In crochet
// we can't use kinds because it supports:
// - sub-typing
// - optional function params
// - optional properties on object types
// - etc.

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
