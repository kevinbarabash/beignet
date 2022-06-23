use super::r#type::*;
use super::pred::*;

// data Scheme = Forall [Kind] (Qual Type)
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Scheme {
    Forall(Qual<Type>)
}
