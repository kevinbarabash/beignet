use super::r#type::*;
use super::id::*;

// data Pred   = IsIn Id Type
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Pred {
    IsIn(ID, Type)
}

// data Qual t = [Pred] :=> t
pub type Qual<T> = (Pred, T);
