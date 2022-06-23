use super::id::*;
use super::r#type::*;

// type Subst  = [(Tyvar, Type)]
pub type Subst = Vec<(ID, Type)>;
