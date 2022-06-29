use std::hash::Hash;

use super::pred::*;
use super::r#type::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lit {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(String),
    Bool(bool),
    Str(String),
    // Null,
    // Undefined,
}

pub fn ti_lit(lit: &Lit) -> (Vec<Pred>, Type) {
    let t = Type {
        usage: None,
        variant: Variant::Lit(lit.to_owned())
    };
    (vec![], t)
}
