use std::hash::Hash;

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
