use itertools::join;
use std::fmt;
use std::hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qualified<T>
where
    T: Clone + fmt::Debug + fmt::Display + PartialEq + Eq + hash::Hash,
{
    pub qualifiers: Vec<i32>,
    pub t: T,
}

impl<T> fmt::Display for Qualified<T>
where
    T: Clone + fmt::Debug + fmt::Display + PartialEq + Eq + hash::Hash,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { qualifiers, t } = self;
        if qualifiers.is_empty() {
            write!(f, "{t}")
        } else {
            write!(
                f,
                "<{}>{}",
                join(qualifiers.iter().map(|id| { format!("t{id}") }), ", "),
                t
            )
        }
    }
}
