use itertools::join;
use std::fmt;

use crate::Type;

// A `Scheme` should be defined as `Qualified<Type>`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Scheme {
    pub qualifiers: Vec<i32>,
    pub t: Type,
}

impl From<Type> for Scheme {
    fn from(t: Type) -> Self {
        Scheme {
            qualifiers: vec![],
            t,
        }
    }
}

impl From<&Type> for Scheme {
    fn from(t: &Type) -> Self {
        Scheme {
            qualifiers: vec![],
            t: t.clone(),
        }
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Scheme { qualifiers, t } = self;

        if qualifiers.is_empty() {
            write!(f, "{}", t)
        } else {
            let mut quals = qualifiers.clone();
            quals.sort_unstable();
            write!(
                f,
                "<{}>{}",
                join(quals.iter().map(|id| { format!("t{id}") }), ", "),
                t
            )
        }
    }
}
