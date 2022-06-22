use itertools::join;
use std::fmt;

use crate::types::{Type, freeze};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub qualifiers: Vec<i32>,
    pub ty: Type,
}

impl From<Type> for Scheme {
    fn from(ty: Type) -> Self {
        Scheme {
            qualifiers: vec![],
            ty,
        }
    }
}

impl From<&Type> for Scheme {
    fn from(ty: &Type) -> Self {
        Scheme {
            qualifiers: vec![],
            ty: ty.clone(),
        }
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Scheme { qualifiers, ty } = self;

        if qualifiers.is_empty() {
            write!(f, "{}", ty)
        } else {
            let mut quals = qualifiers.clone();
            quals.sort_unstable();
            write!(
                f,
                "<{}>{}",
                join(
                    quals.iter().map(|id| {
                        format!("t{id}")
                    }),
                    ", "
                ),
                ty
            )
        }
    }
}

pub fn freeze_scheme(scheme: Scheme) -> Scheme {
    Scheme { 
        ty: freeze(scheme.ty),
        ..scheme
    }
}
