use crate::qualified::Qualified;
use crate::Type;

pub type Scheme = Qualified<Type>;

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
