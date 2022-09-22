use itertools::join;
use std::fmt;

use crate::keyword::TKeyword;
use crate::lam::TLam;
use crate::r#type::Type;
use crate::Scheme;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TObjElem {
    Call(TLam),
    // Index(TIndex),
    Prop(TProp),
    // Getter
    // Setter
}

impl fmt::Display for TObjElem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TObjElem::Call(lam) => write!(f, "({}) => {}", join(&lam.params, ", "), lam.ret),
            TObjElem::Prop(prop) => write!(f, "{prop}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TProp {
    pub name: String,
    pub optional: bool,
    pub mutable: bool,
    pub scheme: Scheme,
}

impl TProp {
    pub fn get_scheme(&self) -> Scheme {
        match self.optional {
            true => Scheme::from(Type::Union(vec![
                self.scheme.ty.to_owned(),
                Type::Keyword(TKeyword::Undefined),
            ])),
            false => self.scheme.to_owned(),
        }
    }
}

impl fmt::Display for TProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            name,
            optional,
            mutable,
            scheme: ty,
        } = self;
        match (optional, mutable) {
            (false, false) => write!(f, "{name}: {ty}"),
            (true, false) => write!(f, "{name}?: {ty}"),
            (false, true) => write!(f, "mut {name}: {ty}"),
            (true, true) => write!(f, "mut {name}?: {ty}"),
        }
    }
}
