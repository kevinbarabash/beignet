use std::fmt;

use crate::keyword::TKeyword;
use crate::lam::TLam;
use crate::qualified::Qualified;
use crate::r#type::Type;
use crate::{Scheme, TFnParam};

pub type TCall = Qualified<TLam>;
pub type TConstructor = Qualified<TLam>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TObjElem {
    Call(TCall),
    Constructor(TConstructor),
    Index(TIndex),
    Prop(TProp),
    // Getter
    // Setter
    // RestSpread - we can use this instead of converting {a, ...x} to {a} & tvar
}

impl fmt::Display for TObjElem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TObjElem::Call(lam) => write!(f, "{lam}"),
            TObjElem::Constructor(lam) => write!(f, "new {lam}"),
            TObjElem::Index(index) => write!(f, "{index}"),
            TObjElem::Prop(prop) => write!(f, "{prop}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TIndex {
    pub key: TFnParam, // identifier + type
    pub mutable: bool,
    pub scheme: Scheme,
}

impl fmt::Display for TIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            key,
            mutable,
            scheme,
        } = self;
        match mutable {
            false => write!(f, "[{key}]: {scheme}"),
            true => write!(f, "mut [{key}]: {scheme}"),
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
                self.scheme.t.to_owned(),
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
            scheme,
        } = self;
        match (optional, mutable) {
            (false, false) => write!(f, "{name}: {scheme}"),
            (true, false) => write!(f, "{name}?: {scheme}"),
            (false, true) => write!(f, "mut {name}: {scheme}"),
            (true, true) => write!(f, "mut {name}?: {scheme}"),
        }
    }
}
