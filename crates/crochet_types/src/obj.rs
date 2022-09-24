use std::fmt;

use crate::lam::TLam;
use crate::r#type::Type;
use crate::TFnParam;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TObjElem {
    Call(TLam),
    Constructor(TLam),
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
    pub t: Type,
    pub type_params: Option<Vec<i32>>,
}

impl fmt::Display for TIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            key,
            mutable,
            t,
            // TODO: handle generic indexers
            type_params: _,
        } = self;
        match mutable {
            false => write!(f, "[{key}]: {t}"),
            true => write!(f, "mut [{key}]: {t}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TProp {
    pub name: String,
    pub optional: bool,
    pub mutable: bool,
    pub t: Type,
}

impl fmt::Display for TProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            name,
            optional,
            mutable,
            t,
        } = self;
        match (optional, mutable) {
            (false, false) => write!(f, "{name}: {t}"),
            (true, false) => write!(f, "{name}?: {t}"),
            (false, true) => write!(f, "mut {name}: {t}"),
            (true, true) => write!(f, "mut {name}?: {t}"),
        }
    }
}
