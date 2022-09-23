use itertools::join;
use std::fmt;

use crate::keyword::TKeyword;
use crate::r#type::Type;
use crate::{Scheme, TFnParam};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TObjElem {
    Call(TCall),
    // Index(TIndex),
    Prop(TProp),
    // Getter
    // Setter
    // RestSpread - we can use this instead of converting {a, ...x} to {a} & tvar
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
pub struct TCall {
    pub params: Vec<TFnParam>,
    pub ret: Box<Type>,
    pub qualifiers: Vec<i32>,
}

impl fmt::Display for TCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            params,
            ret,
            qualifiers,
        } = self;

        if qualifiers.is_empty() {
            write!(f, "({}) => {}", join(params, ", "), ret)
        } else {
            write!(
                f,
                "<{}>({}) => {}",
                join(qualifiers.iter().map(|id| { format!("t{id}") }), ", "),
                join(params, ", "),
                ret
            )
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
