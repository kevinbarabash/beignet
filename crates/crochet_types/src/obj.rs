use itertools::join;
use std::fmt;

use crate::r#type::{TVar, Type};
use crate::TFnParam;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TCallable {
    pub params: Vec<TFnParam>,
    pub ret: Box<Type>,
    pub type_params: Vec<TVar>,
}

impl fmt::Display for TCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            params,
            ret,
            type_params,
        } = self;
        if type_params.is_empty() {
            write!(f, "({}) => {}", join(params, ", "), ret)
        } else {
            let type_params = type_params.iter().map(|tp| format!("t{tp}"));
            write!(
                f,
                "<{}>({}) => {}",
                join(type_params, ", "),
                join(params, ", "),
                ret
            )
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TObjElem {
    Call(TCallable),
    Constructor(TCallable),
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
    // TODO: update this to only allow `<ident>: string` or `<ident>: number`
    pub key: TFnParam,
    pub mutable: bool,
    pub t: Type,
}

impl fmt::Display for TIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { key, mutable, t } = self;
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
