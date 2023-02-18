use derive_visitor::{Drive, DriveMut};
use itertools::join;
use std::fmt;

use crate::types::binding::BindingIdent;
use crate::types::r#type::Type;

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub enum TPat {
    Ident(BindingIdent),
    Rest(RestPat),
    Array(ArrayPat),
    Object(TObjectPat),
}

impl fmt::Display for TPat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TPat::Ident(bi) => write!(f, "{bi}"),
            TPat::Rest(rest) => write!(f, "{rest}"),
            TPat::Array(array) => write!(f, "{array}"),
            TPat::Object(obj) => write!(f, "{obj}"),
        }
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct RestPat {
    pub arg: Box<TPat>,
}

impl fmt::Display for RestPat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { arg } = self;
        write!(f, "...{arg}")
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayPat {
    pub elems: Vec<Option<TPat>>,
}

impl fmt::Display for ArrayPat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { elems } = self;
        let elems = elems.iter().map(|elem| match elem {
            Some(elem) => format!("{elem}"),
            None => String::from(" "),
        });
        write!(f, "[{}]", join(elems, ", "))
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TObjectPat {
    pub props: Vec<TObjectPatProp>,
}

impl fmt::Display for TObjectPat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { props } = self;
        write!(f, "{{{}}}", join(props, ", "))
    }
}

// TODO: update this to match AST changes to ObjectPatProp
#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub enum TObjectPatProp {
    KeyValue(TObjectKeyValuePatProp),
    Assign(TObjectAssignPatProp),
    Rest(RestPat),
}

impl fmt::Display for TObjectPatProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TObjectPatProp::KeyValue(kv) => write!(f, "{kv}"),
            TObjectPatProp::Assign(assign) => write!(f, "{assign}"),
            TObjectPatProp::Rest(rest) => write!(f, "{rest}"),
        }
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TObjectKeyValuePatProp {
    #[drive(skip)]
    pub key: String,
    pub value: TPat,
}

impl fmt::Display for TObjectKeyValuePatProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { key, value } = self;
        write!(f, "{key}: {value}")
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TObjectAssignPatProp {
    #[drive(skip)]
    pub key: String,
    pub value: Option<Type>,
}

impl fmt::Display for TObjectAssignPatProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { key, value } = self;
        match value {
            Some(value) => write!(f, "{key} = {value}"),
            None => write!(f, "{key}"),
        }
    }
}
