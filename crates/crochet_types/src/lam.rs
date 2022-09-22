use std::fmt;
use std::hash::Hash;

use crate::keyword::TKeyword;
use crate::pat::TPat;
use crate::r#type::Type;

#[derive(Clone, Debug, Eq)]
pub struct TLam {
    pub params: Vec<TFnParam>,
    pub ret: Box<Type>,
}

impl PartialEq for TLam {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.ret == other.ret
    }
}

impl Hash for TLam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.params.hash(state);
        self.ret.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TFnParam {
    pub pat: TPat,
    pub ty: Type,
    pub optional: bool,
}

impl TFnParam {
    pub fn get_type(&self) -> Type {
        match self.optional {
            true => Type::Union(vec![self.ty.to_owned(), Type::Keyword(TKeyword::Undefined)]),
            false => self.ty.to_owned(),
        }
    }
    pub fn get_name(&self, index: &usize) -> String {
        match &self.pat {
            TPat::Ident(bi) => bi.name.to_owned(),
            _ => format!("arg{index}"),
        }
    }
}

impl fmt::Display for TFnParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { pat, ty, optional } = self;
        match optional {
            true => write!(f, "{pat}?: {ty}"),
            false => write!(f, "{pat}: {ty}"),
        }
    }
}
