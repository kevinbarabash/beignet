use itertools::join;
use std::fmt;
use std::hash::Hash;

use crate::keyword::TKeyword;
use crate::pat::TPat;
use crate::r#type::{Type, TypeKind};

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

impl fmt::Display for TLam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { params, ret } = self;
        write!(f, "({}) => {}", join(params, ", "), ret)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TFnParam {
    pub pat: TPat,
    pub t: Type,
    pub optional: bool,
}

impl TFnParam {
    pub fn get_type(&self) -> Type {
        match self.optional {
            true => {
                let undefined = Type {
                    kind: TypeKind::Keyword(TKeyword::Undefined),
                };
                Type {
                    kind: TypeKind::Union(vec![self.t.to_owned(), undefined]),
                }
            }
            false => self.t.to_owned(),
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
        let Self { pat, t, optional } = self;
        match optional {
            true => write!(f, "{pat}?: {t}"),
            false => write!(f, "{pat}: {t}"),
        }
    }
}
