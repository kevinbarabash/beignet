use itertools::join;
use std::fmt;

use crate::types::keyword::TKeyword;
use crate::types::pat::TPat;
use crate::types::r#type::{Type, TypeKind};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TLam {
    pub params: Vec<TFnParam>,
    pub ret: Box<Type>,
}

impl fmt::Display for TLam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { params, ret } = self;
        write!(f, "({}) => {}", join(params, ", "), ret)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
                    provenance: None,
                };
                Type {
                    kind: TypeKind::Union(vec![self.t.to_owned(), undefined]),
                    provenance: None,
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
