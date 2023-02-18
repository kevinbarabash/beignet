use derive_visitor::{Drive, DriveMut};
use itertools::join;
use std::fmt;

use crate::types::keyword::TKeyword;
use crate::types::pat::TPat;
use crate::types::r#type::{Type, TypeKind};
use crate::types::TypeParam;

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TLam {
    pub params: Vec<TFnParam>,
    pub ret: Box<Type>,
    pub type_params: Option<Vec<TypeParam>>,
}

impl fmt::Display for TLam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            type_params,
            params,
            ret,
        } = self;
        if let Some(type_params) = &type_params {
            let type_params: Vec<_> = type_params.iter().map(|tp| tp.to_string()).collect();
            write!(f, "<{}>", type_params.join(", "))?;
        }
        write!(f, "({}) => {}", join(params, ", "), ret)
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TFnParam {
    pub pat: TPat,
    pub t: Type,
    #[drive(skip)]
    pub optional: bool,
}

impl TFnParam {
    pub fn get_type(&self) -> Type {
        match self.optional {
            true => {
                let undefined = Type {
                    kind: TypeKind::Keyword(TKeyword::Undefined),
                    provenance: None, // TODO: map this back to the `?`
                    mutable: false,
                };
                Type {
                    kind: TypeKind::Union(vec![self.t.to_owned(), undefined]),
                    provenance: None,
                    mutable: false,
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
