use itertools::{join, Itertools};
use std::fmt;
use std::hash::Hash;

use crate::keyword::TKeyword;
use crate::lit::TLit;
use crate::obj::TProp;
use crate::pat::TPat;
use crate::prim::TPrim;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TApp {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
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

#[derive(Clone, Debug, Eq)]
pub struct TAlias {
    pub name: String,
    pub type_params: Option<Vec<Type>>,
}

impl PartialEq for TAlias {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.type_params == other.type_params
    }
}

impl Hash for TAlias {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.type_params.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(i32), // i32 is the if of the type variable
    App(TApp),
    Lam(TLam),
    // Query, // use for typed holes
    Prim(TPrim),
    Lit(TLit),
    Keyword(TKeyword),
    Union(Vec<Type>),
    Intersection(Vec<Type>),
    Object(Vec<TProp>),
    Alias(TAlias),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Rest(Box<Type>), // TODO: rename this to Spread
}

impl From<TLit> for Type {
    fn from(lit: TLit) -> Self {
        Type::Lit(lit)
    }
}

impl fmt::Display for Type {
    // TODO: add in parentheses where necessary to get the precedence right
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Type::Var(id) => write!(f, "t{id}"),
            Type::App(TApp { args, ret }) => {
                write!(f, "({}) => {}", join(args, ", "), ret)
            }
            Type::Lam(TLam { params, ret, .. }) => {
                write!(f, "({}) => {}", join(params, ", "), ret)
            }
            Type::Prim(prim) => write!(f, "{}", prim),
            Type::Lit(lit) => write!(f, "{}", lit),
            Type::Keyword(keyword) => write!(f, "{}", keyword),
            Type::Union(types) => {
                let strings: Vec<_> = types.iter().map(|t| format!("{t}")).sorted().collect();
                write!(f, "{}", join(strings, " | "))
            }
            Type::Intersection(types) => {
                let strings: Vec<_> = types.iter().map(|t| format!("{t}")).sorted().collect();
                write!(f, "{}", join(strings, " & "))
            }
            Type::Object(props) => write!(f, "{{{}}}", join(props, ", ")),
            Type::Alias(TAlias {
                name, type_params, ..
            }) => match type_params {
                Some(params) => write!(f, "{name}<{}>", join(params, ", ")),
                None => write!(f, "{name}"),
            },
            Type::Tuple(types) => write!(f, "[{}]", join(types, ", ")),
            Type::Array(t) => write!(f, "{t}[]"),
            Type::Rest(arg) => write!(f, "...{arg}"),
        }
    }
}
