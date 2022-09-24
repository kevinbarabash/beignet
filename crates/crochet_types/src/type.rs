use itertools::{join, Itertools};
use std::fmt;
use std::hash::Hash;

use crate::keyword::TKeyword;
use crate::lam::TLam;
use crate::lit::TLit;
use crate::obj::TObjElem;
use crate::prim::TPrim;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TApp {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
    // TODO: use type_args instead of type_params
}

#[derive(Clone, Debug, Eq)]
pub struct TAlias {
    pub name: String,
    pub type_params: Option<Vec<Type>>, // TODO: rename to type_args
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TObject {
    pub elems: Vec<TObjElem>,
    pub type_params: Option<Vec<i32>>,
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
    Object(TObject),
    Alias(TAlias),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Rest(Box<Type>), // TODO: rename this to Spread
    This,
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
            Type::Lam(lam) => write!(f, "{lam}"),
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
            Type::Object(TObject {
                elems, type_params, ..
            }) => match type_params {
                Some(params) => {
                    let params = params.iter().map(|p| format!("t{p}"));
                    write!(f, "<{}>{{{}}}", join(params, ", "), join(elems, ", "))
                }
                None => write!(f, "{{{}}}", join(elems, ", ")),
            },
            Type::Alias(TAlias {
                name, type_params, ..
            }) => match type_params {
                Some(params) => write!(f, "{name}<{}>", join(params, ", ")),
                None => write!(f, "{name}"),
            },
            Type::Tuple(types) => write!(f, "[{}]", join(types, ", ")),
            Type::Array(t) => write!(f, "{t}[]"),
            Type::Rest(arg) => write!(f, "...{arg}"),
            Type::This => write!(f, "this"),
        }
    }
}
