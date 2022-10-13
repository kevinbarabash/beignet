use itertools::{join, Itertools};
use std::fmt;
use std::hash::Hash;

use crate::keyword::TKeyword;
use crate::lam::TLam;
use crate::lit::TLit;
use crate::obj::TObjElem;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TApp {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
    // TODO: add type_args property to support explicit specification of type args
}

#[derive(Clone, Debug, Eq)]
pub struct TRef {
    pub name: String,
    // TODO: make this non-optional
    pub type_args: Option<Vec<Type>>,
}

impl PartialEq for TRef {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.type_args == other.type_args
    }
}

impl Hash for TRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.type_args.hash(state);
    }
}

impl fmt::Display for TRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let TRef { name, type_args } = self;
        match type_args {
            Some(params) => write!(f, "{name}<{}>", join(params, ", ")),
            None => write!(f, "{name}"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TObject {
    pub elems: Vec<TObjElem>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TIndexAccess {
    pub object: Box<Type>,
    pub index: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TGeneric {
    pub t: Box<Type>,
    pub type_params: Vec<i32>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TVar {
    pub id: i32,
    pub quals: Option<Vec<TRef>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TVar),
    App(TApp),
    Lam(TLam),
    // Query, // use for typed holes
    Lit(TLit),
    Keyword(TKeyword),
    Union(Vec<Type>),
    Intersection(Vec<Type>),
    Object(TObject),
    Ref(TRef),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Rest(Box<Type>), // TODO: rename this to Spread
    This,
    KeyOf(Box<Type>),
    IndexAccess(TIndexAccess),
    Generic(TGeneric),
}

impl From<TLit> for Type {
    fn from(lit: TLit) -> Self {
        Type::Lit(lit)
    }
}

impl fmt::Display for Type {
    // TODO: add in parentheses where necessary to get the precedence right
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Generic(TGeneric { t, type_params }) => {
                if type_params.is_empty() {
                    write!(f, "{t}")
                } else {
                    let params: Vec<_> = type_params
                        .iter()
                        .map(|param| format!("t{param}"))
                        .collect();
                    write!(f, "<{}>{t}", join(params, ", "))
                }
            }
            Type::Var(TVar { id, quals }) => match quals {
                Some(quals) => write!(f, "t{id} extends {}", join(quals, ", ")),
                None => write!(f, "t{id}"),
            },
            Type::App(TApp { args, ret }) => {
                write!(f, "({}) => {}", join(args, ", "), ret)
            }
            Type::Lam(lam) => write!(f, "{lam}"),
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
            Type::Object(TObject { elems, .. }) => {
                write!(f, "{{{}}}", join(elems, ", "))
            }
            Type::Ref(tr) => write!(f, "{tr}"),
            Type::Tuple(types) => write!(f, "[{}]", join(types, ", ")),
            Type::Array(t) => write!(f, "{t}[]"),
            Type::Rest(arg) => write!(f, "...{arg}"),
            Type::This => write!(f, "this"),
            Type::KeyOf(t) => write!(f, "keyof {t}"),
            Type::IndexAccess(TIndexAccess { object, index }) => write!(f, "{object}[{index}]"),
        }
    }
}

// TODO: add unit tests to verify the fmt::Display output
