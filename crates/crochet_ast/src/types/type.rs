use derivative::*;
use itertools::{join, Itertools};
use std::fmt;

use crate::types::keyword::TKeyword;
use crate::types::lam::TLam;
use crate::types::lit::TLit;
use crate::types::obj::TObjElem;
use crate::types::provenance::Provenance;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TApp {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
    // TODO: add type_args property to support explicit specification of type args
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TRef {
    pub name: String,
    // TODO: make this non-optional
    pub type_args: Option<Vec<Type>>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TObject {
    pub elems: Vec<TObjElem>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TIndexAccess {
    pub object: Box<Type>,
    pub index: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TGeneric {
    pub t: Box<Type>,
    pub type_params: Vec<TVar>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TVar {
    pub id: i32,
    pub constraint: Option<Box<Type>>,
}

// impl fmt::Display for TVar {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let TVar { id, constraint } = self;
//         match constraint {
//             // TODO: The only time we should include `extends constraint` is if
//             // the TVar is being printed as a type param (not a type arg).
//             Some(_constraint) => write!(f, "t{id}"), // extends {constraint}"),
//             None => write!(f, "t{id}"),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeKind {
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

#[derive(Derivative)]
#[derivative(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type {
    pub kind: TypeKind,
    pub mutable: bool,
    #[derivative(PartialOrd = "ignore")]
    #[derivative(Ord = "ignore")]
    #[derivative(PartialEq = "ignore")] // we don't care about provenance when comparing types
    pub provenance: Option<Box<Provenance>>,
}

impl From<TypeKind> for Type {
    fn from(kind: TypeKind) -> Self {
        Type {
            kind,
            provenance: None,
            mutable: false,
        }
    }
}

impl From<TLit> for Type {
    fn from(lit: TLit) -> Self {
        Type::from(TypeKind::Lit(lit))
    }
}

impl fmt::Display for Type {
    // TODO: add in parentheses where necessary to get the precedence right
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.mutable {
            write!(f, "mut ")?;
        }
        match &self.kind {
            TypeKind::Generic(TGeneric { t, type_params }) => {
                if type_params.is_empty() {
                    write!(f, "{t}")
                } else {
                    let type_params = type_params.iter().map(|tp| {
                        let TVar { id, constraint } = tp;
                        match constraint {
                            Some(constraint) => format!("t{id} extends {constraint}"),
                            None => format!("t{id}"),
                        }
                    });
                    // e.g. <T extends number | string>(a: T, b: T) => T
                    write!(f, "<{}>{t}", join(type_params, ", "))
                }
            }
            TypeKind::Var(tv) => write!(f, "t{}", tv.id),
            TypeKind::App(TApp { args, ret }) => {
                write!(f, "({}) => {}", join(args, ", "), ret)
            }
            TypeKind::Lam(lam) => write!(f, "{lam}"),
            TypeKind::Lit(lit) => write!(f, "{}", lit),
            TypeKind::Keyword(keyword) => write!(f, "{}", keyword),
            TypeKind::Union(types) => {
                let strings: Vec<_> = types.iter().map(|t| format!("{t}")).sorted().collect();
                write!(f, "{}", join(strings, " | "))
            }
            TypeKind::Intersection(types) => {
                let strings: Vec<_> = types.iter().map(|t| format!("{t}")).sorted().collect();
                write!(f, "{}", join(strings, " & "))
            }
            TypeKind::Object(TObject { elems, .. }) => {
                write!(f, "{{{}}}", join(elems, ", "))
            }
            TypeKind::Ref(tr) => write!(f, "{tr}"),
            TypeKind::Tuple(types) => write!(f, "[{}]", join(types, ", ")),
            TypeKind::Array(t) => write!(f, "{t}[]"),
            TypeKind::Rest(arg) => write!(f, "...{arg}"),
            TypeKind::This => write!(f, "this"),
            TypeKind::KeyOf(t) => write!(f, "keyof {t}"),
            TypeKind::IndexAccess(TIndexAccess { object, index }) => write!(f, "{object}[{index}]"),
        }
    }
}

// TODO: add unit tests to verify the fmt::Display output
