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
    pub type_params: Vec<TVar>, // TODO: update to use TypeParam
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TVar {
    pub id: i32,
    pub constraint: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeParam {
    pub name: String,
    pub tvar: TVar,
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
pub struct TMappedType {
    pub type_param: TVar,
    pub optional: Option<TMappedTypeChangeProp>,
    pub mutable: Option<TMappedTypeChangeProp>,
    pub t: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TMappedTypeChangeProp {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TConditionalType {
    pub check_type: Box<Type>,
    pub extends_type: Box<Type>,
    pub true_type: Box<Type>,
    pub false_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeKind {
    Var(TVar),
    App(TApp),
    Lam(TLam),
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

    // Type operations
    KeyOf(Box<Type>),
    IndexAccess(TIndexAccess),
    MappedType(TMappedType),
    ConditionalType(TConditionalType),
    // Query, // use for typed holes

    // We encapsulate type polymorphism in a wrapper type instead of a separate
    // data structure like a `Scheme`.  This allows us to nest polymorphic types
    // with independent type parameters which isn't possible when using `Scheme`.
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
            TypeKind::MappedType(TMappedType {
                type_param,
                optional,
                mutable,
                t,
            }) => {
                write!(f, "{{")?;

                if let Some(mutable) = mutable {
                    match mutable {
                        TMappedTypeChangeProp::Plus => write!(f, "+mut ")?,
                        TMappedTypeChangeProp::Minus => write!(f, "-mut ")?,
                    }
                }

                write!(f, "[t{}", type_param.id)?;
                if let Some(constraint) = &type_param.constraint {
                    write!(f, " in {constraint}")?;
                }
                write!(f, "]")?;

                if let Some(optional) = optional {
                    match optional {
                        TMappedTypeChangeProp::Plus => write!(f, "+?")?,
                        TMappedTypeChangeProp::Minus => write!(f, "-?")?,
                    }
                }

                write!(f, ": {t}}}")
            }
            TypeKind::ConditionalType(TConditionalType {
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => {
                write!(
                    f,
                    "{check_type} extends {extends_type} ? {true_type} : {false_type}"
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_mapped_type_display() {
        let constraint = Type::from(TypeKind::Ref(TRef {
            name: String::from("T"),
            type_args: None,
        }));
        let t = Type::from(TypeKind::MappedType(TMappedType {
            type_param: TVar {
                id: 5,
                constraint: Some(Box::from(constraint)),
            },
            optional: None,
            mutable: None,
            t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
        }));

        assert_eq!(format!("{t}"), "{[t5 in T]: number}");
    }

    #[test]
    fn complex_mapped_type_display() {
        let constraint = Type::from(TypeKind::Ref(TRef {
            name: String::from("T"),
            type_args: None,
        }));
        let t = Type::from(TypeKind::MappedType(TMappedType {
            type_param: TVar {
                id: 5,
                constraint: Some(Box::from(constraint)),
            },
            optional: Some(TMappedTypeChangeProp::Plus),
            mutable: Some(TMappedTypeChangeProp::Plus),
            t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
        }));

        assert_eq!(format!("{t}"), "{+mut [t5 in T]+?: number}");
    }
}
