use derive_visitor::{Drive, DriveMut};
use itertools::{join, Itertools};
use std::cmp::Ordering;
use std::fmt;

use crate::types::keyword::TKeyword;
use crate::types::lam::TLam;
use crate::types::lit::TLit;
use crate::types::obj::TObjElem;
use crate::types::provenance::Provenance;

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TApp {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
    pub type_args: Option<Vec<Type>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TRef {
    #[drive(skip)]
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

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TObject {
    pub elems: Vec<TObjElem>,
    #[drive(skip)]
    pub is_interface: bool, // Used for `interface` types and classes
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TIndexAccess {
    pub object: Box<Type>,
    pub index: Box<Type>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TVar {
    #[drive(skip)]
    pub id: u32, // This should never be mutated
    pub constraint: Option<Box<Type>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeParam {
    #[drive(skip)]
    pub name: String,
    pub constraint: Option<Box<Type>>,
    pub default: Option<Box<Type>>,
}

impl fmt::Display for TypeParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            name,
            constraint,
            default,
        } = self;
        write!(f, "{name}")?;
        if let Some(constraint) = constraint {
            write!(f, " : {constraint}")?;
        };
        if let Some(default) = default {
            write!(f, " = {default}")?;
        };
        Ok(())
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TMappedType {
    pub type_param: TypeParam,
    #[drive(skip)]
    pub optional: Option<TMappedTypeChangeProp>,
    #[drive(skip)]
    pub mutable: Option<TMappedTypeChangeProp>,
    pub t: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TMappedTypeChangeProp {
    Plus,
    Minus,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TConditionalType {
    pub check_type: Box<Type>,
    pub extends_type: Box<Type>,
    pub true_type: Box<Type>,
    pub false_type: Box<Type>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
pub struct TInferType {
    #[drive(skip)]
    pub name: String,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq, PartialOrd, Ord)]
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
    InferType(TInferType),
    // Query, // use for typed holes
}

#[derive(Clone, Debug, Drive, DriveMut, Eq)]
pub struct Type {
    pub kind: TypeKind,
    #[drive(skip)]
    pub id: u32,
    #[drive(skip)]
    pub mutable: bool,
    #[drive(skip)]
    pub provenance: Option<Box<Provenance>>,
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> Ordering {
        let result = self.kind.cmp(&other.kind);

        if result == Ordering::Equal {
            self.mutable.cmp(&other.mutable)
        } else {
            result
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.mutable == other.mutable
    }
}

// impl From<TypeKind> for Type {
//     fn from(kind: TypeKind) -> Self {
//         Type {
//             kind,
//             provenance: None,
//             mutable: false,
//         }
//     }
// }

// impl From<TLit> for Type {
//     fn from(lit: TLit) -> Self {
//         Type::from(TypeKind::Lit(lit))
//     }
// }

impl fmt::Display for Type {
    // TODO: add in parentheses where necessary to get the precedence right
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.mutable {
            write!(f, "mut ")?;
        }
        match &self.kind {
            TypeKind::Var(tv) => {
                write!(f, "t{}", tv.id)?;
                // TODO: Figure out how to only print the constraints when
                // printint out type variables that appear in a TGeneric's
                // type_params.
                // if let Some(constraint) = &tv.constraint {
                //     write!(f, " extends {constraint}")?;
                // }
                Ok(())
            }
            TypeKind::App(TApp {
                args,
                ret,
                type_args: _, // TODO
            }) => {
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
            // TypeKind::Regex(TRegex { pattern, flags }) => {
            //     write!(f, "/{pattern}/")?;
            //     if let Some(flags) = flags {
            //         write!(f, "{flags}")?;
            //     }
            //     Ok(())
            // }
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

                write!(f, "[{}", type_param.name)?;
                if let Some(constraint) = &type_param.constraint {
                    write!(f, " in {constraint}")?;
                }
                if let Some(default) = &type_param.default {
                    write!(f, " = {default}")?;
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
            TypeKind::InferType(TInferType { name }) => write!(f, "infer {name}"),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn simple_mapped_type_display() {
//         let constraint = Type::from(TypeKind::Ref(TRef {
//             name: String::from("K"),
//             type_args: None,
//         }));
//         let t = Type::from(TypeKind::MappedType(TMappedType {
//             type_param: TypeParam {
//                 name: String::from("T"),
//                 constraint: Some(Box::from(constraint)),
//                 default: None,
//             },
//             optional: None,
//             mutable: None,
//             t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
//         }));

//         assert_eq!(format!("{t}"), "{[T in K]: number}");
//     }

//     #[test]
//     fn complex_mapped_type_display() {
//         let constraint = Type::from(TypeKind::Ref(TRef {
//             name: String::from("K"),
//             type_args: None,
//         }));
//         let t = Type::from(TypeKind::MappedType(TMappedType {
//             type_param: TypeParam {
//                 name: String::from("T"),
//                 constraint: Some(Box::from(constraint)),
//                 default: None,
//             },
//             optional: Some(TMappedTypeChangeProp::Plus),
//             mutable: Some(TMappedTypeChangeProp::Plus),
//             t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
//         }));

//         assert_eq!(format!("{t}"), "{+mut [T in K]+?: number}");
//     }
// }
