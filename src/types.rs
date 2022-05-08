use itertools::{join};
use std::fmt;
use std::hash::{Hash, Hasher};

use super::literal::Literal;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
    Num,
    Bool,
    Str,
    Undefined,
    Null,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Num => write!(f, "number",),
            Primitive::Bool => write!(f, "boolean"),
            Primitive::Str => write!(f, "string"),
            Primitive::Null => write!(f, "null"),
            Primitive::Undefined => write!(f, "undefined"),
        }
    }
}

// #[derive(Debug)]
// struct Property {
//     name: String,
//     ty: Type,
// }

#[derive(Clone, Debug, PartialOrd, Ord)]
pub struct TVar {
    pub id: i32,
}

impl fmt::Display for TVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { id, .. } = self;
        let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".chars().collect();
        let id = chars.get(id.to_owned() as usize).unwrap();
        write!(f, "{}", id)
    }
}

impl PartialEq for TVar {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for TVar {}

impl Hash for TVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct TLam {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl fmt::Display for TLam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { args, ret } = self;
        write!(f, "({}) => {}", join(args, ", "), ret)
    }
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Var(TVar),
    Lam(TLam),
    Prim(Primitive),
    Lit(Literal),
}

#[derive(Clone, Debug)]
pub struct Type {
    pub id: i32,
    pub kind: TypeKind,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type {kind: TypeKind::Var(tv), ..} => write!(f, "{}", tv),
            Type {kind: TypeKind::Lam(tlam), ..} => write!(f, "{}", tlam),
            Type {kind: TypeKind::Prim(prim), ..} => write!(f, "{}", prim),
            Type {kind: TypeKind::Lit(lit), ..} => write!(f, "{}", lit),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub qualifiers: Vec<i32>,
    pub ty: Type,
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Scheme { qualifiers, ty } = self;
        let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".chars().collect();

        if qualifiers.is_empty() {
            write!(f, "{}", ty)
        } else {
            let mut quals = qualifiers.clone();
            quals.sort();
            write!(f, "<{}>{}", join(quals.iter().map(|id| {
                let id = chars.get(id.to_owned() as usize).unwrap();
                format!("{id}")
            }), ", "), ty)
        }
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_fmt_literal() {
        assert_eq!(
            format!("{}", Literal::from("hello")),
            String::from("\"hello\"")
        );
        assert_eq!(
            format!("{}", Literal::Num(String::from("5.0"))),
            String::from("5.0")
        );
        assert_eq!(format!("{}", Literal::from(true)), String::from("true"));
    }

    // #[test]
    // fn test_fmt_type() {
    //     assert_eq!(
    //         format!("{}", Type::from(Literal::from("hello"))),
    //         String::from("\"hello\""),
    //     );

    //     let ty = Type::Lam(TLam {
    //         args: vec![Type::from(Primitive::Num), Type::from(Primitive::Bool)],
    //         ret: Box::new(Type::from(Primitive::Num)),
    //     });
    //     assert_eq!(format!("{}", ty), "(number, boolean) => number");
    // }
}
