use itertools::join;
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

impl From<Primitive> for Type {
    fn from(prim: Primitive) -> Self {
        Type::Prim(prim)
    }
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
    pub name: String,
}

impl fmt::Display for TVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { name, id } = self;
        write!(f, "{}{}", name, id)
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

impl From<TVar> for Type {
    fn from(tvar: TVar) -> Self {
        Type::Var(tvar)
    }
}

impl From<&TVar> for Type {
    fn from(tvar: &TVar) -> Self {
        Type::Var(tvar.clone())
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

impl From<TLam> for Type {
    fn from(tlam: TLam) -> Self {
        Type::Lam(tlam)
    }
}

impl From<&TLam> for Type {
    fn from(tlam: &TLam) -> Self {
        Type::Lam(tlam.clone())
    }
}

// TODO: add `id` to each of these (maybe we could make having an `id` a trait)
#[derive(Clone, Debug)]
pub enum Type {
    Var(TVar),
    Lam(TLam),
    Prim(Primitive),
    Lit(Literal),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var(tv) => write!(f, "{}", tv),
            Type::Lam(tlam) => write!(f, "{}", tlam),
            Type::Prim(prim) => write!(f, "{}", prim),
            Type::Lit(lit) => write!(f, "{}", lit),
        }
    }
}

impl From<Literal> for Type {
    fn from(lit: Literal) -> Self {
        Type::Lit(lit)
    }
}

impl From<&Literal> for Type {
    fn from(lit: &Literal) -> Self {
        Type::Lit(lit.clone())
    }
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub qualifiers: Vec<TVar>,
    pub ty: Type,
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Scheme { qualifiers, ty } = self;

        if qualifiers.is_empty() {
            write!(f, "{}", ty)
        } else {
            let mut quals = qualifiers.clone();
            quals.sort();
            write!(f, "<{}>{}", join(quals, ", "), ty)
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

    #[test]
    fn test_fmt_type() {
        assert_eq!(
            format!("{}", Type::from(Literal::from("hello"))),
            String::from("\"hello\""),
        );

        let ty = Type::Lam(TLam {
            args: vec![Type::from(Primitive::Num), Type::from(Primitive::Bool)],
            ret: Box::new(Type::from(Primitive::Num)),
        });
        assert_eq!(format!("{}", ty), "(number, boolean) => number");
    }
}
