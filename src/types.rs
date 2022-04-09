use itertools::join;
use std::fmt;
use std::hash::{Hash, Hasher};

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(String),
    Bool(String),
    Str(String),
    Null,
    Undefined,
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        Literal::Str(s.to_owned())
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::Str(s)
    }
}

impl From<&bool> for Literal {
    fn from(b: &bool) -> Self {
        Literal::Bool(b.to_string())
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        Literal::Bool(b.to_string())
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Num(val) => write!(f, "{}", val),
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::Str(val) => write!(f, "\"{}\"", val),
            Literal::Null => write!(f, "null"),
            Literal::Undefined => write!(f, "undefined"),
        }
    }
}

// #[derive(Debug)]
// struct Property {
//     name: String,
//     ty: Type,
// }

#[derive(Clone, Debug)]
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
pub struct TFun {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl fmt::Display for TFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { args, ret } = self;
        write!(f, "({}) => {}", join(args, ", "), ret)
    }
}

impl From<TFun> for Type {
    fn from(tfun: TFun) -> Self {
        Type::Fun(tfun)
    }
}

impl From<&TFun> for Type {
    fn from(tfun: &TFun) -> Self {
        Type::Fun(tfun.clone())
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Var(TVar),
    Fun(TFun),
    Prim(Primitive),
    Lit(Literal),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var(tv) => write!(f, "{}", tv),
            Type::Fun(tfun) => write!(f, "{}", tfun),
            Type::Prim(prim) => write!(f, "{}", prim),
            Type::Lit(lit) => write!(f, "{}", lit),
        }
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
            write!(f, "<{}>{}", join(qualifiers, ", "), ty)
        }
    }
}

use std::collections::HashMap;
use std::collections::HashSet;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    fn ftv(&self) -> HashSet<TVar>;
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

        let ty = Type::Fun(TFun {
            args: vec![Type::from(Primitive::Num), Type::from(Primitive::Bool)],
            ret: Box::new(Type::from(Primitive::Num)),
        });
        assert_eq!(format!("{}", ty), "(number, boolean) => number");
    }
}
