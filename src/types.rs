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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(String),
    Bool(String),
    Str(String),
    Undefined,
    Null,
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
    pub ret: Box<Type>
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

#[derive(Clone, Debug)]
pub struct Scheme  {
    pub qualifiers: Vec<TVar>,
    pub ty: Type,
}

use std::collections::HashMap;
use std::collections::HashSet;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable<A> {
    fn apply(&self, subs: &Subst) -> A;
    fn ftv(&self) -> HashSet<TVar>;
}
