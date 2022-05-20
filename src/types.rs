use itertools::join;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(String),
    Bool(bool),
    Str(String),
    Null,
    Undefined,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Num(n) => write!(f, "{}", n),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::Str(s) => write!(f, "\"{}\"", s),
            Lit::Null => write!(f, "null"),
            Lit::Undefined => write!(f, "undefined"),
        }
    }
}

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

#[derive(Clone, Debug)]
pub struct TProp {
    pub name: String,
    pub ty: Type,
}

impl fmt::Display for TProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { name, ty } = self;
        write!(f, "{name}: {ty}")
    }
}

#[derive(Clone, Debug)]
pub struct VarType {
    pub id: i32,
    pub frozen: bool,
}

#[derive(Clone, Debug)]
pub struct LamType {
    pub id: i32,
    pub frozen: bool,
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Clone, Debug)]
pub struct PrimType {
    pub id: i32,
    pub frozen: bool,
    pub prim: Primitive,
}

#[derive(Clone, Debug)]
pub struct LitType {
    pub id: i32,
    pub frozen: bool,
    pub lit: Lit,
}

#[derive(Clone, Debug)]
pub struct UnionType {
    pub id: i32,
    pub frozen: bool,
    pub types: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct ObjectType {
    pub id: i32,
    pub frozen: bool,
    pub props: Vec<TProp>,
}

#[derive(Clone, Debug)]
pub struct AliasType {
    pub id: i32,
    pub frozen: bool,
    pub name: String,
    pub type_params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Var(VarType),
    Lam(LamType),
    Prim(PrimType),
    Lit(LitType),
    Union(UnionType),
    Object(ObjectType),
    Alias(AliasType),
}

impl Type {
    pub fn frozen(&self) -> bool {
        match self {
            Type::Var(x) => x.frozen,
            Type::Lam(x) => x.frozen,
            Type::Prim(x) => x.frozen,
            Type::Lit(x) => x.frozen,
            Type::Union(x) => x.frozen,
            Type::Object(x) => x.frozen,
            Type::Alias(x) => x.frozen,
        }
    }

    pub fn id(&self) -> i32 {
        match self {
            Type::Var(x) => x.id,
            Type::Lam(x) => x.id,
            Type::Prim(x) => x.id,
            Type::Lit(x) => x.id,
            Type::Union(x) => x.id,
            Type::Object(x) => x.id,
            Type::Alias(x) => x.id,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var(VarType { id, .. }) => {
                let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                    .chars()
                    .collect();
                let id = chars.get(id.to_owned() as usize).unwrap();
                write!(f, "{}", id)
            }
            Type::Lam(LamType {args, ret, ..}) => write!(f, "({}) => {}", join(args, ", "), ret),
            Type::Prim(PrimType {prim, ..}) => write!(f, "{}", prim),
            Type::Lit(LitType {lit, ..}) => write!(f, "{}", lit),
            Type::Union(UnionType {types, ..}) => write!(f, "{}", join(types, " | ")),
            Type::Object(ObjectType {props, ..}) => write!(f, "{{{}}}", join(props, ", ")),
            Type::Alias(AliasType { name, type_params, .. }) => {
                write!(f, "{name}<{}>", join(type_params, ", "))
            }
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
        let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
            .chars()
            .collect();

        if qualifiers.is_empty() {
            write!(f, "{}", ty)
        } else {
            let mut quals = qualifiers.clone();
            quals.sort();
            write!(
                f,
                "<{}>{}",
                join(
                    quals.iter().map(|id| {
                        let id = chars.get(id.to_owned() as usize).unwrap();
                        format!("{id}")
                    }),
                    ", "
                ),
                ty
            )
        }
    }
}
