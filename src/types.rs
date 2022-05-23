use itertools::join;
use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, Eq)]
pub struct VarType {
    pub id: i32,
    pub frozen: bool,
}

impl PartialEq for VarType {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for VarType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct LamType {
    pub id: i32,
    pub frozen: bool,
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl PartialEq for LamType {
    fn eq(&self, other: &Self) -> bool {
        self.args == other.args && self.ret == other.ret
    }
}

impl Hash for LamType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.args.hash(state);
        self.ret.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct PrimType {
    pub id: i32,
    pub frozen: bool,
    pub prim: Primitive,
}

impl PartialEq for PrimType {
    fn eq(&self, other: &Self) -> bool {
        self.prim == other.prim
    }
}

impl Hash for PrimType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.prim.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct LitType {
    pub id: i32,
    pub frozen: bool,
    pub lit: Lit,
}

impl PartialEq for LitType {
    fn eq(&self, other: &Self) -> bool {
        self.lit == other.lit
    }
}

impl Hash for LitType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.lit.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct UnionType {
    pub id: i32,
    pub frozen: bool,
    pub types: Vec<Type>,
}

impl PartialEq for UnionType {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl Hash for UnionType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.types.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct ObjectType {
    pub id: i32,
    pub frozen: bool,
    pub props: Vec<TProp>,
}

impl PartialEq for ObjectType {
    fn eq(&self, other: &Self) -> bool {
        self.props == other.props
    }
}

impl Hash for ObjectType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.props.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct AliasType {
    pub id: i32,
    pub frozen: bool,
    pub name: String,
    pub type_params: Option<Vec<Type>>,
}

impl PartialEq for AliasType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.type_params == other.type_params
    }
}

impl Hash for AliasType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.type_params.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
            Type::Lam(LamType { args, ret, .. }) => write!(f, "({}) => {}", join(args, ", "), ret),
            Type::Prim(PrimType { prim, .. }) => write!(f, "{}", prim),
            Type::Lit(LitType { lit, .. }) => write!(f, "{}", lit),
            Type::Union(UnionType { types, .. }) => write!(f, "{}", join(types, " | ")),
            Type::Object(ObjectType { props, .. }) => write!(f, "{{{}}}", join(props, ", ")),
            Type::Alias(AliasType {
                name, type_params, ..
            }) => match type_params {
                Some(params) => write!(f, "{name}<{}>", join(params, ", ")),
                None => write!(f, "{name}"),
            },
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
            quals.sort_unstable();
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

pub fn freeze(ty: Type) -> Type {
    match ty {
        Type::Var(var) => Type::Var(VarType {
            frozen: true,
            ..var
        }),
        Type::Lam(lam) => Type::Lam(LamType {
            frozen: true,
            ..lam
        }),
        Type::Prim(prim) => Type::Prim(PrimType {
            frozen: true,
            ..prim
        }),
        Type::Lit(lit) => Type::Lit(LitType {
            frozen: true,
            ..lit
        }),
        Type::Union(union) => Type::Union(UnionType {
            frozen: true,
            ..union
        }),
        Type::Object(obj) => Type::Object(ObjectType {
            frozen: true,
            ..obj
        }),
        Type::Alias(alias) => Type::Alias(AliasType {
            frozen: true,
            ..alias
        }),
    }
}
