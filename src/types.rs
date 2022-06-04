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
    pub optional: bool,
    pub ty: Type,
}

impl fmt::Display for TProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { name, optional, ty } = self;
        match optional {
            false => write!(f, "{name}: {ty}"),
            true => write!(f, "{name}?: {ty}"),
        }
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
    pub params: Vec<Type>, // TOOD: rename this params
    pub ret: Box<Type>,
}

impl PartialEq for LamType {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.ret == other.ret
    }
}

impl Hash for LamType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.params.hash(state);
        self.ret.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct PrimType {
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
pub struct IntersectionType {
    pub types: Vec<Type>,
}

impl PartialEq for IntersectionType {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl Hash for IntersectionType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.types.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WidenFlag {
    Intersection,
    Union,
}

#[derive(Clone, Debug, Eq)]
pub struct ObjectType {
    pub props: Vec<TProp>,
    pub widen_flag: Option<WidenFlag>,
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

#[derive(Clone, Debug, Eq)]
pub struct TupleType {
    pub types: Vec<Type>,
}

impl PartialEq for TupleType {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl Hash for TupleType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.types.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct RestType {
    pub ty: Box<Type>,
}

impl PartialEq for RestType {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Hash for RestType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct MemberType {
    pub obj: Box<Type>,
    pub prop: String, // TODO: allow numbers as well for accessing elements on tuples and arrays
}

impl PartialEq for MemberType {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj && self.prop == other.prop
    }
}

impl Hash for MemberType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.obj.hash(state);
        self.prop.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Variant {
    Var,
    Lam(LamType),
    Prim(PrimType),
    Lit(LitType),
    Union(UnionType),
    Intersection(IntersectionType),
    Object(ObjectType),
    Alias(AliasType),
    Tuple(TupleType),
    Rest(RestType),
    Member(MemberType),
}

#[derive(Clone, Debug, Eq)]
pub struct Type {
    pub variant: Variant,
    pub id: i32,
    pub frozen: bool,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match &self.variant {
            Variant::Var => self.id == other.id,
            _ => self.variant == other.variant,
        }
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.variant {
            Variant::Var => self.id.hash(state),
            _ => self.variant.hash(state),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.variant {
            Variant::Var => {
                let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                    .chars()
                    .collect();
                let id = chars.get(self.id as usize).unwrap();
                write!(f, "{}", id)
            }
            Variant::Lam(LamType { params, ret, .. }) => {
                write!(f, "({}) => {}", join(params, ", "), ret)
            }
            Variant::Prim(PrimType { prim, .. }) => write!(f, "{}", prim),
            Variant::Lit(LitType { lit, .. }) => write!(f, "{}", lit),
            Variant::Union(UnionType { types, .. }) => write!(f, "{}", join(types, " | ")),
            Variant::Intersection(IntersectionType { types, .. }) => {
                write!(f, "{}", join(types, " & "))
            }
            Variant::Object(ObjectType { props, .. }) => write!(f, "{{{}}}", join(props, ", ")),
            Variant::Alias(AliasType {
                name, type_params, ..
            }) => match type_params {
                Some(params) => write!(f, "{name}<{}>", join(params, ", ")),
                None => write!(f, "{name}"),
            },
            Variant::Tuple(TupleType { types, .. }) => write!(f, "[{}]", join(types, ", ")),
            Variant::Rest(RestType { ty, .. }) => write!(f, "...{ty}"),
            Variant::Member(MemberType { obj, prop, .. }) => write!(f, "{obj}[\"{prop}\"]"),
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

// TODO: make this recursive
pub fn freeze(ty: Type) -> Type {
    let variant = match ty.variant {
        Variant::Var => Variant::Var,
        Variant::Lam(lam) => Variant::Lam(LamType {
            params: lam.params.into_iter().map(freeze).collect(),
            ret: Box::from(freeze(lam.ret.as_ref().clone())),
        }),
        Variant::Prim(prim) => Variant::Prim(prim),
        Variant::Lit(lit) => Variant::Lit(lit),
        Variant::Union(union) => Variant::Union(UnionType {
            types: union.types.into_iter().map(freeze).collect(),
        }),
        Variant::Intersection(intersection) => Variant::Intersection(IntersectionType {
            types: intersection.types.into_iter().map(freeze).collect(),
        }),
        Variant::Object(obj) => Variant::Object(ObjectType {
            props: obj
                .props
                .into_iter()
                .map(|prop| TProp {
                    ty: freeze(prop.ty),
                    ..prop
                })
                .collect(),
            widen_flag: obj.widen_flag,
        }),
        Variant::Alias(alias) => Variant::Alias(AliasType {
            name: alias.name,
            type_params: alias
                .type_params
                .map(|type_params| type_params.into_iter().map(freeze).collect()),
        }),
        Variant::Tuple(tuple) => Variant::Tuple(TupleType {
            types: tuple.types.into_iter().map(freeze).collect(),
        }),
        Variant::Rest(rest) => Variant::Rest(RestType {
            ty: Box::from(freeze(rest.ty.as_ref().clone())),
        }),
        Variant::Member(member) => Variant::Member(MemberType {
            obj: Box::from(freeze(member.obj.as_ref().clone())),
            prop: member.prop,
        }),
    };
    Type { variant, id: ty.id, frozen: true }
}
