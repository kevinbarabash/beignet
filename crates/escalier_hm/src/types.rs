// Types and type constructors
use generational_arena::{Arena, Index};
use std::convert::From;
use std::fmt;

// TODO: create type versions of these so that we don't have to bother
// with source locations when doing type-level stuff.
use escalier_ast::{BindingIdent, Literal as Lit};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub id: usize,
    pub instance: Option<Index>,
    pub constraint: Option<Index>,
}

// TODO: rename this TypeRef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub name: String,
    // TODO: rename this to type_args
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Number,
    Boolean,
    String,
    Symbol,
    Null,
    Undefined,
    Unknown,
    Never,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Keyword::Number => "number",
            Keyword::Boolean => "boolean",
            Keyword::String => "string",
            Keyword::Symbol => "symbol",
            Keyword::Null => "null",
            Keyword::Undefined => "undefined",
            Keyword::Unknown => "unknown",
            Keyword::Never => "never",
        };
        write!(f, "{result}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub params: Vec<FuncParam>,
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncParam {
    pub pattern: TPat,
    pub t: Index,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TPat {
    Ident(BindingIdent),
    Rest(RestPat),
    Tuple(TuplePat),
    Object(TObjectPat),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RestPat {
    pub arg: Box<TPat>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TuplePat {
    pub elems: Vec<Option<TPat>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TObjectPat {
    pub props: Vec<TObjectPatProp>,
}

// TODO: update this to match AST changes to ObjectPatProp
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TObjectPatProp {
    KeyValue(TObjectKeyValuePatProp),
    Assign(TObjectAssignPatProp),
    Rest(RestPat),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TObjectKeyValuePatProp {
    pub key: String,
    pub value: TPat,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TObjectAssignPatProp {
    pub key: String,
    pub value: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeParam {
    pub name: String,
    pub constraint: Option<Index>,
    pub default: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub args: Vec<Index>,
    pub ret: Index,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TMethod {
    pub name: TPropKey,
    pub params: Vec<FuncParam>,
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
    pub is_mutating: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TIndex {
    pub key: TIndexKey,
    pub mutable: bool,
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TIndexKey {
    pub name: String,
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TPropKey {
    StringKey(String),
    NumberKey(String),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TProp {
    pub name: TPropKey,
    pub optional: bool,
    pub mutable: bool,
    pub t: Index,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TCallable {
    pub params: Vec<FuncParam>,
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
    // TODO: support mutating callables? ...they'd still need have the same type
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TGetter {
    pub name: TPropKey,
    pub ret: Index,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TSetter {
    pub name: TPropKey,
    pub param: FuncParam,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TObjElem {
    Call(TCallable),
    Constructor(TCallable),
    Method(TMethod),
    Getter(TGetter),
    Setter(TSetter),
    Index(TIndex),
    Prop(TProp),
    // RestSpread - we can use this instead of converting {a, ...x} to {a} & tvar
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object {
    pub props: Vec<TObjElem>,
}

// NOTE: this is only used for the rest element in array patterns since we
// treat `{a, ...x}` as `{a} & x` where `x` is a type variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rest {
    pub arg: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UtilityKind {
    KeyOf,
    Index,
    Cond,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Utility {
    pub kind: UtilityKind,
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mutable {
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Union {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Intersection {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, Hash)]
pub enum TypeKind {
    Variable(Variable),       // TODO: rename to TypeVar
    Constructor(Constructor), // TODO: rename to TypeRef
    Union(Union),
    Intersection(Intersection),
    Tuple(Tuple),
    Keyword(Keyword),
    Literal(Lit),
    Function(Function),
    Object(Object),
    Rest(Rest),
    Utility(Utility),
    Mutable(Mutable),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    // TODO: add `provenance` to support error reporting
}

impl From<TypeKind> for Type {
    fn from(kind: TypeKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scheme {
    pub t: Index,
    pub type_params: Option<Vec<TypeParam>>,
}

/// A type variable standing for an arbitrary type.
///
/// All type variables have a unique id, but names are
/// only assigned lazily, when required.

impl Type {
    pub fn set_instance(&mut self, instance: Index) {
        match &mut self.kind {
            TypeKind::Variable(var) => {
                var.instance = Some(instance);
            }
            _ => {
                unimplemented!()
            }
        }
    }

    pub fn as_string(&self, arena: &Arena<Type>) -> String {
        match &self.kind {
            TypeKind::Variable(Variable {
                instance: Some(inst),
                ..
            }) => arena[*inst].as_string(arena),
            TypeKind::Variable(Variable { id, constraint, .. }) => match constraint {
                Some(constraint) => format!("t{id}:{}", arena[*constraint].as_string(arena)),
                None => format!("t{id}"),
            },
            TypeKind::Union(Union { types }) => types_to_strings(arena, types).join(" | "),
            TypeKind::Intersection(Intersection { types }) => {
                types_to_strings(arena, types).join(" & ")
            }
            TypeKind::Tuple(Tuple { types }) => {
                format!("[{}]", types_to_strings(arena, types).join(", "))
            }
            TypeKind::Constructor(Constructor { name, types }) => {
                if types.is_empty() {
                    name.to_string()
                } else {
                    format!("{}<{}>", name, types_to_strings(arena, types).join(", "))
                }
            }
            TypeKind::Keyword(keyword) => keyword.to_string(),
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Object(object) => {
                let mut fields = vec![];
                for prop in &object.props {
                    match prop {
                        TObjElem::Constructor(TCallable {
                            params,
                            ret,
                            type_params,
                        }) => {
                            todo!()
                        }
                        TObjElem::Call(TCallable {
                            params,
                            ret,
                            type_params,
                        }) => {
                            todo!()
                        }
                        TObjElem::Method(TMethod {
                            name,
                            params,
                            ret,
                            type_params,
                            is_mutating: _, // TODO
                        }) => {
                            let name = match name {
                                TPropKey::StringKey(s) => s,
                                TPropKey::NumberKey(n) => n,
                            };
                            let mut result = name.to_string();
                            match type_params {
                                Some(type_params) if !type_params.is_empty() => {
                                    let type_params = type_params
                                        .iter()
                                        .map(|tp| match &tp.constraint {
                                            Some(constraint) => format!(
                                                "{}:{}",
                                                tp.name.clone(),
                                                arena[*constraint].as_string(arena)
                                            ),
                                            None => tp.name.clone(),
                                        })
                                        .collect::<Vec<_>>();
                                    result.push_str(&format!("<{}>", type_params.join(", ")))
                                }
                                _ => (),
                            };
                            result.push_str(&format!(
                                "({}): {}",
                                params_to_strings(arena, params).join(", "),
                                arena[*ret].as_string(arena)
                            ));
                        }
                        TObjElem::Getter(TGetter { name, ret }) => {
                            todo!()
                        }
                        TObjElem::Setter(TSetter { name, param }) => {
                            todo!()
                        }
                        TObjElem::Index(TIndex { key, mutable, t }) => {
                            let t = arena[*t].as_string(arena);
                            let key_t = arena[key.t].as_string(arena);
                            let name = &key.name;
                            if *mutable {
                                fields.push(format!("[{name}: {key_t}]: mut {t}"));
                            } else {
                                fields.push(format!("[{name}: {key_t}]: {t}"));
                            }
                        }
                        TObjElem::Prop(TProp {
                            name,
                            optional,
                            mutable: _,
                            t,
                        }) => {
                            let name = match name {
                                TPropKey::StringKey(s) => s,
                                TPropKey::NumberKey(n) => n,
                            };
                            let t = arena[*t].as_string(arena);
                            if *optional {
                                fields.push(format!("{}?: {}", name, t));
                            } else {
                                fields.push(format!("{}: {}", name, t));
                            }
                        }
                    }
                }
                format!("{{{}}}", fields.join(", "))
            }
            TypeKind::Rest(rest) => {
                format!("...{}", arena[rest.arg].as_string(arena))
            }
            TypeKind::Function(func) => {
                let type_params = match &func.type_params {
                    Some(type_params) if !type_params.is_empty() => {
                        let type_params = type_params
                            .iter()
                            .map(|tp| match &tp.constraint {
                                Some(constraint) => format!(
                                    "{}:{}",
                                    tp.name.clone(),
                                    arena[*constraint].as_string(arena)
                                ),
                                None => tp.name.clone(),
                            })
                            .collect::<Vec<_>>();
                        format!("<{}>", type_params.join(", "))
                    }
                    _ => "".to_string(),
                };
                format!(
                    "{type_params}({}) => {}",
                    params_to_strings(arena, &func.params).join(", "),
                    arena[func.ret].as_string(arena),
                )
            }
            TypeKind::Utility(Utility { kind: name, types }) => match name {
                UtilityKind::KeyOf => format!("keyof {}", arena[types[0]].as_string(arena)),
                UtilityKind::Index => format!(
                    "{}[{}]",
                    arena[types[0]].as_string(arena),
                    arena[types[1]].as_string(arena)
                ),
                UtilityKind::Cond => format!(
                    "{} extends {} ? {} : {}",
                    arena[types[0]].as_string(arena),
                    arena[types[1]].as_string(arena),
                    arena[types[2]].as_string(arena),
                    arena[types[3]].as_string(arena),
                ),
            },
            TypeKind::Mutable(Mutable { t }) => format!("mut {}", arena[*t].as_string(arena)),
        }
    }
}

fn types_to_strings(a: &Arena<Type>, types: &[Index]) -> Vec<String> {
    let mut strings = vec![];
    for v in types {
        strings.push(a[*v].as_string(a));
    }
    strings
}

fn params_to_strings(arena: &Arena<Type>, params: &[FuncParam]) -> Vec<String> {
    let mut strings = vec![];
    for FuncParam {
        pattern,
        t,
        optional,
    } in params
    {
        let name = tpat_to_string(arena, pattern);
        strings.push(match optional {
            true => format!("{name}?: {}", arena[*t].as_string(arena)),
            false => format!("{name}: {}", arena[*t].as_string(arena)),
        });
    }
    strings
}

fn tpat_to_string(_arena: &Arena<Type>, pattern: &TPat) -> String {
    match pattern {
        TPat::Ident(BindingIdent {
            name, mutable: _, ..
        }) => name.to_owned(),
        TPat::Rest(RestPat { arg }) => format!("...{}", tpat_to_string(_arena, arg.as_ref())),
        TPat::Tuple(TuplePat { elems }) => format!(
            "[{}]",
            elems
                .iter()
                .map(|elem| match elem {
                    Some(elem) => tpat_to_string(_arena, elem),
                    None => " ".to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
        TPat::Object(TObjectPat { props }) => {
            let props: Vec<String> = props
                .iter()
                .map(|prop| match prop {
                    TObjectPatProp::KeyValue(TObjectKeyValuePatProp { key, value }) => {
                        match value {
                            TPat::Ident(_) => key.to_string(),
                            _ => format!("{}: {}", key, tpat_to_string(_arena, value)),
                        }
                    }
                    // TODO: handle assignments in object patterns
                    TObjectPatProp::Assign(TObjectAssignPatProp { key, value: _ }) => {
                        key.to_string()
                    }
                    TObjectPatProp::Rest(RestPat { arg }) => {
                        format!("...{}", tpat_to_string(_arena, arg.as_ref()))
                    }
                })
                .collect();
            format!("{{{}}}", props.join(", "))
        }
    }
}

/// A binary type constructor which builds function types
pub fn new_func_type(
    arena: &mut Arena<Type>,
    params: &[FuncParam],
    ret: Index,
    type_params: Option<Vec<TypeParam>>,
) -> Index {
    arena.insert(Type::from(TypeKind::Function(Function {
        params: params.to_vec(),
        ret: ret.to_owned(),
        type_params,
    })))
}

pub fn new_union_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    arena.insert(Type::from(TypeKind::Union(Union {
        types: types.to_owned(),
    })))
}

pub fn new_intersection_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    arena.insert(Type::from(TypeKind::Intersection(Intersection {
        types: types.to_owned(),
    })))
}

pub fn new_tuple_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    arena.insert(Type::from(TypeKind::Tuple(Tuple {
        types: types.to_owned(),
    })))
}

pub fn new_object_type(arena: &mut Arena<Type>, elems: &[TObjElem]) -> Index {
    arena.insert(Type::from(TypeKind::Object(Object {
        props: elems.to_vec(),
    })))
}

/// A binary type constructor which builds function types
pub fn new_var_type(arena: &mut Arena<Type>, constraint: Option<Index>) -> Index {
    arena.insert(Type::from(TypeKind::Variable(Variable {
        id: arena.len(), // use for debugging purposes only
        instance: None,
        constraint,
    })))
}

/// A binary type constructor which builds function types
pub fn new_constructor(arena: &mut Arena<Type>, name: &str, types: &[Index]) -> Index {
    arena.insert(Type::from(TypeKind::Constructor(Constructor {
        name: name.to_string(),
        types: types.to_vec(),
    })))
}

pub fn new_keyword(arena: &mut Arena<Type>, keyword: Keyword) -> Index {
    arena.insert(Type::from(TypeKind::Keyword(keyword)))
}

pub fn new_rest_type(arena: &mut Arena<Type>, t: Index) -> Index {
    arena.insert(Type::from(TypeKind::Rest(Rest { arg: t })))
}

pub fn new_lit_type(arena: &mut Arena<Type>, lit: &Lit) -> Index {
    arena.insert(Type::from(TypeKind::Literal(lit.clone())))
}

pub fn new_utility_type(arena: &mut Arena<Type>, kind: UtilityKind, types: &[Index]) -> Index {
    arena.insert(Type::from(TypeKind::Utility(Utility {
        kind,
        types: types.to_vec(),
    })))
}

pub fn new_mutable_type(arena: &mut Arena<Type>, t: Index) -> Index {
    arena.insert(Type::from(TypeKind::Mutable(Mutable { t })))
}

impl Type {
    pub fn equals(&self, other: &Type, arena: &Arena<Type>) -> bool {
        match (&self.kind, &other.kind) {
            (TypeKind::Variable(v1), TypeKind::Variable(v2)) => {
                let (a, b) = match (v1.instance, v2.instance) {
                    (Some(a), Some(b)) => (&arena[a], &arena[b]),
                    (Some(a), None) => (&arena[a], other),
                    (None, Some(b)) => (self, &arena[b]),
                    (None, None) => return v1.id == v2.id,
                };
                a.equals(b, arena)
            }
            (TypeKind::Constructor(c1), TypeKind::Constructor(c2)) => {
                c1.name == c2.name && types_equal(arena, &c1.types, &c2.types)
            }
            (TypeKind::Union(union1), TypeKind::Union(union2)) => {
                types_equal(arena, &union1.types, &union2.types)
            }
            (TypeKind::Intersection(int1), TypeKind::Intersection(int2)) => {
                types_equal(arena, &int1.types, &int2.types)
            }
            (TypeKind::Tuple(tuple1), TypeKind::Tuple(tuple2)) => {
                types_equal(arena, &tuple1.types, &tuple2.types)
            }
            (TypeKind::Keyword(kw1), TypeKind::Keyword(kw2)) => kw1 == kw2,
            (TypeKind::Literal(l1), TypeKind::Literal(l2)) => l1 == l2,
            (TypeKind::Function(f1), TypeKind::Function(f2)) => {
                eprintln!("checking function types");
                let ret1 = &arena[f1.ret];
                let ret2 = &arena[f2.ret];
                f1.params.len() == f2.params.len()
                    && f1.params.iter().zip(f2.params.iter()).all(|(a, b)| {
                        let a = &arena[a.t];
                        let b = &arena[b.t];
                        a.equals(b, arena)
                    })
                    && ret1.equals(ret2, arena)
            }
            (TypeKind::Object(o1), TypeKind::Object(o2)) => {
                o1.props.len() == o2.props.len()
                    && o1
                        .props
                        .iter()
                        .all(|p1| o2.props.iter().any(|p2| obj_elem_equals(arena, p1, p2)))
            }
            (TypeKind::Rest(r1), TypeKind::Rest(r2)) => {
                let t1 = &arena[r1.arg];
                let t2 = &arena[r2.arg];
                t1.equals(t2, arena)
            }
            (TypeKind::Utility(_), TypeKind::Utility(_)) => {
                todo!()
            }
            (TypeKind::Mutable(m1), TypeKind::Mutable(m2)) => {
                let t1 = &arena[m1.t];
                let t2 = &arena[m2.t];
                t1.equals(t2, arena)
            }
            _ => false,
        }
    }
}

fn types_equal(arena: &Arena<Type>, types1: &[Index], types2: &[Index]) -> bool {
    types1.len() == types2.len()
        && types1.iter().zip(types2.iter()).all(|(a, b)| {
            let a = &arena[*a];
            let b = &arena[*b];
            a.equals(b, arena)
        })
}

fn obj_elem_equals(arena: &Arena<Type>, elem1: &TObjElem, elem2: &TObjElem) -> bool {
    match (elem1, elem2) {
        (TObjElem::Method(m1), TObjElem::Method(m2)) => {
            m1.name == m2.name
                && m1.params.len() == m2.params.len()
                && m1.params.iter().zip(m2.params.iter()).all(|(a, b)| {
                    let a = &arena[a.t];
                    let b = &arena[b.t];
                    a.equals(b, arena)
                })
        }
        (TObjElem::Index(i1), TObjElem::Index(i2)) => {
            // TODO: compare key types as well
            let t1 = &arena[i1.t];
            let t2 = &arena[i2.t];
            t1.equals(t2, arena)
        }
        (TObjElem::Prop(p1), TObjElem::Prop(p2)) => {
            let t1 = &arena[p1.t];
            let t2 = &arena[p2.t];
            t1.equals(t2, arena)
        }
        _ => false,
    }
}

//impl fmt::Debug for Type {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        match self {
//            write!(f, "TypeVariable(id = {})", self.id)
//            write!(f, "TypeOperator(name, )", self.id)
//        }
//    }
//}
