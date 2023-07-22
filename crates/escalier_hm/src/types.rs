// Types and type constructors
use generational_arena::{Arena, Index};
use std::convert::From;
use std::fmt;

// TODO: create type versions of these so that we don't have to bother
// with source locations when doing type-level stuff.
use escalier_ast::{BindingIdent, Literal as Lit};

use crate::provenance::Provenance;

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
    Null,
    Undefined,
    Unknown,
    Never,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Self::Null => "null",
            Self::Undefined => "undefined",
            Self::Unknown => "unknown",
            Self::Never => "never",
        };
        write!(f, "{result}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Number,
    Boolean,
    String,
    Symbol,
}

impl Primitive {
    pub fn get_scheme_name(&self) -> &'static str {
        match self {
            Self::Number => "Number",
            Self::Boolean => "Boolean",
            Self::String => "String",
            Self::Symbol => "Symbol",
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Self::Number => "number",
            Self::Boolean => "boolean",
            Self::String => "string",
            Self::Symbol => "symbol",
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
    Lit(TLitPat),
    Is(TIsPat),
    Wildcard,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TLitPat {
    pub lit: Lit,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TIsPat {
    pub ident: String,
    pub is_id: String,
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

impl fmt::Display for TPropKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TPropKey::StringKey(key) => write!(f, "{key}"),
            TPropKey::NumberKey(key) => write!(f, "{key}"),
        }
    }
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
    // NOTE: type_params on constructors should be a subset of type_params on
    // the object scheme in which they live
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
    pub elems: Vec<TObjElem>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeyOf {
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexedAccess {
    pub obj: Index,
    pub index: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Conditional {
    pub check: Index,
    pub extends: Index,
    pub true_type: Index,
    pub false_type: Index,
}

#[derive(Debug, Clone, Hash)]
pub enum TypeKind {
    Variable(Variable),       // TODO: rename to TypeVar
    Constructor(Constructor), // TODO: rename to TypeRef
    Union(Union),
    Intersection(Intersection),
    Tuple(Tuple),
    Keyword(Keyword),
    Primitive(Primitive),
    Literal(Lit),
    Function(Function),
    Object(Object),
    Rest(Rest), // Why is this its own type?
    KeyOf(KeyOf),
    IndexedAccess(IndexedAccess),
    Conditional(Conditional),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub provenance: Option<Provenance>,
}

impl From<TypeKind> for Type {
    fn from(kind: TypeKind) -> Self {
        Self {
            kind,
            provenance: None,
        }
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
            TypeKind::Primitive(primitive) => primitive.to_string(),
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Object(object) => {
                let mut fields = vec![];
                for prop in &object.elems {
                    match prop {
                        TObjElem::Constructor(TCallable {
                            params,
                            ret,
                            type_params,
                        }) => {
                            let mut result = "fn".to_string();
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
                            fields.push(result);
                        }
                        TObjElem::Call(TCallable {
                            params,
                            ret,
                            type_params,
                        }) => {
                            let mut result = "fn".to_string();
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
                            fields.push(result);
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
                            let mut result = format!("fn {name}");
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
                            fields.push(result);
                        }
                        TObjElem::Getter(TGetter { name, ret }) => {
                            let name = match name {
                                TPropKey::StringKey(s) => s,
                                TPropKey::NumberKey(n) => n,
                            };
                            fields.push(format!(
                                "get {name}(self): {}",
                                arena[*ret].as_string(arena)
                            ))
                        }
                        TObjElem::Setter(TSetter { name, param }) => {
                            let name = match name {
                                TPropKey::StringKey(s) => s,
                                TPropKey::NumberKey(n) => n,
                            };
                            fields.push(format!(
                                "set {name}(self, {}): undefined",
                                param_to_string(arena, param)
                            ))
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
            TypeKind::KeyOf(KeyOf { t }) => format!("keyof {}", arena[*t].as_string(arena)),
            TypeKind::IndexedAccess(IndexedAccess { obj, index }) => format!(
                "{}[{}]",
                arena[*obj].as_string(arena),
                arena[*index].as_string(arena)
            ),
            TypeKind::Conditional(Conditional {
                check,
                extends,
                true_type,
                false_type,
            }) => {
                format!(
                    "{} extends {} ? {} : {}",
                    arena[*check].as_string(arena),
                    arena[*extends].as_string(arena),
                    arena[*true_type].as_string(arena),
                    arena[*false_type].as_string(arena),
                )
            }
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

fn param_to_string(arena: &Arena<Type>, param: &FuncParam) -> String {
    let name = tpat_to_string(arena, &param.pattern);
    match param.optional {
        true => format!("{name}?: {}", arena[param.t].as_string(arena)),
        false => format!("{name}: {}", arena[param.t].as_string(arena)),
    }
}

fn params_to_strings(arena: &Arena<Type>, params: &[FuncParam]) -> Vec<String> {
    let mut strings = vec![];
    for param in params {
        strings.push(param_to_string(arena, param))
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
        TPat::Lit(TLitPat { lit }) => lit.to_string(),
        TPat::Is(TIsPat { ident, is_id }) => {
            format!("{ident} is {is_id}")
        }
        TPat::Wildcard => "_".to_string(),
    }
}

/// A binary type constructor which builds function types
pub fn new_func_type(
    arena: &mut Arena<Type>,
    params: &[FuncParam],
    ret: Index,
    type_params: &Option<Vec<TypeParam>>,
) -> Index {
    arena.insert(Type::from(TypeKind::Function(Function {
        params: params.to_vec(),
        ret: ret.to_owned(),
        type_params: type_params.to_owned(),
    })))
}

pub fn new_union_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    match types.len() {
        0 => new_keyword(arena, Keyword::Never),
        1 => types[0],
        _ => arena.insert(Type::from(TypeKind::Union(Union {
            types: types.to_owned(),
        }))),
    }
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
        elems: elems.to_vec(),
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

pub fn new_primitive(arena: &mut Arena<Type>, primitive: Primitive) -> Index {
    arena.insert(Type::from(TypeKind::Primitive(primitive)))
}

pub fn new_rest_type(arena: &mut Arena<Type>, t: Index) -> Index {
    arena.insert(Type::from(TypeKind::Rest(Rest { arg: t })))
}

pub fn new_lit_type(arena: &mut Arena<Type>, lit: &Lit) -> Index {
    arena.insert(Type::from(TypeKind::Literal(lit.clone())))
}

pub fn new_keyof_type(arena: &mut Arena<Type>, t: Index) -> Index {
    arena.insert(Type::from(TypeKind::KeyOf(KeyOf { t })))
}

pub fn new_indexed_access_type(arena: &mut Arena<Type>, obj: Index, index: Index) -> Index {
    arena.insert(Type::from(TypeKind::IndexedAccess(IndexedAccess {
        obj,
        index,
    })))
}

pub fn new_conditional_type(
    arena: &mut Arena<Type>,
    check: Index,
    extends: Index,
    true_type: Index,
    false_type: Index,
) -> Index {
    arena.insert(Type::from(TypeKind::Conditional(Conditional {
        check,
        extends,
        true_type,
        false_type,
    })))
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
            (TypeKind::Primitive(prim1), TypeKind::Primitive(prim2)) => prim1 == prim2,
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
                o1.elems.len() == o2.elems.len()
                    && o1
                        .elems
                        .iter()
                        .all(|p1| o2.elems.iter().any(|p2| obj_elem_equals(arena, p1, p2)))
            }
            (TypeKind::Rest(r1), TypeKind::Rest(r2)) => {
                let t1 = &arena[r1.arg];
                let t2 = &arena[r2.arg];
                t1.equals(t2, arena)
            }
            // TODO:
            // - unification of object and intersection
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
