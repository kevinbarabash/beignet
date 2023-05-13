// Types and type constructors
use generational_arena::{Arena, Index};

use crate::ast::Lit;

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
pub struct Ref {
    pub name: String,
    // TODO: Add support for type args
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub params: Vec<Index>, // TODO: require param names
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TMethod {
    pub name: TPropKey,
    pub params: Vec<Index>, // TODO: require param names
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
pub enum TObjElem {
    // Call(TCallable),
    // Constructor(TCallable),
    Method(TMethod),
    // Getter(TGetter),
    // Setter(TSetter),
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
pub enum TypeKind {
    Variable(Variable),       // TODO: rename to TypeVar
    Constructor(Constructor), // TODO: rename to TypeRef
    Literal(Lit),
    Function(Function),
    Object(Object),
    Rest(Rest),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    // pub id: Index,
    pub kind: TypeKind,
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
            TypeKind::Constructor(Constructor { name, types }) => match name.as_str() {
                "@@tuple" => format!("[{}]", types_to_strings(arena, types).join(", ")),
                "@@union" => types_to_strings(arena, types).join(" | "),
                "@@intersection" => types_to_strings(arena, types).join(" & "),
                _ => {
                    let mut coll = vec![];
                    for v in types {
                        coll.push(arena[*v].as_string(arena));
                    }

                    if coll.is_empty() {
                        name.to_string()
                    } else {
                        format!("{}<{}>", name, coll.join(", "))
                    }
                }
            },
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Object(object) => {
                let mut fields = vec![];
                for prop in &object.props {
                    match prop {
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
                                types_to_strings(arena, params).join(", "),
                                arena[*ret].as_string(arena)
                            ));
                        }
                        TObjElem::Index(TIndex { key, mutable, t }) => {
                            let t = arena[*t].as_string(arena);
                            if *mutable {
                                fields.push(format!("{}: mut {}", key.name, t));
                            } else {
                                fields.push(format!("{}: {}", key.name, t));
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
                    types_to_strings(arena, &func.params).join(", "),
                    arena[func.ret].as_string(arena),
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

/// A binary type constructor which builds function types
pub fn new_func_type(
    arena: &mut Arena<Type>,
    params: &[Index],
    ret: Index,
    type_params: Option<Vec<TypeParam>>,
) -> Index {
    arena.insert(Type {
        kind: TypeKind::Function(Function {
            params: params.to_vec(),
            ret: ret.to_owned(),
            type_params,
        }),
    })
}

pub fn new_union_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    new_constructor(arena, "@@union", types)
}

pub fn new_intersection_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    new_constructor(arena, "@@intersection", types)
}

pub fn new_tuple_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    new_constructor(arena, "@@tuple", types)
}

pub fn new_object_type(arena: &mut Arena<Type>, elems: &[TObjElem]) -> Index {
    arena.insert(Type {
        kind: TypeKind::Object(Object {
            props: elems.to_vec(),
        }),
    })
}

/// A binary type constructor which builds function types
pub fn new_var_type(arena: &mut Arena<Type>, constraint: Option<Index>) -> Index {
    arena.insert(Type {
        kind: TypeKind::Variable(Variable {
            id: arena.len(), // use for debugging purposes only
            instance: None,
            constraint,
        }),
    })
}

/// A binary type constructor which builds function types
pub fn new_constructor(arena: &mut Arena<Type>, name: &str, types: &[Index]) -> Index {
    arena.insert(Type {
        kind: TypeKind::Constructor(Constructor {
            name: name.to_string(),
            types: types.to_vec(),
        }),
    })
}

pub fn new_rest_type(arena: &mut Arena<Type>, t: Index) -> Index {
    arena.insert(Type {
        kind: TypeKind::Rest(Rest { arg: t }),
    })
}

pub fn new_lit_type(arena: &mut Arena<Type>, lit: &Lit) -> Index {
    arena.insert(Type {
        kind: TypeKind::Literal(lit.clone()),
    })
}

//impl fmt::Debug for Type {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        match self {
//            write!(f, "TypeVariable(id = {})", self.id)
//            write!(f, "TypeOperator(name, )", self.id)
//        }
//    }
//}
