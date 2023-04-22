// Types and type constructors
use generational_arena::{Arena, Index};

use crate::ast::Lit;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub id: usize,
    pub instance: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub name: String,
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ref {
    pub name: String,
    // TODO: Add support for type args
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub params: Vec<Index>,
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParam {
    pub name: String,
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
pub struct Tuple {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object {
    pub props: Vec<(String, Index)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Variable(Variable),
    Constructor(Constructor),
    Ref(Ref),
    Literal(Lit),
    Function(Function),
    Union(Union),
    Tuple(Tuple),
    Object(Object),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    // pub id: Index,
    pub kind: TypeKind,
}

/// A type variable standing for an arbitrary type.
///
/// All type variables have a unique id, but names are
/// only assigned lazily, when required.

impl Type {
    pub fn set_instance(&mut self, instance: Index) {
        match &mut self.kind {
            TypeKind::Variable(Variable {
                instance: ref mut inst,
                ..
            }) => {
                *inst = Some(instance);
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
            TypeKind::Variable(Variable { id, .. }) => {
                format!("t{id}")
            }
            TypeKind::Constructor(con) => match con.types.len() {
                0 => con.name.clone(),
                2 => {
                    let l = arena[con.types[0]].as_string(arena);
                    let r = arena[con.types[1]].as_string(arena);
                    format!("({} {} {})", l, con.name, r)
                }
                _ => {
                    let mut coll = vec![];
                    for v in &con.types {
                        coll.push(arena[*v].as_string(arena));
                    }

                    if coll.is_empty() {
                        con.name.to_string()
                    } else {
                        format!("{}<{}>", con.name, coll.join(", "))
                    }
                }
            },
            TypeKind::Ref(Ref { name }) => name.clone(),
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Tuple(tuple) => {
                format!("[{}]", types_to_strings(arena, &tuple.types).join(", "))
            }
            TypeKind::Object(object) => {
                let mut fields = vec![];
                for (k, v) in &object.props {
                    fields.push(format!("{}: {}", k, arena[*v].as_string(arena)));
                }
                format!("{{{}}}", fields.join(", "))
            }
            TypeKind::Function(func) => {
                let type_params = match &func.type_params {
                    Some(type_params) if !type_params.is_empty() => {
                        let type_params = type_params
                            .iter()
                            .map(|tp| tp.name.clone())
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
            TypeKind::Union(union) => types_to_strings(arena, &union.types).join(" | "),
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
            ret,
            type_params: type_params.map(|x| x.to_vec()),
        }),
    })
}

pub fn new_union_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    arena.insert(Type {
        kind: TypeKind::Union(Union {
            types: types.to_vec(),
        }),
    })
}

pub fn new_tuple_type(arena: &mut Arena<Type>, types: &[Index]) -> Index {
    arena.insert(Type {
        kind: TypeKind::Tuple(Tuple {
            types: types.to_vec(),
        }),
    })
}

pub fn new_object_type(arena: &mut Arena<Type>, props: &[(String, Index)]) -> Index {
    arena.insert(Type {
        kind: TypeKind::Object(Object {
            props: props.to_vec(),
        }),
    })
}

/// A binary type constructor which builds function types
pub fn new_var_type(arena: &mut Arena<Type>) -> Index {
    arena.insert(Type {
        kind: TypeKind::Variable(Variable {
            instance: None,
            id: arena.len(),
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

pub fn new_type_ref(arena: &mut Arena<Type>, name: &str) -> Index {
    arena.insert(Type {
        kind: TypeKind::Ref(Ref {
            name: name.to_string(),
        }),
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
