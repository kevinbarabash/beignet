// Types and type constructors
use std::collections::HashMap;

pub type ArenaType = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub instance: Option<ArenaType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub name: String,
    pub types: Vec<ArenaType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub params: Vec<ArenaType>,
    pub ret: ArenaType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Variable(Variable),
    Constructor(Constructor),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub id: ArenaType,
    pub kind: TypeKind,
}

/// A type variable standing for an arbitrary type.
///
/// All type variables have a unique id, but names are
/// only assigned lazily, when required.

impl Type {
    pub fn new_variable(idx: ArenaType) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Variable(Variable { instance: None }),
        }
    }

    pub fn new_constructor(idx: ArenaType, name: &str, types: &[ArenaType]) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Constructor(Constructor {
                name: name.to_string(),
                types: types.to_vec(),
            }),
        }
    }

    pub fn new_function(idx: ArenaType, param_types: &[ArenaType], ret_type: ArenaType) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Function(Function {
                params: param_types.to_vec(),
                ret: ret_type,
            }),
        }
    }

    pub fn set_instance(&mut self, instance: ArenaType) {
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

    pub fn as_string(&self, a: &Vec<Type>, namer: &mut Namer) -> String {
        match &self.kind {
            TypeKind::Variable(Variable {
                instance: Some(inst),
            }) => a[*inst].as_string(a, namer),
            TypeKind::Variable(_) => namer.name(self.id),
            TypeKind::Constructor(con) => match con.types.len() {
                0 => con.name.clone(),
                2 => {
                    let l = a[con.types[0]].as_string(a, namer);
                    let r = a[con.types[1]].as_string(a, namer);
                    format!("({} {} {})", l, con.name, r)
                }
                _ => {
                    let mut coll = vec![];
                    for v in &con.types {
                        coll.push(a[*v].as_string(a, namer));
                    }
                    format!("{} {}", con.name, coll.join(" "))
                }
            },
            TypeKind::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|param| a[*param].as_string(a, namer))
                    .collect::<Vec<_>>();

                let ret = a[func.ret].as_string(a, namer);

                format!("({} -> {ret})", params.join(", "))
            }
        }
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

pub struct Namer {
    pub value: char,
    pub set: HashMap<ArenaType, String>,
}
