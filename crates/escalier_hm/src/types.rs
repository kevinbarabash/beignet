// Types and type constructors
use derive_visitor::{Drive, DriveMut};

use crate::literal::Literal;

#[derive(Debug, Clone, Drive, DriveMut, Copy, PartialEq, Eq, Hash)]
pub struct ArenaType {
    #[drive(skip)]
    pub value: usize,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Variable {
    pub instance: Option<ArenaType>,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Constructor {
    #[drive(skip)]
    pub name: String,
    pub types: Vec<ArenaType>,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Function {
    pub params: Vec<ArenaType>,
    pub ret: ArenaType,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Call {
    pub args: Vec<ArenaType>,
    pub ret: ArenaType,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Union {
    pub types: Vec<ArenaType>,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub types: Vec<ArenaType>,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct Object {
    pub props: Vec<ObjProp>,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub struct ObjProp {
    #[drive(skip)]
    pub name: String,
    pub t: ArenaType,
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Variable(Variable),
    Constructor(Constructor),
    Literal(Literal),
    Function(Function),
    Union(Union),
    Tuple(Tuple),
    Object(Object),
}

#[derive(Debug, Clone, Drive, DriveMut, PartialEq, Eq, Hash)]
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

    pub fn new_literal(idx: ArenaType, lit: &Literal) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Literal(lit.clone()),
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

    pub fn new_union(idx: ArenaType, types: &[ArenaType]) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Union(Union {
                types: types.to_vec(),
            }),
        }
    }

    pub fn new_tuple(idx: ArenaType, types: &[ArenaType]) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Tuple(Tuple {
                types: types.to_vec(),
            }),
        }
    }

    pub fn new_object(idx: ArenaType, props: &[ObjProp]) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Object(Object {
                props: props.to_vec(),
            }),
        }
    }

    pub fn set_instance(&mut self, instance: &ArenaType) {
        match &mut self.kind {
            TypeKind::Variable(Variable {
                instance: ref mut inst,
                ..
            }) => {
                *inst = Some(instance.to_owned());
            }
            _ => {
                unimplemented!()
            }
        }
    }

    pub fn as_string(&self, a: &Vec<Type>) -> String {
        match &self.kind {
            TypeKind::Variable(Variable {
                instance: Some(inst),
            }) => a[inst.value].as_string(a),
            TypeKind::Variable(_) => format!("t{}", self.id.value),
            TypeKind::Constructor(con) => match con.types.len() {
                0 => con.name.clone(),
                2 => {
                    let l = a[con.types[0].value].as_string(a);
                    let r = a[con.types[1].value].as_string(a);
                    format!("({} {} {})", l, con.name, r)
                }
                _ => {
                    let mut coll = vec![];
                    for v in &con.types {
                        coll.push(a[v.value].as_string(a));
                    }
                    format!("{} {}", con.name, coll.join(" "))
                }
            },
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Tuple(tuple) => {
                format!("[{}]", types_to_strings(a, &tuple.types).join(", "))
            }
            TypeKind::Object(object) => {
                let mut fields = vec![];
                for prop in &object.props {
                    fields.push(format!("{}: {}", prop.name, a[prop.t.value].as_string(a)));
                }
                format!("{{{}}}", fields.join(", "))
            }
            TypeKind::Function(func) => {
                format!(
                    "({}) => {}",
                    types_to_strings(a, &func.params).join(", "),
                    a[func.ret.value].as_string(a),
                )
            }
            TypeKind::Union(union) => types_to_strings(a, &union.types).join(" | "),
        }
    }
}

fn types_to_strings(a: &Vec<Type>, types: &[ArenaType]) -> Vec<String> {
    let mut strings = vec![];
    for v in types {
        strings.push(a[v.value].as_string(a));
    }
    strings
}

/// A binary type constructor which builds function types
pub fn new_func_type(a: &mut Vec<Type>, params: &[ArenaType], ret: ArenaType) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_function(a_t, params, ret);
    a.push(t);
    a_t
}

pub fn new_union_type(a: &mut Vec<Type>, types: &[ArenaType]) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_union(a_t, types);
    a.push(t);
    a_t
}

pub fn new_tuple_type(a: &mut Vec<Type>, types: &[ArenaType]) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_tuple(a_t, types);
    a.push(t);
    a_t
}

pub fn new_object_type(a: &mut Vec<Type>, props: &[ObjProp]) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_object(a_t, props);
    a.push(t);
    a_t
}

/// A binary type constructor which builds function types
pub fn new_var_type(a: &mut Vec<Type>) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_variable(a_t);
    a.push(t);
    a_t
}

/// A binary type constructor which builds function types
pub fn new_constructor(a: &mut Vec<Type>, name: &str, types: &[ArenaType]) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_constructor(a_t, name, types);
    a.push(t);
    a_t
}

pub fn new_lit_type(a: &mut Vec<Type>, lit: &Literal) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_literal(a_t, lit);
    a.push(t);
    a_t
}

pub fn new_num_lit_type(a: &mut Vec<Type>, value: &str) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_literal(a_t, &Literal::Number(value.to_string()));
    a.push(t);
    a_t
}

pub fn new_str_lit_type(a: &mut Vec<Type>, value: &str) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_literal(a_t, &Literal::String(value.to_string()));
    a.push(t);
    a_t
}

pub fn new_bool_lit_type(a: &mut Vec<Type>, value: bool) -> ArenaType {
    let a_t = ArenaType { value: a.len() };
    let t = Type::new_literal(a_t, &Literal::Boolean(value));
    a.push(t);
    a_t
}

//impl fmt::Debug for Type {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        match self {
//            write!(f, "TypeVariable(id = {})", self.id)
//            write!(f, "TypeOperator(name, )", self.id)
//        }
//    }
//}
