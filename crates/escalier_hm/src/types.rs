// Types and type constructors
use std::collections::HashMap;

use crate::literal::Literal;

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
pub struct Call {
    pub args: Vec<ArenaType>,
    pub ret: ArenaType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Union {
    pub types: Vec<ArenaType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Variable(Variable),
    Constructor(Constructor),
    Literal(Literal),
    Function(Function),
    Call(Call),
    Union(Union),
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

    pub fn new_call(idx: ArenaType, arg_types: &[ArenaType], ret_type: ArenaType) -> Type {
        Type {
            id: idx,
            kind: TypeKind::Call(Call {
                args: arg_types.to_vec(),
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
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|param| a[*param].as_string(a, namer))
                    .collect::<Vec<_>>();

                let ret = a[func.ret].as_string(a, namer);

                format!("({}) => {ret}", params.join(", "))
            }
            TypeKind::Call(call) => {
                eprintln!("call: {:?}", call);
                let args = call
                    .args
                    .iter()
                    .map(|arg| a[*arg].as_string(a, namer))
                    .collect::<Vec<_>>();

                let ret = a[call.ret].as_string(a, namer);

                // Should this be formatted more like a function call
                format!("({}) => {ret}", args.join(", "))
            }
            TypeKind::Union(union) => {
                let types = union
                    .types
                    .iter()
                    .map(|t| a[*t].as_string(a, namer))
                    .collect::<Vec<_>>();

                types.join(" | ")
            }
        }
    }
}

/// A binary type constructor which builds function types
pub fn new_func_type(a: &mut Vec<Type>, params: &[ArenaType], ret: ArenaType) -> ArenaType {
    let t = Type::new_function(a.len(), params, ret);
    a.push(t);
    a.len() - 1
}

pub fn new_call_type(a: &mut Vec<Type>, args: &[ArenaType], ret: ArenaType) -> ArenaType {
    let t = Type::new_call(a.len(), args, ret);
    a.push(t);
    a.len() - 1
}

pub fn new_union_type(a: &mut Vec<Type>, types: &[ArenaType]) -> ArenaType {
    let t = Type::new_union(a.len(), types);
    a.push(t);
    a.len() - 1
}

/// A binary type constructor which builds function types
pub fn new_var_type(a: &mut Vec<Type>) -> ArenaType {
    let t = Type::new_variable(a.len());
    a.push(t);
    a.len() - 1
}

/// A binary type constructor which builds function types
pub fn new_constructor(a: &mut Vec<Type>, name: &str, types: &[ArenaType]) -> ArenaType {
    let t = Type::new_constructor(a.len(), name, types);
    a.push(t);
    a.len() - 1
}

pub fn new_lit_type(a: &mut Vec<Type>, lit: &Literal) -> ArenaType {
    let t = Type::new_literal(a.len(), lit);
    a.push(t);
    a.len() - 1
}

pub fn new_num_lit_type(a: &mut Vec<Type>, value: &str) -> ArenaType {
    let t = Type::new_literal(a.len(), &Literal::Number(value.to_string()));
    a.push(t);
    a.len() - 1
}

pub fn new_str_lit_type(a: &mut Vec<Type>, value: &str) -> ArenaType {
    let t = Type::new_literal(a.len(), &Literal::String(value.to_string()));
    a.push(t);
    a.len() - 1
}

pub fn new_bool_lit_type(a: &mut Vec<Type>, value: bool) -> ArenaType {
    let t = Type::new_literal(a.len(), &Literal::Boolean(value));
    a.push(t);
    a.len() - 1
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

impl Namer {
    fn next(&mut self) -> String {
        let v = self.value;
        self.value = ((self.value as u8) + 1) as char;
        format!("{}", v)
    }

    fn name(&mut self, t: ArenaType) -> String {
        let k = self.set.get(&t).cloned();
        if let Some(val) = k {
            val
        } else {
            let v = self.next();
            self.set.insert(t, v.clone());
            v
        }
    }
}
