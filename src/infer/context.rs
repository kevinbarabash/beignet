use std::cell::Cell;
use std::collections::HashMap;

use crate::ast::literal::Lit;
use crate::types::{self, Scheme, Type, Variant, WidenFlag};

use super::substitutable::*;

pub type Env = HashMap<String, types::Scheme>;

#[derive(Clone)]
pub struct State {
    pub count: Cell<i32>,
}

#[derive(Clone)]
pub struct Context {
    pub values: Env,
    pub types: Env,
    pub state: State,
    pub is_async: bool,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
            types: HashMap::new(),
            state: State {
                count: Cell::from(0),
            },
            is_async: false,
        }
    }
}

impl Context {
    pub fn lookup_value(&self, name: &str) -> Type {
        let scheme = self.values.get(name).unwrap();
        self.instantiate(scheme)
    }

    fn instantiate(&self, scheme: &Scheme) -> Type {
        let ids = scheme.qualifiers.iter().map(|id| id.to_owned());
        let fresh_quals = scheme.qualifiers.iter().map(|_| self.fresh_var());
        let subs: Subst = ids.zip(fresh_quals).collect();

        scheme.ty.apply(&subs)
    }

    pub fn fresh_id(&self) -> i32 {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);
        id
    }

    pub fn fresh_var(&self) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Var,
            widen_flag: None,
        }
    }
    pub fn lam(&self, params: Vec<Type>, ret: Box<Type>) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Lam(types::LamType { params, ret }),
            widen_flag: None,
        }
    }
    pub fn prim(&self, prim: types::Primitive) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Prim(prim),
            widen_flag: None,
        }
    }
    pub fn prim_with_flag(&self, prim: types::Primitive, widen_flag: WidenFlag) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Prim(prim),
            widen_flag: Some(widen_flag),
        }
    }
    pub fn lit(&self, lit: Lit) -> Type {
        let lit = match lit {
            Lit::Num(n) => types::Lit::Num(n.value),
            Lit::Bool(b) => types::Lit::Bool(b.value),
            Lit::Str(s) => types::Lit::Str(s.value),
            Lit::Null(_) => types::Lit::Null,
            Lit::Undefined(_) => types::Lit::Undefined,
        };
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Lit(lit),
            widen_flag: None,
        }
    }
    pub fn lit_type(&self, lit: types::Lit) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Lit(lit),
            widen_flag: None,
        }
    }
    pub fn union(&self, types: Vec<Type>) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Union(types),
            widen_flag: None,
        }
    }
    pub fn intersection(&self, types: Vec<Type>) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Intersection(types),
            widen_flag: None,
        }
    }
    pub fn object(&self, props: &[types::TProp], widen_flag: Option<WidenFlag>) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Object(props.to_vec()),
            widen_flag,
        }
    }
    pub fn prop(&self, name: &str, ty: Type, optional: bool) -> types::TProp {
        types::TProp {
            name: name.to_owned(),
            optional,
            ty,
        }
    }
    pub fn alias(&self, name: &str, type_params: Option<Vec<Type>>) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Alias(types::AliasType {
                name: name.to_owned(),
                type_params,
            }),
            widen_flag: None,
        }
    }
    pub fn tuple(&self, types: Vec<Type>) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Tuple(types),
            widen_flag: None,
        }
    }
    pub fn rest(&self, arg: Type) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Rest(Box::from(arg)),
            widen_flag: None,
        }
    }
    pub fn mem(&self, obj: Type, prop: &str) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            variant: Variant::Member(types::MemberType {
                obj: Box::from(obj),
                prop: prop.to_owned(),
            }),
            widen_flag: None,
        }
    }
}
