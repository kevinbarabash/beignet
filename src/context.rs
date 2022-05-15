use super::substitutable::*;
use super::types::{self, Scheme, Type, TypeKind};
use super::literal::Lit;

use std::cell::Cell;
use std::collections::HashMap;

pub type Env = HashMap<String, types::Scheme>;

#[derive(Clone)]
pub struct State {
    pub count: Cell<i32>,
}

#[derive(Clone)]
pub struct Context {
    pub env: Env,
    pub state: State,
    pub r#async: bool,
}

impl From<Env> for Context {
    fn from(env: Env) -> Self {
        Context {
            env,
            state: State {
                count: Cell::new(0),
            },
            r#async: false,
        }
    }
}

impl Context {
    pub fn lookup_env(&self, name: &str) -> Type {
        let scheme = self.env.get(name).unwrap();
        self.instantiate(scheme)
    }

    fn instantiate(&self, scheme: &Scheme) -> Type {
        let fresh_quals = scheme.qualifiers.iter().map(|_| self.fresh_tvar());

        let ids = scheme.qualifiers.iter().map(|id| id.to_owned());
        // The iterator returned by into_iter may yield any of T, &T or &mut T,
        // depending on the context.
        let subs: Subst = ids.zip(fresh_quals).collect();

        scheme.ty.apply(&subs)
    }

    pub fn fresh_id(&self) -> i32 {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);
        id
    }
    pub fn fresh_tvar(&self) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            kind: types::TypeKind::Var,
        }
    }

    pub fn from_lam(&self, lam: types::TLam) -> Type {
        types::Type {
            id: self.fresh_id(),
            frozen: false,
            kind: types::TypeKind::Lam(lam),
        }
    }
    pub fn from_prim(&self, prim: types::Primitive) -> Type {
        types::Type {
            id: self.fresh_id(),
            frozen: false,
            kind: types::TypeKind::Prim(prim),
        }
    }
    pub fn from_lit(&self, lit: Lit) -> Type {
        types::Type {
            id: self.fresh_id(),
            frozen: false,
            kind: match lit {
                Lit::Num(n) => types::TypeKind::Lit(types::Lit::Num(n.value)),
                Lit::Bool(b) => types::TypeKind::Lit(types::Lit::Bool(b.value)),
                Lit::Str(s) => types::TypeKind::Lit(types::Lit::Str(s.value)),
                Lit::Null(_) => types::TypeKind::Lit(types::Lit::Null),
                Lit::Undefined(_) => types::TypeKind::Lit(types::Lit::Undefined),
            }
        }
    }
    pub fn union(&self, types: &[Type]) -> Type {
        types::Type {
            id: self.fresh_id(),
            frozen: false,
            kind: types::TypeKind::Union(types.to_owned()),
        }
    }
    pub fn obj(&self, properties: &[types::TProp]) -> Type {
        Type {
            id: self.fresh_id(),
            frozen: false,
            kind: TypeKind::Obj(properties.to_vec()),
        }
    }
    pub fn prop(&self, name: &str, ty: Type) -> types::TProp {
        types::TProp {
            name: name.to_owned(),
            ty,
        }
    }
    pub fn alias(&self, name: &str, type_params: Vec<Type>) -> Type {
        types::Type {
            id: self.fresh_id(),
            frozen: false,
            kind: TypeKind::Alias {
                name: name.to_owned(),
                type_params: type_params.to_owned(),
            },
        }
    }
}
