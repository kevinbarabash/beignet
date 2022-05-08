use super::substitutable::*;
use super::types::{self, Scheme, Type};
use super::literal::Literal;

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
}

impl From<Env> for Context {
    fn from(env: Env) -> Self {
        Context {
            env,
            state: State {
                count: Cell::new(0),
            },
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

    pub fn fresh(&self) -> types::TVar {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);

        types::TVar { id }
    }
    pub fn fresh_tvar(&self) -> Type {
        let tvar = self.fresh();

        Type { id: tvar.id, kind: types::TypeKind::Var(tvar) }
    }
    pub fn fresh_id(&self) -> i32 {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);
        id
    }

    pub fn from_lam(&self, lam: types::TLam) -> Type {
        types::Type {
            id: self.fresh_id(),
            kind: types::TypeKind::Lam(lam),
        }
    }
    pub fn from_prim(&self, prim: types::Primitive) -> Type {
        types::Type {
            id: self.fresh_id(),
            kind: types::TypeKind::Prim(prim),
        }
    }
    pub fn from_lit(&self, lit: Literal) -> Type {
        types::Type {
            id: self.fresh_id(),
            kind: types::TypeKind::Lit(lit),
        }
    }
}
