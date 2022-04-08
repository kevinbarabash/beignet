use super::types::{self, Scheme, Subst, Substitutable, Type};
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
        let fresh_quals = scheme.qualifiers.iter().map(|_| Type::from(self.fresh()));

        let ids = scheme.qualifiers.iter().map(|qual| qual.id);
        // The iterator returned by into_iter may yield any of T, &T or &mut T,
        // depending on the context.
        let subs: Subst = ids.zip(fresh_quals).collect();

        scheme.ty.apply(&subs)
    }
    pub fn fresh(&self) -> types::TVar {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);

        types::TVar {
            id,
            name: String::from("a"),
        }
    }
}
