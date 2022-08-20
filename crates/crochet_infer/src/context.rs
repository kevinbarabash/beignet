use crochet_types::*;
use std::cell::Cell;
use std::collections::HashMap;

use super::substitutable::*;

// This maps to the Assump data type in THIH which was a tuple
// of (Id, Scheme) where Id was a String.
pub type Env = HashMap<String, Scheme>;

#[derive(Clone, Debug)]
pub struct State {
    pub count: Cell<i32>,
}

#[derive(Clone, Debug)]
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
    pub fn lookup_value(&self, name: &str) -> Result<Type, String> {
        let scheme = self
            .values
            .get(name)
            .ok_or(format!("Can't find type: {name}"))?;
        Ok(self.instantiate(scheme))
    }

    // TODO: Make this return a Result<Type, String>
    pub fn lookup_type(&self, name: &str) -> Result<Type, String> {
        let scheme = self
            .types
            .get(name)
            .ok_or(format!("Can't find type: {name}"))?;
        Ok(self.instantiate(scheme))
    }

    pub fn lookup_alias(&self, alias: &TAlias) -> Result<Type, String> {
        match self.types.get(&alias.name) {
            Some(scheme) => {
                // Replaces qualifiers in the scheme with the corresponding type params
                // from the alias type.
                let ids = scheme.qualifiers.iter().map(|id| id.to_owned());
                let subs: Subst = match &alias.type_params {
                    Some(type_params) => {
                        if scheme.qualifiers.len() != type_params.len() {
                            return Err(String::from("mismatch between number of qualifiers in scheme and number of type params"));
                        }
                        ids.zip(type_params.iter().cloned()).collect()
                    }
                    None => {
                        if !scheme.qualifiers.is_empty() {
                            return Err(String::from("mismatch between number of qualifiers in scheme and number of type params"));
                        }
                        ids.zip(scheme.qualifiers.iter().map(|_| self.fresh_var()))
                            .collect()
                    }
                };

                Ok(scheme.ty.apply(&subs))
            }
            None => Err(String::from("Can't find alias '{name}' in context")),
        }
    }

    pub fn instantiate(&self, scheme: &Scheme) -> Type {
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
        Type::Var(self.fresh_id())
    }
}
