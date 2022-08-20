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

#[derive(Clone, Debug, Default)]
pub struct Scope {
    pub values: Env,
    pub types: Env,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub scopes: Vec<Scope>,
    pub state: State,
    pub is_async: bool,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            state: State {
                count: Cell::from(0),
            },
            is_async: false,
        }
    }
}

impl Context {
    pub fn apply(&mut self, s: &Subst) {
        let current_scope = self.scopes.last_mut().unwrap();
        for (k, v) in current_scope.values.clone() {
            // Should we be apply substitions to types as well?
            current_scope.values.insert(k.to_owned(), v.apply(s));
        }
    }

    pub fn get_all_types(&self) -> Env {
        // TODO: gather types from all scopes being careful with shadowing
        let current_scope = self.scopes.last().unwrap();
        current_scope.types.to_owned()
    }

    pub fn insert_value(&mut self, name: String, scheme: Scheme) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.values.insert(name, scheme);
    }

    pub fn insert_type(&mut self, name: String, scheme: Scheme) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.types.insert(name, scheme);
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, String> {
        let current_scope = self.scopes.last().unwrap();
        let scheme = current_scope
            .values
            .get(name)
            .ok_or(format!("Can't find type: {name}"))?;
        Ok(self.instantiate(scheme))
    }

    pub fn lookup_value_scheme(&self, name: &str) -> Result<Scheme, String> {
        let current_scope = self.scopes.last().unwrap();
        let scheme = current_scope
            .values
            .get(name)
            .ok_or(format!("Can't find type: {name}"))?;
        Ok(scheme.to_owned())
    }

    // TODO: Make this return a Result<Type, String>
    pub fn lookup_type(&self, name: &str) -> Result<Type, String> {
        let current_scope = self.scopes.last().unwrap();
        let scheme = current_scope
            .types
            .get(name)
            .ok_or(format!("Can't find type: {name}"))?;
        Ok(self.instantiate(scheme))
    }

    pub fn lookup_alias(&self, alias: &TAlias) -> Result<Type, String> {
        let current_scope = self.scopes.last().unwrap();
        match current_scope.types.get(&alias.name) {
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
