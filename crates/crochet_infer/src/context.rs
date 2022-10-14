use crochet_types::*;
use std::cell::Cell;
use std::collections::HashMap;

use crate::substitutable::*;
use crate::util::get_type_params;

// NOTE: This is the same as the Assump type in assump.rs
pub type Env = HashMap<String, Type>;

#[derive(Clone, Debug)]
pub struct State {
    pub count: Cell<i32>,
}

// TODO: each scope has separate namespaces for values and types.  Namespace module
// imports/decls can include types and values.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    pub namespaces: HashMap<String, Box<Scope>>,
    pub values: Env,
    pub types: Env,
    pub is_async: bool,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub scopes: Vec<Scope>,
    pub state: State,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            state: State {
                count: Cell::from(0),
            },
        }
    }
}

impl Context {
    pub fn push_scope(&mut self, is_async: bool) {
        self.scopes.push(Scope {
            is_async,
            ..Scope::default()
        });
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn is_async(&self) -> bool {
        let current_scope = self.scopes.last().unwrap();
        current_scope.is_async
    }

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

    pub fn insert_value(&mut self, name: String, t: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.values.insert(name, t);
    }

    pub fn insert_type(&mut self, name: String, t: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.types.insert(name, t);
    }

    pub fn insert_namespace(&mut self, name: String, namespace: Scope) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.namespaces.insert(name, Box::from(namespace));
    }

    pub fn lookup_value_and_instantiate(&self, name: &str) -> Result<Type, String> {
        for scope in self.scopes.iter().rev() {
            match scope.values.get(name) {
                Some(t) => return Ok(self.instantiate(t)),
                None => (),
            }
        }
        Err(format!("Can't find value: {name}"))
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, String> {
        for scope in self.scopes.iter().rev() {
            match scope.values.get(name) {
                Some(t) => return Ok(t.to_owned()),
                None => (),
            }
        }
        Err(format!("Can't find value: {name}"))
    }

    pub fn lookup_type_and_instantiate(&self, name: &str) -> Result<Type, String> {
        for scope in self.scopes.iter().rev() {
            match scope.types.get(name) {
                Some(t) => return Ok(self.instantiate(t)),
                None => (),
            }
        }
        Err(format!("Can't find type: {name}"))
    }

    pub fn lookup_type(&self, name: &str) -> Result<Type, String> {
        for scope in self.scopes.iter().rev() {
            match scope.types.get(name) {
                Some(t) => return Ok(t.to_owned()),
                None => (),
            }
        }
        Err(format!("Can't find type: {name}"))
    }

    pub fn lookup_namespace(&self, name: &str) -> Result<Box<Scope>, String> {
        for scope in self.scopes.iter().rev() {
            match scope.namespaces.get(name) {
                Some(namespace) => return Ok(namespace.to_owned()),
                None => (),
            }
        }
        Err(format!("Can't find namespace: {name}"))
    }

    pub fn lookup_ref_and_instantiate(&self, alias: &TRef) -> Result<Type, String> {
        let name = &alias.name;
        for scope in self.scopes.iter().rev() {
            match scope.types.get(name) {
                Some(t) => {
                    let type_params = get_type_params(t);

                    // Replaces qualifiers in the type with the corresponding type params
                    // from the alias type.
                    let ids = type_params.iter().map(|id| id.to_owned());
                    let subs: Subst = match &alias.type_args {
                        Some(type_params) => {
                            if type_params.len() != type_params.len() {
                                println!("type = {t}");
                                println!("type_params = {type_params:#?}");
                                return Err(String::from(
                                    "mismatch between the number of qualifiers and type params",
                                ));
                            }
                            ids.zip(type_params.iter().cloned()).collect()
                        }
                        None => {
                            if !type_params.is_empty() {
                                println!("type = {t}");
                                println!("no type params");
                                return Err(String::from(
                                    "mismatch between the number of qualifiers and type params",
                                ));
                            }
                            ids.zip(type_params.iter().map(|_| self.fresh_var()))
                                .collect()
                        }
                    };

                    return Ok(t.apply(&subs));
                }
                None => (),
            }
        }
        Err(format!("Can't find type: {name}"))
    }

    pub fn instantiate(&self, t: &Type) -> Type {
        match t {
            Type::Generic(TGeneric { t, type_params }) => {
                let ids = type_params.iter().map(|id| id.to_owned());
                let fresh_quals = type_params.iter().map(|_| self.fresh_var());
                let subs: Subst = ids.zip(fresh_quals).collect();

                t.apply(&subs)
            }
            _ => {
                let type_params = get_type_params(t);

                let ids = type_params.iter().map(|id| id.to_owned());
                let fresh_quals = type_params.iter().map(|_| self.fresh_var());
                let subs: Subst = ids.zip(fresh_quals).collect();

                t.apply(&subs)
            }
        }
    }

    pub fn fresh_id(&self) -> i32 {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);
        id
    }

    pub fn fresh_var(&self) -> Type {
        Type::Var(TVar {
            id: self.fresh_id(),
            constraint: None,
        })
    }
}
