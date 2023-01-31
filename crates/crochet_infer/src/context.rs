use crochet_ast::types::*;
use std::cell::Cell;
use std::collections::HashMap;

use crate::scheme::{generalize, instantiate, Scheme};
use crate::substitutable::*;
use crate::type_error::TypeError;
use crate::util::immutable_obj_type;

pub type Env = HashMap<String, Scheme>;

#[derive(Clone, Debug)]
pub struct State {
    pub count: Cell<i32>,
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub mutable: bool,
    pub t: Type,
}

// TODO: each scope has separate namespaces for values and types.  Namespace module
// imports/decls can include types and values.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    pub namespaces: HashMap<String, Box<Scope>>,
    pub values: HashMap<String, Binding>,
    pub types: Env,
    pub is_async: bool,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub scopes: Vec<Scope>,
    pub state: State,
}

// #[derive(Clone, Debug)]
// pub struct Context {
//     pub values: HashMap<String, Binding>,
//     pub types: HashMap<String, Scheme>,
//     pub is_async: bool,
//     pub count: Cell<i32>,
// }

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
        for (k, b) in current_scope.values.clone() {
            // Should we be apply substitions to types as well?
            current_scope.values.insert(
                k.to_owned(),
                Binding {
                    mutable: b.mutable,
                    t: b.t.apply(s),
                },
            );
        }
    }

    pub fn get_all_types(&self) -> Env {
        // TODO: gather types from all scopes being careful with shadowing
        let current_scope = self.scopes.last().unwrap();
        current_scope.types.to_owned()
    }

    pub fn insert_value(&mut self, name: String, t: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope
            .values
            .insert(name, Binding { mutable: false, t });
    }

    pub fn insert_binding(&mut self, name: String, b: Binding) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.values.insert(name, b);
    }

    // TODO: determine if we need to generalize the inserted type everywhere
    // this is being called.
    pub fn insert_type(&mut self, name: String, t: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        let scheme = generalize(&current_scope.types, &t);
        current_scope.types.insert(name, scheme);
    }

    pub fn insert_scheme(&mut self, name: String, scheme: Scheme) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.types.insert(name, scheme);
    }

    pub fn insert_namespace(&mut self, name: String, namespace: Scope) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.namespaces.insert(name, Box::from(namespace));
    }

    // TODO: update to instantiate Lam
    pub fn lookup_value_and_instantiate(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.values.get(name) {
                return Ok(binding.t.to_owned());
            }
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.values.get(name) {
                return Ok(b.t.to_owned());
            }
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_binding(&self, name: &str) -> Result<Binding, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.values.get(name) {
                return Ok(b.to_owned());
            }
        }
        eprintln!("lookup_binding failed");
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_scheme(&self, name: &str) -> Result<Scheme, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(scheme) = scope.types.get(name) {
                return Ok(scheme.to_owned());
            }
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_type(&self, name: &str, mutable: bool) -> Result<Type, Vec<TypeError>> {
        let mut t = self._lookup_type(name)?;

        if !mutable {
            if let TypeKind::Object(obj) = &t.kind {
                if let Some(obj) = immutable_obj_type(obj) {
                    t = Type {
                        kind: TypeKind::Object(obj),
                        mutable: false,
                        provenance: t.provenance,
                    }
                }
            }
        }
        // TODO: convert mutable type to immutable type of `mutable` is true

        Ok(t)
    }

    pub fn _lookup_type(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(scheme) = scope.types.get(name) {
                return Ok(instantiate(self, scheme));
            }
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    // pub fn lookup_namespace(&self, name: &str) -> Result<Box<Scope>, TypeError> {
    //     for scope in self.scopes.iter().rev() {
    //         if let Some(namespace) = scope.namespaces.get(name) {
    //             return Ok(namespace.to_owned());
    //         }
    //     }
    //     // TODO: Add a CantFindNamespace error
    //     Err(TypeError::CantFindIdent(name.to_owned()))
    // }

    pub fn fresh_id(&self) -> i32 {
        let id = self.state.count.get() + 1;
        self.state.count.set(id);
        id
    }

    pub fn fresh_var(&self) -> Type {
        Type::from(TypeKind::Var(TVar {
            id: self.fresh_id(),
            constraint: None,
        }))
    }
}
