use crochet_ast::types::*;
use std::cell::Cell;
use std::collections::HashMap;

use crate::substitutable::*;
use crate::type_error::TypeError;

// NOTE: This is the same as the Assump type in assump.rs
pub type Env = HashMap<String, Type>;

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
        for (_, b) in current_scope.values.iter_mut() {
            b.apply(s);
            // We need to re-insert the binding since we can't gaurantee that it
            // wasn't cloned.
            // current_scope.values.insert(k.to_owned(), b);
            // Should we be apply substitions to types as well?
            // current_scope.values.insert(
            //     k.to_owned(),
            //     Binding {
            //         mutable: b.mutable,
            //         t: b.t.apply(s),
            //     },
            // );
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

    pub fn insert_type(&mut self, name: String, t: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.types.insert(name, t);
    }

    pub fn insert_namespace(&mut self, name: String, namespace: Scope) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.namespaces.insert(name, Box::from(namespace));
    }

    pub fn lookup_value_and_instantiate(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.values.get(name) {
                let t = binding.t.clone();
                return Ok(self.instantiate(&t));
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
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_type_and_instantiate(
        &self,
        name: &str,
        mutable: bool,
    ) -> Result<Type, Vec<TypeError>> {
        let t = self.lookup_type(name, mutable)?;
        Ok(self.instantiate(&t))
    }

    // This method gets confused by
    // type Obj = {[key: string]: boolean};
    // type ReadonlyObj = Readonly<Obj>;
    // TODO: Figure out to fix this so that we can use this method in `expand_alias_type`
    pub fn lookup_type(&self, name: &str, mutable: bool) -> Result<Type, Vec<TypeError>> {
        if !mutable {
            if let Ok(t) = self._lookup_type(&format!("Readonly{name}")) {
                return Ok(t);
            }
        }
        self._lookup_type(name)
    }

    pub fn _lookup_type(&self, name: &str) -> Result<Type, Vec<TypeError>> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) {
                return Ok(t.to_owned());
            }
        }
        Err(vec![TypeError::CantFindIdent(name.to_owned())])
    }

    pub fn lookup_namespace(&self, name: &str) -> Result<Box<Scope>, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(namespace) = scope.namespaces.get(name) {
                return Ok(namespace.to_owned());
            }
        }
        // TODO: Add a CantFindNamespace error
        Err(TypeError::CantFindIdent(name.to_owned()))
    }

    pub fn instantiate(&self, t: &Type) -> Type {
        match &t.kind {
            TypeKind::Generic(TGeneric { t, type_params }) => {
                let ids = type_params.iter().map(|tv| tv.id.to_owned());
                let fresh_params = type_params.iter().map(|tp| {
                    Type::from(TypeKind::Var(TVar {
                        id: self.fresh_id(),
                        constraint: tp.constraint.to_owned(),
                    }))
                });
                let subs: Subst = ids.zip(fresh_params).collect();
                let mut t = t.as_ref().to_owned();
                t.apply(&subs);
                t
            }
            _ => t.to_owned(),
        }
    }

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
