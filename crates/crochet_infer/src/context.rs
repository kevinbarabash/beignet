use crochet_ast::types::*;
use error_stack::{Report, Result};
use std::cell::Cell;
use std::collections::HashMap;

use crate::substitutable::*;
use crate::type_error::TypeError;
use crate::util::get_type_params;

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

    pub fn insert_type(&mut self, name: String, t: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.types.insert(name, t);
    }

    pub fn insert_namespace(&mut self, name: String, namespace: Scope) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.namespaces.insert(name, Box::from(namespace));
    }

    pub fn lookup_value_and_instantiate(&self, name: &str) -> Result<Type, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.values.get(name) {
                return Ok(self.instantiate(&b.t));
            }
        }
        Err(Report::new(TypeError::CantFindIdent(name.to_owned()))
            .attach_printable(format!("Can't find value: {name}")))
    }

    pub fn lookup_value(&self, name: &str) -> Result<Type, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.values.get(name) {
                return Ok(b.t.to_owned());
            }
        }
        Err(Report::new(TypeError::CantFindIdent(name.to_owned()))
            .attach_printable(format!("Can't find value: {name}")))
    }

    pub fn lookup_binding(&self, name: &str) -> Result<Binding, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.values.get(name) {
                return Ok(b.to_owned());
            }
        }
        Err(Report::new(TypeError::CantFindIdent(name.to_owned()))
            .attach_printable(format!("Can't find value: {name}")))
    }

    pub fn lookup_type_and_instantiate(
        &self,
        name: &str,
        mutable: bool,
    ) -> Result<Type, TypeError> {
        let t = self.lookup_type(name, mutable)?;
        Ok(self.instantiate(&t))
    }

    pub fn lookup_type(&self, name: &str, mutable: bool) -> Result<Type, TypeError> {
        if !mutable {
            if let Ok(t) = self._lookup_type(&format!("Readonly{name}")) {
                return Ok(t);
            }
        }
        self._lookup_type(name)
    }

    fn _lookup_type(&self, name: &str) -> Result<Type, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) {
                return Ok(t.to_owned());
            }
        }
        Err(Report::new(TypeError::CantFindIdent(name.to_owned()))
            .attach_printable(format!("Can't find type: {name}")))
    }

    pub fn lookup_namespace(&self, name: &str) -> Result<Box<Scope>, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(namespace) = scope.namespaces.get(name) {
                return Ok(namespace.to_owned());
            }
        }
        Err(Report::new(TypeError::CantFindIdent(name.to_owned()))
            .attach_printable(format!("Can't find namespace: {name}")))
    }

    pub fn lookup_ref_and_instantiate(&self, alias: &TRef) -> Result<Type, TypeError> {
        let name = &alias.name;
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) {
                let type_params = get_type_params(t);
                println!("type_params = {type_params:#?}");

                // Replaces qualifiers in the type with the corresponding type params
                // from the alias type.
                let ids = type_params.iter().map(|tv| tv.id.to_owned());
                let subs: Subst = match &alias.type_args {
                    Some(type_params) => {
                        if type_params.len() != type_params.len() {
                            println!("type = {t}");
                            println!("type_params = {type_params:#?}");
                            return Err(Report::new(TypeError::TypeInstantiationFailure)
                                .attach_printable(
                                    "mismatch between the number of qualifiers and type params",
                                ));
                        }
                        ids.zip(type_params.iter().cloned()).collect()
                    }
                    None => {
                        if !type_params.is_empty() {
                            println!("type = {t}");
                            println!("no type params");
                            return Err(Report::new(TypeError::TypeInstantiationFailure)
                                .attach_printable(
                                    "mismatch between the number of qualifiers and type params",
                                ));
                        }
                        ids.zip(type_params.iter().map(|tp| {
                            Type::from(TypeKind::Var(TVar {
                                id: self.fresh_id(),
                                constraint: tp.constraint.to_owned(),
                            }))
                        }))
                        .collect()
                    }
                };
                println!("subs: {subs:#?}");
                println!("before subs: {t:#?}");
                let t = t.apply(&subs);
                println!("after subs: {t:#?}");
                return Ok(t);
            }
        }
        Err(Report::new(TypeError::CantFindIdent(name.to_owned()))
            .attach_printable(format!("Can't find type: {name}")))
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

                t.apply(&subs)
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
