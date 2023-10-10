use generational_arena::Index;
use im::hashmap::HashMap;
use im::hashset::HashSet;

use crate::checker::Checker;
use crate::folder::walk_index;
use crate::folder::{self, Folder};
use crate::key_value_store::KeyValueStore;
use crate::type_error::TypeError;
use crate::types::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub index: Index,
    pub is_mut: bool,
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    // Maps variables to their types.
    pub values: HashMap<String, Binding>,
    // Maps type aliases to type types definitions.
    // TODO: figure out how we want to track types and schemes
    pub schemes: HashMap<String, Scheme>,
    // A set of non-generic TypeVariables.
    // NOTE: The same type variable can be both generic and non-generic in
    // different contexts.
    pub non_generic: HashSet<Index>,
    // Whether we're in an async function body or not.
    pub is_async: bool,
}

impl Context {
    pub fn get_scheme(&self, name: &str) -> Result<Scheme, TypeError> {
        match self.schemes.get(name) {
            Some(scheme) => Ok(scheme.to_owned()),
            None => Err(TypeError {
                message: format!("{} is not in scope", name),
            }),
        }
    }

    pub fn get_binding(&self, name: &str) -> Result<Binding, TypeError> {
        match self.values.get(name) {
            Some(binding) => Ok(binding.to_owned()),
            None => Err(TypeError {
                message: format!("{} is not in scope", name),
            }),
        }
    }
}

impl Checker {
    /// Get the type of identifier name from the type context ctx.
    ///
    /// Args:
    ///     name: The identifier name
    ///     ctx: The current context
    ///
    /// Raises:
    ///     ParseError: Raised if name is an undefined symbol in the type
    ///         environment.
    pub fn get_type(&mut self, name: &str, ctx: &Context) -> Result<Index, TypeError> {
        if let Some(value) = ctx.values.get(name) {
            let result = self.fresh(&value.index, ctx);
            Ok(result)
        } else {
            Err(TypeError {
                message: format!("Undefined symbol {:?}", name),
            })
        }
    }

    /// Makes a copy of a type expression.
    ///
    /// The type t is copied. The the generic variables are duplicated and the
    /// non_generic variables are shared.
    ///
    /// Args:
    ///     t: A type to be copied.
    ///     non_generic: A set of non-generic TypeVariables
    fn fresh(&mut self, index: &Index, ctx: &Context) -> Index {
        let mut fresh = Fresh {
            checker: self,
            ctx,
            mapping: HashMap::default(),
        };

        fresh.fold_index(index)
    }

    pub fn instantiate_type(
        &mut self,
        t: &Index,
        mapping: &std::collections::HashMap<String, Index>,
    ) -> Index {
        let mut instantiate = Instantiate {
            checker: self,
            mapping,
        };
        instantiate.fold_index(t)
    }

    pub fn instantiate_func(
        &mut self,
        func: &Function,
        type_args: Option<&[Index]>,
    ) -> Result<Function, TypeError> {
        // A mapping of TypeVariables to TypeVariables
        let mut mapping = std::collections::HashMap::default();

        if let Some(type_params) = &func.type_params {
            match type_args {
                Some(type_args) => {
                    if type_args.len() != type_params.len() {
                        return Err(TypeError {
                            message: "wrong number of type args".to_string(),
                        });
                    }

                    for (tp, ta) in type_params.iter().zip(type_args.iter()) {
                        mapping.insert(tp.name.to_owned(), *ta);
                    }
                }
                None => {
                    for tp in type_params {
                        mapping.insert(tp.name.to_owned(), self.new_type_var(tp.constraint));
                    }
                }
            }
        }

        let mut instantiate = Instantiate {
            checker: self,
            mapping: &mut mapping,
        };

        let params = func
            .params
            .iter()
            .map(|param| FuncParam {
                t: instantiate.fold_index(&param.t),
                ..param.to_owned()
            })
            .collect::<Vec<_>>();

        let ret = instantiate.fold_index(&func.ret);

        let throws = func.throws.map(|t| instantiate.fold_index(&t));

        Ok(Function {
            params,
            ret,
            type_params: None,
            throws,
        })
    }
}

struct Fresh<'a, 'b> {
    checker: &'a mut Checker,
    ctx: &'b Context,

    mapping: HashMap<Index, Index>,
}

// TODO: Have `Checker` implement `KeyValueStore` instead of `Fresh`
impl<'a, 'b> KeyValueStore<Index, Type> for Fresh<'a, 'b> {
    fn get_type(&mut self, index: &Index) -> Type {
        self.checker.arena[*index].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.checker.arena.insert(t)
    }
}

impl<'a, 'b> Folder for Fresh<'a, 'b> {
    fn fold_index(&mut self, index: &Index) -> Index {
        let index = self.checker.prune(*index);
        let t = self.get_type(&index);

        match &t.kind {
            TypeKind::TypeVar(TypeVar {
                id: _,
                instance: _,
                constraint,
            }) => {
                // NOTE: This check requires that `index` be pruned.
                if !self.checker.occurs_in(index, &self.ctx.non_generic) {
                    self.mapping
                        .entry(index)
                        .or_insert_with(|| self.checker.new_type_var(*constraint))
                        .to_owned()
                } else {
                    index
                }
            }
            _ => walk_index(self, &index),
        }
    }
}

pub struct Instantiate<'a> {
    pub checker: &'a mut Checker,
    pub mapping: &'a std::collections::HashMap<String, Index>,
}

impl<'a> KeyValueStore<Index, Type> for Instantiate<'a> {
    // NOTE: The reason we return both an Index and a Type is that
    // this method calls `prune` which maybe return a different Index
    // from the one passed to it. We need to ensure this method returns
    // an Index that corresponds to the returned Type.
    fn get_type(&mut self, idx: &Index) -> Type {
        // let idx = prune(self.arena, *idx);
        self.checker.arena[*idx].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.checker.arena.insert(t)
    }
}

impl<'a> Folder for Instantiate<'a> {
    fn fold_index(&mut self, index: &Index) -> Index {
        // let index = prune(self.arena, *index);
        let t = self.get_type(index);

        match &t.kind {
            TypeKind::TypeRef(TypeRef {
                name,
                scheme, // TODO: walk the scheme's types as well
                type_args: types,
            }) => {
                let new_types = folder::walk_indexes(self, types);

                match self.mapping.get(name) {
                    Some(index) => {
                        // What does it mean to replace the constructor name
                        // when it has type args?
                        *index
                    }
                    None => {
                        if &new_types != types {
                            self.checker
                                .new_type_ref(name, scheme.to_owned(), &new_types)
                        } else {
                            *index
                        }
                    }
                }
            }
            _ => walk_index(self, index),
        }
    }
}
