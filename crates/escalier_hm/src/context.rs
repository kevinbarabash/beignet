use generational_arena::{Arena, Index};
use im::hashmap::HashMap;
use im::hashset::HashSet;

use crate::errors::*;
use crate::folder::walk_index;
use crate::folder::{self, Folder};
use crate::key_value_store::KeyValueStore2;
use crate::types::*;
use crate::util::*;

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

/// Get the type of identifier name from the type context ctx.
///
/// Args:
///     name: The identifier name
///     ctx: The current context
///
/// Raises:
///     ParseError: Raised if name is an undefined symbol in the type
///         environment.
pub fn get_type(arena: &mut Arena<Type>, name: &str, ctx: &Context) -> Result<Index, Errors> {
    if let Some(value) = ctx.values.get(name) {
        Ok(fresh(arena, value.index, ctx))
    } else {
        Err(Errors::InferenceError(format!(
            "Undefined symbol {:?}",
            name
        )))
    }
}

struct Fresh<'a> {
    arena: &'a mut Arena<Type>,
    ctx: &'a Context,

    mapping: HashMap<Index, Index>,
}

impl<'a> KeyValueStore2<Index, Type> for Fresh<'a> {
    fn get_type(&mut self, index: &Index) -> Type {
        self.arena[*index].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Folder for Fresh<'a> {
    fn fold_index(&mut self, index: &Index) -> Index {
        // QUESTION: Why do we need to `prune` here?  Maybe because we don't
        // copy the `instance` when creating an new type variable.
        let index = prune(self.arena, *index);
        let t = self.get_type(&index);

        match &t.kind {
            TypeKind::Variable(Variable {
                id: _,
                instance: _,
                constraint,
            }) => {
                if is_generic(self.arena, index, self.ctx) {
                    self.mapping
                        .entry(index)
                        .or_insert_with(|| new_var_type(self.arena, *constraint))
                        .to_owned()
                } else {
                    index
                }
            }
            _ => walk_index(self, &index),
        }
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
pub fn fresh(arena: &mut Arena<Type>, index: Index, ctx: &Context) -> Index {
    let mut fresh = Fresh {
        arena,
        ctx,
        mapping: HashMap::default(),
    };

    fresh.fold_index(&index)
}

pub struct Instantiate<'a> {
    pub arena: &'a mut Arena<Type>,
    pub mapping: &'a std::collections::HashMap<String, Index>,
}

impl<'a> KeyValueStore2<Index, Type> for Instantiate<'a> {
    // NOTE: The reason we return both an Index and a Type is that
    // this method calls `prune` which maybe return a different Index
    // from the one passed to it. We need to ensure this method returns
    // an Index that corresponds to the returned Type.
    fn get_type(&mut self, idx: &Index) -> Type {
        // let idx = prune(self.arena, *idx);
        self.arena[*idx].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Folder for Instantiate<'a> {
    fn fold_index(&mut self, index: &Index) -> Index {
        // let index = prune(self.arena, *index);
        let t = self.get_type(index);

        match &t.kind {
            TypeKind::Constructor(Constructor { name, types }) => {
                let new_types = folder::walk_indexes(self, types);

                match self.mapping.get(name) {
                    Some(index) => {
                        // What does it mean to replace the constructor name
                        // when it has type args?
                        *index
                    }
                    None => {
                        if &new_types != types {
                            new_constructor(self.arena, name, &new_types)
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

pub fn instantiate_scheme(
    arena: &mut Arena<Type>,
    t: Index,
    mapping: &std::collections::HashMap<String, Index>,
) -> Index {
    let mut instantiate2 = Instantiate { arena, mapping };
    instantiate2.fold_index(&t)
    // let mut instantiate = Instantiate { arena, mapping };
    // instantiate.visit_index(&t)
}

pub fn instantiate_func(
    arena: &mut Arena<Type>,
    func: &Function,
    type_args: Option<&[Index]>,
) -> Result<Function, Errors> {
    // A mapping of TypeVariables to TypeVariables
    let mut mapping = std::collections::HashMap::default();

    if let Some(type_params) = &func.type_params {
        match type_args {
            Some(type_args) => {
                if type_args.len() != type_params.len() {
                    return Err(Errors::InferenceError(
                        "wrong number of type args".to_string(),
                    ));
                }

                for (tp, ta) in type_params.iter().zip(type_args.iter()) {
                    mapping.insert(tp.name.to_owned(), *ta);
                }
            }
            None => {
                for tp in type_params {
                    mapping.insert(tp.name.to_owned(), new_var_type(arena, tp.constraint));
                }
            }
        }
    }

    let mut instantiate = Instantiate {
        arena,
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

/// Checks whether a given variable occurs in a list of non-generic variables
///
/// Note that a variables in such a list may be instantiated to a type term,
/// in which case the variables contained in the type term are considered
/// non-generic.
///
/// Note: Must be called with v pre-pruned
///
/// Args:
///     t: The TypeVariable to be tested for genericity
///     non_generic: A set of non-generic TypeVariables
///
/// Returns:
///     True if v is a generic variable, otherwise False
pub fn is_generic(arena: &mut Arena<Type>, t: Index, ctx: &Context) -> bool {
    !occurs_in(arena, t, &ctx.non_generic)
}
