use generational_arena::{Arena, Index};
use im::hashmap::HashMap;
use im::hashset::HashSet;

use crate::errors::*;
use crate::types::*;
use crate::util::*;
use crate::visitor::{KeyValueStore, Visitor};

#[derive(Clone, Debug, Default)]
pub struct Context {
    // Maps variables to their types.
    pub values: HashMap<String, Index>,
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
        Ok(fresh(arena, *value, ctx))
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

impl<'a> KeyValueStore<Index, Type> for Fresh<'a> {
    // NOTE: The reason we return both an Index and a Type is that
    // this method calls `prune` which maybe return a different Index
    // from the one passed to it. We need to ensure this method returns
    // an Index that corresponds to the returned Type.
    fn get_type(&mut self, idx: &Index) -> (Index, Type) {
        let idx = prune(self.arena, *idx);
        let t = self.arena[idx].clone();
        (idx, t)
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Visitor for Fresh<'a> {
    fn visit_type_var(&mut self, _: &Variable, idx: &Index) -> Index {
        if is_generic(self.arena, *idx, self.ctx) {
            self.mapping
                .entry(*idx)
                .or_insert_with(|| new_var_type(self.arena, None))
                .to_owned()
        } else {
            *idx
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
pub fn fresh(arena: &mut Arena<Type>, t: Index, ctx: &Context) -> Index {
    let mut fresh = Fresh {
        arena,
        ctx,
        mapping: HashMap::default(),
    };

    fresh.visit_index(&t)
}

pub struct Instantiate<'a> {
    pub arena: &'a mut Arena<Type>,
    pub mapping: &'a std::collections::HashMap<String, Index>,
}

impl<'a> KeyValueStore<Index, Type> for Instantiate<'a> {
    // NOTE: The reason we return both an Index and a Type is that
    // this method calls `prune` which maybe return a different Index
    // from the one passed to it. We need to ensure this method returns
    // an Index that corresponds to the returned Type.
    fn get_type(&mut self, idx: &Index) -> (Index, Type) {
        let idx = prune(self.arena, *idx);
        let t = self.arena[idx].clone();
        (idx, t)
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Visitor for Instantiate<'a> {
    fn visit_type_var(&mut self, _: &Variable, idx: &Index) -> Index {
        // We assume that any type variables were created by instantiate and
        // thus there is no need to process them further.
        *idx
    }
    fn visit_type_ref(&mut self, tref: &Constructor, idx: &Index) -> Index {
        let types = self.visit_indexes(&tref.types);

        match self.mapping.get(&tref.name) {
            Some(idx) => {
                // What does it mean to replace the constructor name
                // when it has type args?
                *idx
            }
            None => {
                if types != tref.types {
                    new_constructor(self.arena, &tref.name, &types)
                } else {
                    *idx
                }
            }
        }
    }
}

pub fn instantiate_scheme(
    arena: &mut Arena<Type>,
    t: Index,
    mapping: &std::collections::HashMap<String, Index>,
) -> Index {
    let mut instantiate = Instantiate { arena, mapping };

    instantiate.visit_index(&t)
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
            t: instantiate.visit_index(&param.t),
            ..param.to_owned()
        })
        .collect::<Vec<_>>();

    let ret = instantiate.visit_index(&func.ret);

    Ok(Function {
        params,
        ret,
        type_params: None,
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
