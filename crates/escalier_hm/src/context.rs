use generational_arena::{Arena, Index};
use im::hashmap::HashMap;
use im::hashset::HashSet;

use crate::errors::*;
use crate::types::*;
use crate::util::*;

#[derive(Clone, Debug, Default)]
pub struct Context {
    // The type environment mapping from identifier names to types
    pub env: HashMap<String, Index>,
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
    if let Some(value) = ctx.env.get(name) {
        Ok(fresh(arena, *value, ctx))
    } else {
        Err(Errors::InferenceError(format!(
            "Undefined symbol {:?}",
            name
        )))
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
    // A mapping of TypeVariables to TypeVariables
    let mut mappings = HashMap::default();

    // TODO: dedupe with instrec and generalize_rec
    fn freshrec(
        arena: &mut Arena<Type>,
        tp: Index,
        mappings: &mut HashMap<Index, Index>,
        ctx: &Context,
    ) -> Index {
        let p = prune(arena, tp);
        match &arena.get(p).unwrap().clone().kind {
            TypeKind::Variable(_) => {
                if is_generic(arena, p, ctx) {
                    mappings
                        .entry(p)
                        .or_insert_with(|| new_var_type(arena))
                        .to_owned()
                } else {
                    p
                }
            }
            TypeKind::Literal(lit) => new_lit_type(arena, lit),
            TypeKind::Object(object) => {
                let props: Vec<_> = object
                    .props
                    .iter()
                    .map(|prop| match prop {
                        TObjElem::Index(index) => {
                            let t = freshrec(arena, index.t, mappings, ctx);
                            TObjElem::Index(TIndex { t, ..index.clone() })
                        }
                        TObjElem::Prop(prop) => {
                            let t = freshrec(arena, prop.t, mappings, ctx);
                            TObjElem::Prop(TProp { t, ..prop.clone() })
                        }
                    })
                    .collect();
                if props != object.props {
                    new_object_type(arena, &props)
                } else {
                    p
                }
            }
            TypeKind::Rest(rest) => {
                let arg = freshrec(arena, rest.arg, mappings, ctx);
                if arg != rest.arg {
                    new_rest_type(arena, arg)
                } else {
                    p
                }
            }
            TypeKind::Function(func) => {
                let params = freshrec_many(arena, &func.params, mappings, ctx);
                let ret = freshrec(arena, func.ret, mappings, ctx);
                let type_params = func.type_params.clone();
                // TODO: copy the type params
                if params != func.params || ret != func.ret {
                    new_func_type(arena, &params, ret, type_params)
                } else {
                    p
                }
            }
            TypeKind::Constructor(con) => {
                let types = freshrec_many(arena, &con.types, mappings, ctx);
                if types != con.types {
                    new_constructor(arena, &con.name, &types)
                } else {
                    p
                }
            }
        }
    }

    pub fn freshrec_many(
        a: &mut Arena<Type>,
        types: &[Index],
        mappings: &mut HashMap<Index, Index>,
        ctx: &Context,
    ) -> Vec<Index> {
        types
            .iter()
            .map(|x| freshrec(a, *x, mappings, ctx))
            .collect()
    }

    freshrec(arena, t, &mut mappings, ctx)
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
pub fn is_generic(a: &mut Arena<Type>, t: Index, ctx: &Context) -> bool {
    !occurs_in(a, t, &ctx.non_generic)
}
