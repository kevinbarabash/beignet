use im::hashmap::HashMap;
use im::hashset::HashSet;

use crate::errors::*;
use crate::types::*;
use crate::util::*;

#[derive(Clone, Debug, Default)]
pub struct Context {
    // The type environment mapping from identifier names to types
    pub env: HashMap<String, ArenaType>,
    // A set of non-generic TypeVariables.
    // NOTE: The same type variable can be both generic and non-generic in
    // different contexts.
    pub non_generic: HashSet<ArenaType>,
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
pub fn get_type(a: &mut Vec<Type>, name: &str, ctx: &Context) -> Result<ArenaType, Errors> {
    if let Some(value) = ctx.env.get(name) {
        Ok(fresh(a, value, ctx))
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
pub fn fresh(a: &mut Vec<Type>, t: &ArenaType, ctx: &Context) -> ArenaType {
    // A mapping of TypeVariables to TypeVariables
    let mut mappings = HashMap::default();

    fn freshrec(
        a: &mut Vec<Type>,
        tp: &ArenaType,
        mappings: &mut HashMap<ArenaType, ArenaType>,
        ctx: &Context,
    ) -> ArenaType {
        let p = prune(a, tp);
        match &a.get(p.value).unwrap().clone().kind {
            TypeKind::Variable(_) => {
                if is_generic(a, &p, ctx) {
                    mappings
                        .entry(p)
                        .or_insert_with(|| new_var_type(a))
                        .to_owned()
                } else {
                    p
                }
            }
            TypeKind::Constructor(con) => {
                let types = freshrec_many(a, &con.types, mappings, ctx);
                new_constructor(a, &con.name, &types)
            }
            TypeKind::Literal(lit) => new_lit_type(a, lit),
            TypeKind::Tuple(tuple) => {
                let types = freshrec_many(a, &tuple.types, mappings, ctx);
                new_tuple_type(a, &types)
            }
            TypeKind::Object(object) => {
                let fields: Vec<_> = object
                    .props
                    .iter()
                    .map(|(name, tp)| (name.clone(), freshrec(a, tp, mappings, ctx)))
                    .collect();
                new_object_type(a, &fields)
            }
            TypeKind::Function(func) => {
                let params = freshrec_many(a, &func.params, mappings, ctx);
                let ret = freshrec(a, &func.ret, mappings, ctx);
                new_func_type(a, &params, ret)
            }
            TypeKind::Union(union) => {
                let types = freshrec_many(a, &union.types, mappings, ctx);
                new_union_type(a, &types)
            }
        }
    }

    pub fn freshrec_many(
        a: &mut Vec<Type>,
        types: &[ArenaType],
        mappings: &mut HashMap<ArenaType, ArenaType>,
        ctx: &Context,
    ) -> Vec<ArenaType> {
        types
            .iter()
            .map(|x| freshrec(a, x, mappings, ctx))
            .collect()
    }

    freshrec(a, t, &mut mappings, ctx)
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
pub fn is_generic(a: &mut Vec<Type>, t: &ArenaType, ctx: &Context) -> bool {
    !occurs_in(a, t, &ctx.non_generic)
}
