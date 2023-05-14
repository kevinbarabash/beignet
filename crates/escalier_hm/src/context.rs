use generational_arena::{Arena, Index};
use im::hashmap::HashMap;
use im::hashset::HashSet;

use crate::errors::*;
use crate::types::*;
use crate::util::*;

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
                        .or_insert_with(|| new_var_type(arena, None))
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
                        TObjElem::Method(method) => {
                            let params = freshrec_many(arena, &method.params, mappings, ctx);
                            let ret = freshrec(arena, method.ret, mappings, ctx);
                            TObjElem::Method(TMethod {
                                params,
                                ret,
                                ..method.to_owned()
                            })
                        }
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

pub fn instantiate_scheme(
    arena: &mut Arena<Type>,
    t: Index,
    mapping: &std::collections::HashMap<String, Index>,
) -> Index {
    // A mapping of TypeVariables to TypeVariables
    // let mut mappings = HashMap::default();

    // TODO: dedupe with instrec and generalize_rec
    fn instantiate_scheme_rec(
        arena: &mut Arena<Type>,
        tp: Index,
        mapping: &std::collections::HashMap<String, Index>,
    ) -> Index {
        let p = prune(arena, tp);
        match &arena.get(p).unwrap().clone().kind {
            // NOTE: we should really try to avoid instantiate schemes with type variables in them
            TypeKind::Variable(Variable {
                id: _,
                instance,
                constraint,
            }) => {
                let instance = instance.map(|idx| instantiate_scheme_rec(arena, idx, mapping));
                let constraint = constraint.map(|idx| instantiate_scheme_rec(arena, idx, mapping));
                arena.insert(Type {
                    kind: TypeKind::Variable(Variable {
                        id: arena.len(), // use for debugging purposes only
                        instance,
                        constraint,
                    }),
                })
            }
            TypeKind::Literal(lit) => new_lit_type(arena, lit),
            TypeKind::Object(object) => {
                let props: Vec<_> = object
                    .props
                    .iter()
                    .map(|prop| match prop {
                        TObjElem::Method(method) => {
                            let params =
                                instantiate_scheme_rec_many(arena, &method.params, mapping);
                            let ret = instantiate_scheme_rec(arena, method.ret, mapping);
                            TObjElem::Method(TMethod {
                                params,
                                ret,
                                ..method.to_owned()
                            })
                        }
                        TObjElem::Index(index) => {
                            let t = instantiate_scheme_rec(arena, index.t, mapping);
                            TObjElem::Index(TIndex { t, ..index.clone() })
                        }
                        TObjElem::Prop(prop) => {
                            let t = instantiate_scheme_rec(arena, prop.t, mapping);
                            TObjElem::Prop(TProp { t, ..prop.clone() })
                        }
                    })
                    .collect();
                new_object_type(arena, &props)
            }
            TypeKind::Rest(rest) => {
                let arg = instantiate_scheme_rec(arena, rest.arg, mapping);
                new_rest_type(arena, arg)
            }
            TypeKind::Function(func) => {
                let params = instantiate_scheme_rec_many(arena, &func.params, mapping);
                let ret = instantiate_scheme_rec(arena, func.ret, mapping);
                let type_params = func.type_params.clone();
                new_func_type(arena, &params, ret, type_params)
            }
            TypeKind::Constructor(con) => {
                let types = instantiate_scheme_rec_many(arena, &con.types, mapping);

                match mapping.get(&con.name) {
                    Some(idx) => {
                        // What does it mean to replace the constructor name
                        // when it has type args?
                        idx.to_owned()
                    }
                    None => new_constructor(arena, &con.name, &types),
                }
            }
        }
    }

    pub fn instantiate_scheme_rec_many(
        arena: &mut Arena<Type>,
        types: &[Index],
        mapping: &std::collections::HashMap<String, Index>,
    ) -> Vec<Index> {
        types
            .iter()
            .map(|x| instantiate_scheme_rec(arena, *x, mapping))
            .collect()
    }

    instantiate_scheme_rec(arena, t, mapping)
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
