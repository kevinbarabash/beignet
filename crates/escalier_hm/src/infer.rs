use std::collections::HashSet;

use crate::env::*;
use crate::errors::*;
use crate::syntax::*;
use crate::types::*;
use crate::unify::*;

/// Computes the type of the expression given by node.
///
/// The type of the node is computed in the context of the
/// supplied type environment env. Data types can be introduced into the
/// language simply by having a predefined set of identifiers in the initial
/// environment. environment; this way there is no need to change the syntax or, more
/// importantly, the type-checking program when extending the language.
///
/// Args:
///     node: The root of the abstract syntax tree.
///     env: The type environment is a mapping of expression identifier names
///         to type assignments.
///     non_generic: A set of non-generic variables, or None
///
/// Returns:
///     The computed type of the expression.
///
/// Raises:
///     InferenceError: The type of the expression could not be inferred, for example
///         if it is not possible to unify two types such as Integer and Bool
///     ParseError: The abstract syntax tree rooted at node could not be parsed
pub fn infer(
    a: &mut Vec<Type>,
    node: &Syntax,
    env: &Env,
    non_generic: &HashSet<ArenaType>,
) -> Result<ArenaType, Errors> {
    match node {
        Syntax::Identifier(Identifier { name }) => get_type(a, name, env, non_generic),
        Syntax::Literal(literal) => {
            let t = new_lit_type(a, literal);
            Ok(t)
        }
        Syntax::Apply(Apply { func, args }) => {
            let func_type = infer(a, func, env, non_generic)?;
            let arg_types = args
                .iter()
                .map(|arg| infer(a, arg, env, non_generic))
                .collect::<Result<Vec<_>, _>>()?;
            let result_type = new_var_type(a);
            let call_type = new_call_type(a, &arg_types, result_type);
            unify(a, call_type, func_type)?;
            Ok(result_type)
        }
        Syntax::Lambda(Lambda { params, body }) => {
            let mut param_types = vec![];
            let mut new_env = env.clone();
            let mut new_non_generic = non_generic.clone();
            for param in params {
                let arg_type = new_var_type(a);
                new_env.0.insert(param.clone(), arg_type);
                new_non_generic.insert(arg_type);
                param_types.push(arg_type);
            }
            let result_type = infer(a, body, &new_env, &new_non_generic)?;
            let t = new_func_type(a, &param_types, result_type);
            Ok(t)
        }
        Syntax::Let(Let { defn, v, body }) => {
            let defn_type = infer(a, defn, env, non_generic)?;
            let mut new_env = env.clone();
            new_env.0.insert(v.clone(), defn_type);
            infer(a, body, &new_env, non_generic)
        }
        Syntax::Letrec(Letrec { defn, v, body }) => {
            let new_type = new_var_type(a);
            let mut new_env = env.clone();
            new_env.0.insert(v.clone(), new_type);
            let mut new_non_generic = non_generic.clone();
            new_non_generic.insert(new_type);
            let defn_type = infer(a, defn, &new_env, &new_non_generic)?;
            unify(a, new_type, defn_type)?;
            infer(a, body, &new_env, non_generic)
        }
        Syntax::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = infer(a, cond, env, non_generic)?;
            let bool_type = new_constructor(a, "boolean", &[]);
            unify(a, cond_type, bool_type)?;
            let consequent_type = infer(a, consequent, env, non_generic)?;
            let alternate_type = infer(a, alternate, env, non_generic)?;
            let t = new_union_type(a, &[consequent_type, alternate_type]);
            Ok(t)
        }
    }
}
