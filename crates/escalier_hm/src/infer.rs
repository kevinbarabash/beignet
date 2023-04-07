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
pub fn infer_expression(
    a: &mut Vec<Type>,
    node: &Expression,
    env: &mut Env,
    non_generic: &HashSet<ArenaType>,
) -> Result<ArenaType, Errors> {
    match node {
        Expression::Identifier(Identifier { name }) => get_type(a, name, env, non_generic),
        Expression::Literal(literal) => {
            let t = new_lit_type(a, literal);
            Ok(t)
        }
        Expression::Apply(Apply { func, args }) => {
            let func_type = infer_expression(a, func, env, non_generic)?;
            let arg_types = args
                .iter()
                .map(|arg| infer_expression(a, arg, env, non_generic))
                .collect::<Result<Vec<_>, _>>()?;
            let result_type = new_var_type(a);
            let call_type = new_call_type(a, &arg_types, result_type);
            unify(a, call_type, func_type)?;
            Ok(result_type)
        }
        Expression::Lambda(Lambda { params, body }) => {
            let mut param_types = vec![];
            let mut new_env = env.clone();
            let mut new_non_generic = non_generic.clone();

            for param in params {
                let arg_type = new_var_type(a);
                new_env.0.insert(param.clone(), arg_type);
                new_non_generic.insert(arg_type);
                param_types.push(arg_type);
            }

            for stmt in body {
                let t = infer_statement(a, stmt, &mut new_env, &new_non_generic)?;
                if let Statement::Return(_) = stmt {
                    let ret_t = t;
                    let func_t = new_func_type(a, &param_types, ret_t);
                    // TODO: warn if there are any statements after the return
                    return Ok(func_t);
                }
            }

            // TODO: create a new type for undefined and use that as the return type
            let ret_t = infer_statement(a, &body[0], &mut new_env, &new_non_generic)?;
            let t = new_func_type(a, &param_types, ret_t);
            Ok(t)
        }
        Expression::Let(Let { defn, var, body }) => {
            let defn_type = infer_expression(a, defn, env, non_generic)?;
            let mut new_env = env.clone();
            new_env.0.insert(var.clone(), defn_type);
            infer_expression(a, body, &mut new_env, non_generic)
        }
        Expression::Letrec(letrec) => {
            let mut new_env = env.clone();
            let mut new_non_generic = non_generic.clone();

            // Create all of the types new types first

            let mut new_types = vec![];

            for (var, ..) in &letrec.decls {
                let new_type = new_var_type(a);
                new_env.0.insert(var.clone(), new_type);
                new_non_generic.insert(new_type);

                new_types.push(new_type);
            }

            // Then infer the defintions and unify them with the new types

            for ((.., defn), new_type) in letrec.decls.iter().zip(new_types.iter()) {
                let defn_type = infer_expression(a, defn, &mut new_env, &new_non_generic)?;
                unify(a, *new_type, defn_type)?;
            }

            infer_expression(a, &letrec.body, &mut new_env, non_generic)
        }
        Expression::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = infer_expression(a, cond, env, non_generic)?;
            let bool_type = new_constructor(a, "boolean", &[]);
            unify(a, cond_type, bool_type)?;
            let consequent_type = infer_expression(a, consequent, env, non_generic)?;
            let alternate_type = infer_expression(a, alternate, env, non_generic)?;
            let t = new_union_type(a, &[consequent_type, alternate_type]);
            Ok(t)
        }
    }
}

pub fn infer_statement(
    a: &mut Vec<Type>,
    statement: &Statement,
    env: &mut Env,
    non_generic: &HashSet<ArenaType>,
) -> Result<ArenaType, Errors> {
    match statement {
        Statement::Declaration(Declaration { var, defn }) => {
            let t = infer_expression(a, defn, env, non_generic)?;
            env.0.insert(var.clone(), t);
            Ok(t) // TODO: Should this be unit?
        }
        Statement::Expression(expr) => {
            let t = infer_expression(a, expr, env, non_generic)?;
            Ok(t)
        }
        Statement::Return(Return { expr }) => {
            let t = infer_expression(a, expr, env, non_generic)?;
            Ok(t)
        }
    }
}

pub fn infer_program(a: &mut Vec<Type>, node: &Program, env: &mut Env) -> Result<(), Errors> {
    let non_generic = HashSet::new();

    for stmt in &node.statements {
        infer_statement(a, stmt, env, &non_generic)?;
    }

    Ok(())
}
