use crate::context::*;
use crate::errors::*;
use crate::literal::*;
use crate::syntax::{self, *};
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
    ctx: &mut Context,
) -> Result<ArenaType, Errors> {
    match node {
        Expression::Identifier(Identifier { name }) => get_type(a, name, ctx),
        Expression::Literal(literal) => {
            let t = new_lit_type(a, literal);
            Ok(t)
        }
        Expression::Tuple(syntax::Tuple { elems }) => {
            let mut element_types = vec![];
            for element in elems {
                let t = infer_expression(a, element, ctx)?;
                element_types.push(t);
            }
            let t = new_tuple_type(a, &element_types);
            Ok(t)
        }
        Expression::Object(syntax::Object { props }) => {
            let mut prop_types = vec![];
            for (name, value) in props {
                let t = infer_expression(a, value, ctx)?;
                prop_types.push((name.to_owned(), t));
            }
            let t = new_object_type(a, &prop_types);
            Ok(t)
        }
        Expression::Apply(Apply { func, args }) => {
            let func_type = infer_expression(a, func, ctx)?;

            let arg_types = args
                .iter()
                .map(|arg| infer_expression(a, arg, ctx))
                .collect::<Result<Vec<_>, _>>()?;

            let ret_type = unify_call(a, &arg_types, func_type)?;
            Ok(ret_type)
        }
        Expression::Lambda(Lambda { params, body }) => {
            let mut param_types = vec![];
            let mut new_ctx = ctx.clone();

            for param in params {
                let arg_type = new_var_type(a);
                new_ctx.env.insert(param.clone(), arg_type);
                new_ctx.non_generic.insert(arg_type);
                param_types.push(arg_type);
            }

            for stmt in body {
                new_ctx = new_ctx.clone();
                let t = infer_statement(a, stmt, &mut new_ctx)?;
                if let Statement::Return(_) = stmt {
                    let ret_t = t;
                    let func_t = new_func_type(a, &param_types, ret_t);
                    // TODO: warn if there are any statements after the return
                    return Ok(func_t);
                }
            }

            // TODO: create a new type for undefined and use that as the return type
            let ret_t = infer_statement(a, &body[0], &mut new_ctx)?;
            let t = new_func_type(a, &param_types, ret_t);
            Ok(t)
        }
        Expression::Letrec(letrec) => {
            let mut new_ctx = ctx.clone();

            // Create all of the types new types first

            let mut new_types = vec![];

            for (var, ..) in &letrec.decls {
                let new_type = new_var_type(a);
                new_ctx.env.insert(var.clone(), new_type);
                new_ctx.non_generic.insert(new_type);

                new_types.push(new_type);
            }

            // Then infer the defintions and unify them with the new types

            for ((.., defn), new_type) in letrec.decls.iter().zip(new_types.iter()) {
                let defn_type = infer_expression(a, defn, &mut new_ctx)?;
                unify(a, *new_type, defn_type)?;
            }

            infer_expression(a, &letrec.body, &mut new_ctx)
        }
        Expression::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = infer_expression(a, cond, ctx)?;
            let bool_type = new_constructor(a, "boolean", &[]);
            unify(a, cond_type, bool_type)?;
            let consequent_type = infer_expression(a, consequent, ctx)?;
            let alternate_type = infer_expression(a, alternate, ctx)?;
            let t = new_union_type(a, &[consequent_type, alternate_type]);
            Ok(t)
        }
        Expression::Member(Member { obj, prop }) => {
            let obj_type = infer_expression(a, obj, ctx)?;
            let prop_type = infer_expression(a, prop, ctx)?;

            let obj_type = a[obj_type].clone();
            let prop_type = a[prop_type].clone();

            match (&obj_type.kind, &prop_type.kind) {
                (TypeKind::Object(object), TypeKind::Literal(Literal::String(name))) => {
                    for (key, t) in &object.props {
                        if key == name {
                            return Ok(*t);
                        }
                    }
                    Err(Errors::InferenceError(format!(
                        "Couldn't find property '{name}' on object",
                    )))
                }
                (TypeKind::Tuple(tuple), TypeKind::Literal(Literal::Number(value))) => {
                    let index: usize = value.parse().unwrap();
                    if index < tuple.types.len() {
                        return Ok(tuple.types[index]);
                    }
                    Err(Errors::InferenceError(format!(
                        "{index} was outside the bounds 0..{} of the tuple",
                        tuple.types.len()
                    )))
                }
                _ => Err(Errors::InferenceError(
                    "Can only access properties on objects/tuples".to_string(),
                )),
            }
        }
    }
}

pub fn infer_statement(
    a: &mut Vec<Type>,
    statement: &Statement,
    ctx: &mut Context,
) -> Result<ArenaType, Errors> {
    match statement {
        Statement::Declaration(Declaration { var, defn }) => {
            let t = infer_expression(a, defn, ctx)?;
            ctx.env.insert(var.clone(), t);
            Ok(t) // TODO: Should this be unit?
        }
        Statement::Expression(expr) => {
            let t = infer_expression(a, expr, ctx)?;
            Ok(t)
        }
        Statement::Return(Return { expr }) => {
            let t = infer_expression(a, expr, ctx)?;
            Ok(t)
        }
    }
}

pub fn infer_program(a: &mut Vec<Type>, node: &Program, ctx: &mut Context) -> Result<(), Errors> {
    for stmt in &node.statements {
        infer_statement(a, stmt, ctx)?;
    }

    Ok(())
}
