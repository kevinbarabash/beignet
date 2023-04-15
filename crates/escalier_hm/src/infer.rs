use generational_arena::Arena;

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
pub fn infer_expression<'a>(
    arena: &mut Arena<Type>,
    node: &'a mut Expression,
    ctx: &mut Context,
) -> Result<ArenaType, Errors> {
    let t: ArenaType = match &mut node.kind {
        ExprKind::Identifier(Identifier { name }) => get_type(arena, name, ctx)?,
        ExprKind::Literal(literal) => new_lit_type(arena, literal),
        ExprKind::Tuple(syntax::Tuple { elems }) => {
            let mut element_types = vec![];
            for element in elems {
                let t = infer_expression(arena, element, ctx)?;
                element_types.push(t);
            }
            new_tuple_type(arena, &element_types)
        }
        ExprKind::Object(syntax::Object { props }) => {
            let mut prop_types = vec![];
            for (name, value) in props {
                let t = infer_expression(arena, value, ctx)?;
                prop_types.push((name.to_owned(), t));
            }
            new_object_type(arena, &prop_types)
        }
        ExprKind::Apply(Apply { func, args }) => {
            let func_type = infer_expression(arena, func, ctx)?;

            let arg_types = args
                .iter_mut()
                .map(|arg| infer_expression(arena, arg, ctx))
                .collect::<Result<Vec<_>, _>>()?;

            unify_call(arena, &arg_types, func_type)?
        }
        ExprKind::Lambda(Lambda { params, body }) => {
            let mut param_types = vec![];
            let mut new_ctx = ctx.clone();

            for FuncParam { pattern } in params {
                let param_type = new_var_type(arena);
                if let PatternKind::Ident(BindingIdent { name, mutable: _ }) = &pattern.kind {
                    pattern.inferred_type = Some(param_type);
                    new_ctx.env.insert(name.to_owned(), param_type);
                    new_ctx.non_generic.insert(param_type);
                    param_types.push(param_type);
                } else {
                    return Err(Errors::InferenceError(
                        "Other patterns are not yet supported as function params".to_owned(),
                    ));
                }
            }

            match body {
                BlockOrExpr::Block(Block { stmts }) => {
                    for stmt in stmts {
                        new_ctx = new_ctx.clone();
                        let t = infer_statement(arena, stmt, &mut new_ctx)?;
                        if let StmtKind::Return(_) = stmt.kind {
                            let ret_t = t;
                            let func_t = new_func_type(arena, &param_types, ret_t);
                            // TODO: warn if there are any statements after the return
                            // TODO: update AST with the inferred type
                            return Ok(func_t);
                        }
                    }

                    // If there's no return statement in the block, then the
                    // return type for the function is `undefined`.
                    let undefined = new_constructor(arena, "undefined", &[]);
                    new_func_type(arena, &param_types, undefined)
                }
                BlockOrExpr::Expr(expr) => {
                    let ret_type = infer_expression(arena, expr, &mut new_ctx)?;
                    new_func_type(arena, &param_types, ret_type)
                }
            }
        }
        ExprKind::Letrec(letrec) => {
            let mut new_ctx = ctx.clone();

            // Create all of the types new types first

            let mut new_types = vec![];

            for (var, ..) in &letrec.decls {
                let new_type = new_var_type(arena);
                new_ctx.env.insert(var.clone(), new_type);
                new_ctx.non_generic.insert(new_type);

                new_types.push(new_type);
            }

            // Then infer the defintions and unify them with the new types

            for ((.., defn), new_type) in letrec.decls.iter_mut().zip(new_types.iter()) {
                let defn_type = infer_expression(arena, defn, &mut new_ctx)?;
                unify(arena, *new_type, defn_type)?;
            }

            infer_expression(arena, &mut letrec.body, &mut new_ctx)?
        }
        ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = infer_expression(arena, cond, ctx)?;
            let bool_type = new_constructor(arena, "boolean", &[]);
            unify(arena, cond_type, bool_type)?;
            let consequent_type = infer_expression(arena, consequent, ctx)?;
            let alternate_type = infer_expression(arena, alternate, ctx)?;
            new_union_type(arena, &[consequent_type, alternate_type])
        }
        ExprKind::Member(Member { obj, prop }) => {
            let obj_type = infer_expression(arena, obj, ctx)?;
            let prop_type = infer_expression(arena, prop, ctx)?;

            let obj_type = arena[obj_type].clone();
            let prop_type = arena[prop_type].clone();

            match (&obj_type.kind, &prop_type.kind) {
                (TypeKind::Object(object), TypeKind::Literal(Literal::String(name))) => {
                    for (key, t) in &object.props {
                        if key == name {
                            // TODO: update AST with the inferred type
                            return Ok(*t);
                        }
                    }
                    return Err(Errors::InferenceError(format!(
                        "Couldn't find property '{name}' on object",
                    )));
                }
                (TypeKind::Tuple(tuple), TypeKind::Literal(Literal::Number(value))) => {
                    let index: usize = value.parse().unwrap();
                    if index < tuple.types.len() {
                        // TODO: update AST with the inferred type
                        return Ok(tuple.types[index]);
                    }
                    return Err(Errors::InferenceError(format!(
                        "{index} was outside the bounds 0..{} of the tuple",
                        tuple.types.len()
                    )));
                }
                _ => {
                    return Err(Errors::InferenceError(
                        "Can only access properties on objects/tuples".to_string(),
                    ));
                }
            }
        }
    };

    node.inferred_type = Some(t);

    Ok(t)
}

pub fn infer_statement<'a>(
    arena: &'a mut Arena<Type>,
    statement: &mut Statement,
    ctx: &mut Context,
) -> Result<ArenaType, Errors> {
    let t = match &mut statement.kind {
        StmtKind::Declaration(Declaration { pattern, defn }) => {
            if let PatternKind::Ident(BindingIdent { name, mutable: _ }) = &pattern.kind {
                let t = infer_expression(arena, defn, ctx)?;
                ctx.env.insert(name.clone(), t);
                pattern.inferred_type = Some(t);
                t // TODO: Should this be unit?
            } else {
                return Err(Errors::InferenceError(
                    "Can only declare variables with identifiers".to_string(),
                ));
            }
        }
        StmtKind::Expression(expr) => infer_expression(arena, expr, ctx)?,
        StmtKind::Return(Return { expr }) => infer_expression(arena, expr, ctx)?,
    };

    statement.inferred_type = Some(t);

    Ok(t)
}

pub fn infer_program<'a>(
    arena: &mut Arena<Type>,
    node: &'a mut Program,
    ctx: &mut Context,
) -> Result<(), Errors> {
    for stmt in &mut node.statements {
        infer_statement(arena, stmt, ctx)?;
    }

    Ok(())
}
