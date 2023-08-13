use generational_arena::{Arena, Index};
use itertools::Itertools;
use std::collections::{BTreeMap, HashSet};

use escalier_ast::{self as syntax, *};

use crate::ast_utils::find_returns;
use crate::ast_utils::{find_throws, find_throws_in_block};
use crate::context::*;
use crate::errors::*;
use crate::infer_pattern::*;
use crate::provenance::Provenance;
use crate::types::{self, *};
use crate::unify::*;
use crate::util::*;
use crate::visitor::{KeyValueStore, Visitor};

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
    arena: &mut Arena<Type>,
    node: &mut Expr,
    ctx: &mut Context,
) -> Result<Index, Errors> {
    let idx: Index = match &mut node.kind {
        ExprKind::Ident(Ident { name, .. }) => get_type(arena, name, ctx)?,
        ExprKind::Str(str) => arena.insert(Type::from(TypeKind::Literal(syntax::Literal::String(
            str.value.to_owned(),
        )))),
        ExprKind::Num(num) => arena.insert(Type::from(TypeKind::Literal(syntax::Literal::Number(
            num.value.to_owned(),
        )))),
        ExprKind::Bool(bool) => arena.insert(Type::from(TypeKind::Literal(
            syntax::Literal::Boolean(bool.value),
        ))),
        ExprKind::Null(_) => arena.insert(Type::from(TypeKind::Literal(syntax::Literal::Null))),
        ExprKind::Undefined(_) => {
            arena.insert(Type::from(TypeKind::Literal(syntax::Literal::Undefined)))
        }
        ExprKind::Tuple(syntax::Tuple {
            elements: elems, ..
        }) => {
            let mut element_types = vec![];
            for element in elems.iter_mut() {
                let t = match element {
                    ExprOrSpread::Expr(expr) => infer_expression(arena, expr, ctx)?,
                    ExprOrSpread::Spread(_) => todo!(), // TODO: handle spreads
                };
                element_types.push(t);
            }
            new_tuple_type(arena, &element_types)
        }
        ExprKind::Object(syntax::Object {
            properties: props, ..
        }) => {
            let mut prop_types: Vec<types::TObjElem> = vec![];
            for prop_or_spread in props.iter_mut() {
                match prop_or_spread {
                    PropOrSpread::Spread(_) => todo!(),
                    PropOrSpread::Prop(prop) => match prop {
                        expr::Prop::Shorthand(ident) => {
                            prop_types.push(types::TObjElem::Prop(types::TProp {
                                name: TPropKey::StringKey(ident.to_owned()),
                                modifier: None,
                                t: get_type(arena, ident, ctx)?,
                                mutable: false,
                                optional: false,
                            }));
                        }
                        expr::Prop::Property { key, value } => {
                            let prop = match key {
                                ObjectKey::Ident(ident) => types::TProp {
                                    name: TPropKey::StringKey(ident.name.to_owned()),
                                    modifier: None,
                                    t: infer_expression(arena, value, ctx)?,
                                    mutable: false,
                                    optional: false,
                                },
                                ObjectKey::String(name) => types::TProp {
                                    name: TPropKey::StringKey(name.to_owned()),
                                    modifier: None,
                                    t: infer_expression(arena, value, ctx)?,
                                    mutable: false,
                                    optional: false,
                                },
                                ObjectKey::Number(name) => types::TProp {
                                    name: TPropKey::StringKey(name.to_owned()),
                                    modifier: None,
                                    t: infer_expression(arena, value, ctx)?,
                                    mutable: false,
                                    optional: false,
                                },
                                ObjectKey::Computed(_) => todo!(),
                            };
                            prop_types.push(types::TObjElem::Prop(prop));
                        }
                    },
                }
            }
            new_object_type(arena, &prop_types)
        }
        ExprKind::Call(syntax::Call {
            callee: func,
            args,
            type_args,
            opt_chain,
            throws,
        }) => {
            let mut func_idx = infer_expression(arena, func, ctx)?;
            let mut has_undefined = false;
            if *opt_chain {
                if let TypeKind::Union(union) = &arena[func_idx].kind {
                    let types = filter_nullables(arena, &union.types);
                    has_undefined = types.len() != union.types.len();
                    func_idx = new_union_type(arena, &types);
                }
            }

            let (result, new_throws) = match type_args {
                Some(type_args) => {
                    let type_args = type_args
                        .iter_mut()
                        .map(|type_arg| infer_type_ann(arena, type_arg, ctx))
                        .collect::<Result<Vec<_>, _>>()?;

                    unify_call(arena, ctx, args, Some(&type_args), func_idx)?
                }
                None => unify_call(arena, ctx, args, None, func_idx)?,
            };

            if let Some(new_throws) = new_throws {
                throws.replace(new_throws);
            }

            match *opt_chain && has_undefined {
                true => {
                    let undefined = new_keyword(arena, Keyword::Undefined);

                    if let TypeKind::Union(union) = &arena[result].kind {
                        let mut types = filter_nullables(arena, &union.types);

                        if types.len() != union.types.len() {
                            // If we didn't end up removing any `undefined`s then
                            // itmeans that `result` already contains `undefined`
                            // and we can return it as is.
                            result
                        } else {
                            types.push(undefined);
                            new_union_type(arena, &types)
                        }
                    } else {
                        new_union_type(arena, &[result, undefined])
                    }
                }
                false => result,
            }
        }
        ExprKind::Function(syntax::Function {
            params,
            body,
            is_async,
            is_gen: _,
            type_params,
            type_ann: return_type,
            throws: sig_throws,
        }) => {
            let mut func_params: Vec<types::FuncParam> = vec![];
            let mut sig_ctx = ctx.clone();

            let type_params = infer_type_params(arena, type_params, &mut sig_ctx)?;

            for syntax::FuncParam {
                pattern,
                type_ann,
                optional,
            } in params.iter_mut()
            {
                let type_ann_t = match type_ann {
                    Some(type_ann) => infer_type_ann(arena, type_ann, &mut sig_ctx)?,
                    None => new_var_type(arena, None),
                };
                pattern.inferred_type = Some(type_ann_t);

                let (assumps, param_t) = infer_pattern(arena, pattern, &sig_ctx)?;
                unify(arena, &sig_ctx, param_t, type_ann_t)?;

                for (name, binding) in assumps {
                    sig_ctx.non_generic.insert(binding.index);
                    sig_ctx.values.insert(name.to_owned(), binding);
                }

                func_params.push(types::FuncParam {
                    pattern: pattern_to_tpat(pattern, true),
                    t: type_ann_t,
                    optional: *optional,
                });
            }

            let mut body_ctx = sig_ctx.clone();
            body_ctx.is_async = *is_async;

            let mut body_t = 'outer: {
                match body {
                    BlockOrExpr::Block(Block { stmts, .. }) => {
                        for stmt in stmts.iter_mut() {
                            body_ctx = body_ctx.clone();
                            infer_statement(arena, stmt, &mut body_ctx, false)?;
                            if let StmtKind::Return { arg: _ } = stmt.kind {
                                let ret_types: Vec<Index> = find_returns(body)
                                    .iter()
                                    .filter_map(|ret| ret.inferred_type)
                                    .collect();

                                // TODO: warn about unreachable code.
                                break 'outer new_union_type(arena, &ret_types);
                            }
                        }

                        // If we don't encounter a return statement, we assume
                        // the return type is `undefined`.
                        new_keyword(arena, Keyword::Undefined)
                    }
                    BlockOrExpr::Expr(expr) => {
                        // TODO: use `find_returns` here as well
                        infer_expression(arena, expr, &mut body_ctx)?
                    }
                }
            };

            // TODO: search for `throw` expressions in the body and include
            // them in the throws type.

            let body_throws = find_throws(body);
            let body_throws = if body_throws.is_empty() {
                None
            } else {
                Some(new_union_type(
                    arena,
                    // TODO: compare string reps of the types for deduplication
                    &body_throws.into_iter().unique().collect_vec(),
                ))
            };

            let sig_throws = sig_throws
                .as_mut()
                .map(|t| infer_type_ann(arena, t, &mut sig_ctx))
                .transpose()?;

            let throws = match (body_throws, sig_throws) {
                (Some(call_throws), Some(sig_throws)) => {
                    unify(arena, &sig_ctx, call_throws, sig_throws)?;
                    Some(sig_throws)
                }
                (Some(call_throws), None) => Some(call_throws),
                // This should probably be a warning.  If the function doesn't
                // throw anything, then it shouldn't be marked as such.
                (None, Some(sig_throws)) => Some(sig_throws),
                (None, None) => None,
            };

            // TODO: Make the return type `Promise<body_t, throws>` if the function
            // is async.  Async functions cannot throw.  They can only return a
            // rejected promise.
            if *is_async && !is_promise(&arena[body_t]) {
                let never = new_keyword(arena, Keyword::Never);
                let throws_t = throws.unwrap_or(never);
                body_t = new_constructor(arena, "Promise", &[body_t, throws_t]);

                match return_type {
                    Some(return_type) => {
                        let ret_t = infer_type_ann(arena, return_type, &mut sig_ctx)?;
                        // TODO: add sig_ctx which is a copy of ctx but with all of
                        // the type params added to sig_ctx.schemes so that they can
                        // be looked up.
                        unify(arena, &sig_ctx, body_t, ret_t)?;
                        new_func_type(arena, &func_params, ret_t, &type_params, None)
                    }
                    None => new_func_type(arena, &func_params, body_t, &type_params, None),
                }
            } else {
                match return_type {
                    Some(return_type) => {
                        let ret_t = infer_type_ann(arena, return_type, &mut sig_ctx)?;
                        // TODO: add sig_ctx which is a copy of ctx but with all of
                        // the type params added to sig_ctx.schemes so that they can
                        // be looked up.
                        unify(arena, &sig_ctx, body_t, ret_t)?;
                        new_func_type(arena, &func_params, ret_t, &type_params, throws)
                    }
                    None => new_func_type(arena, &func_params, body_t, &type_params, throws),
                }
            }
        }
        ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = infer_expression(arena, cond, ctx)?;
            let bool_type = new_primitive(arena, Primitive::Boolean);
            unify(arena, ctx, cond_type, bool_type)?;
            let consequent_type = infer_block(arena, consequent, ctx)?;
            let alternate_type = match alternate {
                Some(alternate) => match alternate {
                    BlockOrExpr::Block(block) => infer_block(arena, block, ctx)?,
                    BlockOrExpr::Expr(expr) => infer_expression(arena, expr, ctx)?,
                },
                None => new_keyword(arena, Keyword::Undefined),
            };
            new_union_type(arena, &[consequent_type, alternate_type])
        }
        ExprKind::Member(Member {
            object: obj,
            property: prop,
            opt_chain,
        }) => {
            let mut obj_idx = infer_expression(arena, obj, ctx)?;
            let mut has_undefined = false;
            if *opt_chain {
                if let TypeKind::Union(union) = &arena[obj_idx].kind {
                    let types = filter_nullables(arena, &union.types);
                    has_undefined = types.len() != union.types.len();
                    obj_idx = new_union_type(arena, &types);
                }
            }

            let result = match prop {
                MemberProp::Ident(Ident { name, .. }) => {
                    let key_idx = new_lit_type(arena, &Literal::String(name.to_owned()));
                    get_ident_member(arena, ctx, obj_idx, key_idx)?
                }
                MemberProp::Computed(ComputedPropName { expr, .. }) => {
                    let prop_type = infer_expression(arena, expr, ctx)?;
                    get_computed_member(arena, ctx, obj_idx, prop_type)?
                }
            };

            match *opt_chain && has_undefined {
                true => {
                    let undefined = new_keyword(arena, Keyword::Undefined);

                    if let TypeKind::Union(union) = &arena[result].kind {
                        let mut types = filter_nullables(arena, &union.types);

                        if types.len() != union.types.len() {
                            // If we didn't end up removing any `undefined`s then
                            // itmeans that `result` already contains `undefined`
                            // and we can return it as is.
                            result
                        } else {
                            types.push(undefined);
                            new_union_type(arena, &types)
                        }
                    } else {
                        new_union_type(arena, &[result, undefined])
                    }
                }
                false => result,
            }
        }
        ExprKind::JSXElement(_) => todo!(),
        ExprKind::Assign(Assign { left, op: _, right }) => {
            if !lvalue_mutability(ctx, left)? {
                return Err(Errors::InferenceError(
                    "Cannot assign to immutable lvalue".to_string(),
                ));
            }

            let l_t = infer_expression(arena, left, ctx)?;
            let r_t = infer_expression(arena, right, ctx)?;
            unify(arena, ctx, r_t, l_t)?;

            r_t
        }
        ExprKind::Binary(Binary { op, left, right }) => {
            let number = new_primitive(arena, Primitive::Number);
            let boolean = new_primitive(arena, Primitive::Boolean);
            let left_type = infer_expression(arena, left, ctx)?;
            let right_type = infer_expression(arena, right, ctx)?;

            match op {
                BinaryOp::Plus
                | BinaryOp::Minus
                | BinaryOp::Times
                | BinaryOp::Divide
                | BinaryOp::Modulo => match (&arena[left_type].kind, &arena[right_type].kind) {
                    (
                        TypeKind::Literal(Literal::Number(left)),
                        TypeKind::Literal(Literal::Number(right)),
                    ) => {
                        let left = left.parse::<f64>().unwrap();
                        let right = right.parse::<f64>().unwrap();

                        let result = match op {
                            BinaryOp::Plus => left + right,
                            BinaryOp::Minus => left - right,
                            BinaryOp::Times => left * right,
                            BinaryOp::Divide => left / right,
                            BinaryOp::Modulo => left % right,
                            _ => unreachable!(),
                        };

                        new_lit_type(arena, &Literal::Number(result.to_string()))
                    }
                    (_, _) => {
                        unify(arena, ctx, left_type, number)?;
                        unify(arena, ctx, right_type, number)?;
                        number
                    }
                },
                BinaryOp::GreaterThan
                | BinaryOp::GreaterThanOrEqual
                | BinaryOp::LessThan
                | BinaryOp::LessThanOrEqual => {
                    match (&arena[left_type].kind, &arena[right_type].kind) {
                        (
                            TypeKind::Literal(Literal::Number(left)),
                            TypeKind::Literal(Literal::Number(right)),
                        ) => {
                            let left = left.parse::<f64>().unwrap();
                            let right = right.parse::<f64>().unwrap();

                            let result = match op {
                                BinaryOp::GreaterThan => left > right,
                                BinaryOp::GreaterThanOrEqual => left >= right,
                                BinaryOp::LessThan => left < right,
                                BinaryOp::LessThanOrEqual => left <= right,
                                _ => unreachable!(),
                            };

                            new_lit_type(arena, &Literal::Boolean(result))
                        }
                        (_, _) => {
                            unify(arena, ctx, left_type, number)?;
                            unify(arena, ctx, right_type, number)?;
                            boolean
                        }
                    }
                }
                BinaryOp::And | BinaryOp::Or => {
                    unify(arena, ctx, left_type, boolean)?;
                    unify(arena, ctx, right_type, boolean)?;
                    boolean
                }
                BinaryOp::Equals | BinaryOp::NotEquals => {
                    match (&arena[left_type].kind, &arena[right_type].kind) {
                        (
                            TypeKind::Literal(Literal::Number(left)),
                            TypeKind::Literal(Literal::Number(right)),
                        ) => {
                            let result = match op {
                                BinaryOp::Equals => left == right,
                                BinaryOp::NotEquals => left != right,
                                _ => unreachable!(),
                            };

                            new_lit_type(arena, &Literal::Boolean(result))
                        }
                        (
                            TypeKind::Literal(Literal::String(left)),
                            TypeKind::Literal(Literal::String(right)),
                        ) => {
                            let result = match op {
                                BinaryOp::Equals => left == right,
                                BinaryOp::NotEquals => left != right,
                                _ => unreachable!(),
                            };

                            new_lit_type(arena, &Literal::Boolean(result))
                        }
                        (_, _) => {
                            let var_a = new_var_type(arena, None);
                            let var_b = new_var_type(arena, None);
                            unify(arena, ctx, left_type, var_a)?;
                            unify(arena, ctx, right_type, var_b)?;
                            boolean
                        }
                    }
                }
            }
        }
        ExprKind::Unary(Unary {
            op,
            right: arg, // TODO: rename `right` to `arg`
        }) => {
            let number = new_primitive(arena, Primitive::Number);
            let boolean = new_primitive(arena, Primitive::Boolean);
            let arg_type = infer_expression(arena, arg, ctx)?;

            match op {
                UnaryOp::Minus => {
                    unify(arena, ctx, arg_type, number)?;
                    number
                }
                UnaryOp::Plus => {
                    unify(arena, ctx, arg_type, number)?;
                    number
                }
                UnaryOp::Not => {
                    unify(arena, ctx, arg_type, boolean)?;
                    boolean
                }
            }
        }
        ExprKind::Await(Await { arg: expr, throws }) => {
            if !ctx.is_async {
                return Err(Errors::InferenceError(
                    "Can't use await outside of an async function".to_string(),
                ));
            }

            let expr_t = infer_expression(arena, expr, ctx)?;
            let inner_t = new_var_type(arena, None);
            let throws_t = new_var_type(arena, None);
            // TODO: Merge Constructor and TypeRef
            // NOTE: This isn't quite right because we can await non-promise values.
            // That being said, we should avoid doing so.
            let promise_t = new_constructor(arena, "Promise", &[inner_t, throws_t]);
            unify(arena, ctx, expr_t, promise_t)?;
            *throws = Some(throws_t);

            inner_t
        }
        // ExprKind::Empty => todo!(),
        ExprKind::TemplateLiteral(_) => todo!(),
        // ExprKind::TaggedTemplateLiteral(_) => todo!(),
        ExprKind::Match(Match { expr, arms }) => {
            let expr_idx = infer_expression(arena, expr, ctx)?;
            let mut body_types: Vec<Index> = vec![];

            for arm in arms.iter_mut() {
                let (pat_bindings, pat_idx) = infer_pattern(arena, &mut arm.pattern, ctx)?;

                // Checks that the pattern is a sub-type of expr
                unify(arena, ctx, pat_idx, expr_idx)?;

                let mut new_ctx = ctx.clone();
                for (name, binding) in pat_bindings {
                    // TODO: Update .env to store bindings so that we can handle
                    // mutability correctly
                    new_ctx.values.insert(name, binding);
                }

                let body_type = match arm.body {
                    BlockOrExpr::Block(ref mut block) => infer_block(arena, block, &mut new_ctx)?,
                    BlockOrExpr::Expr(ref mut expr) => infer_expression(arena, expr, &mut new_ctx)?,
                };
                body_types.push(body_type);
            }

            let t0 = prune(arena, body_types[0]);
            eprintln!("t0 = {}", arena[t0].as_string(arena));

            let t1 = prune(arena, body_types[1]);
            eprintln!("t1 = {}", arena[t1].as_string(arena));

            new_union_type(arena, &body_types)
        }
        ExprKind::Class(_) => todo!(),
        ExprKind::Do(Do { body }) => infer_block(arena, body, ctx)?,
        ExprKind::Try(Try {
            body,
            catch,
            finally: _, // Don't include this in the result
        }) => {
            let body_t = infer_block(arena, body, ctx)?;

            match catch {
                Some(catch) => {
                    let throws = find_throws_in_block(body);

                    let init_idx = new_union_type(arena, &throws);

                    if let Some(pattern) = &mut catch.param {
                        let (pat_bindings, pat_type) = infer_pattern(arena, pattern, ctx)?;

                        unify(arena, ctx, init_idx, pat_type)?;

                        for (name, binding) in pat_bindings {
                            ctx.values.insert(name.clone(), binding);
                        }

                        pattern.inferred_type = Some(init_idx);
                    }

                    let catch_t = infer_block(arena, &mut catch.body, ctx)?;
                    new_union_type(arena, &[body_t, catch_t])
                }
                None => body_t,
            }
        }
        ExprKind::Yield(_) => todo!(),
        ExprKind::Throw(Throw { arg, throws }) => {
            throws.replace(infer_expression(arena, arg, ctx)?);
            new_keyword(arena, Keyword::Never)
        }
        ExprKind::JSXFragment(_) => todo!(),
    };

    let t = &mut arena[idx];
    t.provenance = Some(Provenance::Expr(Box::new(node.to_owned())));

    node.inferred_type = Some(idx);

    Ok(idx)
}

fn is_promise(t: &Type) -> bool {
    matches!(
        t,
        Type {
            kind: TypeKind::Constructor(types::Constructor { name, .. }),
            provenance: _,
        } if name == "Promise"
    )
}

pub fn infer_block(
    arena: &mut Arena<Type>,
    block: &mut Block,
    ctx: &mut Context,
) -> Result<Index, Errors> {
    let mut new_ctx = ctx.clone();
    let mut result_t = new_keyword(arena, Keyword::Undefined);

    for stmt in &mut block.stmts.iter_mut() {
        result_t = infer_statement(arena, stmt, &mut new_ctx, false)?;
    }

    Ok(result_t)
}

pub fn infer_type_ann(
    arena: &mut Arena<Type>,
    type_ann: &mut TypeAnn,
    ctx: &mut Context,
) -> Result<Index, Errors> {
    let idx = match &mut type_ann.kind {
        TypeAnnKind::Function(FunctionType {
            params,
            ret,
            type_params,
            throws,
        }) => {
            // NOTE: We clone `ctx` so that type params don't escape the signature
            let mut sig_ctx = ctx.clone();

            let type_params = infer_type_params(arena, type_params, &mut sig_ctx)?;

            let func_params = params
                .iter_mut()
                .map(|param| {
                    let t = match &mut param.type_ann {
                        Some(type_ann) => infer_type_ann(arena, type_ann, &mut sig_ctx)?,
                        None => new_var_type(arena, None),
                    };

                    Ok(types::FuncParam {
                        pattern: pattern_to_tpat(&param.pattern, true),
                        t,
                        optional: param.optional,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let ret_idx = infer_type_ann(arena, ret.as_mut(), &mut sig_ctx)?;

            let throws = throws
                .as_mut()
                .map(|throws| infer_type_ann(arena, throws, &mut sig_ctx))
                .transpose()?;

            new_func_type(arena, &func_params, ret_idx, &type_params, throws)
        }
        TypeAnnKind::NumLit(value) => arena.insert(Type::from(TypeKind::Literal(
            syntax::Literal::Number(value.to_owned()),
        ))),
        TypeAnnKind::StrLit(value) => arena.insert(Type::from(TypeKind::Literal(
            syntax::Literal::String(value.to_owned()),
        ))),
        TypeAnnKind::BoolLit(value) => arena.insert(Type::from(TypeKind::Literal(
            syntax::Literal::Boolean(value.to_owned()),
        ))),
        // TypeAnnKind::Null => arena.insert(Type {
        //     kind: TypeKind::Literal(syntax::Literal::Null),
        // }),
        // TypeAnnKind::Undefined => arena.insert(Type {
        //     kind: TypeKind::Literal(syntax::Literal::Undefined),
        // }),
        // TypeAnnKind::Lit(lit) => new_lit_type(arena, lit),
        TypeAnnKind::Number => new_primitive(arena, Primitive::Number),
        TypeAnnKind::Boolean => new_primitive(arena, Primitive::Boolean),
        TypeAnnKind::String => new_primitive(arena, Primitive::String),
        TypeAnnKind::Symbol => new_primitive(arena, Primitive::Symbol),

        TypeAnnKind::Null => new_keyword(arena, Keyword::Null),
        TypeAnnKind::Undefined => new_keyword(arena, Keyword::Undefined),
        TypeAnnKind::Unknown => new_keyword(arena, Keyword::Unknown),
        TypeAnnKind::Never => new_keyword(arena, Keyword::Never),

        // TODO: How we make sure that create a fresh type variable for this
        // whenever it's used?  Maybe we can have an actual TypeKind::Wildcard
        // instead of creating a type variable here.
        TypeAnnKind::Wildcard(constraint) => match constraint {
            Some(constraint) => {
                let constraint = infer_type_ann(arena, constraint, ctx)?;
                new_wildcard_type(arena, Some(constraint))
            }
            None => new_wildcard_type(arena, None),
        },
        TypeAnnKind::Infer(name) => new_infer_type(arena, name),

        TypeAnnKind::Object(obj) => {
            let mut props: Vec<types::TObjElem> = Vec::new();
            for elem in obj.iter_mut() {
                match elem {
                    ObjectProp::Indexer(Indexer { key, type_ann, .. }) => {
                        let key = types::TIndexKey {
                            name: key.name.to_owned(),
                            t: infer_type_ann(arena, &mut key.type_ann, ctx)?,
                        };
                        props.push(types::TObjElem::Index(types::TIndex {
                            key,
                            t: infer_type_ann(arena, type_ann, ctx)?,
                            mutable: false,
                        }));
                    }
                    ObjectProp::Prop(prop) => {
                        let modifier = prop.modifier.as_ref().map(|modifier| match modifier {
                            PropModifier::Getter => TPropModifier::Getter,
                            PropModifier::Setter => TPropModifier::Setter,
                        });
                        props.push(types::TObjElem::Prop(types::TProp {
                            name: TPropKey::StringKey(prop.name.to_owned()),
                            modifier,
                            t: infer_type_ann(arena, &mut prop.type_ann, ctx)?,
                            mutable: prop.mutable,
                            optional: prop.optional,
                        }));
                    }
                    ObjectProp::Call(ObjCallable {
                        span: _,
                        type_params,
                        params,
                        ret,
                    }) => {
                        // TODO: dedupe with `Function` inference code above
                        // NOTE: We clone `ctx` so that type params don't escape the signature
                        let mut sig_ctx = ctx.clone();

                        let type_params = infer_type_params(arena, type_params, &mut sig_ctx)?;

                        let func_params = params
                            .iter_mut()
                            .enumerate()
                            .map(|(i, param)| {
                                let t = match &mut param.type_ann {
                                    Some(type_ann) => {
                                        infer_type_ann(arena, type_ann, &mut sig_ctx)?
                                    }
                                    None => new_var_type(arena, None),
                                };

                                Ok(types::FuncParam {
                                    pattern: TPat::Ident(BindingIdent {
                                        name: param.pattern.get_name(&i),
                                        mutable: false,
                                        span: Span { start: 0, end: 0 },
                                    }),
                                    t,
                                    optional: param.optional,
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        let ret_idx = infer_type_ann(arena, ret.as_mut(), &mut sig_ctx)?;

                        props.push(TObjElem::Call(TCallable {
                            params: func_params,
                            ret: ret_idx,
                            type_params,
                        }))
                    }
                    ObjectProp::Constructor(ObjCallable {
                        span: _,
                        type_params,
                        params,
                        ret,
                    }) => {
                        // TODO: dedupe with `Function` inference code above
                        // NOTE: We clone `ctx` so that type params don't escape the signature
                        let mut sig_ctx = ctx.clone();

                        let type_params = infer_type_params(arena, type_params, &mut sig_ctx)?;

                        let func_params = params
                            .iter_mut()
                            .enumerate()
                            .map(|(i, param)| {
                                let t = match &mut param.type_ann {
                                    Some(type_ann) => {
                                        infer_type_ann(arena, type_ann, &mut sig_ctx)?
                                    }
                                    None => new_var_type(arena, None),
                                };

                                Ok(types::FuncParam {
                                    pattern: TPat::Ident(BindingIdent {
                                        name: param.pattern.get_name(&i),
                                        mutable: false,
                                        span: Span { start: 0, end: 0 },
                                    }),
                                    t,
                                    optional: param.optional,
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        let ret_idx = infer_type_ann(arena, ret.as_mut(), &mut sig_ctx)?;

                        props.push(TObjElem::Constructor(TCallable {
                            params: func_params,
                            ret: ret_idx,
                            type_params,
                        }))
                    }
                }
            }
            new_object_type(arena, &props)
        }
        TypeAnnKind::TypeRef(name, type_args) => {
            if ctx.schemes.get(name).is_none() {
                return Err(Errors::InferenceError(format!("{} is not in scope", name)));
            }

            match type_args {
                Some(type_args) => {
                    let mut type_args_idxs = Vec::new();
                    for type_arg in type_args.iter_mut() {
                        type_args_idxs.push(infer_type_ann(arena, type_arg, ctx)?);
                    }
                    // TODO: check that the type args conform to any constraints
                    // present in the type params.
                    new_constructor(arena, name, &type_args_idxs)
                }
                None => new_constructor(arena, name, &[]),
            }
        }
        TypeAnnKind::Union(types) => {
            let mut idxs = Vec::new();
            for type_ann in types.iter_mut() {
                idxs.push(infer_type_ann(arena, type_ann, ctx)?);
            }
            new_union_type(arena, &idxs)
        }
        TypeAnnKind::Intersection(types) => {
            let mut idxs = Vec::new();
            for type_ann in types.iter_mut() {
                idxs.push(infer_type_ann(arena, type_ann, ctx)?);
            }
            new_intersection_type(arena, &idxs)
        }
        TypeAnnKind::Tuple(types) => {
            let mut idxs = Vec::new();
            for type_ann in types.iter_mut() {
                idxs.push(infer_type_ann(arena, type_ann, ctx)?);
            }
            new_tuple_type(arena, &idxs)
        }
        TypeAnnKind::Rest(rest) => {
            let idx = infer_type_ann(arena, rest, ctx)?;
            new_rest_type(arena, idx)
        }
        TypeAnnKind::Array(elem_type) => {
            let idx = infer_type_ann(arena, elem_type, ctx)?;
            new_constructor(arena, "Array", &[idx])
        }
        TypeAnnKind::IndexedAccess(obj_type, index_type) => {
            let obj_idx = infer_type_ann(arena, obj_type, ctx)?;
            let index_idx = infer_type_ann(arena, index_type, ctx)?;
            new_indexed_access_type(arena, obj_idx, index_idx)
        }
        TypeAnnKind::TypeOf(expr) => infer_expression(arena, expr, ctx)?,

        // TODO: Create types for all of these
        TypeAnnKind::KeyOf(type_ann) => {
            let t = infer_type_ann(arena, type_ann, ctx)?;
            let t = new_keyof_type(arena, t);
            expand_type(arena, ctx, t)?
        }
        // TypeAnnKind::Mapped(_) => todo!(),
        TypeAnnKind::Condition(ConditionType {
            check,
            extends,
            true_type,
            false_type,
        }) => {
            let mut cond_ctx = ctx.clone();
            let check_idx = infer_type_ann(arena, check, &mut cond_ctx)?;
            let extends_idx = infer_type_ann(arena, extends, &mut cond_ctx)?;

            // Create a copy of `ctx` so that we can add type aliases to it
            // without them leaking out of the conditional type.
            // let mut true_ctx = ctx.clone();

            let infer_types = find_infer_types(arena, &extends_idx);
            for infer in infer_types {
                let tp = new_var_type(arena, None);
                let scheme = Scheme {
                    type_params: None,
                    t: tp,
                };
                cond_ctx.schemes.insert(infer.name, scheme);
                // QUESTION: Do we need to do something with ctx.non_generic here?
                // true_ctx.non_generic.insert(tp);
            }

            let true_idx = infer_type_ann(arena, true_type, &mut cond_ctx)?;
            let false_idx = infer_type_ann(arena, false_type, &mut cond_ctx)?;
            new_conditional_type(arena, check_idx, extends_idx, true_idx, false_idx)
        }
        TypeAnnKind::Match(MatchType { matchable, cases }) => {
            let check_idx = infer_type_ann(arena, matchable, ctx)?;

            let case = cases.last_mut().unwrap();

            let extends_idx = infer_type_ann(arena, &mut case.extends, ctx)?;
            let true_idx = infer_type_ann(arena, &mut case.true_type, ctx)?;
            let false_idx = new_keyword(arena, Keyword::Never);

            let mut cond_type =
                new_conditional_type(arena, check_idx, extends_idx, true_idx, false_idx);

            for case in cases.iter_mut().rev().skip(1) {
                let extends_idx = infer_type_ann(arena, &mut case.extends, ctx)?;
                let true_idx = infer_type_ann(arena, &mut case.true_type, ctx)?;
                let false_idx = cond_type;

                cond_type =
                    new_conditional_type(arena, check_idx, extends_idx, true_idx, false_idx);
            }

            cond_type
        }
        // TODO: move this logic to `expand_type`.
        TypeAnnKind::Binary(BinaryTypeAnn { left, op, right }) => {
            let left = infer_type_ann(arena, left, ctx)?;
            let right = infer_type_ann(arena, right, ctx)?;

            let op = match op {
                BinaryOp::Plus => TBinaryOp::Add,
                BinaryOp::Minus => TBinaryOp::Sub,
                BinaryOp::Times => TBinaryOp::Mul,
                BinaryOp::Divide => TBinaryOp::Div,
                BinaryOp::Modulo => TBinaryOp::Mod,
                BinaryOp::Equals => todo!(),
                BinaryOp::NotEquals => todo!(),
                BinaryOp::LessThan => todo!(),
                BinaryOp::LessThanOrEqual => todo!(),
                BinaryOp::GreaterThan => todo!(),
                BinaryOp::GreaterThanOrEqual => todo!(),
                BinaryOp::Or => todo!(),
                BinaryOp::And => todo!(),
            };

            arena.insert(Type::from(TypeKind::Binary(BinaryT { op, left, right })))
        }
    };

    let t = &mut arena[idx];
    t.provenance = Some(Provenance::TypeAnn(Box::new(type_ann.to_owned())));

    type_ann.inferred_type = Some(idx);

    Ok(idx)
}

pub fn infer_statement(
    arena: &mut Arena<Type>,
    statement: &mut Stmt,
    ctx: &mut Context,
    top_level: bool,
) -> Result<Index, Errors> {
    let t = match &mut statement.kind {
        StmtKind::Let {
            is_declare,
            pattern,
            expr: init,
            type_ann,
            ..
        } => {
            let (pat_bindings, pat_type) = infer_pattern(arena, pattern, ctx)?;

            match (is_declare, init, type_ann) {
                (false, Some(init), type_ann) => {
                    let init_idx = infer_expression(arena, init, ctx)?;

                    let init_type = arena.get(init_idx).unwrap().clone();
                    let init_idx = match &init_type.kind {
                        TypeKind::Function(func) if top_level => generalize_func(arena, func),
                        _ => init_idx,
                    };

                    let tpat = pattern_to_tpat(pattern, false);
                    let mutability = check_mutability(ctx, &tpat, init)?;

                    let idx = match type_ann {
                        Some(type_ann) => {
                            let type_ann_idx = infer_type_ann(arena, type_ann, ctx)?;

                            // The initializer must conform to the type annotation's
                            // inferred type.
                            match mutability {
                                true => unify_mut(arena, ctx, init_idx, type_ann_idx)?,
                                false => unify(arena, ctx, init_idx, type_ann_idx)?,
                            };

                            // Results in bindings introduced by the LHS pattern
                            // having their types inferred.
                            // It's okay for pat_type to be the super type here
                            // because all initializers it introduces are type
                            // variables.  It also prevents patterns from including
                            // variables that don't exist in the type annotation.
                            unify(arena, ctx, type_ann_idx, pat_type)?;

                            type_ann_idx
                        }
                        None => {
                            // Results in bindings introduced by the LHS pattern
                            // having their types inferred.
                            // It's okay for pat_type to be the super type here
                            // because all initializers it introduces are type
                            // variables.  It also prevents patterns from including
                            // variables that don't exist in the initializer.
                            unify(arena, ctx, init_idx, pat_type)?;

                            init_idx
                        }
                    };

                    for (name, binding) in pat_bindings {
                        ctx.values.insert(name.clone(), binding);
                    }

                    pattern.inferred_type = Some(idx);

                    idx // TODO: Should this be unit?
                }
                (false, None, _) => {
                    return Err(Errors::InferenceError(
                        "Variable declarations not using `declare` must have an initializer"
                            .to_string(),
                    ))
                }
                (true, None, Some(type_ann)) => {
                    let idx = infer_type_ann(arena, type_ann, ctx)?;

                    unify(arena, ctx, idx, pat_type)?;

                    for (name, binding) in pat_bindings {
                        ctx.values.insert(name.clone(), binding);
                    }

                    idx
                }
                (true, Some(_), _) => {
                    return Err(Errors::InferenceError(
                        "Variable declarations using `declare` cannot have an initializer"
                            .to_string(),
                    ))
                }
                (true, None, None) => {
                    return Err(Errors::InferenceError(
                        "Variable declarations using `declare` must have a type annotation"
                            .to_string(),
                    ))
                }
            }
        }
        StmtKind::Expr { expr } => infer_expression(arena, expr, ctx)?,
        StmtKind::Return { arg: expr } => {
            // TODO: handle multiple return statements
            // TODO: warn about unreachable code after a return statement
            match expr {
                Some(expr) => infer_expression(arena, expr, ctx)?,
                None => {
                    // TODO: return `undefined` or `void`.
                    todo!()
                }
            }
        }
        // StmtKind::ClassDecl(_) => todo!(),
        StmtKind::TypeDecl {
            name,
            type_ann,
            type_params,
        } => {
            // NOTE: We clone `ctx` so that type params don't escape the signature
            let mut sig_ctx = ctx.clone();

            let type_params = infer_type_params(arena, type_params, &mut sig_ctx)?;
            let t = infer_type_ann(arena, type_ann, &mut sig_ctx)?;

            // TODO: generalize type `t` into a scheme
            let scheme = Scheme { t, type_params };

            ctx.schemes.insert(name.to_owned(), scheme);

            t
        } // StmtKind::ForStmt(_) => todo!(),
    };

    statement.inferred_type = Some(t);

    Ok(t)
}

// TODO: introduce `infer_script` which has the same semantics as those used
// to infer the body of a function.
// TODO: rename to `infer_module`
pub fn infer_program(
    arena: &mut Arena<Type>,
    node: &mut Program,
    ctx: &mut Context,
) -> Result<(), Errors> {
    for stmt in &node.stmts {
        if let StmtKind::TypeDecl { name, .. } = &stmt.kind {
            let placeholder_scheme = Scheme {
                t: new_keyword(arena, Keyword::Unknown),
                type_params: None,
            };
            let name = name.to_owned();
            if ctx
                .schemes
                .insert(name.clone(), placeholder_scheme)
                .is_some()
            {
                return Err(Errors::InferenceError(format!(
                    "{name} cannot be redeclared at the top-level"
                )));
            }
        }
    }

    for stmt in &mut node.stmts {
        if let StmtKind::Let { pattern, .. } = &mut stmt.kind {
            let (bindings, _) = infer_pattern(arena, pattern, ctx)?;

            for (name, binding) in bindings {
                ctx.non_generic.insert(binding.index);
                if ctx.values.insert(name.to_owned(), binding).is_some() {
                    return Err(Errors::InferenceError(format!(
                        "{name} cannot be redeclared at the top-level"
                    )));
                }
            }
        }
    }

    // TODO: capture all type decls and do a second pass to valid them

    // TODO: figure out how to avoid parsing patterns twice
    for stmt in &mut node.stmts.iter_mut() {
        infer_statement(arena, stmt, ctx, true)?;
    }

    Ok(())
}

struct Generalize<'a> {
    arena: &'a mut Arena<Type>,
    mapping: &'a mut BTreeMap<Index, String>,
}

impl<'a> KeyValueStore<Index, Type> for Generalize<'a> {
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

impl<'a> Visitor for Generalize<'a> {
    fn visit_type_var(&mut self, _: &Variable, idx: &Index) -> Index {
        // Replace with a type reference/constructor
        let name = match self.mapping.get(idx) {
            Some(name) => name.clone(),
            None => {
                // TODO: create a name generator that can avoid duplicating
                // names of explicitly provided type params.
                let name = ((self.mapping.len() as u8) + 65) as char;
                let name = format!("{}", name);
                // let name = format!("'{}", mappings.len());
                self.mapping.insert(*idx, name.clone());
                name
            }
        };
        new_constructor(self.arena, &name, &[])
    }
}

pub fn generalize_func(arena: &'_ mut Arena<Type>, func: &types::Function) -> Index {
    // A mapping of TypeVariables to TypeVariables
    let mut mapping = BTreeMap::default();
    let mut generalize = Generalize {
        arena,
        mapping: &mut mapping,
    };

    let params = func
        .params
        .iter()
        .map(|param| types::FuncParam {
            t: generalize.visit_index(&param.t),
            ..param.to_owned()
        })
        .collect::<Vec<_>>();
    let ret = generalize.visit_index(&func.ret);
    let throws = func.throws.map(|throws| generalize.visit_index(&throws));

    let mut type_params: Vec<types::TypeParam> = vec![];

    if let Some(explicit_type_params) = &func.type_params {
        type_params.extend(explicit_type_params.to_owned());
    }

    for (_, name) in mapping {
        type_params.push(types::TypeParam {
            name: name.clone(),
            constraint: None,
            default: None,
        });
    }

    if type_params.is_empty() {
        new_func_type(arena, &params, ret, &None, throws)
    } else {
        new_func_type(arena, &params, ret, &Some(type_params), throws)
    }
}

fn get_ident_member(
    arena: &mut Arena<Type>,
    ctx: &mut Context,
    obj_idx: Index,
    key_idx: Index,
) -> Result<Index, Errors> {
    // NOTE: cloning is fine here because we aren't mutating `obj_type`
    let obj_type = arena[obj_idx].clone();

    match &obj_type.kind {
        TypeKind::Object(_) => get_prop(arena, ctx, obj_idx, key_idx),
        // declare let obj: {x: number} | {x: string}
        // obj.x; // number | string
        TypeKind::Union(union) => {
            let mut result_types = vec![];
            let mut undefined_count = 0;
            for idx in &union.types {
                match get_prop(arena, ctx, *idx, key_idx) {
                    Ok(t) => result_types.push(t),
                    Err(_) => {
                        // TODO: check what the error is, we may want to propagate
                        // certain errors
                        if undefined_count == 0 {
                            let undefined = new_keyword(arena, Keyword::Undefined);
                            result_types.push(undefined);
                        }
                        undefined_count += 1;
                    }
                }
            }
            if undefined_count == union.types.len() {
                Err(Errors::InferenceError(format!(
                    "Couldn't find property {} on object",
                    arena[key_idx].as_string(arena)
                )))
            } else {
                Ok(new_union_type(arena, &result_types))
            }
        }
        TypeKind::Constructor(types::Constructor {
            name: alias_name,
            types,
            ..
        }) => match ctx.schemes.get(alias_name) {
            Some(scheme) => {
                let obj_idx = expand_alias(arena, ctx, alias_name, scheme, types)?;
                get_ident_member(arena, ctx, obj_idx, key_idx)
            }
            None => Err(Errors::InferenceError(format!(
                "Can't find type alias for {alias_name}"
            ))),
        },
        TypeKind::Tuple(types::Tuple { types }) => match ctx.schemes.get("Array") {
            Some(scheme) => {
                let t = new_union_type(arena, types);
                let obj_idx = expand_alias(arena, ctx, "Array", scheme, &[t])?;
                get_ident_member(arena, ctx, obj_idx, key_idx)
            }
            None => Err(Errors::InferenceError(
                "Can't find type alias for Array".to_string(),
            )),
        },
        _ => Err(Errors::InferenceError(format!(
            "Can't access properties on {}",
            obj_type.as_string(arena)
        ))),
    }
}

fn infer_type_params(
    arena: &mut Arena<Type>,
    type_params: &mut Option<Vec<syntax::TypeParam>>,
    sig_ctx: &mut Context,
) -> Result<Option<Vec<types::TypeParam>>, Errors> {
    if let Some(type_params) = type_params {
        for tp in type_params.iter_mut() {
            let constraint = match &mut tp.bound {
                Some(constraint) => Some(infer_type_ann(arena, constraint, sig_ctx)?),
                None => None,
            };

            // Adds the param to the context and set its type to the constraint
            // or `unknown` if there is no constraint.
            let scheme = Scheme {
                t: match constraint {
                    Some(constraint) => constraint,
                    None => new_keyword(arena, Keyword::Unknown),
                },
                type_params: None,
            };
            sig_ctx.schemes.insert(tp.name.to_owned(), scheme);
        }
    }

    // TODO: move this up and do this at the same time we do the other
    // processing of `type_params`.
    let mut type_param_names: HashSet<String> = HashSet::new();
    let type_params = match type_params {
        Some(type_params) => Some(
            type_params
                .iter_mut()
                .map(|tp| {
                    if !type_param_names.insert(tp.name.to_owned()) {
                        return Err(Errors::InferenceError(
                            "type param identifiers must be unique".to_string(),
                        ));
                    }
                    Ok(types::TypeParam {
                        name: tp.name.to_owned(),
                        constraint: match &mut tp.bound {
                            Some(constraint) => Some(infer_type_ann(arena, constraint, sig_ctx)?),
                            None => None,
                        },
                        default: match &mut tp.default {
                            Some(default) => Some(infer_type_ann(arena, default, sig_ctx)?),
                            None => None,
                        },
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        ),
        None => None,
    };

    Ok(type_params)
}

// NOTE: It's possible to have a mix of mutable and immutable bindings be
// introduced.  In that situation, we only need to check certain parts of
// the initializer for mutability.
pub fn check_mutability(ctx: &Context, tpat: &TPat, init: &Expr) -> Result<bool, Errors> {
    let mut lhs_mutable = false;

    // TODO: handle other patterns
    if let TPat::Ident(binding) = tpat {
        lhs_mutable = lhs_mutable || binding.mutable;
    }

    let idents = find_identifiers(init)?;

    if idents.is_empty() {
        return Ok(false);
    }

    let mut rhs_mutable = false;
    for Ident { name, span: _ } in idents {
        let binding = ctx.values.get(&name).unwrap();
        rhs_mutable = rhs_mutable || binding.is_mut;
    }

    if lhs_mutable && !rhs_mutable {
        // TODO: include which bindings are involved in the assignment
        return Err(Errors::InferenceError(
            "Can't assign immutable value to mutable binding".to_string(),
        ));
    }

    Ok(lhs_mutable && rhs_mutable)
}

// TODO: find the rest of the identifiers in the expression
fn find_identifiers(expr: &Expr) -> Result<Vec<Ident>, Errors> {
    let mut idents = vec![];

    if let ExprKind::Ident(ident) = &expr.kind {
        idents.push(ident.to_owned());
    }

    Ok(idents)
}

fn lvalue_mutability(ctx: &Context, expr: &Expr) -> Result<bool, Errors> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            let binding = ctx.values.get(&ident.name).unwrap();
            Ok(binding.is_mut)
        }
        ExprKind::Member(member) => lvalue_mutability(ctx, &member.object),
        _ => Err(Errors::InferenceError(
            "Can't assign to non-lvalue".to_string(),
        )),
    }
}
