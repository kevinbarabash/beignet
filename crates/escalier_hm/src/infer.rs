use generational_arena::Index;
use itertools::Itertools;
use std::collections::{BTreeMap, HashSet};

use escalier_ast::{self as syntax, *};

use crate::ast_utils::find_returns;
use crate::ast_utils::{find_throws, find_throws_in_block};
use crate::checker::Checker;
use crate::context::*;
use crate::folder::{self, Folder};
use crate::infer_pattern::*;
use crate::key_value_store::KeyValueStore;
use crate::provenance::Provenance;
use crate::type_error::TypeError;
use crate::types::{self, *};
use crate::util::*;

impl Checker {
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
        &mut self,
        node: &mut Expr,
        ctx: &mut Context,
    ) -> Result<Index, TypeError> {
        self.push_report();
        let idx: Index = match &mut node.kind {
            ExprKind::Ident(Ident { name, .. }) => self.get_type(name, ctx)?,
            ExprKind::Str(str) => {
                self.arena
                    .insert(Type::from(TypeKind::Literal(syntax::Literal::String(
                        str.value.to_owned(),
                    ))))
            }
            ExprKind::Num(num) => {
                self.arena
                    .insert(Type::from(TypeKind::Literal(syntax::Literal::Number(
                        num.value.to_owned(),
                    ))))
            }
            ExprKind::Bool(bool) => {
                self.arena
                    .insert(Type::from(TypeKind::Literal(syntax::Literal::Boolean(
                        bool.value,
                    ))))
            }
            ExprKind::Null(_) => self
                .arena
                .insert(Type::from(TypeKind::Literal(syntax::Literal::Null))),
            ExprKind::Undefined(_) => self
                .arena
                .insert(Type::from(TypeKind::Literal(syntax::Literal::Undefined))),
            ExprKind::Tuple(syntax::Tuple {
                elements: elems, ..
            }) => {
                let mut element_types = vec![];
                for element in elems.iter_mut() {
                    let t = match element {
                        ExprOrSpread::Expr(expr) => self.infer_expression(expr, ctx)?,
                        ExprOrSpread::Spread(_) => todo!(), // TODO: handle spreads
                    };
                    element_types.push(t);
                }
                self.new_tuple_type(&element_types)
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
                                    t: self.get_type(ident, ctx)?,
                                    mutable: false,
                                    optional: false,
                                }));
                            }
                            expr::Prop::Property { key, value } => {
                                let prop = match key {
                                    ObjectKey::Ident(ident) => types::TProp {
                                        name: TPropKey::StringKey(ident.name.to_owned()),
                                        modifier: None,
                                        t: self.infer_expression(value, ctx)?,
                                        mutable: false,
                                        optional: false,
                                    },
                                    ObjectKey::String(name) => types::TProp {
                                        name: TPropKey::StringKey(name.to_owned()),
                                        modifier: None,
                                        t: self.infer_expression(value, ctx)?,
                                        mutable: false,
                                        optional: false,
                                    },
                                    ObjectKey::Number(name) => types::TProp {
                                        name: TPropKey::StringKey(name.to_owned()),
                                        modifier: None,
                                        t: self.infer_expression(value, ctx)?,
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
                self.new_object_type(&prop_types)
            }
            ExprKind::Call(syntax::Call {
                callee: func,
                args,
                type_args,
                opt_chain,
                throws,
            }) => {
                let mut func_idx = self.infer_expression(func, ctx)?;
                let mut has_undefined = false;
                if *opt_chain {
                    if let TypeKind::Union(union) = &self.arena[func_idx].kind {
                        let types = filter_nullables(&self.arena, &union.types);
                        has_undefined = types.len() != union.types.len();
                        func_idx = self.new_union_type(&types);
                    }
                }

                let (result, new_throws) = match type_args {
                    Some(type_args) => {
                        let type_args = type_args
                            .iter_mut()
                            .map(|type_arg| self.infer_type_ann(type_arg, ctx))
                            .collect::<Result<Vec<_>, _>>()?;

                        self.unify_call(ctx, args, Some(&type_args), func_idx)?
                    }
                    None => self.unify_call(ctx, args, None, func_idx)?,
                };

                if let Some(new_throws) = new_throws {
                    throws.replace(new_throws);
                }

                match *opt_chain && has_undefined {
                    true => {
                        let undefined = self.new_keyword(Keyword::Undefined);

                        if let TypeKind::Union(union) = &self.arena[result].kind.clone() {
                            let mut types = filter_nullables(&self.arena, &union.types);

                            if types.len() != union.types.len() {
                                // If we didn't end up removing any `undefined`s then
                                // itmeans that `result` already contains `undefined`
                                // and we can return it as is.
                                result
                            } else {
                                types.push(undefined);
                                self.new_union_type(&types)
                            }
                        } else {
                            self.new_union_type(&[result, undefined])
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

                let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;

                for syntax::FuncParam {
                    pattern,
                    type_ann,
                    optional,
                } in params.iter_mut()
                {
                    let type_ann_t = match type_ann {
                        Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                        None => self.new_var_type(None),
                    };
                    pattern.inferred_type = Some(type_ann_t);

                    let (assumps, param_t) = self.infer_pattern(pattern, &sig_ctx)?;
                    self.unify(&sig_ctx, param_t, type_ann_t)?;

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
                                self.infer_statement(stmt, &mut body_ctx, false)?;
                                if let StmtKind::Return { arg: _ } = stmt.kind {
                                    let ret_types: Vec<Index> = find_returns(body)
                                        .iter()
                                        .filter_map(|ret| ret.inferred_type)
                                        .collect();

                                    // TODO: warn about unreachable code.
                                    break 'outer self.new_union_type(&ret_types);
                                }
                            }

                            // If we don't encounter a return statement, we assume
                            // the return type is `undefined`.
                            self.new_keyword(Keyword::Undefined)
                        }
                        BlockOrExpr::Expr(expr) => {
                            // TODO: use `find_returns` here as well
                            self.infer_expression(expr, &mut body_ctx)?
                        }
                    }
                };

                // TODO: search for `throw` expressions in the body and include
                // them in the throws type.

                let body_throws = find_throws(body);
                let body_throws = if body_throws.is_empty() {
                    None
                } else {
                    Some(self.new_union_type(
                        // TODO: compare string reps of the types for deduplication
                        &body_throws.into_iter().unique().collect_vec(),
                    ))
                };

                let sig_throws = sig_throws
                    .as_mut()
                    .map(|t| self.infer_type_ann(t, &mut sig_ctx))
                    .transpose()?;

                let throws = match (body_throws, sig_throws) {
                    (Some(call_throws), Some(sig_throws)) => {
                        self.unify(&sig_ctx, call_throws, sig_throws)?;
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
                if *is_async && !is_promise(&self.arena[body_t]) {
                    let never = self.new_keyword(Keyword::Never);
                    let throws_t = throws.unwrap_or(never);
                    body_t = self.new_constructor("Promise", &[body_t, throws_t]);

                    match return_type {
                        Some(return_type) => {
                            let ret_t = self.infer_type_ann(return_type, &mut sig_ctx)?;
                            // TODO: add sig_ctx which is a copy of ctx but with all of
                            // the type params added to sig_ctx.schemes so that they can
                            // be looked up.
                            self.unify(&sig_ctx, body_t, ret_t)?;
                            self.new_func_type(&func_params, ret_t, &type_params, None)
                        }
                        None => self.new_func_type(&func_params, body_t, &type_params, None),
                    }
                } else {
                    match return_type {
                        Some(return_type) => {
                            let ret_t = self.infer_type_ann(return_type, &mut sig_ctx)?;
                            // TODO: add sig_ctx which is a copy of ctx but with all of
                            // the type params added to sig_ctx.schemes so that they can
                            // be looked up.
                            self.unify(&sig_ctx, body_t, ret_t)?;
                            self.new_func_type(&func_params, ret_t, &type_params, throws)
                        }
                        None => self.new_func_type(&func_params, body_t, &type_params, throws),
                    }
                }
            }
            ExprKind::IfElse(IfElse {
                cond,
                consequent,
                alternate,
            }) => {
                let cond_type = self.infer_expression(cond, ctx)?;
                let bool_type = self.new_primitive(Primitive::Boolean);
                self.unify(ctx, cond_type, bool_type)?;
                let consequent_type = self.infer_block(consequent, ctx)?;
                let alternate_type = match alternate {
                    Some(alternate) => match alternate {
                        BlockOrExpr::Block(block) => self.infer_block(block, ctx)?,
                        BlockOrExpr::Expr(expr) => self.infer_expression(expr, ctx)?,
                    },
                    None => self.new_keyword(Keyword::Undefined),
                };
                self.new_union_type(&[consequent_type, alternate_type])
            }
            ExprKind::Member(Member {
                object: obj,
                property: prop,
                opt_chain,
            }) => {
                let mut obj_idx = self.infer_expression(obj, ctx)?;
                let mut has_undefined = false;
                if *opt_chain {
                    if let TypeKind::Union(union) = &self.arena[obj_idx].kind {
                        let types = filter_nullables(&self.arena, &union.types);
                        has_undefined = types.len() != union.types.len();
                        obj_idx = self.new_union_type(&types);
                    }
                }

                let result = match prop {
                    MemberProp::Ident(Ident { name, .. }) => {
                        let key_idx = self.new_lit_type(&Literal::String(name.to_owned()));
                        self.get_ident_member(ctx, obj_idx, key_idx)?
                    }
                    MemberProp::Computed(ComputedPropName { expr, .. }) => {
                        let prop_type = self.infer_expression(expr, ctx)?;
                        self.get_computed_member(ctx, obj_idx, prop_type)?
                    }
                };

                match *opt_chain && has_undefined {
                    true => {
                        let undefined = self.new_keyword(Keyword::Undefined);

                        if let TypeKind::Union(union) = &self.arena[result].kind {
                            let mut types = filter_nullables(&self.arena, &union.types);

                            if types.len() != union.types.len() {
                                // If we didn't end up removing any `undefined`s then
                                // itmeans that `result` already contains `undefined`
                                // and we can return it as is.
                                result
                            } else {
                                types.push(undefined);
                                self.new_union_type(&types)
                            }
                        } else {
                            self.new_union_type(&[result, undefined])
                        }
                    }
                    false => result,
                }
            }
            ExprKind::JSXElement(_) => todo!(),
            ExprKind::Assign(Assign { left, op: _, right }) => {
                if !lvalue_mutability(ctx, left)? {
                    self.pop_report();
                    return Err(TypeError {
                        message: "Cannot assign to immutable lvalue".to_string(),
                    });
                }

                let l_t = self.infer_expression(left, ctx)?;
                let r_t = self.infer_expression(right, ctx)?;
                self.unify(ctx, r_t, l_t)?;

                r_t
            }
            ExprKind::Binary(Binary { op, left, right }) => {
                let number = self.new_primitive(Primitive::Number);
                let boolean = self.new_primitive(Primitive::Boolean);
                let left_type = self.infer_expression(left, ctx)?;
                let right_type = self.infer_expression(right, ctx)?;

                match op {
                    BinaryOp::Plus
                    | BinaryOp::Minus
                    | BinaryOp::Times
                    | BinaryOp::Divide
                    | BinaryOp::Modulo => {
                        match (&self.arena[left_type].kind, &self.arena[right_type].kind) {
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

                                self.new_lit_type(&Literal::Number(result.to_string()))
                            }
                            (_, _) => {
                                self.unify(ctx, left_type, number)?;
                                self.unify(ctx, right_type, number)?;
                                number
                            }
                        }
                    }
                    BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual => {
                        match (&self.arena[left_type].kind, &self.arena[right_type].kind) {
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

                                self.new_lit_type(&Literal::Boolean(result))
                            }
                            (_, _) => {
                                self.unify(ctx, left_type, number)?;
                                self.unify(ctx, right_type, number)?;
                                boolean
                            }
                        }
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        self.unify(ctx, left_type, boolean)?;
                        self.unify(ctx, right_type, boolean)?;
                        boolean
                    }
                    BinaryOp::Equals | BinaryOp::NotEquals => {
                        match (&self.arena[left_type].kind, &self.arena[right_type].kind) {
                            (
                                TypeKind::Literal(Literal::Number(left)),
                                TypeKind::Literal(Literal::Number(right)),
                            ) => {
                                let result = match op {
                                    BinaryOp::Equals => left == right,
                                    BinaryOp::NotEquals => left != right,
                                    _ => unreachable!(),
                                };

                                self.new_lit_type(&Literal::Boolean(result))
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

                                self.new_lit_type(&Literal::Boolean(result))
                            }
                            (_, _) => {
                                let var_a = self.new_var_type(None);
                                let var_b = self.new_var_type(None);
                                self.unify(ctx, left_type, var_a)?;
                                self.unify(ctx, right_type, var_b)?;
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
                let number = self.new_primitive(Primitive::Number);
                let boolean = self.new_primitive(Primitive::Boolean);
                let arg_type = self.infer_expression(arg, ctx)?;

                match op {
                    UnaryOp::Minus => {
                        self.unify(ctx, arg_type, number)?;
                        number
                    }
                    UnaryOp::Plus => {
                        self.unify(ctx, arg_type, number)?;
                        number
                    }
                    UnaryOp::Not => {
                        self.unify(ctx, arg_type, boolean)?;
                        boolean
                    }
                }
            }
            ExprKind::Await(Await { arg: expr, throws }) => {
                if !ctx.is_async {
                    self.pop_report();
                    return Err(TypeError {
                        message: "Can't use await outside of an async function".to_string(),
                    });
                }

                let expr_t = self.infer_expression(expr, ctx)?;
                let inner_t = self.new_var_type(None);
                let throws_t = self.new_var_type(None);
                // TODO: Merge Constructor and TypeRef
                // NOTE: This isn't quite right because we can await non-promise values.
                // That being said, we should avoid doing so.
                let promise_t = self.new_constructor("Promise", &[inner_t, throws_t]);
                self.unify(ctx, expr_t, promise_t)?;
                *throws = Some(throws_t);

                inner_t
            }
            // ExprKind::Empty => todo!(),
            ExprKind::TemplateLiteral(_) => todo!(),
            // ExprKind::TaggedTemplateLiteral(_) => todo!(),
            ExprKind::Match(Match { expr, arms }) => {
                let expr_idx = self.infer_expression(expr, ctx)?;
                let mut body_types: Vec<Index> = vec![];

                for arm in arms.iter_mut() {
                    let (pat_bindings, pat_idx) = self.infer_pattern(&mut arm.pattern, ctx)?;

                    // Checks that the pattern is a sub-type of expr
                    self.unify(ctx, pat_idx, expr_idx)?;

                    let mut new_ctx = ctx.clone();
                    for (name, binding) in pat_bindings {
                        // TODO: Update .env to store bindings so that we can handle
                        // mutability correctly
                        new_ctx.values.insert(name, binding);
                    }

                    let body_type = match arm.body {
                        BlockOrExpr::Block(ref mut block) => {
                            self.infer_block(block, &mut new_ctx)?
                        }
                        BlockOrExpr::Expr(ref mut expr) => {
                            self.infer_expression(expr, &mut new_ctx)?
                        }
                    };
                    body_types.push(body_type);
                }

                let t0 = self.prune(body_types[0]);
                eprintln!("t0 = {}", self.print_type(&t0));

                let t1 = self.prune(body_types[1]);
                eprintln!("t1 = {}", self.print_type(&t1));

                self.new_union_type(&body_types)
            }
            ExprKind::Class(_) => todo!(),
            ExprKind::Do(Do { body }) => self.infer_block(body, ctx)?,
            ExprKind::Try(Try {
                body,
                catch,
                finally: _, // Don't include this in the result
            }) => {
                let body_t = self.infer_block(body, ctx)?;

                match catch {
                    Some(catch) => {
                        let throws = find_throws_in_block(body);

                        let init_idx = self.new_union_type(&throws);

                        if let Some(pattern) = &mut catch.param {
                            let (pat_bindings, pat_type) = self.infer_pattern(pattern, ctx)?;

                            self.unify(ctx, init_idx, pat_type)?;

                            for (name, binding) in pat_bindings {
                                ctx.values.insert(name.clone(), binding);
                            }

                            pattern.inferred_type = Some(init_idx);
                        }

                        let catch_t = self.infer_block(&mut catch.body, ctx)?;
                        self.new_union_type(&[body_t, catch_t])
                    }
                    None => body_t,
                }
            }
            ExprKind::Yield(_) => todo!(),
            ExprKind::Throw(Throw { arg, throws }) => {
                throws.replace(self.infer_expression(arg, ctx)?);
                self.new_keyword(Keyword::Never)
            }
            ExprKind::JSXFragment(_) => todo!(),
        };

        let t = &mut self.arena[idx];
        t.provenance = Some(Provenance::Expr(Box::new(node.to_owned())));

        node.inferred_type = Some(idx);

        self.pop_report();

        Ok(idx)
    }

    pub fn infer_block(
        &mut self,
        block: &mut Block,
        ctx: &mut Context,
    ) -> Result<Index, TypeError> {
        let mut new_ctx = ctx.clone();
        let mut result_t = self.new_keyword(Keyword::Undefined);

        for stmt in &mut block.stmts.iter_mut() {
            result_t = self.infer_statement(stmt, &mut new_ctx, false)?;
        }

        Ok(result_t)
    }

    pub fn infer_type_ann(
        &mut self,
        type_ann: &mut TypeAnn,
        ctx: &mut Context,
    ) -> Result<Index, TypeError> {
        let idx = match &mut type_ann.kind {
            TypeAnnKind::Function(FunctionType {
                params,
                ret,
                type_params,
                throws,
            }) => {
                // NOTE: We clone `ctx` so that type params don't escape the signature
                let mut sig_ctx = ctx.clone();

                let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;

                let func_params = params
                    .iter_mut()
                    .map(|param| {
                        let t = match &mut param.type_ann {
                            Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                            None => self.new_var_type(None),
                        };

                        Ok(types::FuncParam {
                            pattern: pattern_to_tpat(&param.pattern, true),
                            t,
                            optional: param.optional,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let ret_idx = self.infer_type_ann(ret.as_mut(), &mut sig_ctx)?;

                let throws = throws
                    .as_mut()
                    .map(|throws| self.infer_type_ann(throws, &mut sig_ctx))
                    .transpose()?;

                self.new_func_type(&func_params, ret_idx, &type_params, throws)
            }

            TypeAnnKind::NumLit(value) => {
                self.arena
                    .insert(Type::from(TypeKind::Literal(syntax::Literal::Number(
                        value.to_owned(),
                    ))))
            }
            TypeAnnKind::StrLit(value) => {
                self.arena
                    .insert(Type::from(TypeKind::Literal(syntax::Literal::String(
                        value.to_owned(),
                    ))))
            }
            TypeAnnKind::BoolLit(value) => {
                self.arena
                    .insert(Type::from(TypeKind::Literal(syntax::Literal::Boolean(
                        value.to_owned(),
                    ))))
            }

            TypeAnnKind::Number => self.new_primitive(Primitive::Number),
            TypeAnnKind::Boolean => self.new_primitive(Primitive::Boolean),
            TypeAnnKind::String => self.new_primitive(Primitive::String),
            TypeAnnKind::Symbol => self.new_primitive(Primitive::Symbol),

            TypeAnnKind::Null => self.new_keyword(Keyword::Null),
            TypeAnnKind::Undefined => self.new_keyword(Keyword::Undefined),
            TypeAnnKind::Unknown => self.new_keyword(Keyword::Unknown),
            TypeAnnKind::Never => self.new_keyword(Keyword::Never),

            // TODO: How we make sure that create a fresh type variable for this
            // whenever it's used?  Maybe we can have an actual TypeKind::Wildcard
            // instead of creating a type variable here.
            TypeAnnKind::Wildcard => self.new_wildcard_type(),
            TypeAnnKind::Infer(name) => self.new_infer_type(name),

            TypeAnnKind::Object(obj) => {
                let mut props: Vec<types::TObjElem> = Vec::new();
                for elem in obj.iter_mut() {
                    match elem {
                        ObjectProp::Mapped(Mapped {
                            key,
                            value,
                            target,
                            source,
                            check,
                            extends,
                        }) => {
                            let mut type_ctx = ctx.clone();

                            let source = self.infer_type_ann(source, &mut type_ctx)?;
                            let scheme = Scheme {
                                type_params: None,
                                t: source,
                            };
                            type_ctx.schemes.insert(target.to_owned(), scheme);

                            let key = self.infer_type_ann(key, &mut type_ctx)?;
                            let value = self.infer_type_ann(value, &mut type_ctx)?;

                            let check = match check {
                                Some(check) => Some(self.infer_type_ann(check, &mut type_ctx)?),
                                None => None,
                            };
                            let extends = match extends {
                                Some(extends) => Some(self.infer_type_ann(extends, &mut type_ctx)?),
                                None => None,
                            };

                            props.push(types::TObjElem::Mapped(types::MappedType {
                                key,
                                value,
                                target: target.to_owned(),
                                source,
                                check,
                                extends,
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
                                t: self.infer_type_ann(&mut prop.type_ann, ctx)?,
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

                            let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;

                            let func_params = params
                                .iter_mut()
                                .enumerate()
                                .map(|(i, param)| {
                                    let t = match &mut param.type_ann {
                                        Some(type_ann) => {
                                            self.infer_type_ann(type_ann, &mut sig_ctx)?
                                        }
                                        None => self.new_var_type(None),
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

                            let ret_idx = self.infer_type_ann(ret.as_mut(), &mut sig_ctx)?;

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

                            let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;

                            let func_params = params
                                .iter_mut()
                                .enumerate()
                                .map(|(i, param)| {
                                    let t = match &mut param.type_ann {
                                        Some(type_ann) => {
                                            self.infer_type_ann(type_ann, &mut sig_ctx)?
                                        }
                                        None => self.new_var_type(None),
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

                            let ret_idx = self.infer_type_ann(ret.as_mut(), &mut sig_ctx)?;

                            props.push(TObjElem::Constructor(TCallable {
                                params: func_params,
                                ret: ret_idx,
                                type_params,
                            }))
                        }
                    }
                }
                self.new_object_type(&props)
            }
            TypeAnnKind::TypeRef(name, type_args) => {
                let type_args = match type_args {
                    Some(type_args) => {
                        let mut type_args_idxs = Vec::new();
                        for type_arg in type_args.iter_mut() {
                            type_args_idxs.push(self.infer_type_ann(type_arg, ctx)?);
                        }
                        type_args_idxs
                    }
                    None => vec![],
                };

                let Scheme { type_params, .. } = ctx.get_scheme(name)?;

                let type_params = match type_params {
                    Some(type_params) => type_params,
                    None => vec![],
                };

                if type_params.len() != type_args.len() {
                    return Err(TypeError {
                        message: format!(
                            "{name} expects {} type args, but was passed {}",
                            type_params.len(),
                            type_args.len()
                        ),
                    });
                }

                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    if let Some(constraint) = param.constraint {
                        self.unify(ctx, *arg, constraint)?;
                    }
                }

                self.new_constructor(name, &type_args)
            }
            TypeAnnKind::Union(types) => {
                let mut idxs = Vec::new();
                for type_ann in types.iter_mut() {
                    idxs.push(self.infer_type_ann(type_ann, ctx)?);
                }
                self.new_union_type(&idxs)
            }
            TypeAnnKind::Intersection(types) => {
                let mut idxs = Vec::new();
                for type_ann in types.iter_mut() {
                    idxs.push(self.infer_type_ann(type_ann, ctx)?);
                }
                self.new_intersection_type(&idxs)
            }
            TypeAnnKind::Tuple(types) => {
                let mut idxs = Vec::new();
                for type_ann in types.iter_mut() {
                    idxs.push(self.infer_type_ann(type_ann, ctx)?);
                }
                self.new_tuple_type(&idxs)
            }
            TypeAnnKind::Rest(rest) => {
                let idx = self.infer_type_ann(rest, ctx)?;
                self.new_rest_type(idx)
            }
            TypeAnnKind::Array(elem_type) => {
                let idx = self.infer_type_ann(elem_type, ctx)?;
                self.new_constructor("Array", &[idx])
            }
            TypeAnnKind::IndexedAccess(obj_type, index_type) => {
                let obj_idx = self.infer_type_ann(obj_type, ctx)?;
                let index_idx = self.infer_type_ann(index_type, ctx)?;
                self.new_indexed_access_type(obj_idx, index_idx)
            }
            TypeAnnKind::TypeOf(expr) => self.infer_expression(expr, ctx)?,

            // TODO: Create types for all of these
            TypeAnnKind::KeyOf(type_ann) => {
                let t = self.infer_type_ann(type_ann, ctx)?;
                self.new_keyof_type(t)
                // expand_type(self.arena, ctx, t)?
            }
            // TypeAnnKind::Mapped(_) => todo!(),
            TypeAnnKind::Condition(ConditionType {
                check,
                extends,
                true_type,
                false_type,
            }) => {
                let mut cond_ctx = ctx.clone();
                let check_idx = self.infer_type_ann(check, &mut cond_ctx)?;
                let extends_idx = self.infer_type_ann(extends, &mut cond_ctx)?;

                // Create a copy of `ctx` so that we can add type aliases to it
                // without them leaking out of the conditional type.
                // let mut true_ctx = ctx.clone();

                let infer_types = find_infer_types(&mut self.arena, &extends_idx);
                for infer in infer_types {
                    let tp = self.new_var_type(None);
                    let scheme = Scheme {
                        type_params: None,
                        t: tp,
                    };
                    cond_ctx.schemes.insert(infer.name, scheme);
                    // QUESTION: Do we need to do something with ctx.non_generic here?
                    // true_ctx.non_generic.insert(tp);
                }

                let true_idx = self.infer_type_ann(true_type, &mut cond_ctx)?;
                let false_idx = self.infer_type_ann(false_type, &mut cond_ctx)?;
                self.new_conditional_type(check_idx, extends_idx, true_idx, false_idx)
            }
            TypeAnnKind::Match(MatchType { matchable, cases }) => {
                let check_idx = self.infer_type_ann(matchable, ctx)?;

                let case = cases.last_mut().unwrap();

                let extends_idx = self.infer_type_ann(&mut case.extends, ctx)?;
                let true_idx = self.infer_type_ann(&mut case.true_type, ctx)?;
                let false_idx = self.new_keyword(Keyword::Never);

                let mut cond_type =
                    self.new_conditional_type(check_idx, extends_idx, true_idx, false_idx);

                for case in cases.iter_mut().rev().skip(1) {
                    let extends_idx = self.infer_type_ann(&mut case.extends, ctx)?;
                    let true_idx = self.infer_type_ann(&mut case.true_type, ctx)?;
                    let false_idx = cond_type;

                    cond_type =
                        self.new_conditional_type(check_idx, extends_idx, true_idx, false_idx);
                }

                cond_type
            }
            TypeAnnKind::Binary(BinaryTypeAnn { left, op, right }) => {
                let left = self.infer_type_ann(left, ctx)?;
                let right = self.infer_type_ann(right, ctx)?;

                let number = self.new_primitive(Primitive::Number);
                self.unify(ctx, left, number)?;
                self.unify(ctx, right, number)?;

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

                self.arena
                    .insert(Type::from(TypeKind::Binary(BinaryT { op, left, right })))
            }
        };

        let t = &mut self.arena[idx];
        t.provenance = Some(Provenance::TypeAnn(Box::new(type_ann.to_owned())));

        type_ann.inferred_type = Some(idx);

        Ok(idx)
    }

    pub fn infer_statement(
        &mut self,
        statement: &mut Stmt,
        ctx: &mut Context,
        top_level: bool,
    ) -> Result<Index, TypeError> {
        self.push_report();
        let t = match &mut statement.kind {
            StmtKind::Let {
                is_declare,
                pattern,
                expr: init,
                type_ann,
                ..
            } => {
                let (pat_bindings, pat_type) = self.infer_pattern(pattern, ctx)?;

                match (is_declare, init, type_ann) {
                    (false, Some(init), type_ann) => {
                        let init_idx = self.infer_expression(init, ctx)?;

                        let init_type = self.arena.get(init_idx).unwrap().clone();
                        let init_idx = match &init_type.kind {
                            TypeKind::Function(func) if top_level => generalize_func(self, func),
                            _ => init_idx,
                        };

                        let tpat = pattern_to_tpat(pattern, false);
                        let mutability = check_mutability(ctx, &tpat, init)?;

                        let idx = match type_ann {
                            Some(type_ann) => {
                                let type_ann_idx = self.infer_type_ann(type_ann, ctx)?;

                                // The initializer must conform to the type annotation's
                                // inferred type.
                                match mutability {
                                    true => self.unify_mut(ctx, init_idx, type_ann_idx)?,
                                    false => self.unify(ctx, init_idx, type_ann_idx)?,
                                };

                                // Results in bindings introduced by the LHS pattern
                                // having their types inferred.
                                // It's okay for pat_type to be the super type here
                                // because all initializers it introduces are type
                                // variables.  It also prevents patterns from including
                                // variables that don't exist in the type annotation.
                                self.unify(ctx, type_ann_idx, pat_type)?;

                                type_ann_idx
                            }
                            None => {
                                // Results in bindings introduced by the LHS pattern
                                // having their types inferred.
                                // It's okay for pat_type to be the super type here
                                // because all initializers it introduces are type
                                // variables.  It also prevents patterns from including
                                // variables that don't exist in the initializer.
                                self.unify(ctx, init_idx, pat_type)?;

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
                        self.pop_report();
                        return Err(TypeError {
                            message:
                                "Variable declarations not using `declare` must have an initializer"
                                    .to_string(),
                        });
                    }
                    (true, None, Some(type_ann)) => {
                        let idx = self.infer_type_ann(type_ann, ctx)?;

                        self.unify(ctx, idx, pat_type)?;

                        for (name, binding) in pat_bindings {
                            ctx.values.insert(name.clone(), binding);
                        }

                        idx
                    }
                    (true, Some(_), _) => {
                        self.pop_report();
                        return Err(TypeError {
                            message:
                                "Variable declarations using `declare` cannot have an initializer"
                                    .to_string(),
                        });
                    }
                    (true, None, None) => {
                        self.pop_report();
                        return Err(TypeError {
                            message:
                                "Variable declarations using `declare` must have a type annotation"
                                    .to_string(),
                        });
                    }
                }
            }
            StmtKind::Expr { expr } => self.infer_expression(expr, ctx)?,
            StmtKind::Return { arg: expr } => {
                // TODO: handle multiple return statements
                // TODO: warn about unreachable code after a return statement
                match expr {
                    Some(expr) => self.infer_expression(expr, ctx)?,
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

                let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;
                let t = self.infer_type_ann(type_ann, &mut sig_ctx)?;

                // TODO: generalize type `t` into a scheme
                let scheme = Scheme { t, type_params };

                ctx.schemes.insert(name.to_owned(), scheme);

                t
            } // StmtKind::ForStmt(_) => todo!(),
        };

        statement.inferred_type = Some(t);

        self.pop_report();

        Ok(t)
    }

    // TODO: introduce `infer_script` which has the same semantics as those used
    // to infer the body of a function.
    // TODO: rename to `infer_module`
    pub fn infer_program(
        &mut self,
        node: &mut Program,
        ctx: &mut Context,
    ) -> Result<(), TypeError> {
        for stmt in &node.stmts {
            if let StmtKind::TypeDecl { name, .. } = &stmt.kind {
                let placeholder_scheme = Scheme {
                    t: self.new_keyword(Keyword::Unknown),
                    type_params: None,
                };
                let name = name.to_owned();
                if ctx
                    .schemes
                    .insert(name.clone(), placeholder_scheme)
                    .is_some()
                {
                    return Err(TypeError {
                        message: format!("{name} cannot be redeclared at the top-level"),
                    });
                }
            }
        }

        for stmt in &mut node.stmts {
            if let StmtKind::Let { pattern, .. } = &mut stmt.kind {
                let (bindings, _) = self.infer_pattern(pattern, ctx)?;

                for (name, binding) in bindings {
                    ctx.non_generic.insert(binding.index);
                    if ctx.values.insert(name.to_owned(), binding).is_some() {
                        return Err(TypeError {
                            message: format!("{name} cannot be redeclared at the top-level"),
                        });
                    }
                }
            }
        }

        // TODO: capture all type decls and do a second pass to valid them

        // TODO: figure out how to avoid parsing patterns twice
        for stmt in &mut node.stmts.iter_mut() {
            self.infer_statement(stmt, ctx, true)?;
        }

        Ok(())
    }

    fn get_ident_member(
        &mut self,
        ctx: &mut Context,
        obj_idx: Index,
        key_idx: Index,
    ) -> Result<Index, TypeError> {
        // NOTE: cloning is fine here because we aren't mutating `obj_type`
        match &self.arena[obj_idx].kind.clone() {
            TypeKind::Object(_) => self.get_prop(ctx, obj_idx, key_idx),
            // declare let obj: {x: number} | {x: string}
            // obj.x; // number | string
            TypeKind::Union(union) => {
                let mut result_types = vec![];
                let mut undefined_count = 0;
                for idx in &union.types {
                    match self.get_prop(ctx, *idx, key_idx) {
                        Ok(t) => result_types.push(t),
                        Err(_) => {
                            // TODO: check what the error is, we may want to propagate
                            // certain errors
                            if undefined_count == 0 {
                                let undefined = self.new_keyword(Keyword::Undefined);
                                result_types.push(undefined);
                            }
                            undefined_count += 1;
                        }
                    }
                }
                if undefined_count == union.types.len() {
                    Err(TypeError {
                        message: format!(
                            "Couldn't find property {} on object",
                            self.print_type(&key_idx),
                        ),
                    })
                } else {
                    Ok(self.new_union_type(&result_types))
                }
            }
            TypeKind::Constructor(types::Constructor { name, types, .. }) => {
                let obj_idx = self.expand_alias(ctx, name, types)?;
                self.get_ident_member(ctx, obj_idx, key_idx)
            }
            TypeKind::Tuple(types::Tuple { types }) => {
                let t = self.new_union_type(types);
                let obj_idx = self.expand_alias(ctx, "Array", &[t])?;
                self.get_ident_member(ctx, obj_idx, key_idx)
            }
            _ => Err(TypeError {
                message: format!("Can't access properties on {}", self.print_type(&obj_idx)),
            }),
        }
    }

    fn infer_type_params(
        &mut self,
        type_params: &mut Option<Vec<syntax::TypeParam>>,
        sig_ctx: &mut Context,
    ) -> Result<Option<Vec<types::TypeParam>>, TypeError> {
        if let Some(type_params) = type_params {
            for tp in type_params.iter_mut() {
                let constraint = match &mut tp.bound {
                    Some(constraint) => Some(self.infer_type_ann(constraint, sig_ctx)?),
                    None => None,
                };

                // Adds the param to the context and set its type to the constraint
                // or `unknown` if there is no constraint.
                let scheme = Scheme {
                    t: match constraint {
                        Some(constraint) => constraint,
                        None => self.new_keyword(Keyword::Unknown),
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
                            return Err(TypeError {
                                message: "type param identifiers must be unique".to_string(),
                            });
                        }
                        Ok(types::TypeParam {
                            name: tp.name.to_owned(),
                            constraint: match &mut tp.bound {
                                Some(constraint) => Some(self.infer_type_ann(constraint, sig_ctx)?),
                                None => None,
                            },
                            default: match &mut tp.default {
                                Some(default) => Some(self.infer_type_ann(default, sig_ctx)?),
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

// NOTE: It's possible to have a mix of mutable and immutable bindings be
// introduced.  In that situation, we only need to check certain parts of
// the initializer for mutability.
pub fn check_mutability(ctx: &Context, tpat: &TPat, init: &Expr) -> Result<bool, TypeError> {
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
        return Err(TypeError {
            message: "Can't assign immutable value to mutable binding".to_string(),
        });
    }

    Ok(lhs_mutable && rhs_mutable)
}

// TODO: find the rest of the identifiers in the expression
fn find_identifiers(expr: &Expr) -> Result<Vec<Ident>, TypeError> {
    let mut idents = vec![];

    if let ExprKind::Ident(ident) = &expr.kind {
        idents.push(ident.to_owned());
    }

    Ok(idents)
}

fn lvalue_mutability(ctx: &Context, expr: &Expr) -> Result<bool, TypeError> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            let binding = ctx.values.get(&ident.name).unwrap();
            Ok(binding.is_mut)
        }
        ExprKind::Member(member) => lvalue_mutability(ctx, &member.object),
        _ => Err(TypeError {
            message: "Can't assign to non-lvalue".to_string(),
        }),
    }
}

struct Generalize<'a, 'b> {
    checker: &'a mut Checker,
    mapping: &'b mut BTreeMap<Index, String>,
}

// TODO: have `Checker` implement this trait
impl<'a, 'b> KeyValueStore<Index, Type> for Generalize<'a, 'b> {
    fn get_type(&mut self, index: &Index) -> Type {
        self.checker.arena[*index].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.checker.arena.insert(t)
    }
}

impl<'a, 'b> Folder for Generalize<'a, 'b> {
    fn fold_index(&mut self, index: &Index) -> Index {
        // QUESTION: Why do we need to `prune` here?  Maybe because we don't
        // copy the `instance` when creating an new type variable.
        let index = self.checker.prune(*index);
        let t = self.get_type(&index);

        match &t.kind {
            TypeKind::Variable(Variable {
                id: _,
                instance: _,
                constraint: _,
            }) => {
                let name = match self.mapping.get(&index) {
                    Some(name) => name.clone(),
                    None => {
                        // TODO: create a name generator that can avoid duplicating
                        // names of explicitly provided type params.
                        let name = ((self.mapping.len() as u8) + 65) as char;
                        let name = format!("{}", name);
                        // let name = format!("'{}", mappings.len());
                        self.mapping.insert(index, name.clone());
                        name
                    }
                };
                self.checker.new_constructor(&name, &[])
            }
            _ => folder::walk_index(self, &index),
        }
    }
}

pub fn generalize_func(checker: &mut Checker, func: &types::Function) -> Index {
    // A mapping of TypeVariables to TypeVariables
    let mut mapping = BTreeMap::default();
    let mut generalize = Generalize {
        checker,
        mapping: &mut mapping,
    };

    let params = func
        .params
        .iter()
        .map(|param| types::FuncParam {
            t: generalize.fold_index(&param.t),
            ..param.to_owned()
        })
        .collect::<Vec<_>>();
    let ret = generalize.fold_index(&func.ret);
    let throws = func.throws.map(|throws| generalize.fold_index(&throws));

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
        checker.new_func_type(&params, ret, &None, throws)
    } else {
        checker.new_func_type(&params, ret, &Some(type_params), throws)
    }
}
