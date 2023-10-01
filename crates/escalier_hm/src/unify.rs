use defaultmap::*;
use generational_arena::Index;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};
use std::mem::transmute;

use escalier_ast::{BindingIdent, Expr, Literal as Lit, Span};

use crate::checker::Checker;
use crate::context::*;
use crate::diagnostic::Diagnostic;
use crate::infer::check_mutability;
use crate::type_error::TypeError;
use crate::types::*;

impl Checker {
    /// Unify the two types t1 and t2.
    ///
    /// Makes the types t1 and t2 the same.
    ///
    /// Args:
    ///     t1: The first type to be made equivalent (subtype)
    ///     t2: The second type to be be equivalent (supertype)
    ///
    /// Returns:
    ///     None
    ///
    /// Raises:
    ///     InferenceError: Raised if the types cannot be unified.
    pub fn unify(&mut self, ctx: &Context, t1: Index, t2: Index) -> Result<(), TypeError> {
        let a = self.prune(t1);
        let b = self.prune(t2);

        // TODO: only expand if unification fails since it's expensive

        let a_t = self.arena[a].clone();
        let b_t = self.arena[b].clone();

        match (&a_t.kind, &b_t.kind) {
            (TypeKind::TypeVar(_), _) => self.bind(ctx, a, b),
            (_, TypeKind::TypeVar(_)) => self.bind(ctx, b, a),

            // Wildcards are always unifiable
            (TypeKind::Wildcard, _) => Ok(()),
            (_, TypeKind::Wildcard) => Ok(()),

            (TypeKind::Keyword(kw1), TypeKind::Keyword(kw2)) => {
                if kw1 == kw2 {
                    Ok(())
                } else {
                    Err(TypeError {
                        message: format!(
                            "type mismatch: {} != {}",
                            self.print_type(&a),
                            self.print_type(&b),
                        ),
                    })
                }
            }

            (_, TypeKind::Keyword(Keyword::Unknown)) => {
                // All types are assignable to `unknown`
                Ok(())
            }

            (TypeKind::Union(union), _) => {
                // All types in the union must be subtypes of t2
                for t in union.types.iter() {
                    self.unify(ctx, *t, b)?;
                }
                Ok(())
            }
            (_, TypeKind::Union(union)) => {
                // If t1 is a subtype of any of the types in the union, then it is a
                // subtype of the union.
                for t2 in union.types.iter() {
                    if self.unify(ctx, a, *t2).is_ok() {
                        return Ok(());
                    }
                }

                Err(TypeError {
                    message: format!(
                        "type mismatch: unify({}, {}) failed",
                        self.print_type(&a),
                        self.print_type(&b),
                    ),
                })
            }
            (TypeKind::Tuple(tuple1), TypeKind::Tuple(tuple2)) => {
                'outer: {
                    if tuple1.types.len() < tuple2.types.len() {
                        // If there's a rest pattern in tuple1, then it can unify
                        // with the reamining elements of tuple2.
                        if let Some(last) = tuple1.types.last() {
                            if let TypeKind::Rest(_) = self.arena[*last].kind {
                                break 'outer;
                            }
                        }

                        return Err(TypeError {
                            message: format!(
                                "Expected tuple of length {}, got tuple of length {}",
                                tuple2.types.len(),
                                tuple1.types.len()
                            ),
                        });
                    }
                }

                for (i, (p, q)) in tuple1.types.iter().zip(tuple2.types.iter()).enumerate() {
                    // let q_t = arena[*q];
                    match (&self.arena[*p].kind, &self.arena[*q].kind) {
                        (TypeKind::Rest(_), TypeKind::Rest(_)) => {
                            return Err(TypeError {
                                message: "Can't unify two rest elements".to_string(),
                            })
                        }
                        (TypeKind::Rest(_), _) => {
                            let rest_q = self.new_tuple_type(&tuple2.types[i..]);
                            self.unify(ctx, *p, rest_q)?;
                        }
                        (_, TypeKind::Rest(_)) => {
                            let rest_p = self.new_tuple_type(&tuple1.types[i..]);
                            self.unify(ctx, rest_p, *q)?;
                        }
                        (_, _) => self.unify(ctx, *p, *q)?,
                    }
                }
                Ok(())
            }
            (TypeKind::Tuple(tuple), TypeKind::Array(array)) => {
                // TODO: handle rest elements in the tuple

                let mut types = vec![];
                for t in &tuple.types {
                    match &self.arena[*t].kind {
                        TypeKind::Rest(Rest { arg }) => self.unify(ctx, *arg, t2)?,
                        _ => types.push(*t),
                    }
                }

                if !types.is_empty() {
                    let p = self.new_union_type(&types);
                    self.unify(ctx, p, array.t)?;
                }

                Ok(())
            }
            (TypeKind::Array(array), TypeKind::Tuple(tuple)) => {
                let p = array.t;
                for q in &tuple.types {
                    let undefined = self.new_lit_type(&Lit::Undefined);
                    let p_or_undefined = self.new_union_type(&[p, undefined]);

                    match &self.arena[*q].kind {
                        TypeKind::Rest(_) => self.unify(ctx, a, *q)?,
                        _ => self.unify(ctx, p_or_undefined, *q)?,
                    }
                }
                Ok(())
            }
            (TypeKind::Rest(rest), TypeKind::Array(_)) => self.unify(ctx, rest.arg, b),
            (TypeKind::Rest(rest), TypeKind::Tuple(_)) => self.unify(ctx, rest.arg, b),
            (TypeKind::Array(_), TypeKind::Rest(rest)) => self.unify(ctx, a, rest.arg),
            (TypeKind::Tuple(_), TypeKind::Rest(rest)) => self.unify(ctx, a, rest.arg),
            (TypeKind::Array(array_a), TypeKind::Array(array_b)) => {
                self.unify(ctx, array_a.t, array_b.t)
            }
            (TypeKind::TypeRef(con_a), TypeKind::TypeRef(con_b)) => {
                // TODO: support type constructors with optional and default type params
                if con_a.name != con_b.name || con_a.types.len() != con_b.types.len() {
                    return Err(TypeError {
                        message: format!(
                            "type mismatch: {} != {}",
                            self.print_type(&a),
                            self.print_type(&b),
                        ),
                    });
                }
                for (p, q) in con_a.types.iter().zip(con_b.types.iter()) {
                    self.unify(ctx, *p, *q)?;
                }
                Ok(())
            }
            (TypeKind::Function(func_a), TypeKind::Function(func_b)) => {
                // Is this the right place to instantiate the function types?
                let func_a = self.instantiate_func(func_a, None)?;
                let func_b = self.instantiate_func(func_b, None)?;

                let mut params_a = func_a.params;
                let mut params_b = func_b.params;

                if let Some(param) = params_a.get(0) {
                    if param.is_self() {
                        params_a.remove(0);
                    }
                }

                if let Some(param) = params_b.get(0) {
                    if param.is_self() {
                        params_b.remove(0);
                    }
                }

                let mut rest_a = None;
                let mut rest_b = None;

                for param in &params_a {
                    if let TPat::Rest(rest) = &param.pattern {
                        if rest_a.is_some() {
                            return Err(TypeError {
                                message: "multiple rest params in function".to_string(),
                            });
                        }
                        rest_a = Some((rest, param.t));
                    }
                }

                for param in &params_b {
                    if let TPat::Rest(rest) = &param.pattern {
                        if rest_b.is_some() {
                            return Err(TypeError {
                                message: "multiple rest params in function".to_string(),
                            });
                        }
                        rest_b = Some((rest, param.t));
                    }
                }

                // TODO: remove leading `self` or `mut self` param before proceding

                let min_params_a = params_a.len() - rest_a.is_some() as usize;
                let min_params_b = params_b.len() - rest_b.is_some() as usize;

                if min_params_a > min_params_b {
                    if let Some(rest_b) = rest_b {
                        for i in 0..min_params_b {
                            let p = &params_a[i];
                            let q = &params_b[i];
                            // NOTE: We reverse the order of the params here because func_a
                            // should be able to accept any params that func_b can accept,
                            // its params may be more lenient.
                            self.unify(ctx, q.t, p.t)?;
                        }

                        let mut remaining_args_a = vec![];

                        for p in &params_a[min_params_b..] {
                            let arg = match &p.pattern {
                                TPat::Rest(_) => match &self.arena[p.t].kind {
                                    TypeKind::Tuple(tuple) => {
                                        for t in &tuple.types {
                                            remaining_args_a.push(*t);
                                        }
                                        continue;
                                    }
                                    TypeKind::Array(_) => self.new_rest_type(p.t),
                                    TypeKind::TypeRef(_) => todo!(),
                                    _ => {
                                        return Err(TypeError {
                                            message: format!(
                                                "rest param must be an array or tuple, got {}",
                                                self.print_type(&p.t)
                                            ),
                                        });
                                    }
                                },
                                _ => p.t,
                            };

                            remaining_args_a.push(arg);
                        }

                        let remaining_args_a = self.new_tuple_type(&remaining_args_a);

                        // NOTE: We reverse the order of the params here because func_a
                        // should be able to accept any params that func_b can accept,
                        // its params may be more lenient.
                        self.unify(ctx, rest_b.1, remaining_args_a)?;

                        self.unify(ctx, func_a.ret, func_b.ret)?;

                        return Ok(());
                    }

                    return Err(TypeError {
                        message: format!(
                            "{} is not a subtype of {} since it requires more params",
                            self.print_type(&a),
                            self.print_type(&b),
                        ),
                    });
                }

                for i in 0..min_params_a {
                    let p = &params_a[i];
                    let q = &params_b[i];
                    // NOTE: We reverse the order of the params here because func_a
                    // should be able to accept any params that func_b can accept,
                    // its params may be more lenient.
                    self.unify(ctx, q.t, p.t)?;
                }

                if let Some(rest_a) = rest_a {
                    for q in params_b.iter().take(min_params_b).skip(min_params_a) {
                        // NOTE: We reverse the order of the params here because func_a
                        // should be able to accept any params that func_b can accept,
                        // its params may be more lenient.
                        self.unify(ctx, q.t, rest_a.1)?;
                    }

                    if let Some(rest_b) = rest_b {
                        // NOTE: We reverse the order of the params here because func_a
                        // should be able to accept any params that func_b can accept,
                        // its params may be more lenient.
                        self.unify(ctx, rest_b.1, rest_a.1)?;
                    }
                }

                self.unify(ctx, func_a.ret, func_b.ret)?;

                let never = self.new_keyword(Keyword::Never);
                let throws_a = func_a.throws.unwrap_or(never);
                let throws_b = func_b.throws.unwrap_or(never);

                self.unify(ctx, throws_a, throws_b)?;

                Ok(())
            }
            (TypeKind::Literal(lit1), TypeKind::Literal(lit2)) => {
                let equal = match (&lit1, &lit2) {
                    (Lit::Boolean(value1), Lit::Boolean(value2)) => value1 == value2,
                    (Lit::Number(value1), Lit::Number(value2)) => value1 == value2,
                    (Lit::String(value1), Lit::String(value2)) => value1 == value2,
                    (Lit::Undefined, Lit::Undefined) => true,
                    (Lit::Null, Lit::Null) => true,
                    _ => false,
                };
                if !equal {
                    return Err(TypeError {
                        message: format!(
                            "type mismatch: {} != {}",
                            self.print_type(&a),
                            self.print_type(&b),
                        ),
                    });
                }
                Ok(())
            }
            (TypeKind::Literal(Lit::Number(_)), TypeKind::Primitive(Primitive::Number)) => Ok(()),
            (TypeKind::Literal(Lit::String(_)), TypeKind::Primitive(Primitive::String)) => Ok(()),
            (TypeKind::Literal(Lit::Boolean(_)), TypeKind::Primitive(Primitive::Boolean)) => Ok(()),
            (TypeKind::Primitive(prim1), TypeKind::Primitive(prim2)) => match (prim1, prim2) {
                (Primitive::Number, Primitive::Number) => Ok(()),
                (Primitive::String, Primitive::String) => Ok(()),
                (Primitive::Boolean, Primitive::Boolean) => Ok(()),
                (Primitive::Symbol, Primitive::Symbol) => Ok(()),
                _ => Err(TypeError {
                    message: format!(
                        "type mismatch: {} != {}",
                        self.print_type(&a),
                        self.print_type(&b),
                    ),
                }),
            },
            (TypeKind::Object(object1), TypeKind::Object(object2)) => {
                // object1 must have atleast as the same properties as object2
                // This is pretty inefficient... we should have some way of hashing
                // each object element so that we can look them.  The problem comes
                // in with functions where different signatures can expand to be the
                // same.  Do these kinds of checks is going to be really slow.
                // We could also try bucketing the different object element types
                // to reduce the size of the n^2.

                // NOTES:
                // - we don't bother unifying setters because they aren't available
                //   on immutable objects (setters need to be unified inside of
                //   unify_mut() below)
                // - we unify all of the other named elements all at once because
                //   a property could be a function and we want that to unify with
                //   a method of the same name
                // - we should also unify indexers with other named values since
                //   they can be accessed by name as well but are optional

                let mut calls_1: Vec<&TCallable> = vec![];
                let mut constructors_1: Vec<&TCallable> = vec![];
                let mut mapped_1: Vec<&MappedType> = vec![];

                let mut calls_2: Vec<&TCallable> = vec![];
                let mut constructors_2: Vec<&TCallable> = vec![];
                let mut mapped_2: Vec<&MappedType> = vec![];

                let named_props_1: HashMap<_, _> = object1
                    .elems
                    .iter()
                    .filter_map(|elem| match elem {
                        TObjElem::Call(_) => None,
                        TObjElem::Constructor(_) => None,
                        TObjElem::Mapped(_) => None,
                        TObjElem::Prop(prop) => {
                            // TODO: handle getters/setters properly
                            Some((prop.name.to_string(), prop))
                        }
                    })
                    .collect();

                let named_props_2: HashMap<_, _> = object2
                    .elems
                    .iter()
                    .filter_map(|elem| match elem {
                        TObjElem::Call(_) => None,
                        TObjElem::Constructor(_) => None,
                        TObjElem::Mapped(_) => None,
                        TObjElem::Prop(prop) => {
                            // TODO: handle getters/setters properly
                            Some((prop.name.to_string(), prop))
                        }
                    })
                    .collect();

                // object1 must have at least as the same named elements as object2
                // TODO: handle the case where object1 has an indexer that covers
                // some of the named elements of object2
                for (name, prop_2) in &named_props_2 {
                    match named_props_1.get(name) {
                        Some(prop_1) => {
                            let t1 = prop_1.get_type(self);
                            let t2 = prop_2.get_type(self);
                            self.unify(ctx, t1, t2)?;
                        }
                        None => {
                            if prop_2.optional {
                                continue;
                            }

                            return Err(TypeError {
                                message: format!(
                                    "'{}' is missing in {}",
                                    name,
                                    self.print_type(&a),
                                ),
                            });
                        }
                    }
                }

                for prop1 in &object1.elems {
                    match prop1 {
                        TObjElem::Call(call) => calls_1.push(call),
                        TObjElem::Constructor(constructor) => constructors_1.push(constructor),
                        TObjElem::Mapped(mapped) => mapped_1.push(mapped),
                        _ => (),
                    }
                }

                for prop2 in &object2.elems {
                    match prop2 {
                        TObjElem::Call(call) => calls_2.push(call),
                        TObjElem::Constructor(constructor) => constructors_2.push(constructor),
                        TObjElem::Mapped(mapped) => mapped_2.push(mapped),
                        _ => (),
                    }
                }

                match mapped_2.len() {
                    0 => (),
                    1 => {
                        match mapped_1.len() {
                            0 => {
                                for (_, prop_1) in named_props_1 {
                                    let undefined = self.new_lit_type(&Lit::Undefined);
                                    let t1 = prop_1.get_type(self);
                                    let t2 = self.new_union_type(&[mapped_2[0].value, undefined]);
                                    self.unify(ctx, t1, t2)?;
                                }
                            }
                            1 => {
                                self.unify(ctx, mapped_1[0].value, mapped_2[0].value)?;
                                // NOTE: the order is reverse here because object1
                                // has to have at least the same keys as object2,
                                // but it can have more.
                                // TODO: lookup source instead of key... we only need
                                // to look at key when it's not using the source or if
                                // it's modifying the source.

                                let mut mapping: HashMap<String, Index> = HashMap::new();
                                mapping.insert(mapped_1[0].target.to_owned(), mapped_1[0].source);
                                let mapped_1_key =
                                    self.instantiate_type(&mapped_1[0].key, &mapping);

                                let mut mapping: HashMap<String, Index> = HashMap::new();
                                mapping.insert(mapped_2[0].target.to_owned(), mapped_2[0].source);
                                let mapped_2_key =
                                    self.instantiate_type(&mapped_2[0].key, &mapping);

                                self.unify(ctx, mapped_2_key, mapped_1_key)?;
                            }
                            _ => {
                                return Err(TypeError {
                                    message: format!(
                                        "{} has multiple indexers",
                                        self.print_type(&a),
                                    ),
                                })
                            }
                        }
                    }
                    _ => {
                        return Err(TypeError {
                            message: format!("{} has multiple indexers", self.print_type(&b),),
                        })
                    }
                }

                // TODO:
                // - call (all calls in object1 must cover the calls in object2)
                // - constructor (all constructors in object1 must cover the
                //   constructors in object2)
                Ok(())
            }
            (TypeKind::Object(object1), TypeKind::Intersection(intersection)) => {
                let obj_types: Vec<_> = intersection
                    .types
                    .iter()
                    .filter(|t| matches!(self.arena[**t].kind, TypeKind::Object(_)))
                    .cloned()
                    .collect();
                let rest_types: Vec<_> = intersection
                    .types
                    .iter()
                    .filter(|t| matches!(self.arena[**t].kind, TypeKind::TypeVar(_)))
                    .cloned()
                    .collect();
                // TODO: check for other variants, if there are we should error

                let obj_type = simplify_intersection(self, &obj_types);

                match rest_types.len() {
                    0 => self.unify(ctx, t1, obj_type),
                    1 => {
                        let all_obj_elems = match &self.arena[obj_type].kind {
                            TypeKind::Object(obj) => obj.elems.to_owned(),
                            _ => vec![],
                        };

                        let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                            object1.elems.iter().cloned().partition(|e| {
                                all_obj_elems.iter().any(|oe| match (oe, e) {
                                    // What to do about Call signatures?
                                    // (TObjElem::Call(_), TObjElem::Call(_)) => todo!(),
                                    (TObjElem::Prop(op), TObjElem::Prop(p)) => op.name == p.name,
                                    _ => false,
                                })
                            });

                        let new_obj_type = self.new_object_type(&obj_elems);
                        self.unify(ctx, new_obj_type, obj_type)?;

                        let new_rest_type = self.new_object_type(&rest_elems);
                        self.unify(ctx, new_rest_type, rest_types[0])?;

                        Ok(())
                    }
                    _ => Err(TypeError {
                        message: "Inference is undecidable".to_string(),
                    }),
                }
            }
            (TypeKind::Intersection(intersection), TypeKind::Object(object2)) => {
                let obj_types: Vec<_> = intersection
                    .types
                    .iter()
                    .filter(|t| matches!(self.arena[**t].kind, TypeKind::Object(_)))
                    .cloned()
                    .collect();
                let rest_types: Vec<_> = intersection
                    .types
                    .iter()
                    .filter(|t| matches!(self.arena[**t].kind, TypeKind::TypeVar(_)))
                    .cloned()
                    .collect();

                let obj_type = simplify_intersection(self, &obj_types);

                match rest_types.len() {
                    0 => self.unify(ctx, t1, obj_type),
                    1 => {
                        let all_obj_elems = match &self.arena[obj_type].kind {
                            TypeKind::Object(obj) => obj.elems.to_owned(),
                            _ => vec![],
                        };

                        let (obj_elems, rest_elems): (Vec<_>, Vec<_>) =
                            object2.elems.iter().cloned().partition(|e| {
                                all_obj_elems.iter().any(|oe| match (oe, e) {
                                    // What to do about Call signatures?
                                    // (TObjElem::Call(_), TObjElem::Call(_)) => todo!(),
                                    (TObjElem::Prop(op), TObjElem::Prop(p)) => op.name == p.name,
                                    _ => false,
                                })
                            });

                        let new_obj_type = self.new_object_type(&obj_elems);
                        self.unify(ctx, obj_type, new_obj_type)?;

                        let new_rest_type = self.new_object_type(&rest_elems);
                        self.unify(ctx, rest_types[0], new_rest_type)?;

                        Ok(())
                    }
                    _ => Err(TypeError {
                        message: "Inference is undecidable".to_string(),
                    }),
                }
            }
            _ => {
                let expanded_a = self.expand(ctx, a)?;
                let expanded_b = self.expand(ctx, b)?;

                if expanded_a != a || expanded_b != b {
                    return self.unify(ctx, expanded_a, expanded_b);
                }

                Err(TypeError {
                    message: format!(
                        "type mismatch: unify({}, {}) failed",
                        self.print_type(&a),
                        self.print_type(&b),
                    ),
                })
            }
        }
    }

    pub fn unify_mut(&mut self, ctx: &Context, t1: Index, t2: Index) -> Result<(), TypeError> {
        let t1 = self.prune(t1);
        let t2 = self.prune(t2);

        // TODO: only expand if unification fails since it's expensive
        let t1 = self.expand(ctx, t1)?;
        let t2 = self.expand(ctx, t2)?;

        if self.equals(&t1, &t2) {
            Ok(())
        } else {
            Err(TypeError {
                message: format!(
                    "unify_mut: {} != {}",
                    self.print_type(&t1),
                    self.print_type(&t2),
                ),
            })
        }
    }

    // This function unifies and infers the return type of a function call.
    pub fn unify_call(
        &mut self,
        ctx: &mut Context,
        args: &mut [Expr],
        type_args: Option<&[Index]>,
        newable: bool,
        t2: Index,
    ) -> Result<(Index, Option<Index>), TypeError> {
        let ret_type = self.new_type_var(None);
        let mut maybe_throws_type: Option<Index> = None;
        // let throws_type = new_var_type(arena, None);

        let b = self.prune(t2);
        let b_t = self.arena.get(b).unwrap().clone();

        match b_t.kind {
            TypeKind::TypeVar(_) => {
                let arg_types: Vec<FuncParam> = args
                    .iter_mut()
                    .enumerate()
                    .map(|(i, arg)| {
                        let t = self.infer_expression(arg, ctx)?;
                        let param = FuncParam {
                            pattern: TPat::Ident(BindingIdent {
                                name: format!("arg{i}"),
                                mutable: false,
                                // loc: DUMMY_LOC,
                                span: Span { start: 0, end: 0 },
                            }),
                            // name: format!("arg{i}"),
                            t,
                            optional: false,
                        };
                        Ok(param)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let call_type = self.new_func_type(&arg_types, ret_type, &None, None);
                self.bind(ctx, b, call_type)?
            }
            TypeKind::Union(Union { types }) => {
                let mut ret_types = vec![];
                let mut throws_types = vec![];
                for t in types.iter() {
                    let (ret_type, throws_type) =
                        self.unify_call(ctx, args, type_args, newable, *t)?;
                    ret_types.push(ret_type);
                    if let Some(throws_type) = throws_type {
                        throws_types.push(throws_type);
                    }
                }

                let ret = self.new_union_type(&ret_types.into_iter().unique().collect_vec());
                let throws = self.new_union_type(&throws_types.into_iter().unique().collect_vec());

                let throws = match &self.arena[throws].kind {
                    TypeKind::Keyword(Keyword::Never) => None,
                    _ => Some(throws),
                };

                return Ok((ret, throws));
            }
            TypeKind::Intersection(Intersection { types }) => {
                for t in types.iter() {
                    self.push_report();

                    // TODO: if there are multiple overloads that unify, pick the
                    // best one.
                    let (ret_type, maybe_throws_type) =
                        self.unify_call(ctx, args, type_args, newable, *t)?;

                    if self.current_report.diagnostics.is_empty() {
                        self.pop_report();
                        return Ok((ret_type, maybe_throws_type));
                    }

                    // We just throw away reports that don't unify until we find
                    // one that does.  We should probably be saving these so that
                    // we can provide a more detailed report when no overloads
                    // match.
                    if let Some(report) = self.parent_reports.pop() {
                        self.current_report = report;
                    }
                }
                return Err(TypeError {
                    message: "no valid overload for args".to_string(),
                });
            }
            TypeKind::Tuple(_) => {
                return Err(TypeError {
                    message: "tuple is not callable".to_string(),
                })
            }
            TypeKind::Array(_) => {
                return Err(TypeError {
                    message: "array is not callable".to_string(),
                })
            }
            TypeKind::TypeRef(TypeRef {
                name,
                types: type_args,
            }) => {
                let scheme = ctx.get_scheme(&name)?;
                let mut mapping: HashMap<String, Index> = HashMap::new();
                if let Some(type_params) = &scheme.type_params {
                    for (param, arg) in type_params.iter().zip(type_args.iter()) {
                        mapping.insert(param.name.clone(), arg.to_owned());
                    }
                }

                let t = self.instantiate_type(&scheme.t, &mapping);
                // let t = self.expand_alias(ctx, &name, &type_args)?;

                let type_args = if type_args.is_empty() {
                    None
                } else {
                    Some(type_args.as_slice())
                };

                return self.unify_call(ctx, args, type_args, newable, t);
            }
            TypeKind::Literal(lit) => {
                return Err(TypeError {
                    message: format!("literal {lit:#?} is not callable"),
                });
            }
            TypeKind::Primitive(primitive) => {
                return Err(TypeError {
                    message: format!("Primitive {primitive:#?} is not callable"),
                });
            }
            TypeKind::Keyword(keyword) => {
                return Err(TypeError {
                    message: format!("{keyword} is not callable"),
                })
            }
            TypeKind::Object(Object { elems }) => {
                let mut newables = vec![];
                let mut callables = vec![];

                for elem in elems {
                    match elem {
                        TObjElem::Call(callable) => callables.push(callable),
                        TObjElem::Constructor(callable) => newables.push(callable),
                        _ => (),
                    }
                }

                if newable {
                    if newables.is_empty() {
                        return Err(TypeError {
                            message: "Cannot new a non-newable type".to_string(),
                        });
                    }

                    // TODO: Cycle through all of the newables and try to unify
                    // using the best result.  One of criteria might be picking
                    // one that ignores the fewest number of arguments.
                    let func = Function {
                        params: newables[0].params.clone(),
                        ret: newables[0].ret,
                        throws: None, // newables[0].throws,
                        type_params: newables[0].type_params.clone(),
                    };

                    maybe_throws_type =
                        self.unify_func_call(ctx, args, type_args, ret_type, func)?;
                } else {
                    if callables.is_empty() {
                        return Err(TypeError {
                            message: "Cannot call a non-callable type".to_string(),
                        });
                    }

                    // TODO: Cycle through all of the callables and try to unify
                    // using the best result.  One of criteria might be picking
                    // one that ignores the fewest number of arguments.
                    let func = Function {
                        params: callables[0].params.clone(),
                        ret: callables[0].ret,
                        throws: None, // callables[0].throws,
                        type_params: callables[0].type_params.clone(),
                    };

                    maybe_throws_type =
                        self.unify_func_call(ctx, args, type_args, ret_type, func)?;
                }
            }
            TypeKind::Rest(_) => {
                return Err(TypeError {
                    message: "rest is not callable".to_string(),
                });
            }
            // TODO: extract this into a helper function so that it can
            // be reused when unifying callables/newables.
            TypeKind::Function(func) => {
                maybe_throws_type = self.unify_func_call(ctx, args, type_args, ret_type, func)?;
            }
            TypeKind::KeyOf(KeyOf { t }) => {
                return Err(TypeError {
                    message: format!("keyof {} is not callable", self.print_type(&t)),
                });
            }
            TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
                let is_mut = true;
                let t = self.get_prop_value(ctx, obj, index, is_mut)?;
                self.unify_call(ctx, args, type_args, newable, t)?;
            }
            TypeKind::Conditional(Conditional {
                check,
                extends,
                true_type,
                false_type,
            }) => {
                match self.unify(ctx, check, extends) {
                    Ok(_) => self.unify_call(ctx, args, type_args, newable, true_type)?,
                    Err(_) => self.unify_call(ctx, args, type_args, newable, false_type)?,
                };
            }
            TypeKind::Infer(Infer { name }) => {
                return Err(TypeError {
                    message: format!("infer {name} is not callable",),
                });
            }
            TypeKind::Wildcard => {
                return Err(TypeError {
                    message: "_ is not callable".to_string(),
                });
            }
            TypeKind::Binary(BinaryT {
                op: _,
                left: _,
                right: _,
            }) => todo!(),
        }

        // We need to prune the return type, because it might be a type variable.
        let ret_type = self.prune(ret_type);

        Ok((ret_type, maybe_throws_type))
    }

    pub fn unify_func_call(
        &mut self,
        ctx: &mut Context,
        args: &mut [Expr],
        type_args: Option<&[Index]>,
        ret_type: Index,
        func: Function,
    ) -> Result<Option<Index>, TypeError> {
        let func = if func.type_params.is_some() {
            self.instantiate_func(&func, type_args)?
        } else {
            func
        };

        let func_params = match func.params.get(0) {
            Some(param) => {
                if param.is_self() {
                    &func.params[1..]
                } else {
                    &func.params[..]
                }
            }
            None => &[],
        };

        let (params, rest_param) = match func_params.last() {
            Some(param) => match &param.pattern {
                TPat::Rest(_) => (&func_params[..func_params.len() - 1], Some(param)),
                _ => (func_params, None),
            },
            None => (func_params, None),
        };

        let required_params = params.iter().filter(|param| !param.optional).collect_vec();

        if args.len() < required_params.len() {
            return Err(TypeError {
                message: format!(
                    "too few arguments to function: expected {}, got {}",
                    required_params.len(),
                    args.len()
                ),
            });
        }

        let arg_types = args
            .iter_mut()
            .map(|arg| {
                // TODO: handle spreads
                let t = self.infer_expression(arg, ctx)?;
                Ok((arg, t))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut reasons: Vec<TypeError> = vec![];
        for ((arg, p), param) in arg_types.iter().zip(params.iter()) {
            if param.optional {
                if let Some(index) = arg.inferred_type {
                    if let TypeKind::Literal(Lit::Undefined) = &self.arena[index].kind {
                        continue;
                    }
                }
            }

            match check_mutability(ctx, &param.pattern, arg)? {
                true => self.unify_mut(ctx, *p, param.t)?,
                false => match self.unify(ctx, *p, param.t) {
                    Ok(_) => {}
                    Err(error) => reasons.push(error),
                },
            };
        }

        if let Some(rest_param) = rest_param {
            // We're not mutating `kind` so this should be safe.
            let kind: &TypeKind = unsafe { transmute(&self.arena[rest_param.t].kind) };
            match kind {
                TypeKind::Array(array) => {
                    if arg_types.len() >= params.len() {
                        let remaining_arg_types = &arg_types[params.len()..];
                        let t = array.t;
                        for (_, p) in remaining_arg_types.iter() {
                            match self.unify(ctx, *p, t) {
                                Ok(_) => {}
                                Err(error) => reasons.push(error),
                            }
                        }
                    }
                }
                TypeKind::Tuple(tuple) => {
                    let remaining_arg_types = &arg_types[params.len()..];
                    if remaining_arg_types.len() < tuple.types.len() {
                        return Err(TypeError {
                            message: format!(
                                "too few arguments to function: expected {}, got {}",
                                params.len() + tuple.types.len(),
                                params.len() + remaining_arg_types.len()
                            ),
                        });
                    }

                    for ((_, p), t) in remaining_arg_types.iter().zip(tuple.types.iter()) {
                        match self.unify(ctx, *p, *t) {
                            Ok(_) => {}
                            Err(error) => reasons.push(error),
                        };
                    }
                }
                _ => {
                    return Err(TypeError {
                        message: format!(
                            "rest param must be an array, got {}",
                            self.print_type(&rest_param.t)
                        ),
                    });
                }
            }
        }

        if !reasons.is_empty() {
            self.current_report.diagnostics.push(Diagnostic {
                code: 1000,
                message: "Function arguments are incorrect".to_string(),
                reasons,
            });
        }

        self.unify(ctx, ret_type, func.ret)?;

        let mut maybe_throws_type = None;

        if let Some(throws) = func.throws {
            let throws_type = self.new_type_var(None);
            self.unify(ctx, throws_type, throws)?;

            let throws_type = self.prune(throws_type);
            maybe_throws_type = match &self.arena[throws_type].kind {
                TypeKind::Keyword(Keyword::Never) => None,
                _ => Some(throws_type),
            };
        }

        Ok(maybe_throws_type)
    }

    pub fn bind(&mut self, ctx: &Context, a: Index, b: Index) -> Result<(), TypeError> {
        // eprint!("bind(");
        // eprint!("{:#?}", arena[a].as_string(arena));
        // if let Some(provenance) = &arena[a].provenance {
        //     eprint!(" : {:#?}", provenance);
        // }
        // eprint!(", {:#?}", arena[b].as_string(arena));
        // if let Some(provenance) = &arena[b].provenance {
        //     eprint!(" : {:#?}", provenance);
        // }
        // eprintln!(")");

        if a != b {
            if self.occurs_in_type(a, b) {
                // TODO: check if 'b' is a union type and if it is, filter out
                // the types that contain 'a' and then check if the remaining.
                if let TypeKind::Union(Union { types }) = &self.arena[b].kind.clone() {
                    let types = self.flatten_types(types);
                    let types: Vec<Index> =
                        types.iter().filter(|t| a != **t).cloned().collect_vec();

                    match types.as_slice() {
                        [] => return Ok(()),
                        [t] => {
                            return self.bind(ctx, a, *t);
                        }
                        types => {
                            let t = self.new_union_type(types);
                            return self.bind(ctx, a, t);
                        }
                    }
                }

                return Err(TypeError {
                    message: format!(
                        "recursive unification - {} occurs in {}",
                        self.print_type(&a),
                        self.print_type(&b)
                    ),
                });
            }

            match self.arena.get_mut(a) {
                Some(t) => match &mut t.kind {
                    TypeKind::TypeVar(avar) => {
                        avar.instance = Some(b);
                        if let Some(constraint) = avar.constraint {
                            self.unify(ctx, b, constraint)?;
                        }
                    }
                    _ => {
                        unimplemented!("bind not implemented for {:#?}", t.kind);
                    }
                },
                None => todo!(),
            }
        }
        Ok(())
    }

    fn expand(&mut self, ctx: &Context, a: Index) -> Result<Index, TypeError> {
        let a_t = self.arena[a].clone();

        match &a_t.kind {
            TypeKind::TypeRef(TypeRef { name, .. }) if name == "Promise" => Ok(a),
            _ => self.expand_type(ctx, a),
        }
    }

    fn flatten_types(&mut self, types: &[Index]) -> Vec<Index> {
        let mut out_types: Vec<Index> = vec![];
        for t in types {
            let t = self.prune(*t);
            match &self.arena[t].kind {
                TypeKind::Union(Union { types }) => {
                    let mut types = types.clone();
                    out_types.append(&mut types);
                }
                _ => out_types.push(t),
            };
        }
        out_types
    }
}

// TODO: handle optional properties correctly
// Maybe we can have a function that will canonicalize objects by converting
// `x: T | undefined` to `x?: T`
pub fn simplify_intersection(checker: &mut Checker, in_types: &[Index]) -> Index {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|t| match &checker.arena[*t].kind {
            TypeKind::Object(elems) => Some(elems),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, BTreeSet<Index>> = defaulthashmap!();
    for obj in obj_types {
        for elem in &obj.elems {
            match elem {
                // What do we do with Call and Index signatures
                TObjElem::Call(_) => todo!(),
                TObjElem::Constructor(_) => todo!(),
                TObjElem::Mapped(_) => todo!(),
                TObjElem::Prop(prop) => {
                    let key = match &prop.name {
                        TPropKey::StringKey(key) => key.to_owned(),
                        TPropKey::NumberKey(key) => key.to_owned(),
                    };
                    props_map[key].insert(prop.t);
                }
            }
        }
    }

    let mut elems: Vec<TObjElem> = props_map
        .iter()
        .map(|(name, types)| {
            let types: Vec<_> = types.iter().cloned().collect();
            let t: Index = if types.len() == 1 {
                types[0]
            } else {
                // TODO: handle getter/setters correctly
                checker.new_intersection_type(&types)
                // checker.from_type_kind(TypeKind::Intersection(types))
            };
            TObjElem::Prop(TProp {
                name: TPropKey::StringKey(name.to_owned()),
                modifier: None,
                // TODO: determine this field from all of the TProps with
                // the same name.  This should only be optional if all of
                // the TProps with the current name are optional.
                optional: false,
                readonly: false,
                t,
            })
        })
        .collect();
    // How do we sort call and index signatures?
    elems.sort_by_key(|elem| match elem {
        TObjElem::Call(_) => todo!(),
        TObjElem::Constructor(_) => todo!(),
        TObjElem::Mapped(_) => todo!(),
        TObjElem::Prop(prop) => prop.name.clone(),
    }); // ensure a stable order

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|t| !matches!(&checker.arena[**t].kind, TypeKind::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    if !elems.is_empty() {
        out_types.push(checker.new_object_type(&elems));
    }
    // TODO: figure out a consistent way to sort types
    // out_types.sort_by_key(|t| t.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0]
    } else {
        checker.new_intersection_type(&out_types)
    }
}
