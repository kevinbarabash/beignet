use generational_arena::{Arena, Index};
use itertools::Itertools;

use escalier_ast::{self as syntax, *};

use crate::ast_utils::{find_returns, find_throws};
use crate::checker::Checker;
use crate::context::*;
use crate::infer::generalize_func;
use crate::infer_pattern::pattern_to_tpat;
use crate::key_value_store::KeyValueStore;
use crate::type_error::TypeError;
use crate::types::{self, *};
use crate::visitor::{walk_index, Visitor};

impl Checker {
    pub fn infer_class(
        &mut self,
        class: &mut Class,
        ctx: &mut Context,
    ) -> Result<Index, TypeError> {
        let mut cls_ctx = ctx.clone();

        // TODO: mutate the instance_scheme since only the methods need
        // further type checking.
        // TODO: unify _static_type with the static type of the class
        let (instance_scheme, _static_type) = self.infer_class_interface(class, &mut cls_ctx)?;

        cls_ctx
            .schemes
            .insert("Self".to_string(), instance_scheme.clone());

        let mut static_elems: Vec<TObjElem> = vec![];
        let mut instance_elems: Vec<TObjElem> = vec![];

        for member in &mut class.body {
            match member {
                ClassMember::Method(Method {
                    span: _,
                    name,
                    is_public: _, // TODO
                    is_mutating,
                    is_static,
                    function:
                        syntax::Function {
                            type_params,
                            params,
                            body,
                            type_ann: return_type,
                            throws: sig_throws,
                            is_async,
                            is_gen: _,
                        },
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let mut func_params: Vec<types::FuncParam> = vec![];

                    let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;

                    if !*is_static {
                        let binding = Binding {
                            index: self.new_type_ref("Self", Some(instance_scheme.clone()), &[]),
                            is_mut: *is_mutating,
                        };
                        sig_ctx.values.insert("self".to_string(), binding);
                    }

                    for syntax::FuncParam {
                        pattern,
                        type_ann,
                        optional,
                    } in params.iter_mut()
                    {
                        let type_ann_t = match type_ann {
                            Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                            None => self.new_type_var(None),
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

                    // TODO: dedupe with infer_expression
                    let body_t = 'outer: {
                        match body {
                            BlockOrExpr::Block(Block { stmts, .. }) => {
                                for stmt in stmts.iter_mut() {
                                    body_ctx = body_ctx.clone();
                                    self.infer_statement(stmt, &mut body_ctx)?;
                                    if let StmtKind::Return(_) = stmt.kind {
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
                                self.new_lit_type(&Literal::Undefined)
                            }
                            BlockOrExpr::Expr(expr) => {
                                // TODO: use `find_returns` here as well
                                self.infer_expression(expr, &mut body_ctx)?
                            }
                        }
                    };

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

                    let name = match name {
                        PropName::Ident(Ident { name, span: _ }) => name.to_owned(),
                        PropName::Computed(_) => todo!(),
                    };

                    if &name == "constructor" {
                        static_elems.push(TObjElem::Constructor(types::Function {
                            params: func_params,
                            ret: self.new_type_ref("Self", Some(instance_scheme.clone()), &[]),
                            type_params,
                            throws,
                        }));
                        continue;
                    }

                    let ret_t = match return_type {
                        Some(return_type) => self.infer_type_ann(return_type, &mut sig_ctx)?,
                        None => self.new_type_var(None),
                    };

                    self.unify(&sig_ctx, body_t, ret_t)?;

                    let method = TObjElem::Method(TMethod {
                        name: TPropKey::StringKey(name),
                        mutates: *is_mutating,
                        function: types::Function {
                            type_params,
                            params: func_params,
                            ret: ret_t,
                            throws,
                        },
                    });

                    // TODO: generalize method

                    match is_static {
                        true => static_elems.push(method),
                        false => instance_elems.push(method),
                    };
                }
                ClassMember::Getter(_) => todo!(),
                ClassMember::Setter(_) => todo!(),
                ClassMember::Field(Field {
                    span: _,
                    name,
                    is_public: _, // TODO
                    is_static,
                    type_ann,
                    init: _, // TODO: unify in `infer_class`
                             // If there's an initializer, infer its type and then
                             // unify with the type annotation of the field.
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let type_ann_t = match type_ann {
                        Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                        None => self.new_type_var(None),
                    };

                    let field = TObjElem::Prop(TProp {
                        name: TPropKey::StringKey(name.name.to_owned()),
                        t: type_ann_t,
                        optional: false, // TODO
                        readonly: false, // TODO
                    });

                    match is_static {
                        true => static_elems.push(field),
                        false => instance_elems.push(field),
                    };
                }
            }
        }

        // We generalize methods after all of them have been inferred so
        // that mutually recursive method calls can be handled correctly.
        for elem in instance_elems.iter_mut() {
            if let TObjElem::Method(method) = elem {
                let func = generalize_func(self, &method.function);
                method.function = func;
            }
        }

        let instance_type = self.new_object_type(&instance_elems);
        let static_type = self.new_object_type(&static_elems);

        let self_scheme = Scheme {
            type_params: None,
            t: instance_type,
            is_type_param: false,
        };

        replace_self_type_refs(&mut self.arena, &instance_type, &self_scheme);
        replace_self_type_refs(&mut self.arena, &static_type, &self_scheme);

        self.unify(&cls_ctx, instance_scheme.t, instance_type)?;

        Ok(static_type)
    }

    fn infer_class_interface(
        &mut self,
        class: &mut Class,
        ctx: &mut Context,
    ) -> Result<(Scheme, Index), TypeError> {
        let mut instance_elems: Vec<TObjElem> = vec![];
        let mut static_elems: Vec<TObjElem> = vec![];

        let self_type = self.new_type_var(None);

        let mut cls_ctx = ctx.clone();
        cls_ctx.schemes.insert(
            "Self".to_string(),
            Scheme {
                t: self_type,
                type_params: None,
                is_type_param: false,
            },
        );

        for member in &mut class.body {
            match member {
                // TODO: update Method {} to contain `name` and `function` fields
                // so that we can reuse some of the logic around function inference
                ClassMember::Method(Method {
                    span: _,
                    name,
                    is_public: _,
                    is_mutating,
                    is_static,
                    function:
                        syntax::Function {
                            type_params,
                            params,
                            body: _,
                            type_ann: return_type,
                            throws: _,   // TODO: include in signature
                            is_async: _, // return type is a promise
                            is_gen: _,   // return type is a generator
                        },
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;
                    let func_params = params
                        .iter_mut()
                        .map(|param| {
                            let t = match &mut param.type_ann {
                                Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                                None => self.new_type_var(None),
                            };
                            Ok(types::FuncParam {
                                pattern: pattern_to_tpat(&param.pattern, true),
                                t,
                                optional: param.optional,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let ret = match return_type {
                        Some(return_type) => self.infer_type_ann(return_type, &mut sig_ctx)?,
                        None => self.new_type_var(None),
                    };

                    let mut is_constructor = false;
                    let name: TPropKey = match name {
                        PropName::Ident(Ident { name, span: _ }) => {
                            if name == "constructor" {
                                is_constructor = true;
                            }
                            TPropKey::StringKey(name.to_string())
                        }
                        PropName::Computed(_) => todo!(),
                    };

                    if is_constructor {
                        static_elems.push(TObjElem::Constructor(types::Function {
                            params: func_params,
                            ret,
                            type_params,
                            throws: None, // TODO
                        }));
                        continue;
                    }

                    let method = TObjElem::Method(TMethod {
                        name,
                        mutates: *is_mutating,
                        function: types::Function {
                            type_params,
                            params: func_params,
                            ret,
                            throws: None, // TODO: methods can throw
                        },
                    });

                    match is_static {
                        true => static_elems.push(method),
                        false => instance_elems.push(method),
                    };
                }
                ClassMember::Getter(Getter {
                    span: _,
                    name,
                    is_public: _,
                    type_ann,
                    params: _, // should be empty for getters
                    body: _,   // TODO: unify in `infer_class`
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let type_ann_t = match type_ann {
                        Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                        None => self.new_type_var(None),
                    };

                    let name: TPropKey = match name {
                        PropName::Ident(Ident { name, span: _ }) => {
                            TPropKey::StringKey(name.to_string())
                        }
                        PropName::Computed(_) => todo!(),
                    };

                    let getter = TObjElem::Getter(TGetter {
                        name,
                        ret: type_ann_t,
                        throws: None, // TODO
                    });
                    instance_elems.push(getter);
                }
                ClassMember::Setter(Setter {
                    span: _,
                    name,
                    is_public: _, // TODO: change to private
                    type_ann: _,  // should always be `undefined` or `void`
                    params,
                    body: _, // TODO: unify in `infer_class`
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let name: TPropKey = match name {
                        PropName::Ident(Ident { name, span: _ }) => {
                            TPropKey::StringKey(name.to_string())
                        }
                        PropName::Computed(_) => todo!(),
                    };

                    let setter = TObjElem::Setter(TSetter {
                        name,
                        param: self.infer_func_param(&mut params[0], &mut sig_ctx)?,
                        throws: None, // TODO
                    });
                    instance_elems.push(setter);
                }
                ClassMember::Field(Field {
                    span: _,
                    name,
                    is_public: _, // TODO
                    is_static,
                    type_ann,
                    init: _, // TODO: unify in `infer_class`
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let type_ann_t = match type_ann {
                        Some(type_ann) => self.infer_type_ann(type_ann, &mut sig_ctx)?,
                        None => self.new_type_var(None),
                    };

                    let field = TObjElem::Prop(TProp {
                        name: TPropKey::StringKey(name.name.to_owned()),
                        t: type_ann_t,
                        optional: false, // TODO
                        readonly: false, // TODO
                    });

                    match is_static {
                        true => static_elems.push(field),
                        false => instance_elems.push(field),
                    };
                }
            }
        }

        let instance_scheme = Scheme {
            t: self.new_object_type(&instance_elems),
            // TODO: add type params
            // I don't think this is something that can be inferred, by
            // default, each function gets its own type params
            type_params: None,
            is_type_param: false,
        };

        let static_type = self.new_object_type(&static_elems);

        // TODO: How do we keep track of the relationship between these two?
        Ok((instance_scheme, static_type))
    }

    fn infer_func_param(
        &mut self,
        param: &mut syntax::FuncParam,
        sig_ctx: &mut Context,
    ) -> Result<types::FuncParam, TypeError> {
        let type_ann_t = match &mut param.type_ann {
            Some(type_ann) => self.infer_type_ann(type_ann, sig_ctx)?,
            None => self.new_type_var(None),
        };
        param.pattern.inferred_type = Some(type_ann_t);

        let (assumps, param_t) = self.infer_pattern(&mut param.pattern, sig_ctx)?;
        self.unify(sig_ctx, param_t, type_ann_t)?;

        for (name, binding) in assumps {
            sig_ctx.non_generic.insert(binding.index);
            sig_ctx.values.insert(name.to_owned(), binding);
        }

        Ok(types::FuncParam {
            pattern: pattern_to_tpat(&param.pattern, true),
            t: type_ann_t,
            optional: param.optional,
        })
    }
}

pub struct ReplaceVisitor<'a> {
    pub arena: &'a mut Arena<Type>,
    pub scheme: &'a Scheme,
}

impl<'a> KeyValueStore<Index, Type> for ReplaceVisitor<'a> {
    fn get_type(&mut self, idx: &Index) -> Type {
        self.arena[*idx].clone()
    }
    fn put_type(&mut self, t: Type) -> Index {
        self.arena.insert(t)
    }
}

impl<'a> Visitor for ReplaceVisitor<'a> {
    fn visit_index(&mut self, index: &Index) {
        match &mut self.arena[*index].kind {
            TypeKind::TypeRef(tref) => {
                if tref.name == "Self" {
                    tref.scheme = Some(self.scheme.clone());
                }
            }
            _ => walk_index(self, index),
        }
    }
}

pub fn replace_self_type_refs(arena: &mut Arena<Type>, t: &Index, scheme: &Scheme) {
    let mut replace_visitor = ReplaceVisitor { arena, scheme };

    replace_visitor.visit_index(t)
}
