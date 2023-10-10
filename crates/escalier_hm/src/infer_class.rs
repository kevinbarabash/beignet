use generational_arena::Index;

use escalier_ast::{self as syntax, *};

use crate::checker::Checker;
use crate::context::*;
use crate::infer_pattern::pattern_to_tpat;
use crate::type_error::TypeError;
use crate::types::{self, *};

impl Checker {
    pub fn infer_class(
        &mut self,
        class: &mut Class,
        ctx: &mut Context,
    ) -> Result<Index, TypeError> {
        let mut cls_ctx = ctx.clone();

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
                    is_async,
                    is_gen: _, // TODO
                    is_mutating,
                    is_static,
                    type_params,
                    params,
                    body,
                    type_ann: return_type,
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

                    let body_t = 'outer: {
                        for stmt in body.stmts.iter_mut() {
                            body_ctx = body_ctx.clone();
                            self.infer_statement(stmt, &mut body_ctx)?;
                            if let StmtKind::Return(_) = stmt.kind {
                                // TODO: handle unreachable return statements
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
                    };

                    // let until = body_ctx.get_binding("until").unwrap();
                    // eprintln!("until = {}", self.print_type(&until.index));

                    // TODO: search for `throw` expressions in the body and include
                    // them in the throws type.

                    // let body_throws = find_throws(body);
                    // let body_throws = if body_throws.is_empty() {
                    //     None
                    // } else {
                    //     Some(self.new_union_type(
                    //         // TODO: compare string reps of the types for deduplication
                    //         &body_throws.into_iter().unique().collect_vec(),
                    //     ))
                    // };

                    // let sig_throws = sig_throws
                    //     .as_mut()
                    //     .map(|t| self.infer_type_ann(t, &mut sig_ctx))
                    //     .transpose()?;

                    // let throws = match (body_throws, sig_throws) {
                    //     (Some(call_throws), Some(sig_throws)) => {
                    //         self.unify(&sig_ctx, call_throws, sig_throws)?;
                    //         Some(sig_throws)
                    //     }
                    //     (Some(call_throws), None) => Some(call_throws),
                    //     // This should probably be a warning.  If the function doesn't
                    //     // throw anything, then it shouldn't be marked as such.
                    //     (None, Some(sig_throws)) => Some(sig_throws),
                    //     (None, None) => None,
                    // };

                    let name = match name {
                        PropName::Ident(Ident { name, span: _ }) => name.to_owned(),
                        PropName::Computed(_) => todo!(),
                    };

                    if &name == "constructor" {
                        static_elems.push(TObjElem::Constructor(types::Function {
                            params: func_params,
                            ret: self.new_type_ref("Self", Some(instance_scheme.clone()), &[]),
                            // ret: instance_scheme.t,
                            type_params,
                            throws: None, // TODO
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
                        type_params,
                        params: func_params,
                        ret: ret_t,
                        throws: None, // TODO
                        mutates: *is_mutating,
                    });

                    match is_static {
                        true => static_elems.push(method),
                        false => instance_elems.push(method),
                    };
                }
                ClassMember::Getter(_) => todo!(),
                ClassMember::Setter(_) => todo!(),
                ClassMember::Constructor(Constructor {
                    span: _,
                    is_public: _,
                    params: _,
                    body: _,
                }) => {
                    eprintln!("inferring constuctor");
                }
                ClassMember::Field(_) => {
                    // If there's an initializer, infer its type and then
                    // unify with the type annotation of the field.
                }
            }
        }

        let instance_type = self.new_object_type(&instance_elems);
        let static_type = self.new_object_type(&static_elems);

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

        // TODO:
        // - add `Self` type to `sig_ctx` so that it can be referenced in
        //   type annotations
        // - `Self` should expand to whatever the class name is + type params

        for member in &mut class.body {
            match member {
                // Constructors are part of statics and thus not part of the interface
                ClassMember::Constructor(Constructor {
                    span: _,
                    is_public: _,
                    params,
                    body: _,
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let func_params = self.infer_func_params(params, &mut sig_ctx)?;

                    let ret = self.new_type_ref(
                        "Self",
                        Some(Scheme {
                            t: self_type,
                            type_params: None,
                            is_type_param: false,
                        }),
                        &[],
                    );

                    let constructor = TObjElem::Constructor(types::Function {
                        params: func_params,
                        ret,
                        type_params: None,
                        throws: None, // TODO: constructors can throw
                    });

                    static_elems.push(constructor);
                }
                // TODO: update Method {} to contain `name` and `function` fields
                // so that we can reuse some of the logic around function inference
                ClassMember::Method(Method {
                    span: _,
                    name,
                    is_public: _,
                    is_async: _, // return type is a promise
                    is_gen: _,   // return type is a generator
                    is_mutating,
                    is_static,
                    type_params,
                    params,
                    body: _,
                    type_ann: return_type,
                }) => {
                    let mut sig_ctx = cls_ctx.clone();

                    let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;
                    let func_params = params
                        .iter()
                        .map(|param| {
                            let t = self.new_type_var(None);
                            types::FuncParam {
                                pattern: pattern_to_tpat(&param.pattern, true),
                                t,
                                optional: param.optional,
                            }
                        })
                        .collect::<Vec<_>>();

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
                        // replace with `fucntion: Fucntion` - START>
                        type_params,
                        params: func_params,
                        ret,
                        throws: None, // TODO: methods can throw
                        // <END - replace with `fucntion: Fucntion`
                        mutates: *is_mutating,
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

    fn infer_func_params(
        &mut self,
        params: &mut [syntax::FuncParam],
        sig_ctx: &mut Context,
    ) -> Result<Vec<types::FuncParam>, TypeError> {
        let func_params = params
            .iter_mut()
            .map(|func_param| self.infer_func_param(func_param, sig_ctx))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(func_params)
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

struct ReturnVisitor {
    pub returns: Vec<Expr>,
}

impl Visitor for ReturnVisitor {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if let StmtKind::Return(ReturnStmt { arg: Some(arg) }) = &stmt.kind {
            self.returns.push(arg.to_owned());
        }
        walk_stmt(self, stmt);
    }
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // Don't walk into functions, since we don't want to include returns
            // from nested functions
            ExprKind::Function(_) => {}
            _ => walk_expr(self, expr),
        }
    }
}

pub fn find_returns(block: &Block) -> Vec<Expr> {
    let mut visitor = ReturnVisitor { returns: vec![] };

    for stmt in &block.stmts {
        visitor.visit_stmt(stmt);
    }

    visitor.returns
}
