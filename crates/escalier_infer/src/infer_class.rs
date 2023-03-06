use escalier_ast::types::*;
use escalier_ast::values::{class::*, Lambda, PatternKind};
use im::hashmap::HashMap;

use crate::context::Binding;
use crate::scheme::Scheme;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::util::compose_many_subs;

use crate::checker::Checker;

fn is_promise(t: &Type) -> bool {
    matches!(&t, Type {kind: TypeKind::Ref(TRef { name, .. }), ..} if name == "Promise")
}

impl Checker {
    pub fn infer_class(&mut self, class: &mut Class) -> Result<(Subst, Type), Vec<TypeError>> {
        let interface = self.infer_interface_from_class(class)?;

        let class_name = class.ident.name.to_owned();

        let mut statics_elems: Vec<TObjElem> = vec![];
        let mut instance_elems: Vec<TObjElem> = vec![];

        for member in &mut class.body {
            match member {
                ClassMember::Constructor(Constructor { params, body }) => {
                    self.push_scope();
                    self.current_scope.is_async = false; // Constructors cannot be async

                    // Constructors can't have type parameters.
                    let type_params_map = HashMap::default();

                    let mut iter = params.iter_mut();
                    iter.next(); // skip `self`
                    let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = iter
                        .map(|e_param| self.infer_fn_param(e_param, &type_params_map))
                        .collect();
                    let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

                    // TODO: ensure that constructors don't have a return statement
                    let mut interface_t = interface.t.as_ref().to_owned();
                    interface_t.mutable = true;
                    let binding = Binding {
                        mutable: false, // this should be false since we don't want to allow `self` to be re-assigned
                        t: interface_t,
                    };
                    self.current_scope
                        .insert_binding("self".to_string(), binding);
                    let (body_s, _body_t) = self.infer_block(body)?;
                    ss.push(body_s);

                    self.pop_scope();

                    let type_args = class.type_params.as_ref().map(|type_params| {
                        type_params
                            .iter()
                            .map(|type_param| {
                                Type::from(TypeKind::Ref(TRef {
                                    name: type_param.name.name.to_owned(),
                                    type_args: None,
                                }))
                            })
                            .collect::<Vec<Type>>()
                    });

                    let type_params = class.type_params.as_ref().map(|type_params| {
                        type_params
                            .iter()
                            .map(|type_param| TypeParam {
                                name: type_param.name.name.to_owned(),
                                constraint: None, // TODO
                                default: None,    // TODO
                            })
                            .collect()
                    });

                    let elem = TObjElem::Constructor(TCallable {
                        params: t_params,
                        ret: Box::from(Type::from(TypeKind::Ref(TRef {
                            name: class_name.to_owned(),
                            type_args,
                        }))),
                        type_params,
                    });

                    let s = compose_many_subs(&ss);
                    let t = elem.apply(&s);

                    statics_elems.push(t);
                }
                ClassMember::Method(ClassMethod {
                    key,
                    kind,
                    lambda,
                    is_static,
                    is_mutating: _,
                }) => {
                    // TODO: dedupe with infer_expr's ExprKind::Lambda branch

                    let Lambda {
                        params,
                        body,
                        is_async,
                        return_type,
                        type_params,
                    } = lambda;

                    self.push_scope();
                    self.current_scope.is_async = is_async.to_owned();

                    // TODO: aggregate method type params with class type params
                    // This maps type params to type refs with the same name.
                    let type_params_map: HashMap<String, Type> = match type_params {
                        Some(params) => params
                            .iter_mut()
                            .map(|param| {
                                let t = Type::from(TypeKind::Ref(TRef {
                                    name: param.name.name.to_owned(),
                                    type_args: None,
                                }));
                                self.current_scope
                                    .insert_type(param.name.name.clone(), t.clone());
                                Ok((param.name.name.to_owned(), t))
                            })
                            .collect::<Result<HashMap<String, Type>, Vec<TypeError>>>()?,
                        None => HashMap::default(),
                    };

                    let mut iter = params.iter_mut();
                    let mut is_mutating = false;
                    if !*is_static {
                        // TODO: raise an error if there's no first param
                        let param = iter.next(); // skip `self`
                        if let Some(param) = param {
                            if let PatternKind::Ident(binding_ident) = &param.pat.kind {
                                is_mutating = binding_ident.mutable;
                            }
                        }
                    }

                    let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = iter
                        .map(|e_param| {
                            if e_param.type_ann.is_none() {
                                return Err(vec![TypeError::MethodsMustHaveTypes]);
                            }

                            self.infer_fn_param(e_param, &type_params_map)
                        })
                        .collect();
                    let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

                    let mut interface_t = interface.t.as_ref().to_owned();
                    interface_t.mutable = is_mutating;
                    let binding = Binding {
                        mutable: false, // this should be false since we don't want to allow `self` to be re-assigned
                        t: interface_t,
                    };
                    self.current_scope
                        .insert_binding("self".to_string(), binding);

                    let (body_s, mut body_t) = self.infer_block_or_expr(body)?;
                    ss.push(body_s);

                    self.pop_scope();

                    if *is_async && !is_promise(&body_t) {
                        body_t = Type::from(TypeKind::Ref(TRef {
                            name: String::from("Promise"),
                            type_args: Some(vec![body_t]),
                        }))
                    }

                    if let Some(ret_type_ann) = return_type {
                        let (ret_s, ret_t) =
                            self.infer_type_ann_with_params(ret_type_ann, &type_params_map)?;
                        ss.push(ret_s);
                        ss.push(self.unify(&body_t, &ret_t)?);
                    } else if kind != &MethodKind::Setter {
                        return Err(vec![TypeError::MethodsMustHaveTypes]);
                    }

                    let name = TPropKey::StringKey(key.name.to_owned());

                    let elem = match kind {
                        MethodKind::Method => {
                            let ret_t = match &mut lambda.return_type {
                                Some(type_ann) => {
                                    let (s, t) = self
                                        .infer_type_ann_with_params(type_ann, &type_params_map)?;
                                    ss.push(s);
                                    t
                                }
                                None => {
                                    return Err(vec![TypeError::MethodsMustHaveTypes]);
                                }
                            };

                            let type_params = lambda.type_params.as_ref().map(|type_params| {
                                type_params
                                    .iter()
                                    .map(|type_param| TypeParam {
                                        name: type_param.name.name.to_owned(),
                                        constraint: None, // TODO
                                        default: None,    // TODO
                                    })
                                    .collect()
                            });

                            TObjElem::Method(TMethod {
                                name,
                                params: t_params,
                                ret: Box::from(ret_t),
                                type_params,
                                is_mutating,
                            })
                        }
                        MethodKind::Getter => {
                            let ret_t = match &mut lambda.return_type {
                                Some(type_ann) => {
                                    let (s, t) = self.infer_type_ann(type_ann, type_params)?;
                                    ss.push(s);
                                    t
                                }
                                None => body_t,
                            };
                            TObjElem::Getter(TGetter {
                                name,
                                ret: Box::from(ret_t),
                            })
                        }
                        MethodKind::Setter => {
                            if t_params.len() == 1 {
                                TObjElem::Setter(TSetter {
                                    name,
                                    param: t_params[0].to_owned(),
                                })
                            } else {
                                panic!("setters must be passed 'self' and one other parameter")
                            }
                        }
                    };

                    let s = compose_many_subs(&ss);
                    let t = elem.apply(&s);

                    if *is_static {
                        statics_elems.push(t.clone());
                    } else {
                        instance_elems.push(t);
                    }
                }
                ClassMember::Prop(ClassProp {
                    key,
                    value,
                    type_ann,
                    is_static,
                    is_optional,
                    is_mutable,
                }) => {
                    // TODO: make type_ann required in ClassProp
                    // TypeScript gets around this by inferring the property
                    // based on the value assigned to it in the constructor.
                    // We could maybe add support for that later.
                    // In the case of static properties, they must be assigned
                    // as part of the class declaration b/c it doesn't make sense
                    // to assign them in the constructor.
                    let (_s, t) = if let Some(type_ann) = type_ann {
                        self.infer_type_ann(type_ann, &mut None)?
                    } else if let Some(value) = value {
                        self.infer_expr(value, false)?
                    } else {
                        return Err(vec![TypeError::PropertiesMustHaveTypes]);
                        // (Subst::default(), self.current_scope.fresh_var())
                    };

                    let elem = TObjElem::Prop(TProp {
                        name: TPropKey::StringKey(key.name.to_owned()),
                        optional: *is_optional,
                        mutable: *is_mutable,
                        t,
                    });

                    if *is_static {
                        statics_elems.push(elem);
                    } else {
                        instance_elems.push(elem);
                    }
                }
            }
        }

        let statics_t = Type::from(TypeKind::Object(TObject {
            elems: statics_elems,
            is_interface: false,
        }));

        let instance_t = Type::from(TypeKind::Object(TObject {
            elems: instance_elems,
            is_interface: true,
        }));

        let type_params = class.type_params.as_ref().map(|type_params| {
            type_params
                .iter()
                .map(|type_param| TypeParam {
                    name: type_param.name.name.to_owned(),
                    constraint: None, // TODO
                    default: None,    // TODO
                })
                .collect()
        });

        let scheme = Scheme {
            t: Box::from(instance_t),
            // TODO: be consistent about using Option<Vec<>> for type_params
            type_params,
        };
        eprintln!("infer_class, scheme = {scheme}");
        self.current_scope.insert_scheme(class_name, scheme);

        // TODO: capture all of the subsitutions and return them
        let s = Subst::default();

        // NOTE: The caller is responsible for generalizing `statics_t` and adding
        // it to the context.
        Ok((s, statics_t))
    }

    fn infer_interface_from_class(&mut self, class: &mut Class) -> Result<Scheme, Vec<TypeError>> {
        let mut instance_elems: Vec<TObjElem> = vec![];

        for member in &mut class.body {
            match member {
                // Constructors are part of statics and thus not part of the interface
                ClassMember::Constructor(_) => (),
                ClassMember::Method(ClassMethod {
                    key,
                    kind,
                    lambda,
                    is_static,
                    is_mutating: _,
                }) => {
                    if *is_static {
                        continue;
                    }

                    let Lambda {
                        params,
                        type_params,
                        ..
                    } = lambda;

                    // TODO: aggregate method type params with class type params
                    // This maps type params to type refs with the same name.
                    let type_params_map: HashMap<String, Type> = match type_params {
                        Some(params) => params
                            .iter_mut()
                            .map(|param| {
                                let t = Type::from(TypeKind::Ref(TRef {
                                    name: param.name.name.to_owned(),
                                    type_args: None,
                                }));
                                self.current_scope
                                    .insert_type(param.name.name.clone(), t.clone());
                                Ok((param.name.name.to_owned(), t))
                            })
                            .collect::<Result<HashMap<String, Type>, Vec<TypeError>>>()?,
                        None => HashMap::default(),
                    };

                    let mut iter = params.iter_mut();
                    let mut is_mutating = false;

                    // TODO: raise an error if there's no first param
                    let self_param = iter.next(); // skip `self`
                    if let Some(self_param) = self_param {
                        if let PatternKind::Ident(binding_ident) = &self_param.pat.kind {
                            is_mutating = binding_ident.mutable;
                        }
                    }

                    // We create a new Scope here so that bindings inferred from
                    // function params aren't added to the current scope.
                    self.push_scope();
                    let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = iter
                        .map(|e_param| {
                            if e_param.type_ann.is_none() {
                                return Err(vec![TypeError::MethodsMustHaveTypes]);
                            }

                            self.infer_fn_param(e_param, &type_params_map)
                        })
                        .collect();
                    self.pop_scope();
                    let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

                    let name = TPropKey::StringKey(key.name.to_owned());

                    let elem = match kind {
                        MethodKind::Method => {
                            let ret_t = match &mut lambda.return_type {
                                Some(type_ann) => {
                                    let (s, t) = self
                                        .infer_type_ann_with_params(type_ann, &type_params_map)?;
                                    ss.push(s);
                                    t
                                }
                                None => {
                                    return Err(vec![TypeError::MethodsMustHaveTypes]);
                                }
                            };

                            let type_params = lambda.type_params.as_ref().map(|type_params| {
                                type_params
                                    .iter()
                                    .map(|type_param| TypeParam {
                                        name: type_param.name.name.to_owned(),
                                        constraint: None, // TODO
                                        default: None,    // TODO
                                    })
                                    .collect()
                            });

                            TObjElem::Method(TMethod {
                                name,
                                params: t_params,
                                ret: Box::from(ret_t),
                                type_params,
                                is_mutating,
                            })
                        }
                        MethodKind::Getter => {
                            let ret_t = match &mut lambda.return_type {
                                Some(type_ann) => {
                                    let (s, t) = self.infer_type_ann(type_ann, type_params)?;
                                    ss.push(s);
                                    t
                                }
                                None => {
                                    return Err(vec![TypeError::MethodsMustHaveTypes]);
                                }
                            };
                            TObjElem::Getter(TGetter {
                                name,
                                ret: Box::from(ret_t),
                            })
                        }
                        MethodKind::Setter => {
                            if t_params.len() == 1 {
                                TObjElem::Setter(TSetter {
                                    name,
                                    param: t_params[0].to_owned(),
                                })
                            } else {
                                panic!("setters must be passed 'self' and one other parameter")
                            }
                        }
                    };

                    let s = compose_many_subs(&ss);
                    let t = elem.apply(&s);

                    instance_elems.push(t);
                }
                ClassMember::Prop(ClassProp {
                    key,
                    value,
                    type_ann,
                    is_static,
                    is_optional,
                    is_mutable,
                }) => {
                    if *is_static {
                        continue;
                    }

                    // TODO: make type_ann required in ClassProp
                    // TypeScript gets around this by inferring the property
                    // based on the value assigned to it in the constructor.
                    // We could maybe add support for that later.
                    // In the case of static properties, they must be assigned
                    // as part of the class declaration b/c it doesn't make sense
                    // to assign them in the constructor.
                    let (_s, t) = if let Some(type_ann) = type_ann {
                        self.infer_type_ann(type_ann, &mut None)?
                    } else if let Some(value) = value {
                        self.infer_expr(value, false)?
                    } else {
                        return Err(vec![TypeError::PropertiesMustHaveTypes]);
                        // (Subst::default(), self.current_scope.fresh_var())
                    };

                    let elem = TObjElem::Prop(TProp {
                        name: TPropKey::StringKey(key.name.to_owned()),
                        optional: *is_optional,
                        mutable: *is_mutable,
                        t,
                    });

                    instance_elems.push(elem);
                }
            }
        }

        let instance_t = Type::from(TypeKind::Object(TObject {
            elems: instance_elems,
            is_interface: true,
        }));

        let type_params = class.type_params.as_ref().map(|type_params| {
            type_params
                .iter()
                .map(|type_param| TypeParam {
                    name: type_param.name.name.to_owned(),
                    constraint: None, // TODO
                    default: None,    // TODO
                })
                .collect()
        });

        let scheme = Scheme {
            t: Box::from(instance_t),
            // TODO: be consistent about using Option<Vec<>> for type_params
            type_params,
        };

        eprintln!("infer_interface_from_class, scheme = {scheme}");

        Ok(scheme)
    }
}
