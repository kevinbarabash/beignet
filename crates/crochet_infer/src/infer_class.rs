use crochet_ast::types::*;
use crochet_ast::values::{class::*, Lambda, PatternKind};
use std::collections::HashMap;

use crate::context::{Context, Env};
use crate::infer_expr::infer_expr;
use crate::infer_fn_param::infer_fn_param;
use crate::infer_type_ann::*;
use crate::scheme::generalize;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::unify::unify;
use crate::util::compose_many_subs;

fn is_promise(t: &Type) -> bool {
    matches!(&t, Type {kind: TypeKind::Ref(TRef { name, .. }), ..} if name == "Promise")
}

pub fn infer_class(ctx: &mut Context, class: &mut Class) -> Result<(Subst, Type), Vec<TypeError>> {
    let class_name = class.ident.name.to_owned();

    let mut statics_elems: Vec<TObjElem> = vec![];
    let mut mut_instance_elems: Vec<TObjElem> = vec![];

    for member in &mut class.body {
        match member {
            ClassMember::Constructor(Constructor {
                params,
                body,
                type_params,
            }) => {
                ctx.push_scope(false); // Constructors cannot be async

                // QUESTION: should we skip the first param when computing
                // `type_params_map`?
                let type_params_map: HashMap<String, Type> = match type_params {
                    Some(params) => {
                        let mut iter = params.iter_mut();
                        iter.next(); // skip `self`
                        iter.map(|param| {
                            let tv = match &mut param.constraint {
                                Some(type_ann) => {
                                    // TODO: push `s` on to `ss`
                                    let (_s, t) = infer_type_ann(type_ann, ctx, &mut None)?;
                                    Type::from(TypeKind::Var(TVar {
                                        id: ctx.fresh_id(),
                                        constraint: Some(Box::from(t)),
                                    }))
                                }
                                None => {
                                    return Err(vec![TypeError::MethodsMustHaveTypes]);
                                    // ctx.fresh_var()
                                }
                            };
                            ctx.insert_type(param.name.name.clone(), tv.clone());
                            Ok((param.name.name.to_owned(), tv))
                        })
                        .collect::<Result<HashMap<String, Type>, Vec<TypeError>>>()?
                    }
                    None => HashMap::default(),
                };

                let mut iter = params.iter_mut();
                iter.next(); // skip `self`
                let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = iter
                    .map(|e_param| {
                        let (ps, pa, t_param) = infer_fn_param(e_param, ctx, &type_params_map)?;

                        // Inserts any new variables introduced by infer_fn_param() into
                        // the current context.
                        for (name, binding) in pa {
                            ctx.insert_binding(name, binding);
                        }

                        Ok((ps, t_param))
                    })
                    .collect();
                let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

                // TODO: ensure that constructors don't have a return statement
                // TODO: add `self` and `Self` to ctx
                let (body_s, _body_t) = infer_expr(ctx, body, false)?;
                ss.push(body_s);

                ctx.pop_scope();

                let mut elem = TObjElem::Constructor(TCallable {
                    params: t_params,
                    ret: Box::from(Type::from(TypeKind::Ref(TRef {
                        name: class_name.to_owned(),
                        type_args: None,
                    }))),
                    type_params: vec![], // TODO
                });

                let s = compose_many_subs(&ss);
                elem.apply(&s);

                statics_elems.push(elem);
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

                ctx.push_scope(is_async.to_owned());

                // QUESTION: should we skip the first param when computing
                // `type_params_map`?
                let type_params_map: HashMap<String, Type> = match type_params {
                    Some(params) => params
                        .iter_mut()
                        .map(|param| {
                            let tv = match &mut param.constraint {
                                Some(type_ann) => {
                                    // TODO: push `s` on to `ss`
                                    let (_s, t) = infer_type_ann(type_ann, ctx, &mut None)?;
                                    Type::from(TypeKind::Var(TVar {
                                        id: ctx.fresh_id(),
                                        constraint: Some(Box::from(t)),
                                    }))
                                }
                                None => ctx.fresh_var(),
                            };
                            ctx.insert_type(param.name.name.clone(), tv.clone());
                            Ok((param.name.name.to_owned(), tv))
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

                        let (ps, pa, t_param) = infer_fn_param(e_param, ctx, &type_params_map)?;

                        // Inserts any new variables introduced by infer_fn_param() into
                        // the current context.
                        for (name, binding) in pa {
                            ctx.insert_binding(name, binding);
                        }

                        Ok((ps, t_param))
                    })
                    .collect();
                let (mut ss, t_params): (Vec<_>, Vec<_>) = params?.iter().cloned().unzip();

                // TODO: add `self` and `Self` to ctx
                let (body_s, mut body_t) = infer_expr(ctx, body, false)?;
                ss.push(body_s);

                ctx.pop_scope();

                if *is_async && !is_promise(&body_t) {
                    body_t = Type::from(TypeKind::Ref(TRef {
                        name: String::from("Promise"),
                        type_args: Some(vec![body_t]),
                    }))
                }

                if let Some(ret_type_ann) = return_type {
                    let (ret_s, mut ret_t) =
                        infer_type_ann_with_params(ret_type_ann, ctx, &type_params_map)?;
                    ss.push(ret_s);
                    ss.push(unify(&mut body_t, &mut ret_t, ctx)?);
                } else if kind != &MethodKind::Setter {
                    return Err(vec![TypeError::MethodsMustHaveTypes]);
                }

                let name = TPropKey::StringKey(key.name.to_owned());

                let mut elem = match kind {
                    MethodKind::Method => {
                        let ret_t = match &mut lambda.return_type {
                            Some(type_ann) => {
                                let (s, t) = infer_type_ann(type_ann, ctx, type_params)?;
                                ss.push(s);
                                t
                            }
                            None => body_t,
                        };
                        TObjElem::Method(TMethod {
                            name,
                            params: t_params,
                            ret: Box::from(ret_t),
                            type_params: vec![], // TODO
                            is_mutating,
                        })
                    }
                    MethodKind::Getter => {
                        let ret_t = match &mut lambda.return_type {
                            Some(type_ann) => {
                                let (s, t) = infer_type_ann(type_ann, ctx, type_params)?;
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
                elem.apply(&s);

                if *is_static {
                    statics_elems.push(elem.clone());
                } else {
                    mut_instance_elems.push(elem);
                }
            }
            ClassMember::Prop(ClassProp {
                key,
                value,
                type_ann,
                is_static,
                is_optional,
            }) => {
                // TODO: make type_ann required in ClassProp
                // TypeScript gets around this by inferring the property
                // based on the value assigned to it in the constructor.
                // We could maybe add support for that later.
                // In the case of static properties, they must be assigned
                // as part of the class declaration b/c it doesn't make sense
                // to assign them in the constructor.
                let (_s, t) = if let Some(type_ann) = type_ann {
                    infer_type_ann(type_ann, ctx, &mut None)?
                } else if let Some(value) = value {
                    infer_expr(ctx, value, false)?
                } else {
                    return Err(vec![TypeError::PropertiesMustHaveTypes]);
                    // (Subst::default(), ctx.fresh_var())
                };

                let elem = TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(key.name.to_owned()),
                    optional: *is_optional,
                    mutable: false, // TODO
                    t,
                });

                if *is_static {
                    statics_elems.push(elem);
                } else {
                    mut_instance_elems.push(elem);
                }
            }
        }
    }

    let statics_t = Type::from(TypeKind::Object(TObject {
        elems: statics_elems,
        is_interface: false,
    }));

    let instance_t = Type::from(TypeKind::Object(TObject {
        elems: mut_instance_elems,
        is_interface: true,
    }));

    let empty_env = Env::default();

    ctx.insert_scheme(class_name, generalize(&empty_env, &instance_t));

    // TODO: capture all of the subsitutions and return them
    let s = Subst::default();

    // NOTE: The caller is responsible for generalizing `statics_t` and adding
    // it to the context.
    Ok((s, statics_t))
}
