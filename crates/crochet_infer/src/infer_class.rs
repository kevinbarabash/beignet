use crochet_ast::types::*;
use crochet_ast::values::{class::*, Lambda};
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
    let mut statics_elems: Vec<TObjElem> = vec![];
    let mut instance_elems: Vec<TObjElem> = vec![];
    let mut mut_instance_elems: Vec<TObjElem> = vec![];

    for member in &mut class.body {
        match member {
            ClassMember::Constructor(Constructor {
                params,
                body,
                type_params,
            }) => {
                ctx.push_scope(false); // Constructors cannot be async

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

                let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = params
                    .iter_mut()
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
                let (body_s, _body_t) = infer_expr(ctx, body)?;
                ss.push(body_s);

                ctx.pop_scope();

                let mut elem = TObjElem::Constructor(TCallable {
                    params: t_params,
                    ret: Box::from(ctx.fresh_var()), // This should be a TRef to the name of the class
                    type_params: vec![],             // TODO
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
                is_mutating,
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

                let params: Result<Vec<(Subst, TFnParam)>, Vec<TypeError>> = params
                    .iter_mut()
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

                let (body_s, mut body_t) = infer_expr(ctx, body)?;
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
                }

                let name = TPropKey::StringKey(key.name.to_owned());

                let mut elem = match kind {
                    MethodKind::Method => TObjElem::Method(TMethod {
                        name,
                        mutating: *is_mutating,
                        params: t_params,
                        ret: Box::from(body_t),
                        type_params: vec![], // TODO
                    }),
                    MethodKind::Getter => TObjElem::Getter(TGetter {
                        name,
                        ret: Box::from(body_t),
                    }),
                    MethodKind::Setter => {
                        if t_params.len() == 1 {
                            TObjElem::Setter(TSetter {
                                name,
                                param: t_params[0].to_owned(),
                            })
                        } else {
                            panic!("setters must have a single parameter")
                        }
                    }
                };

                let s = compose_many_subs(&ss);
                elem.apply(&s);

                if *is_static {
                    statics_elems.push(elem.clone());
                } else {
                    if !*is_mutating {
                        instance_elems.push(elem.clone());
                    }
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
                    infer_expr(ctx, value)?
                } else {
                    (Subst::default(), ctx.fresh_var())
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
                    // Properties are accessible from both mutable and immutable
                    // references, but can only be update from mutable ones.
                    instance_elems.push(elem.clone());
                    mut_instance_elems.push(elem);
                }
            }
        }
    }

    let statics_t = Type::from(TypeKind::Object(TObject {
        elems: statics_elems,
    }));

    let instance_t = Type::from(TypeKind::Object(TObject {
        elems: instance_elems,
    }));

    let mut_instance_t = Type::from(TypeKind::Object(TObject {
        elems: mut_instance_elems,
    }));

    let name = class.ident.name.to_owned();
    let empty_env = Env::default();

    ctx.insert_scheme(
        format!("Readonly{name}"),
        generalize(&empty_env, &instance_t),
    );
    ctx.insert_scheme(name, generalize(&empty_env, &mut_instance_t));

    // TODO: capture all of the subsitutions and return them
    let s = Subst::default();

    // NOTE: The caller is responsible for generalizing `statics_t` and adding
    // it to the context.
    Ok((s, statics_t))
}
