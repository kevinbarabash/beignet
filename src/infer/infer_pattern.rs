use std::collections::HashMap;
use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, Scheme, Type};

use super::constraint_solver::Constraint;
use super::context::Context;
use super::infer_type_ann::infer_type_ann_with_params;

pub fn infer_pattern(
    pattern: &Pattern,
    ctx: &mut Context,
    constraints: &mut Vec<Constraint>,
    type_param_map: &HashMap<String, Type>,
) -> Type {
    _infer_pattern_rec(pattern, ctx, constraints, type_param_map)
}

// As a result of inferring a pattern, we must:
// - introduce new variables into the context
// - add new constraints between as needed
// We should probably have this recursive function accept a mutable Context
// and a mutable Vec<Constraint> and its wrapper can be responsible for creating
// these.
fn _infer_pattern_rec(
    pattern: &Pattern,
    ctx: &mut Context,
    cs: &mut Vec<Constraint>,
    type_param_map: &HashMap<String, Type>,
) -> Type {
    match pattern {
        Pattern::Ident(BindingIdent { id, type_ann, .. }) => {
            match type_ann {
                Some(type_ann) => {
                    // Infers the type from type annotation and replaces all type references whose names
                    // appear in `mapping` with a type variable whose `id` is the value in the mapping.
                    let type_ann_ty = infer_type_ann_with_params(type_ann, ctx, type_param_map);
                    ctx.values.insert(id.name.to_owned(), type_to_scheme(&type_ann_ty));
                    type_ann_ty
                }
                None => {
                    let tv = ctx.fresh_var();
                    ctx.values.insert(id.name.to_owned(), type_to_scheme(&tv));
                    tv
                }
            }
        }
        Pattern::Rest(RestPat { arg, .. }) => _infer_pattern_rec(arg.as_ref(), ctx, cs, type_param_map),
        Pattern::Array(ArrayPat { elems, .. }) => {
            let elems: Vec<Type> = elems
                .iter()
                .map(|elem| {
                    match elem {
                        Some(elem) => match elem {
                            Pattern::Rest(rest) => {
                                let rest_ty = _infer_pattern_rec(rest.arg.as_ref(), ctx, cs, type_param_map);
                                ctx.rest(rest_ty)
                            }
                            _ => _infer_pattern_rec(elem, ctx, cs, type_param_map),
                        },
                        None => {
                            // TODO: figure how to ignore gaps in the array
                            todo!()
                        }
                    }
                })
                .collect();

            ctx.tuple(elems)
        }
        Pattern::Object(ObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let props: Vec<types::TProp> = props
                .iter()
                .filter_map(|prop| {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        ObjectPatProp::KeyValue(KeyValuePatProp { key, value: _ }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.

                            let tv = ctx.fresh_var();
                            let scheme = type_to_scheme(&tv);
                            ctx.values.insert(key.name.to_owned(), scheme);

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: tv,
                            })
                        }
                        ObjectPatProp::Assign(AssignPatProp { key, value: _, .. }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.

                            let tv = ctx.fresh_var();
                            let scheme = type_to_scheme(&tv);
                            ctx.values.insert(key.name.to_owned(), scheme);

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: tv,
                            })
                        }
                        ObjectPatProp::Rest(rest) => {
                            // TypeScript doesn't support spreading/rest in types so instead we
                            // need to turn:
                            // {x, y, ...rest}
                            // into:
                            // {x: A, y: B} & C
                            // we also need some way to specify that C is an object type of some
                            // sort... maybe it'll just fall out when we trying to unify some other
                            // object type with an intersection type.
                            // essentially C = other_object_type - {x: A, y: B}
                            // TODO: panic if rest_opt_ty is not None, it means that the parser has
                            // failed to ensure that there's only one rest pattern in an object pattern
                            rest_opt_ty = Some(_infer_pattern_rec(rest.arg.as_ref(), ctx, cs, type_param_map));
                            None
                        }
                    }
                })
                .collect();

            let obj_type = ctx.object(&props);

            match rest_opt_ty {
                Some(rest_ty) => ctx.intersection(vec![obj_type, rest_ty]),
                None => obj_type,
            }
        }
    }
}

fn type_to_scheme(ty: &Type) -> Scheme {
    Scheme {
        qualifiers: vec![],
        ty: ty.clone(),
    }
}
