use std::collections::HashMap;
use std::iter::Iterator;

use crate::ast::*;
use crate::infer::constraint_solver::Constraint;
use crate::types::{self, Scheme, Type};

use super::context::Context;
use super::infer_type_ann::infer_type_ann_with_params;

type PatInfData = (Type, Vec<Constraint>, HashMap<String, Scheme>);

// NOTE: The caller is responsible for inserting any new variables introduced
// into the appropriate context.
pub fn infer_pattern(
    pat: &Pattern,
    ctx: &mut Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<PatInfData, String> {
    // Keeps track of all of the variables the need to be introduced by this pattern.
    let mut new_vars: HashMap<String, Scheme> = HashMap::new();

    let pat_ty = infer_pattern_rec(pat, ctx, &mut new_vars)?;

    // If the pattern had a type annotation associated with it, we infer type of the
    // type annotation and add a constraint between the types of the pattern and its
    // type annotation.
    match get_type_ann(pat) {
        Some(type_ann) => {
            let type_ann_ty = infer_type_ann_with_params(&type_ann, ctx, type_param_map);
            let cs = vec![Constraint {
                types: (type_ann_ty.clone(), pat_ty),
            }];
            Ok((type_ann_ty, cs, new_vars))
        }
        None => Ok((pat_ty, vec![], new_vars)),
    }
}

fn get_type_ann(pat: &Pattern) -> Option<TypeAnn> {
    match pat {
        Pattern::Ident(BindingIdent { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Rest(RestPat { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Object(ObjectPat { type_ann, .. }) => type_ann.to_owned(),
        Pattern::Array(ArrayPat { type_ann, .. }) => type_ann.to_owned(),
    }
}

fn infer_pattern_rec(
    pattern: &Pattern,
    ctx: &mut Context,
    new_vars: &mut HashMap<String, Scheme>,
) -> Result<Type, String> {
    match pattern {
        Pattern::Ident(BindingIdent { id, .. }) => {
            let tv = ctx.fresh_var();
            let scheme = Scheme::from(&tv);
            if new_vars.insert(id.name.to_owned(), scheme).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(tv)
        }
        Pattern::Rest(RestPat { arg, .. }) => infer_pattern_rec(arg.as_ref(), ctx, new_vars),
        Pattern::Array(ArrayPat { elems, .. }) => {
            let elems: Result<Vec<Type>, String> = elems
                .iter()
                .map(|elem| {
                    match elem {
                        Some(elem) => match elem {
                            Pattern::Rest(rest) => {
                                let rest_ty = infer_pattern_rec(rest.arg.as_ref(), ctx, new_vars)?;
                                Ok(ctx.rest(rest_ty))
                            }
                            _ => infer_pattern_rec(elem, ctx, new_vars),
                        },
                        None => {
                            // TODO: figure how to ignore gaps in the array
                            todo!()
                        }
                    }
                })
                .collect();

            Ok(ctx.tuple(elems?))
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
                            let scheme = Scheme::from(&tv);
                            if new_vars.insert(key.name.to_owned(), scheme).is_some() {
                                todo!("return an error");
                            }
                            // ctx.values.insert(key.name.to_owned(), scheme);

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: tv,
                            })
                        }
                        ObjectPatProp::Assign(AssignPatProp { key, value: _, .. }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.
                            println!("AssignPatProp = {:#?}", key);

                            let tv = ctx.fresh_var();
                            let scheme = Scheme::from(&tv);
                            if new_vars.insert(key.name.to_owned(), scheme).is_some() {
                                todo!("return an error");
                            }
                            // ctx.values.insert(key.name.to_owned(), scheme);

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

                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            rest_opt_ty = infer_pattern_rec(rest.arg.as_ref(), ctx, new_vars).ok();
                            None
                        }
                    }
                })
                .collect();

            let obj_type = ctx.object(&props);

            match rest_opt_ty {
                Some(rest_ty) => Ok(ctx.intersection(vec![obj_type, rest_ty])),
                None => Ok(obj_type),
            }
        }
    }
}
