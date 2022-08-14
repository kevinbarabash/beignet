use std::collections::HashMap;

use crochet_ast::*;

use super::context::Context;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
use super::types::{self, Scheme, TFnParam, TPat, Type};
use super::unify::unify;

pub type Assump = HashMap<String, Scheme>;

pub fn infer_fn_param(
    param: &EFnParam,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Assump, TFnParam), String> {
    println!("infer_fn_param");
    // Keeps track of all of the variables the need to be introduced by this pattern.
    let mut new_vars: HashMap<String, Scheme> = HashMap::new();

    let pat_type = infer_param_pattern_rec(&param.pat, ctx, &mut new_vars)?;

    // let pat = match &param.pat {
    //     EFnParamPat::Ident(e_bi) => TPat::Ident(types::BindingIdent {
    //         name: e_bi.id.name.to_owned(),
    //         optional: false,
    //         mutable: false,
    //     }),
    //     EFnParamPat::Rest(e_rest) => {
    //         // let rest_arg = infer_param_pattern_rec(rest.arg.as_ref(), ctx, &mut new_vars)?;
    //         TPat::Rest(types::RestPat {
    //             // TODO: replace this with the actual rest arg
    //             arg: Box::from(TPat::Ident(types::BindingIdent {
    //                 name: String::from("rest_placeholder"),
    //                 optional: false,
    //                 mutable: false,
    //             })),
    //         })
    //     }
    //     EFnParamPat::Object(_) => todo!(),
    //     EFnParamPat::Array(_) => todo!(),
    // };
    let pat = e_pat_to_t_pat(&param.pat);

    // If the pattern had a type annotation associated with it, we infer type of the
    // type annotation and add a constraint between the types of the pattern and its
    // type annotation.
    match &param.type_ann {
        Some(type_ann) => {
            let type_ann_ty = infer_type_ann_with_params(type_ann, ctx, type_param_map);

            // Allowing type_ann_ty to be a subtype of pat_type because
            // only non-refutable patterns can have type annotations.
            let s = unify(&pat_type, &type_ann_ty, ctx)?;

            // Substs are applied to any new variables introduced.  This handles
            // the situation where explicit types have be provided for function
            // parameters.
            let a = new_vars.apply(&s);

            let param = TFnParam {
                pat,
                ty: type_ann_ty,
            };

            Ok((s, a, param))
        }
        None => {
            let param = TFnParam { pat, ty: pat_type };

            Ok((Subst::new(), new_vars, param))
        }
    }
}

fn e_pat_to_t_pat(e_pat: &EFnParamPat) -> TPat {
    match e_pat {
        EFnParamPat::Ident(e_bi) => TPat::Ident(types::BindingIdent {
            name: e_bi.id.name.to_owned(),
            optional: false,
            mutable: false,
        }),
        EFnParamPat::Rest(e_rest) => {
            TPat::Rest(types::RestPat {
                arg: Box::from(e_pat_to_t_pat(e_rest.arg.as_ref())),
            })
        }
        EFnParamPat::Object(e_obj) => {
            // let props: Vec<types::TProp> = e_obj.props.iter().map(|e_prop| {
            //     match e_prop {
            //         EFnParamObjectPatProp::KeyValue(_) => todo!(),
            //         EFnParamObjectPatProp::Assign(_) => todo!(),
            //         EFnParamObjectPatProp::Rest(_) => todo!(),
            //     }
            // });
            todo!()
        },
        EFnParamPat::Array(e_array) => {
            TPat::Array(types::ArrayPat {
                // TODO: fill in gaps in array patterns with types from the corresponding
                // type annotation if one exists.
                elems: e_array
                    .elems
                    .iter()
                    .map(|elem| elem.as_ref().map(e_pat_to_t_pat))
                    .collect(),
            })
        }
    }
}

fn infer_param_pattern_rec(
    pat: &EFnParamPat,
    ctx: &Context,
    assump: &mut Assump,
) -> Result<Type, String> {
    match pat {
        EFnParamPat::Ident(EFnParamBindingIdent { id, .. }) => {
            let tv = ctx.fresh_var();
            let scheme = Scheme::from(&tv);
            if assump.insert(id.name.to_owned(), scheme).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(tv)
        }
        EFnParamPat::Rest(EFnParamRestPat { arg, .. }) => {
            // TODO: wrap this in an array type
            let t = infer_param_pattern_rec(arg.as_ref(), ctx, assump)?;
            Ok(ctx.rest(t))
        }
        EFnParamPat::Array(EFnParamArrayPat { elems, .. }) => {
            let elems: Result<Vec<Type>, String> = elems
                .iter()
                .map(|elem| {
                    match elem {
                        Some(elem) => match elem {
                            EFnParamPat::Rest(rest) => {
                                let rest_ty =
                                    infer_param_pattern_rec(rest.arg.as_ref(), ctx, assump)?;
                                Ok(ctx.rest(rest_ty))
                            }
                            _ => infer_param_pattern_rec(elem, ctx, assump),
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
        EFnParamPat::Object(EFnParamObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let props: Vec<types::TProp> = props
                .iter()
                .filter_map(|prop| {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        EFnParamObjectPatProp::KeyValue(EFnParamKeyValuePatProp { key, value }) => {
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            let value_type = infer_param_pattern_rec(value, ctx, assump).unwrap();

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                ty: value_type,
                            })
                        }
                        EFnParamObjectPatProp::Assign(EFnParamAssignPatProp { key, value: _, .. }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.
                            // TODO: handle default values

                            let tv = ctx.fresh_var();
                            let scheme = Scheme::from(&tv);
                            if assump.insert(key.name.to_owned(), scheme).is_some() {
                                todo!("return an error");
                            }

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                ty: tv,
                            })
                        }
                        EFnParamObjectPatProp::Rest(rest) => {
                            if rest_opt_ty.is_some() {
                                // TODO: return an Err() instead of panicking.
                                panic!("Maximum one rest pattern allowed in object patterns")
                            }
                            // TypeScript doesn't support spreading/rest in types so instead we
                            // do the following conversion:
                            // {x, y, ...rest} -> {x: A, y: B} & C
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            rest_opt_ty =
                                infer_param_pattern_rec(rest.arg.as_ref(), ctx, assump).ok();
                            None
                        }
                    }
                })
                .collect();

            let obj_type = ctx.object(props);

            match rest_opt_ty {
                Some(rest_ty) => Ok(ctx.intersection(vec![obj_type, rest_ty])),
                None => Ok(obj_type),
            }
        }
    }
}
