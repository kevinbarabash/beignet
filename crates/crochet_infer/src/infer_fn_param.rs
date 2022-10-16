use crochet_ast::*;
use crochet_types::{self as types, TFnParam, TKeyword, TObject, TPat, Type};

use crate::assump::Assump;
use crate::context::Context;
use crate::infer_type_ann::*;
use crate::substitutable::{Subst, Substitutable};
use crate::unify::unify;
use crate::util::compose_subs;

// NOTE: The caller is responsible for inserting any new variables introduced
// into the appropriate context.
pub fn infer_fn_param(
    param: &mut EFnParam,
    ctx: &mut Context,
    type_param_map: &Assump,
) -> Result<(Subst, Assump, TFnParam), String> {
    // Keeps track of all of the variables the need to be introduced by this pattern.
    let mut new_vars: Assump = Assump::default();

    let pat_type = infer_param_pattern_rec(&param.pat, ctx, &mut new_vars)?;
    let pat = e_pat_to_t_pat(&param.pat);

    // TODO: convert EFnParamPat to Pattern perhaps we can just get rid of EFnParamPat
    // Use infer_let as a model for this

    // If the pattern had a type annotation associated with it, we infer type of the
    // type annotation and add a constraint between the types of the pattern and its
    // type annotation.
    match &mut param.type_ann {
        Some(type_ann) => {
            let (type_ann_s, type_ann_t) =
                infer_type_ann_with_params(type_ann, ctx, type_param_map)?;

            // We probably shouldn't have reversed this.  We did it so that the
            // substitutions would come out in the correct direction.
            let s = unify(&pat_type, &type_ann_t, ctx)?;
            let s = compose_subs(&s, &type_ann_s);

            // Substs are applied to any new variables introduced.  This handles
            // the situation where explicit types have be provided for function
            // parameters.
            let mut a = new_vars.apply(&s);

            if param.optional {
                match a.iter().find(|(_, value)| type_ann_t == **value) {
                    Some((name, t)) => {
                        let t = Type::Union(vec![t.to_owned(), Type::Keyword(TKeyword::Undefined)]);
                        a.insert(name.to_owned(), t);
                    }
                    None => (),
                };
            }

            let param = TFnParam {
                pat,
                // We don't modify the type annotation for the param since the optionality
                // is already tracked by the `optional` field.
                t: type_ann_t,
                optional: param.optional,
            };

            Ok((s, a, param))
        }
        None => {
            if param.optional {
                match new_vars.iter().find(|(_, value)| pat_type == **value) {
                    Some((name, t)) => {
                        let t = Type::Union(vec![t.to_owned(), Type::Keyword(TKeyword::Undefined)]);
                        new_vars.insert(name.to_owned(), t);
                    }
                    None => (),
                };
            }

            let param = TFnParam {
                pat,
                t: pat_type,
                optional: param.optional,
            };

            Ok((Subst::new(), new_vars, param))
        }
    }
}

pub fn e_pat_to_t_pat(e_pat: &EFnParamPat) -> TPat {
    match e_pat {
        EFnParamPat::Ident(e_bi) => TPat::Ident(types::BindingIdent {
            name: e_bi.id.name.to_owned(),
            mutable: false,
        }),
        EFnParamPat::Rest(e_rest) => TPat::Rest(types::RestPat {
            arg: Box::from(e_pat_to_t_pat(e_rest.arg.as_ref())),
        }),
        EFnParamPat::Object(e_obj) => {
            // TODO: replace TProp with the type equivalent of EFnParamObjectPatProp
            let props: Vec<types::TObjectPatProp> = e_obj
                .props
                .iter()
                .map(|e_prop| {
                    match e_prop {
                        EFnParamObjectPatProp::KeyValue(kv) => {
                            types::TObjectPatProp::KeyValue(types::TObjectKeyValuePatProp {
                                key: kv.key.name.to_owned(),
                                value: e_pat_to_t_pat(&kv.value),
                            })
                        }
                        EFnParamObjectPatProp::Assign(assign) => {
                            types::TObjectPatProp::Assign(types::TObjectAssignPatProp {
                                key: assign.key.name.to_owned(),
                                // TODO: figure when/how to set this to a non-None value
                                value: None,
                            })
                        }
                        EFnParamObjectPatProp::Rest(rest) => {
                            types::TObjectPatProp::Rest(types::RestPat {
                                arg: Box::from(e_pat_to_t_pat(rest.arg.as_ref())),
                            })
                        }
                    }
                })
                .collect();
            TPat::Object(types::TObjectPat { props })
        }
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
            if assump.insert(id.name.to_owned(), tv.clone()).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(tv)
        }
        EFnParamPat::Rest(EFnParamRestPat { arg, .. }) => {
            let t = infer_param_pattern_rec(arg.as_ref(), ctx, assump)?;
            Ok(Type::Array(Box::from(t)))
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
                                Ok(Type::Rest(Box::from(rest_ty)))
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

            Ok(Type::Tuple(elems?))
        }
        // TODO: infer type_params
        EFnParamPat::Object(EFnParamObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let elems: Vec<types::TObjElem> = props
                .iter()
                .filter_map(|prop| {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        EFnParamObjectPatProp::KeyValue(EFnParamKeyValuePatProp { key, value }) => {
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            let value_type = infer_param_pattern_rec(value, ctx, assump).unwrap();

                            Some(types::TObjElem::Prop(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                t: value_type,
                            }))
                        }
                        EFnParamObjectPatProp::Assign(EFnParamAssignPatProp {
                            key,
                            value: _,
                            ..
                        }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.
                            // TODO: handle default values

                            let tv = ctx.fresh_var();
                            if assump.insert(key.name.to_owned(), tv.clone()).is_some() {
                                todo!("return an error");
                            }

                            Some(types::TObjElem::Prop(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                t: tv,
                            }))
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

            let obj_type = Type::Object(TObject { elems });

            match rest_opt_ty {
                Some(rest_ty) => Ok(Type::Intersection(vec![obj_type, rest_ty])),
                None => Ok(obj_type),
            }
        }
    }
}
