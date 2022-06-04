use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, freeze, Scheme, Type};

use super::constraint_solver::Constraint;
use super::context::Context;

pub fn infer_pattern(
    pattern: &Pattern,
    ctx: &mut Context,
    constraints: &mut Vec<Constraint>,
) -> Type {

    let inf_type = _infer_pattern_rec(pattern, ctx, constraints);

    if let Some(ta_type) = _get_type_ann_type(pattern, ctx) {
        constraints.push(Constraint { types: (inf_type, ta_type.clone()) });
        ta_type
    } else {
        inf_type
    }
}

fn _get_type_ann_type(pattern: &Pattern, ctx: &Context) -> Option<Type> {
    let type_ann = match pattern {
        Pattern::Ident(ident) => ident.type_ann.to_owned(),
        Pattern::Rest(rest) => rest.type_ann.to_owned(),
        Pattern::Object(obj) => obj.type_ann.to_owned(),
        Pattern::Array(array) => array.type_ann.to_owned(),
    };

    type_ann.map(|type_ann| type_ann_to_type(&type_ann, ctx))
}

// As a result of inferring a pattern, we must:
// - introduce new variables into the context
// - add new constraints between as needed
// We should probably have this recursive function accept a mutable Context
// and a mutable Vec<Constraint> and its wrapper can be responsible for creating
// these.
fn _infer_pattern_rec(pattern: &Pattern, ctx: &mut Context, cs: &mut Vec<Constraint>) -> Type {
    match pattern {
        Pattern::Ident(BindingIdent { id, .. }) => {
            let tv = ctx.fresh_var();
            ctx.values.insert(id.name.to_owned(), type_to_scheme(&tv));
            tv
        }
        Pattern::Rest(RestPat { arg, .. }) => {
            _infer_pattern_rec(arg.as_ref(), ctx, cs)
        }
        Pattern::Array(ArrayPat { elems, .. }) => {
            let elems: Vec<Type> = elems.iter().map(|elem| {
                match elem {
                    Some(elem) => {
                        match elem {
                            Pattern::Rest(rest) => {
                                let rest_ty = _infer_pattern_rec(rest.arg.as_ref(), ctx, cs);
                                ctx.rest(rest_ty)
                            },
                            _ => _infer_pattern_rec(elem, ctx, cs)
                        }
                    },
                    None => {
                        // TODO: figure how to ignore gaps in the array
                        todo!()
                    },
                }
            }).collect();

            ctx.tuple(elems)
        },
        Pattern::Object(ObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let props: Vec<types::TProp> = props.iter().filter_map(|prop| {
                match prop {
                    // re-assignment, e.g. {x: new_x, y: new_y} = point
                    ObjectPatProp::KeyValue(KeyValuePatProp { key, value: _ }) => {
                        // We ignore the value for now, we can come back later to handle
                        // default values.

                        let tv = ctx.fresh_var();
                        let scheme = type_to_scheme(&tv);
                        ctx.values.insert(key.name.to_owned(), scheme);

                        Some(
                            types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: tv,
                            }
                        )
                    },
                    ObjectPatProp::Assign(AssignPatProp { key, value: _, .. }) => {
                        // We ignore the value for now, we can come back later to handle
                        // default values.

                        let tv = ctx.fresh_var();
                        let scheme = type_to_scheme(&tv);
                        ctx.values.insert(key.name.to_owned(), scheme);

                        Some(
                            types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                ty: tv,
                            }
                        )
                    },
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
                        rest_opt_ty = Some(_infer_pattern_rec(rest.arg.as_ref(), ctx, cs));
                        None
                    },
                }
            }).collect();

            let obj_type = ctx.object(&props, None);

            match rest_opt_ty {
                Some(rest_ty) => ctx.intersection(vec![obj_type, rest_ty]),
                None => obj_type,
            }
        },
    }
}


fn type_to_scheme(ty: &Type) -> Scheme {
    Scheme {
        qualifiers: vec![],
        ty: ty.clone(),
    }
}

pub fn type_ann_to_type(type_ann: &TypeAnn, ctx: &Context) -> Type {
    freeze(_type_ann_to_type(type_ann, ctx))
}

fn _type_ann_to_type(type_ann: &TypeAnn, ctx: &Context) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params, ret, .. }) => {
            let params: Vec<_> = params
                .iter()
                .map(|arg| _type_ann_to_type(arg, ctx))
                .collect();
            let ret = Box::from(_type_ann_to_type(ret.as_ref(), ctx));
            ctx.lam(params, ret)
        }
        TypeAnn::Lit(LitType { lit, .. }) => ctx.lit(lit.to_owned()),
        TypeAnn::Prim(PrimType { prim, .. }) => ctx.prim(prim.to_owned()),
        TypeAnn::Object(ObjectType { props, .. }) => {
            let props: Vec<_> = props
                .iter()
                .map(|prop| types::TProp {
                    name: prop.name.to_owned(),
                    optional: prop.optional,
                    ty: _type_ann_to_type(prop.type_ann.as_ref(), ctx),
                })
                .collect();
            ctx.object(&props, None)
        }
        TypeAnn::TypeRef(TypeRef {
            name, type_params, ..
        }) => {
            let type_params = type_params.clone().map(|params| {
                params
                    .iter()
                    .map(|param| _type_ann_to_type(param, ctx))
                    .collect()
            });
            ctx.alias(name, type_params)
        }
        TypeAnn::Union(UnionType { types, .. }) => {
            ctx.union(types.iter().map(|ty| _type_ann_to_type(ty, ctx)).collect())
        }
        TypeAnn::Intersection(IntersectionType { types, .. }) => {
            ctx.intersection(types.iter().map(|ty| _type_ann_to_type(ty, ctx)).collect())
        }
        TypeAnn::Tuple(TupleType { types, .. }) => {
            ctx.tuple(types.iter().map(|ty| _type_ann_to_type(ty, ctx)).collect())
        }
    }
}
