use std::collections::HashMap;
use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, freeze, Flag, Type, Scheme};

use super::context::Context;

// TODO: dedupe with infer_prog.rs
pub fn infer_scheme(type_ann: &TypeAnn, ctx: &Context) -> Scheme {
    let mapping: HashMap<String, i32> = match type_ann {
        TypeAnn::Lam(LamType { type_params, .. }) => {
            let mapping: HashMap<String, i32> = match type_params {
                Some(params) => params.iter().enumerate().map(|(index, param)| {
                    (param.name.name.to_owned(), index as i32)
                }).collect(),
                None => HashMap::default(),
            };
            mapping
        },
        _ => HashMap::new(),
    };

    // Infers the type from type annotation and replaces all type references whose names
    // appear in `mapping` with a type variable whose `id` is the value in the mapping.
    let type_ann_ty = infer_type_ann_with_params(type_ann, ctx, &mapping);

    // Creates a Scheme with the correct qualifiers for the type references that were
    // replaced with type variables.
    Scheme {
        qualifiers: mapping.values().cloned().collect(),
        ty: type_ann_ty,
    }
}

pub fn infer_type_ann(type_ann: &TypeAnn, ctx: &Context) -> Type {
    freeze(infer_type_ann_rec(type_ann, ctx, &HashMap::default()))
}

pub fn infer_type_ann_with_params(
    type_ann: &TypeAnn,
    ctx: &Context,
    mapping: &HashMap<String, i32>,
) -> Type {
    freeze(infer_type_ann_rec(type_ann, ctx, mapping))
}

fn infer_type_ann_rec(type_ann: &TypeAnn, ctx: &Context, mapping: &HashMap<String, i32>) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params, ret, .. }) => {
            let params: Vec<_> = params
                .iter()
                .map(|arg| {
                    let mut ty = infer_type_ann_rec(arg, ctx, mapping);
                    ty.flag = Some(Flag::SubtypeWins);
                    ty
                })
                .collect();
            let ret = Box::from(infer_type_ann_rec(ret.as_ref(), ctx, mapping));
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
                    ty: infer_type_ann_rec(prop.type_ann.as_ref(), ctx, mapping),
                })
                .collect();
            ctx.object(&props)
        }
        TypeAnn::TypeRef(TypeRef {
            name, type_params, ..
        }) => {
            match mapping.get(name) {
                Some(id) => Type {
                    variant: types::Variant::Var,
                    id: id.to_owned(),
                    frozen: false,
                    flag: None,
                },
                None => {
                    let type_params = type_params.clone().map(|params| {
                        params
                            .iter()
                            .map(|param| infer_type_ann_rec(param, ctx, mapping))
                            .collect()
                    });
                    ctx.alias(name, type_params)
                }
            }
        }
        TypeAnn::Union(UnionType { types, .. }) => ctx.union(
            types
                .iter()
                .map(|ty| infer_type_ann_rec(ty, ctx, mapping))
                .collect(),
        ),
        TypeAnn::Intersection(IntersectionType { types, .. }) => ctx.intersection(
            types
                .iter()
                .map(|ty| infer_type_ann_rec(ty, ctx, mapping))
                .collect(),
        ),
        TypeAnn::Tuple(TupleType { types, .. }) => ctx.tuple(
            types
                .iter()
                .map(|ty| infer_type_ann_rec(ty, ctx, mapping))
                .collect(),
        ),
    }
}
