use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, freeze, Type, Flag};

use super::context::Context;

pub fn infer_type_ann(type_ann: &TypeAnn, ctx: &Context) -> Type {
    freeze(infer_type_ann_rec(type_ann, ctx))
}

fn infer_type_ann_rec(type_ann: &TypeAnn, ctx: &Context) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params, ret, .. }) => {
            let params: Vec<_> = params
                .iter()
                .map(|arg| {
                    let mut ty = infer_type_ann_rec(arg, ctx);
                    ty.flag = Some(Flag::SubtypeWins);
                    ty
                })
                .collect();
            let ret = Box::from(infer_type_ann_rec(ret.as_ref(), ctx));
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
                    ty: infer_type_ann_rec(prop.type_ann.as_ref(), ctx),
                })
                .collect();
            ctx.object(&props)
        }
        TypeAnn::TypeRef(TypeRef {
            name, type_params, ..
        }) => {
            let type_params = type_params.clone().map(|params| {
                params
                    .iter()
                    .map(|param| infer_type_ann_rec(param, ctx))
                    .collect()
            });
            ctx.alias(name, type_params)
        }
        TypeAnn::Union(UnionType { types, .. }) => {
            ctx.union(types.iter().map(|ty| infer_type_ann_rec(ty, ctx)).collect())
        }
        TypeAnn::Intersection(IntersectionType { types, .. }) => {
            ctx.intersection(types.iter().map(|ty| infer_type_ann_rec(ty, ctx)).collect())
        }
        TypeAnn::Tuple(TupleType { types, .. }) => {
            ctx.tuple(types.iter().map(|ty| infer_type_ann_rec(ty, ctx)).collect())
        }
    }
}
