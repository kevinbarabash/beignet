use std::collections::HashMap;
use std::iter::Iterator;

use crochet_ast::*;

use super::context::Context;
use super::types::{freeze, Scheme, TProp, Type};

pub fn infer_scheme(type_ann: &TypeAnn, ctx: &Context) -> Scheme {
    match type_ann {
        TypeAnn::Lam(LamType { type_params, .. }) => {
            infer_scheme_with_type_params(type_ann, type_params, ctx)
        }
        _ => Scheme::from(infer_type_ann(type_ann, ctx)),
    }
}

pub fn infer_scheme_with_type_params(
    type_ann: &TypeAnn,
    type_params: &Option<Vec<TypeParam>>,
    ctx: &Context,
) -> Scheme {
    // NOTE: There's a scoping issue when using this mapping hash map.
    // <T>(arg: T, cb: <T>(T) => T) => T
    // The <T> type param list for `cb` shadows the outer `T`
    let type_param_map: HashMap<String, Type> = match type_params {
        Some(params) => params
            .iter()
            .map(|param| (param.name.name.to_owned(), ctx.fresh_var()))
            .collect(),
        None => HashMap::default(),
    };

    // Infers the type from type annotation and replaces all type references whose names
    // appear in `mapping` with a type variable whose `id` is the value in the mapping.
    let type_ann_ty = infer_type_ann_with_params(type_ann, ctx, &type_param_map);

    // Creates a Scheme with the correct qualifiers for the type references that were
    // replaced with type variables.
    Scheme {
        qualifiers: type_param_map.values().map(|tv| tv.id).collect(),
        ty: type_ann_ty,
    }
}

pub fn infer_type_ann(type_ann: &TypeAnn, ctx: &Context) -> Type {
    freeze(infer_type_ann_rec(type_ann, ctx, &HashMap::default()))
}

pub fn infer_type_ann_with_params(
    type_ann: &TypeAnn,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Type {
    freeze(infer_type_ann_rec(type_ann, ctx, type_param_map))
}

fn infer_type_ann_rec(
    type_ann: &TypeAnn,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params, ret, .. }) => {
            let params: Vec<_> = params
                .iter()
                .map(|arg| infer_type_ann_rec(arg, ctx, type_param_map))
                .collect();
            let ret = Box::from(infer_type_ann_rec(ret.as_ref(), ctx, type_param_map));
            ctx.lam(params, ret)
        }
        TypeAnn::Lit(LitType { lit, .. }) => ctx.lit(lit.to_owned()),
        TypeAnn::Prim(PrimType { prim, .. }) => ctx.prim(prim.to_owned()),
        TypeAnn::Object(ObjectType { props, .. }) => {
            let props: Vec<_> = props
                .iter()
                .map(|prop| TProp {
                    name: prop.name.to_owned(),
                    optional: prop.optional,
                    ty: infer_type_ann_rec(prop.type_ann.as_ref(), ctx, type_param_map),
                })
                .collect();
            ctx.object(props)
        }
        TypeAnn::TypeRef(TypeRef {
            name, type_params, ..
        }) => match type_param_map.get(name) {
            Some(tv) => tv.to_owned(),
            None => {
                let type_params = type_params.clone().map(|params| {
                    params
                        .iter()
                        .map(|param| infer_type_ann_rec(param, ctx, type_param_map))
                        .collect()
                });
                ctx.alias(name, type_params)
            }
        },
        TypeAnn::Union(UnionType { types, .. }) => ctx.union(
            types
                .iter()
                .map(|t| infer_type_ann_rec(t, ctx, type_param_map))
                .collect(),
        ),
        TypeAnn::Intersection(IntersectionType { types, .. }) => ctx.intersection(
            types
                .iter()
                .map(|t| infer_type_ann_rec(t, ctx, type_param_map))
                .collect(),
        ),
        TypeAnn::Tuple(TupleType { types, .. }) => ctx.tuple(
            types
                .iter()
                .map(|t| infer_type_ann_rec(t, ctx, type_param_map))
                .collect(),
        ),
        TypeAnn::Array(ArrayType { elem_type, .. }) => {
            ctx.array(infer_type_ann_rec(elem_type, ctx, type_param_map))
        }
    }
}
