use std::collections::HashMap;
use std::iter::Iterator;

use crochet_ast::*;

use super::context::Context;
use super::infer_fn_param::e_pat_to_t_pat;
use super::types::{self, Scheme, TFnParam, TProp, Type};

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
        qualifiers: type_param_map
            .values()
            .map(|t| match t {
                Type::Var(id) => id.to_owned(),
                _ => panic!("{t} is not a type variable"),
            })
            .collect(),
        ty: type_ann_ty,
    }
}

pub fn infer_type_ann(type_ann: &TypeAnn, ctx: &Context) -> Type {
    infer_type_ann_rec(type_ann, ctx, &HashMap::default())
}

pub fn infer_type_ann_with_params(
    type_ann: &TypeAnn,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Type {
    infer_type_ann_rec(type_ann, ctx, type_param_map)
}

fn infer_type_ann_rec(
    type_ann: &TypeAnn,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Type {
    match type_ann {
        TypeAnn::Lam(LamType { params, ret, .. }) => {
            let params: Vec<types::TFnParam> = params
                .iter()
                .map(|param| TFnParam {
                    pat: e_pat_to_t_pat(&param.pat),
                    ty: infer_type_ann_rec(&param.type_ann, ctx, type_param_map),
                    optional: param.optional,
                })
                .collect();
            let ret = Box::from(infer_type_ann_rec(ret.as_ref(), ctx, type_param_map));
            Type::Lam(types::LamType { params, ret })
        }
        TypeAnn::Lit(lit) => Type::from(lit.to_owned()),
        TypeAnn::Prim(PrimType { prim, .. }) => Type::Prim(prim.to_owned()),
        TypeAnn::Object(ObjectType { props, .. }) => {
            let props: Vec<_> = props
                .iter()
                .map(|prop| TProp {
                    name: prop.name.to_owned(),
                    optional: prop.optional,
                    mutable: prop.mutable,
                    ty: infer_type_ann_rec(prop.type_ann.as_ref(), ctx, type_param_map),
                })
                .collect();
            Type::Object(props)
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
                Type::Alias(types::AliasType {
                    name: name.to_owned(),
                    type_params,
                })
            }
        },
        TypeAnn::Union(UnionType { types, .. }) => Type::Union(
            types
                .iter()
                .map(|t| infer_type_ann_rec(t, ctx, type_param_map))
                .collect(),
        ),
        TypeAnn::Intersection(IntersectionType { types, .. }) => Type::Intersection(
            types
                .iter()
                .map(|t| infer_type_ann_rec(t, ctx, type_param_map))
                .collect(),
        ),
        TypeAnn::Tuple(TupleType { types, .. }) => Type::Tuple(
            types
                .iter()
                .map(|t| infer_type_ann_rec(t, ctx, type_param_map))
                .collect(),
        ),
        TypeAnn::Array(ArrayType { elem_type, .. }) => Type::Array(Box::from(infer_type_ann_rec(
            elem_type,
            ctx,
            type_param_map,
        ))),
    }
}
