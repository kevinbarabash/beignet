use std::collections::HashMap;

use swc_ecma_ast::*;

use crochet_ast::types::{TGeneric, TObject, TVar, Type, TypeKind};
use crochet_infer::{
    get_type_params, replace_aliases_rec, set_type_params, Context, Subst, Substitutable,
};

use crate::parse_dts::infer_ts_type_ann;

// TODO: use the same technique we use in infer_type_ann.rs, as this stands, it
// doesn't handle type param shadowing.
pub fn replace_aliases(
    t: &Type,
    type_param_decl: &TsTypeParamDecl,
    ctx: &Context,
) -> Result<Type, String> {
    let mut type_params: Vec<TVar> = vec![];
    let type_param_map: HashMap<String, Type> = type_param_decl
        .params
        .iter()
        .map(|tp| {
            // NOTE: We can't use .map() here because infer_ts_type_ann
            // returns a Result.
            let constraint = match &tp.constraint {
                Some(constraint) => {
                    let t = infer_ts_type_ann(constraint, ctx)?;
                    Some(Box::from(t))
                }
                None => None,
            };
            let tv = TVar {
                id: ctx.fresh_id(),
                constraint,
            };
            // TODO: replace type aliases in `constraint` using entries
            // in `type_param_map`.  It looks like TypeScript allow any order
            // so we'll have to do a second pass to process `type_param_map`.
            type_params.push(tv.clone());
            Ok((tp.name.sym.to_string(), Type::from(TypeKind::Var(tv))))
        })
        .collect::<Result<HashMap<String, Type>, String>>()?;

    // Why does `type_params` contain different `TVar`s from what's in `type_param_map`?
    for mut param in &mut type_params {
        if let Some(constraint) = &param.constraint {
            param.constraint = Some(Box::from(replace_aliases_rec(
                constraint.as_ref(),
                &type_param_map,
            )));
        }
    }

    let t = set_type_params(t, &type_params);
    Ok(replace_aliases_rec(&t, &type_param_map))
}

// TODO: rename to merge_interface_types
pub fn merge_types(t1: &Type, t2: &Type) -> Type {
    let tp1 = get_type_params(t1);
    let tp2 = get_type_params(t2);

    if tp1.len() != tp2.len() {
        panic!("Mismatch in type param count when attempting to merge type");
    }

    // Creates a mapping from type params in t2 to those in t1
    let subs: Subst = tp2
        .into_iter()
        .map(|tv| tv.id.to_owned())
        .zip(
            tp1.iter()
                .map(|tv| Type::from(TypeKind::Var(tv.to_owned()))),
        )
        .collect();

    // Unwrap qualified types since we return a qualified
    // type if there are any type qualifiers.
    let t1 = if let TypeKind::Generic(TGeneric { t, .. }) = &t1.kind {
        t
    } else {
        t1
    };
    let t2 = if let TypeKind::Generic(TGeneric { t, .. }) = &t2.kind {
        t
    } else {
        t2
    };

    // Updates type variables for type params to match t1
    let mut t2 = t2.to_owned();
    t2.apply(&subs);

    let type_params = tp1;
    let t = match (&t1.kind, &t2.kind) {
        (TypeKind::Object(obj1), TypeKind::Object(obj2)) => {
            let elems: Vec<_> = obj1
                .elems
                .iter()
                .cloned()
                .chain(obj2.elems.iter().cloned())
                .collect();

            Type::from(TypeKind::Object(TObject { elems }))
        }
        (_, _) => todo!(),
    };

    if type_params.is_empty() {
        t
    } else {
        Type::from(TypeKind::Generic(TGeneric {
            t: Box::from(t),
            type_params,
        }))
    }
}
