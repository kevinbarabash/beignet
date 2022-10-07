use std::collections::HashMap;
use std::iter::Iterator;

use crochet_ast::*;
use crochet_types::{self as types, TFnParam, TIndex, TKeyword, TLam, TObject, TPrim, TProp, Type};
use types::TObjElem;

use crate::util::compose_many_subs;
use crate::Subst;
use crate::Substitutable;

use super::context::Context;
use super::infer_expr::infer_expr;
use super::infer_fn_param::e_pat_to_t_pat;
use super::util::get_type_params;

pub fn infer_scheme(type_ann: &mut TypeAnn, ctx: &mut Context) -> Result<(Subst, Type), String> {
    if let TypeAnnKind::Lam(_) = &type_ann.kind {
        infer_qualified_type_ann(type_ann, &None, ctx)
    } else {
        infer_type_ann_rec(type_ann, ctx, &HashMap::default())
    }
}

pub fn infer_qualified_type_ann(
    type_ann: &mut TypeAnn,
    type_params: &Option<Vec<TypeParam>>, // used by infer_prog() in infer.rs
    ctx: &mut Context,
) -> Result<(Subst, Type), String> {
    let type_params = if type_params.is_none() {
        match &type_ann.kind {
            TypeAnnKind::Lam(lam) => &lam.type_params,
            _ => &None,
        }
    } else {
        type_params
    };

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
    let (type_ann_s, type_ann_t) = infer_type_ann_with_params(type_ann, ctx, &type_param_map)?;
    let type_params: Vec<_> = type_param_map
        .values()
        .map(|t| match t {
            Type::Var(id) => id.to_owned(),
            _ => panic!("{t} is not a type variable"),
        })
        .collect();

    // set type_params
    let t = match type_ann_t {
        Type::Lam(lam) => Type::Lam(TLam { type_params, ..lam }),
        Type::Object(obj) => Type::Object(TObject { type_params, ..obj }),
        _ => type_ann_t,
    };
    Ok((type_ann_s, t))
}

pub fn infer_type_ann_with_params(
    type_ann: &mut TypeAnn,
    ctx: &mut Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Type), String> {
    infer_type_ann_rec(type_ann, ctx, type_param_map)
}

fn infer_type_ann_rec(
    type_ann: &mut TypeAnn,
    ctx: &mut Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Type), String> {
    match &mut type_ann.kind {
        TypeAnnKind::Lam(lam) => {
            let mut ss: Vec<Subst> = vec![];
            let mut params: Vec<types::TFnParam> = vec![];

            for param in &mut lam.params {
                let (param_s, param_t) =
                    infer_type_ann_rec(&mut param.type_ann, ctx, type_param_map)?;
                ss.push(param_s);
                params.push(TFnParam {
                    pat: e_pat_to_t_pat(&param.pat),
                    t: param_t,
                    optional: param.optional,
                });
            }

            let (ret_s, ret_t) = infer_type_ann_rec(&mut lam.ret, ctx, type_param_map)?;
            let ret = Box::from(ret_t);
            ss.push(ret_s);

            let s = compose_many_subs(&ss);
            let t = Type::Lam(types::TLam {
                params,
                ret,
                type_params: vec![],
            });
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Lit(lit) => {
            let s = Subst::new();
            let t = Type::from(lit.to_owned());
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Prim(PrimType { prim, .. }) => {
            let s = Subst::new();
            let t = Type::from(prim.to_owned());
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Keyword(KeywordType { keyword, .. }) => {
            let s = Subst::new();
            let t = Type::from(keyword.to_owned());
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Object(obj) => {
            let mut ss: Vec<Subst> = vec![];
            let mut elems: Vec<types::TObjElem> = vec![];

            for elem in &mut obj.elems {
                match elem {
                    crochet_ast::TObjElem::Index(index) => {
                        let (index_s, index_t) =
                            infer_type_ann_rec(&mut index.type_ann, ctx, type_param_map)?;

                        let (key_s, key_t) =
                            infer_type_ann_rec(&mut index.key.type_ann, ctx, type_param_map)?;

                        ss.push(index_s);
                        ss.push(key_s);
                        elems.push(TObjElem::Index(TIndex {
                            key: TFnParam {
                                pat: e_pat_to_t_pat(&index.key.pat),
                                t: key_t,
                                optional: index.key.optional,
                            },
                            mutable: index.mutable,
                            t: index_t,
                        }))
                    }
                    crochet_ast::TObjElem::Prop(prop) => {
                        let (prop_s, prop_t) =
                            infer_type_ann_rec(&mut prop.type_ann, ctx, type_param_map)?;

                        ss.push(prop_s);
                        elems.push(TObjElem::Prop(TProp {
                            name: prop.name.to_owned(),
                            optional: prop.optional,
                            mutable: prop.mutable,
                            t: prop_t,
                        }))
                    }
                }
            }

            let s = compose_many_subs(&ss);
            let t = Type::Object(TObject {
                elems,
                type_params: vec![],
            });
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::TypeRef(TypeRef {
            name,
            type_args: type_params,
            ..
        }) => match type_param_map.get(name) {
            Some(tv) => {
                let s = Subst::new();
                let t = tv.to_owned();
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            None => {
                let mut type_args: Vec<Type> = vec![];
                let mut ss: Vec<Subst> = vec![];

                if let Some(type_params) = type_params {
                    for param in type_params {
                        let (type_arg_s, type_arg_t) =
                            infer_type_ann_rec(param, ctx, type_param_map)?;
                        ss.push(type_arg_s);
                        type_args.push(type_arg_t);
                    }
                }

                let s = compose_many_subs(&ss);
                let t = Type::Ref(types::TRef {
                    name: name.to_owned(),
                    type_args: if type_args.is_empty() {
                        None
                    } else {
                        Some(type_args)
                    },
                });
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
        },
        TypeAnnKind::Union(union) => {
            let mut ts: Vec<Type> = vec![];
            let mut ss: Vec<Subst> = vec![];

            for t in &mut union.types {
                let (s, t) = infer_type_ann_rec(t, ctx, type_param_map)?;

                ss.push(s);
                ts.push(t);
            }

            let s = compose_many_subs(&ss);
            let t = Type::Union(ts);
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Intersection(intersection) => {
            let mut ts: Vec<Type> = vec![];
            let mut ss: Vec<Subst> = vec![];

            for t in &mut intersection.types {
                let (s, t) = infer_type_ann_rec(t, ctx, type_param_map)?;

                ss.push(s);
                ts.push(t);
            }

            let s = compose_many_subs(&ss);
            let t = Type::Intersection(ts);
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Tuple(tuple) => {
            let mut ts: Vec<Type> = vec![];
            let mut ss: Vec<Subst> = vec![];

            for t in &mut tuple.types {
                let (s, t) = infer_type_ann_rec(t, ctx, type_param_map)?;

                ss.push(s);
                ts.push(t);
            }

            let s = compose_many_subs(&ss);
            let t = Type::Tuple(ts);
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Array(ArrayType { elem_type, .. }) => {
            let (elem_s, elem_t) = infer_type_ann_rec(elem_type, ctx, type_param_map)?;
            let s = elem_s;
            let t = Type::Array(Box::from(elem_t));
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::KeyOf(KeyOfType { type_ann, .. }) => {
            let (arg_s, arg_t) = infer_type_ann_rec(type_ann, ctx, type_param_map)?;
            let s = arg_s;
            let t = Type::KeyOf(Box::from(arg_t));
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Query(QueryType { expr, .. }) => infer_expr(ctx, expr),
        TypeAnnKind::IndexedAccess(IndexedAccessType {
            obj_type,
            index_type,
            ..
        }) => {
            let (obj_s, obj_t) = infer_type_ann_rec(obj_type, ctx, type_param_map)?;
            let (index_s, index_t) = infer_type_ann_rec(index_type, ctx, type_param_map)?;

            let mut ss = vec![obj_s, index_s];
            let (s, t) = infer_property_type(&obj_t, &index_t, ctx)?;
            ss.push(s);
            let s = compose_many_subs(&ss);
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
    }
}

fn infer_property_type(
    obj_t: &Type,
    index_t: &Type,
    ctx: &mut Context,
) -> Result<(Subst, Type), String> {
    match &obj_t {
        Type::Object(TObject {
            elems,
            type_params: _,
        }) => match index_t {
            Type::Ref(alias) => {
                let t = ctx.lookup_ref_and_instantiate(alias)?;
                infer_property_type(obj_t, &t, ctx)
            }
            Type::Lit(lit) => match lit {
                types::TLit::Num(_) => {
                    // TODO: support number literals if the ojbect has an indexer
                    // that supports them
                    Err(String::from(
                        "a number literal can't be used as an indexed type's index",
                    ))
                }
                types::TLit::Bool(_) => Err(String::from(
                    "a boolean literal can't be used as an indexed type's index",
                )),
                types::TLit::Str(name) => {
                    let mut t = elems.iter().find_map(|elem| match elem {
                        types::TObjElem::Prop(prop) => {
                            if prop.name == *name {
                                Some(prop.t.to_owned())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    });

                    if t.is_none() {
                        t = elems.iter().find_map(|elem| match elem {
                            types::TObjElem::Index(index) => match index.key.t {
                                Type::Prim(TPrim::Str) => Some(index.t.to_owned()),
                                _ => None,
                            },
                            _ => None,
                        });
                    }

                    match t {
                        Some(t) => {
                            let s = Subst::new();
                            Ok((s, t))
                        }
                        None => Err(format!("'{name}' not found in {obj_t}")),
                    }
                }
            },
            t => Err(format!("{t} can't be used as an indexed type's index",)),
        },
        Type::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            infer_property_type(&t, index_t, ctx)
        }
        Type::Lit(lit) => {
            let t = match lit {
                types::TLit::Num(_) => ctx.lookup_type_and_instantiate("Number")?,
                types::TLit::Bool(_) => ctx.lookup_type_and_instantiate("Boolean")?,
                types::TLit::Str(_) => ctx.lookup_type_and_instantiate("String")?,
            };
            infer_property_type(&t, index_t, ctx)
        }
        Type::Prim(prim) => {
            let t = match prim {
                TPrim::Num => ctx.lookup_type_and_instantiate("Number")?,
                TPrim::Bool => ctx.lookup_type_and_instantiate("Boolean")?,
                TPrim::Str => ctx.lookup_type_and_instantiate("String")?,
            };
            infer_property_type(&t, index_t, ctx)
        }
        Type::Keyword(keyword) => match keyword {
            TKeyword::Symbol => {
                let t = ctx.lookup_type_and_instantiate("Symbol")?;
                infer_property_type(&t, index_t, ctx)
            }
            TKeyword::Null => Err("Cannot read property on 'null'".to_owned()),
            TKeyword::Undefined => Err("Cannot read property on 'undefined'".to_owned()),
            TKeyword::Never => Err("Cannot read property on 'never'".to_owned()),
        },
        Type::Array(type_param) => {
            // TODO: Do this for all interfaces that we lookup
            let t = ctx.lookup_type("ReadonlyArray")?;
            let type_params = get_type_params(&t);
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            let s: Subst = Subst::from([(type_params[0], type_param.as_ref().to_owned())]);
            let t = t.apply(&s);
            infer_property_type(&t, index_t, ctx)
        }
        Type::Tuple(elem_types) => {
            // TODO: Do this for all interfaces that we lookup
            let t = ctx.lookup_type("ReadonlyArray")?;
            println!("ReadonlyArray = {t}");
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            // TODO: remove duplicate types
            let type_param = Type::Union(elem_types.to_owned());
            let type_params = get_type_params(&t); // ReadonlyArray type params

            let s: Subst = Subst::from([(type_params[0], type_param)]);
            let t = t.apply(&s);
            infer_property_type(&t, index_t, ctx)
        }
        _ => {
            todo!("Unhandled {obj_t:#?} in infer_property_type")
        }
    }
}
