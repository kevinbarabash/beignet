use error_stack::{Report, Result};
use std::collections::HashMap;
use std::iter::Iterator;

use crochet_ast::types::{
    self as types, Provenance, TFnParam, TIndex, TKeyword, TObjElem, TObject, TProp, TVar, Type,
    TypeKind,
};
use crochet_ast::values::*;

use crate::context::Context;
use crate::infer_expr::infer_expr;
use crate::infer_fn_param::pattern_to_tpat;
use crate::type_error::TypeError;
use crate::util::compose_many_subs;
use crate::util::get_type_params;
use crate::Subst;
use crate::Substitutable;

pub fn infer_type_ann(
    type_ann: &mut TypeAnn,
    ctx: &mut Context,
    type_params: &mut Option<Vec<TypeParam>>,
) -> Result<(Subst, Type), TypeError> {
    let mut type_params = if type_params.is_none() {
        match &mut type_ann.kind {
            TypeAnnKind::Lam(lam) => lam.type_params.to_owned(),
            _ => None,
        }
    } else {
        type_params.to_owned()
    };

    // NOTE: There's a scoping issue when using this mapping hash map.
    // <T>(arg: T, cb: <T>(T) => T) => T
    // The <T> type param list for `cb` shadows the outer `T`
    let type_param_map: HashMap<String, Type> = match &mut type_params {
        Some(params) => params
            .iter_mut()
            .map(|param| {
                let tv = match &mut param.constraint {
                    Some(type_ann) => {
                        // TODO: push `s` on to `ss`
                        let (_s, t) = infer_type_ann(type_ann, ctx, &mut None)?;
                        Type::from(TypeKind::Var(TVar {
                            id: ctx.fresh_id(),
                            constraint: Some(Box::from(t)),
                        }))
                    }
                    None => ctx.fresh_var(),
                };

                Ok((param.name.name.to_owned(), tv))
            })
            .collect::<Result<HashMap<String, Type>, TypeError>>()?,
        None => HashMap::default(),
    };

    infer_type_ann_with_params(type_ann, ctx, &type_param_map)
}

pub fn infer_type_ann_with_params(
    type_ann: &mut TypeAnn,
    ctx: &mut Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Type), TypeError> {
    infer_type_ann_rec(type_ann, ctx, type_param_map)
}

fn infer_type_ann_rec(
    type_ann: &mut TypeAnn,
    ctx: &mut Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Type), TypeError> {
    let (s, mut t) = match &mut type_ann.kind {
        TypeAnnKind::Lam(lam) => {
            let mut ss: Vec<Subst> = vec![];
            let mut params: Vec<types::TFnParam> = vec![];

            for param in &mut lam.params {
                let (param_s, param_t) =
                    infer_type_ann_rec(&mut param.type_ann, ctx, type_param_map)?;
                ss.push(param_s);
                params.push(TFnParam {
                    pat: pattern_to_tpat(&param.pat),
                    t: param_t,
                    optional: param.optional,
                });
            }

            let (ret_s, ret_t) = infer_type_ann_rec(&mut lam.ret, ctx, type_param_map)?;
            let ret = Box::from(ret_t);
            ss.push(ret_s);

            let s = compose_many_subs(&ss);
            let t = Type::from(TypeKind::Lam(types::TLam { params, ret }));
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Lit(lit) => {
            let s = Subst::new();
            let t = Type::from(lit.to_owned());
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
                    crochet_ast::values::TObjElem::Index(index) => {
                        let (index_s, index_t) =
                            infer_type_ann_rec(&mut index.type_ann, ctx, type_param_map)?;

                        let (key_s, key_t) =
                            infer_type_ann_rec(&mut index.key.type_ann, ctx, type_param_map)?;

                        ss.push(index_s);
                        ss.push(key_s);
                        elems.push(TObjElem::Index(TIndex {
                            key: TFnParam {
                                pat: pattern_to_tpat(&index.key.pat),
                                t: key_t,
                                optional: index.key.optional,
                            },
                            mutable: index.mutable,
                            t: index_t,
                        }))
                    }
                    crochet_ast::values::TObjElem::Prop(prop) => {
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
            let t = Type::from(TypeKind::Object(TObject { elems }));
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
                let t = Type::from(TypeKind::Ref(types::TRef {
                    name: name.to_owned(),
                    type_args: if type_args.is_empty() {
                        None
                    } else {
                        Some(type_args)
                    },
                }));
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
            let t = Type::from(TypeKind::Union(ts));
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
            let t = Type::from(TypeKind::Intersection(ts));
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
            let t = Type::from(TypeKind::Tuple(ts));
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Array(ArrayType { elem_type, .. }) => {
            let (elem_s, elem_t) = infer_type_ann_rec(elem_type, ctx, type_param_map)?;
            let s = elem_s;
            let t = Type::from(TypeKind::Array(Box::from(elem_t)));
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::KeyOf(KeyOfType { type_ann, .. }) => {
            let (arg_s, arg_t) = infer_type_ann_rec(type_ann, ctx, type_param_map)?;
            let s = arg_s;
            let t = Type::from(TypeKind::KeyOf(Box::from(arg_t)));
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
        TypeAnnKind::Query(QueryType { expr, .. }) => infer_expr(ctx, expr),
        TypeAnnKind::Mutable(MutableType { type_ann, .. }) => {
            let (s, mut t) = infer_type_ann_rec(type_ann, ctx, type_param_map)?;

            match &t.kind {
                TypeKind::Keyword(_) => {
                    return Err(Report::new(TypeError::PrimitivesCantBeMutable(Box::from(
                        t.to_owned(),
                    ))))
                }
                TypeKind::Tuple(_) => {
                    return Err(Report::new(TypeError::TuplesCantBeMutable(Box::from(
                        t.to_owned(),
                    ))))
                }
                _ => (),
            };

            t.mutable = true;
            type_ann.inferred_type = Some(t.clone());
            Ok((s, t))
        }
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
    }?;

    t.provenance = Some(Box::from(Provenance::TypeAnn(Box::from(
        type_ann.to_owned(),
    ))));

    Ok((s, t))
}

fn infer_property_type(
    obj_t: &Type,
    index_t: &Type,
    ctx: &mut Context,
) -> Result<(Subst, Type), TypeError> {
    match &obj_t.kind {
        TypeKind::Object(TObject { elems }) => match &index_t.kind {
            TypeKind::Ref(alias) => {
                let t = ctx.lookup_ref_and_instantiate(alias)?;
                infer_property_type(obj_t, &t, ctx)
            }
            TypeKind::Lit(lit) => match lit {
                types::TLit::Num(_) => {
                    // TODO: support number literals if the ojbect has an indexer
                    // that supports them
                    Err(
                        Report::new(TypeError::InvalidTypeIndex(Box::from(index_t.to_owned())))
                            .attach_printable(
                                "a number literal can't be used as an indexed type's index",
                            ),
                    )
                }
                types::TLit::Bool(_) => Err(Report::new(TypeError::InvalidTypeIndex(Box::from(
                    index_t.to_owned(),
                )))
                .attach_printable("a boolean literal can't be used as an indexed type's index")),
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
                            types::TObjElem::Index(index) => match &index.key.t.kind {
                                TypeKind::Keyword(TKeyword::String) => Some(index.t.to_owned()),
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
                        None => Err(Report::new(TypeError::MissingTypeIndex)
                            .attach_printable(format!("'{name}' not found in {obj_t}"))),
                    }
                }
            },
            _ => Err(
                Report::new(TypeError::InvalidTypeIndex(Box::from(index_t.to_owned())))
                    .attach_printable(format!(
                        "{index_t} can't be used as an indexed type's index",
                    )),
            ),
        },
        TypeKind::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            infer_property_type(&t, index_t, ctx)
        }
        TypeKind::Lit(lit) => {
            let t = match lit {
                types::TLit::Num(_) => ctx.lookup_type_and_instantiate("Number", false)?,
                types::TLit::Bool(_) => ctx.lookup_type_and_instantiate("Boolean", false)?,
                types::TLit::Str(_) => ctx.lookup_type_and_instantiate("String", false)?,
            };
            infer_property_type(&t, index_t, ctx)
        }
        TypeKind::Keyword(keyword) => match keyword {
            TKeyword::Number => {
                let t = ctx.lookup_type_and_instantiate("Number", false)?;
                infer_property_type(&t, index_t, ctx)
            }
            TKeyword::Boolean => {
                let t = ctx.lookup_type_and_instantiate("Boolean", false)?;
                infer_property_type(&t, index_t, ctx)
            }
            TKeyword::String => {
                let t = ctx.lookup_type_and_instantiate("String", false)?;
                infer_property_type(&t, index_t, ctx)
            }
            TKeyword::Symbol => {
                let t = ctx.lookup_type_and_instantiate("Symbol", false)?;
                infer_property_type(&t, index_t, ctx)
            }
            TKeyword::Null => Err(Report::new(TypeError::NotAnObjectType(Box::from(
                obj_t.to_owned(),
            )))
            .attach_printable("Cannot read property on 'null'")),
            TKeyword::Undefined => Err(Report::new(TypeError::NotAnObjectType(Box::from(
                obj_t.to_owned(),
            )))
            .attach_printable("Cannot read property on 'undefined'")),
            TKeyword::Never => Err(Report::new(TypeError::NotAnObjectType(Box::from(
                obj_t.to_owned(),
            )))
            .attach_printable("Cannot read property on 'never'")),
        },
        TypeKind::Array(type_param) => {
            let t = ctx.lookup_type("Array", obj_t.mutable)?;
            let type_params = get_type_params(&t);
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            let s: Subst =
                Subst::from([(type_params[0].id.to_owned(), type_param.as_ref().to_owned())]);
            let t = t.apply(&s);
            infer_property_type(&t, index_t, ctx)
        }
        TypeKind::Tuple(elem_types) => {
            let t = ctx.lookup_type("Array", obj_t.mutable)?;
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            // TODO: remove duplicate types
            let type_param = Type::from(TypeKind::Union(elem_types.to_owned()));
            let type_params = get_type_params(&t); // ReadonlyArray type params

            let s: Subst = Subst::from([(type_params[0].id.to_owned(), type_param)]);
            let t = t.apply(&s);
            infer_property_type(&t, index_t, ctx)
        }
        _ => {
            todo!("Unhandled {obj_t:#?} in infer_property_type")
        }
    }
}
