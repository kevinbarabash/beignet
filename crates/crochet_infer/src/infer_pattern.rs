use std::collections::HashMap;

use crochet_ast::common::*;
use crochet_ast::types::{self as types, TObject, Type, TypeKind};
use crochet_ast::values::*;

use crate::assump::Assump;
use crate::context::Context;
use crate::infer_expr::infer_expr;
use crate::infer_type_ann::*;
use crate::substitutable::{Subst, Substitutable};
use crate::unify::unify;
use crate::util::*;

// NOTE: The caller is responsible for inserting any new variables introduced
// into the appropriate context.
pub fn infer_pattern(
    pat: &mut Pattern,
    type_ann: &mut Option<TypeAnn>,
    ctx: &mut Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Assump, Type), String> {
    // Keeps track of all of the variables the need to be introduced by this pattern.
    let mut new_vars: HashMap<String, Type> = HashMap::new();

    let pat_type = infer_pattern_rec(pat, ctx, &mut new_vars)?;

    // If the pattern had a type annotation associated with it, we infer type of the
    // type annotation and add a constraint between the types of the pattern and its
    // type annotation.
    match type_ann {
        Some(type_ann) => {
            let (type_ann_s, type_ann_t) =
                infer_type_ann_with_params(type_ann, ctx, type_param_map)?;

            // Allowing type_ann_ty to be a subtype of pat_type because
            // only non-refutable patterns can have type annotations.
            let s = unify(&type_ann_t, &pat_type, ctx)?;
            let s = compose_subs(&s, &type_ann_s);

            // Substs are applied to any new variables introduced.  This handles
            // the situation where explicit types have be provided for function
            // parameters.
            let a = new_vars.apply(&s);
            Ok((s, a, type_ann_t))
        }
        None => Ok((Subst::new(), new_vars, pat_type)),
    }
}

fn infer_pattern_rec(
    pat: &mut Pattern,
    ctx: &Context,
    assump: &mut Assump,
) -> Result<Type, String> {
    match &mut pat.kind {
        PatternKind::Ident(BindingIdent { name }) => {
            let tv = ctx.fresh_var();
            if assump.insert(name.to_owned(), tv.clone()).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(tv)
        }
        PatternKind::Wildcard(_) => {
            // Same as Pattern::Ident but we don't insert an assumption since
            // we don't want to bind it to a variable.
            let tv = ctx.fresh_var();
            Ok(tv)
        }
        PatternKind::Lit(LitPat { lit, .. }) => Ok(Type::from(lit.to_owned())),
        PatternKind::Is(IsPat { id, is_id, .. }) => {
            let kind = match is_id.name.as_str() {
                "string" => TypeKind::Keyword(types::TKeyword::String),
                "number" => TypeKind::Keyword(types::TKeyword::Number),
                "boolean" => TypeKind::Keyword(types::TKeyword::Boolean),
                // The alias type will be used for `instanceof` of checks, but
                // only if the definition of the alias is an object type with a
                // `constructor` method.
                name => TypeKind::Ref(types::TRef {
                    name: name.to_owned(),
                    type_args: None,
                }),
            };
            let t = Type { kind };
            // TODO: we need a method on Context to get all types that currently in
            // scope so that we can pass them to generalize()
            let all_types = ctx.get_all_types();
            let t = generalize(&all_types, &t);
            if assump.insert(id.name.to_owned(), t.clone()).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(t)
        }
        PatternKind::Rest(RestPat { arg, .. }) => {
            let t = infer_pattern_rec(arg, ctx, assump)?;
            Ok(Type {
                kind: TypeKind::Rest(Box::from(t)),
            })
        }
        PatternKind::Array(ArrayPat { elems, .. }) => {
            let elems: Result<Vec<Type>, String> = elems
                .iter_mut()
                .map(|elem| {
                    match elem {
                        Some(elem) => match &mut elem.kind {
                            PatternKind::Rest(rest) => {
                                let rest_ty = infer_pattern_rec(&mut rest.arg, ctx, assump)?;
                                Ok(Type {
                                    kind: TypeKind::Rest(Box::from(rest_ty)),
                                })
                            }
                            _ => infer_pattern_rec(elem, ctx, assump),
                        },
                        None => {
                            // TODO: figure how to ignore gaps in the array
                            todo!()
                        }
                    }
                })
                .collect();

            Ok(Type {
                kind: TypeKind::Tuple(elems?),
            })
        }
        // TODO: infer type_params
        PatternKind::Object(ObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let elems: Vec<types::TObjElem> = props
                .iter_mut()
                .filter_map(|prop| {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        ObjectPatProp::KeyValue(KeyValuePatProp { key, value }) => {
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            let value_type = infer_pattern_rec(value, ctx, assump).unwrap();

                            Some(types::TObjElem::Prop(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                t: value_type,
                            }))
                        }
                        ObjectPatProp::Assign(AssignPatProp { key, value: _, .. }) => {
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
                        ObjectPatProp::Rest(rest) => {
                            if rest_opt_ty.is_some() {
                                // TODO: return an Err() instead of panicking.
                                panic!("Maximum one rest pattern allowed in object patterns")
                            }
                            // TypeScript doesn't support spreading/rest in types so instead we
                            // do the following conversion:
                            // {x, y, ...rest} -> {x: A, y: B} & C
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            rest_opt_ty = infer_pattern_rec(&mut rest.arg, ctx, assump).ok();
                            None
                        }
                    }
                })
                .collect();

            let obj_type = Type {
                kind: TypeKind::Object(TObject { elems }),
            };

            match rest_opt_ty {
                // TODO: Replace this with a proper Rest/Spread type
                // See https://github.com/microsoft/TypeScript/issues/10727
                Some(rest_ty) => Ok(Type {
                    kind: TypeKind::Intersection(vec![obj_type, rest_ty]),
                }),
                None => Ok(obj_type),
            }
        }
    }
}

pub enum PatternUsage {
    Assign,
    Match,
}

pub fn infer_pattern_and_init(
    pat: &mut Pattern,
    type_ann: &mut Option<TypeAnn>,
    init: &mut Expr,
    ctx: &mut Context,
    pu: &PatternUsage,
) -> Result<(Assump, Subst), String> {
    let type_param_map = HashMap::new();
    let (ps, pa, pt) = infer_pattern(pat, type_ann, ctx, &type_param_map)?;

    let (is, it) = infer_expr(ctx, init)?;

    // Unifies initializer and pattern.
    let s = match pu {
        // Assign: The inferred type of the init value must be a sub-type
        // of the pattern it's being assigned to.
        PatternUsage::Assign => unify(&it, &pt, ctx)?,
        // Matching: The pattern must be a sub-type of the expression
        // it's being matched against
        PatternUsage::Match => unify(&pt, &it, ctx)?,
    };

    // infer_pattern can generate a non-empty Subst when the pattern includes
    // a type annotation.
    let s = compose_many_subs(&[is, ps, s]);
    let t = pa.apply(&s);

    Ok((t, s))
}
