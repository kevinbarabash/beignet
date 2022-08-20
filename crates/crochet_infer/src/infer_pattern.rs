use std::collections::HashMap;

use crochet_ast::*;

use super::context::Context;
use super::infer_expr::infer_expr;
use super::infer_type_ann::*;
use super::substitutable::{Subst, Substitutable};
use super::types::{self, Scheme, Type};
use super::unify::unify;
use super::util::*;

pub type Assump = HashMap<String, Scheme>;

// NOTE: The caller is responsible for inserting any new variables introduced
// into the appropriate context.
fn infer_pattern(
    pat: &Pattern,
    type_ann: &Option<TypeAnn>,
    ctx: &Context,
    type_param_map: &HashMap<String, Type>,
) -> Result<(Subst, Assump, Type), String> {
    // Keeps track of all of the variables the need to be introduced by this pattern.
    let mut new_vars: HashMap<String, Scheme> = HashMap::new();

    let pat_type = infer_pattern_rec(pat, ctx, &mut new_vars)?;

    // If the pattern had a type annotation associated with it, we infer type of the
    // type annotation and add a constraint between the types of the pattern and its
    // type annotation.
    match type_ann {
        Some(type_ann) => {
            let type_ann_ty = infer_type_ann_with_params(type_ann, ctx, type_param_map);

            // Allowing type_ann_ty to be a subtype of pat_type because
            // only non-refutable patterns can have type annotations.
            let s = unify(&type_ann_ty, &pat_type, ctx)?;

            // Substs are applied to any new variables introduced.  This handles
            // the situation where explicit types have be provided for function
            // parameters.
            let a = new_vars.apply(&s);
            Ok((s, a, type_ann_ty))
        }
        None => Ok((Subst::new(), new_vars, pat_type)),
    }
}

fn infer_pattern_rec(pat: &Pattern, ctx: &Context, assump: &mut Assump) -> Result<Type, String> {
    match pat {
        Pattern::Ident(BindingIdent { id, .. }) => {
            let tv = ctx.fresh_var();
            let scheme = Scheme::from(&tv);
            if assump.insert(id.name.to_owned(), scheme).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(tv)
        }
        Pattern::Wildcard(_) => {
            // Same as Pattern::Ident but we don't insert an assumption since
            // we don't want to bind it to a variable.
            let tv = ctx.fresh_var();
            Ok(tv)
        }
        Pattern::Lit(LitPat { lit, .. }) => Ok(Type::from(lit.to_owned())),
        Pattern::Is(IsPat { id, is_id, .. }) => {
            let ty = match is_id.name.as_str() {
                "string" => Type::Prim(types::Primitive::Str),
                "number" => Type::Prim(types::Primitive::Num),
                "boolean" => Type::Prim(types::Primitive::Bool),
                // The alias type will be used for `instanceof` of checks, but
                // only if the definition of the alias is an object type with a
                // `constructor` method.
                name => Type::Alias(types::AliasType {
                    name: name.to_owned(),
                    type_params: None,
                }),
            };
            let scheme = generalize(&ctx.types, &ty);
            if assump.insert(id.name.to_owned(), scheme).is_some() {
                return Err(String::from("Duplicate identifier in pattern"));
            }
            Ok(ty)
        }
        Pattern::Rest(RestPat { arg, .. }) => {
            let t = infer_pattern_rec(arg.as_ref(), ctx, assump)?;
            Ok(Type::Rest(Box::from(t)))
        }
        Pattern::Array(ArrayPat { elems, .. }) => {
            let elems: Result<Vec<Type>, String> = elems
                .iter()
                .map(|elem| {
                    match elem {
                        Some(elem) => match elem {
                            Pattern::Rest(rest) => {
                                let rest_ty = infer_pattern_rec(rest.arg.as_ref(), ctx, assump)?;
                                Ok(Type::Rest(Box::from(rest_ty)))
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

            Ok(Type::Tuple(elems?))
        }
        Pattern::Object(ObjectPat { props, .. }) => {
            let mut rest_opt_ty: Option<Type> = None;
            let props: Vec<types::TProp> = props
                .iter()
                .filter_map(|prop| {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        ObjectPatProp::KeyValue(KeyValuePatProp { key, value }) => {
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            let value_type = infer_pattern_rec(value, ctx, assump).unwrap();

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                ty: value_type,
                            })
                        }
                        ObjectPatProp::Assign(AssignPatProp { key, value: _, .. }) => {
                            // We ignore the value for now, we can come back later to handle
                            // default values.
                            // TODO: handle default values

                            let tv = ctx.fresh_var();
                            let scheme = Scheme::from(&tv);
                            if assump.insert(key.name.to_owned(), scheme).is_some() {
                                todo!("return an error");
                            }

                            Some(types::TProp {
                                name: key.name.to_owned(),
                                optional: false,
                                mutable: false,
                                ty: tv,
                            })
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
                            rest_opt_ty = infer_pattern_rec(rest.arg.as_ref(), ctx, assump).ok();
                            None
                        }
                    }
                })
                .collect();

            let obj_type = Type::Object(props);

            match rest_opt_ty {
                // TODO: Replace this with a proper Rest/Spread type
                // See https://github.com/microsoft/TypeScript/issues/10727
                Some(rest_ty) => Ok(Type::Intersection(vec![obj_type, rest_ty])),
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
    pat: &Pattern,
    type_ann: &Option<TypeAnn>,
    init: &Expr,
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
