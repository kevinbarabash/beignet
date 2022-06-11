use super::constraint_solver::Constraint;
use crate::ast::*;
use crate::types::{self, Type, Flag, Variant};

use super::context::Context;
use super::substitutable::*;

pub type InferResult = (Type, Vec<Constraint>);

pub fn infer_mem(
    infer: fn(expr: &Expr, ctx: &Context) -> Result<InferResult, String>,
    mem: &Member,
    ctx: &Context,
) -> Result<InferResult, String> {
    let Member { obj, prop, .. } = mem;

    let (obj_type, mut obj_cs) = infer(obj, ctx)?;
    let (prop_type, mut prop_cs) = type_of_property_on_type(obj_type, prop, ctx)?;

    let mut constraints: Vec<Constraint> = vec![];
    constraints.append(&mut obj_cs);
    constraints.append(&mut prop_cs);

    Ok((unwrap_member_type(&prop_type, ctx), constraints))
}

fn type_of_property_on_type(
    ty: Type,
    prop: &MemberProp,
    ctx: &Context,
) -> Result<InferResult, String> {
    match &ty.variant {
        Variant::Var => {
            // TODO: implementing this correctly should allow for the following
            // expression to be inferred:
            // let mag_square = (point) => point.x * point.x + point.y * point.y
            let tv = ctx.fresh_var();
            let obj = ctx.object_with_flag(
                &[types::TProp {
                    name: prop.name(),
                    // We assume the property is not optional when inferring an
                    // object from a member access.
                    optional: false,
                    ty: tv.clone(),
                }],
                Flag::MemberAccess,
            );
            let mem1 = ctx.mem(ty.clone(), &prop.name());
            let mem2 = ctx.mem(obj.clone(), &prop.name());
            Ok((
                tv,
                vec![
                    Constraint {
                        types: (mem1, mem2),
                    },
                    Constraint { types: (ty, obj) },
                ],
            ))
        }
        Variant::Lam(_) => todo!(),
        Variant::Prim(_) => todo!(),
        Variant::Lit(_) => todo!(),
        Variant::Union(_) => {
            // Check if the property name exists on all elements in the union
            // It should fail if any of the elements in the union are not objects
            // This is also where we'd handle optional chaining
            todo!()
        },
        Variant::Intersection(_) => todo!(),
        Variant::Object(props) => {
            // TODO: allow the use of string literals to access properties on
            // object types.
            let mem = ctx.mem(ty.clone(), &prop.name());
            let prop = props.iter().find(|p| p.name == prop.name());

            match prop {
                Some(_) => Ok((mem, vec![])),
                // TODO: include property name in error message
                None => Err(String::from("Record literal doesn't contain property")),
            }
        }
        Variant::Alias(alias) => {
            match ctx.types.get(&alias.name) {
                Some(scheme) => {
                    // Replaces qualifiers in the scheme with the corresponding type params
                    // from the alias type.
                    let ids = scheme.qualifiers.iter().map(|id| id.to_owned());
                    let subs: Subst = match &alias.type_params {
                        Some(type_params) => {
                            if scheme.qualifiers.len() != type_params.len() {
                                return Err(String::from("mismatch between number of qualifiers in scheme and number of type params"));
                            }
                            ids.zip(type_params.iter().cloned()).collect()
                        },
                        None => {
                            if !scheme.qualifiers.is_empty() {
                                return Err(String::from("mismatch between number of qualifiers in scheme and number of type params"));
                            }
                            ids.zip(scheme.qualifiers.iter().map(|_| ctx.fresh_var())).collect()
                        },
                    };
                    let aliased_def = scheme.ty.apply(&subs);

                    // Recurses to get the prop on the underlying type.
                    type_of_property_on_type(aliased_def, prop, ctx)
                }
                None => Err(String::from("Can't find alias in context")),
            }
        }
        Variant::Tuple(_) => todo!(),
        Variant::Rest(_) => todo!(),
        Variant::Member(_) => todo!(),
    }
}

fn unwrap_member_type(ty: &Type, ctx: &Context) -> Type {
    match &ty.variant {
        Variant::Member(member) => match &member.obj.as_ref().variant {
            Variant::Object(props) => {
                let prop = props.iter().find(|prop| prop.name == member.prop);
                match prop {
                    Some(prop) => match prop.optional {
                        true => ctx.union(vec![
                            unwrap_member_type(&prop.ty, ctx),
                            ctx.prim(types::Primitive::Undefined),
                        ]),
                        false => unwrap_member_type(&prop.ty, ctx),
                    },
                    None => ty.to_owned(),
                }
            }
            _ => ty.to_owned(),
        },
        _ => ty.to_owned(),
    }
}
