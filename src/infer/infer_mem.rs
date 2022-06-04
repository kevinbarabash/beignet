use super::constraint_solver::Constraint;
use crate::ast::*;
use crate::types::{self, Type, WidenFlag};

use super::context::Context;
use super::infer::InferResult;

pub fn infer_mem(
    infer: fn(expr: &Expr, ctx: &Context) -> Result<InferResult, String>,
    mem: &Member,
    ctx: &Context,
) -> Result<InferResult, String> {
    let Member { obj, prop, .. } = mem;

    let (obj_type, mut obj_cs) = infer(obj, ctx)?;
    let (prop_type, mut prop_cs) = type_of_property_on_type(obj_type, prop, ctx)?;

    let mut cs: Vec<Constraint> = vec![];
    cs.append(&mut obj_cs);
    cs.append(&mut prop_cs);

    Ok((unwrap_member_type(&prop_type, ctx), cs))
}

fn type_of_property_on_type(
    ty: Type,
    prop: &MemberProp,
    ctx: &Context,
) -> Result<InferResult, String> {
    match &ty {
        Type::Var(_) => {
            // TODO: implementing this correctly should allow for the following
            // expression to be inferred:
            // let mag_square = (point) => point.x * point.x + point.y * point.y
            let tv = ctx.fresh_var();
            let obj = ctx.object(
                &[types::TProp {
                    name: prop.name(),
                    // We assume the property is not optional when inferring an
                    // object from a member access.
                    optional: false,
                    ty: tv.clone(),
                }],
                Some(WidenFlag::Intersection),
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
        Type::Lam(_) => todo!(),
        Type::Prim(_) => todo!(),
        Type::Lit(_) => todo!(),
        Type::Union(_) => {
            // Check if the property name exists on all elements in the union
            // It should fail if any of the elements in the union are not objects
            // This is also where we'd handle optional chaining
            todo!()
        },
        Type::Intersection(_) => todo!(),
        Type::Object(obj) => {
            // TODO: allow the use of string literals to access properties on
            // object types.
            let mem = ctx.mem(ty.clone(), &prop.name());
            let prop = obj.props.iter().find(|p| p.name == prop.name());

            match prop {
                Some(_) => Ok((mem, vec![])),
                // TODO: include property name in error message
                None => Err(String::from("Record literal doesn't contain property")),
            }
        }
        Type::Alias(alias) => {
            match ctx.types.get(&alias.name) {
                Some(scheme) => {
                    // TODO: handle schemes with qualifiers
                    let aliased_def = scheme.ty.to_owned();
                    type_of_property_on_type(aliased_def, prop, ctx)
                }
                None => Err(String::from("Can't find alias in context")),
            }
        }
        Type::Tuple(_) => todo!(),
        Type::Rest(_) => todo!(),
        Type::Member(_) => todo!(),
    }
}

fn unwrap_member_type(ty: &Type, ctx: &Context) -> Type {
    match ty {
        Type::Member(member) => match member.obj.as_ref() {
            Type::Object(obj) => {
                let prop = obj.props.iter().find(|prop| prop.name == member.prop);
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
