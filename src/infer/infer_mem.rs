use super::constraint_solver::Constraint;
use crate::ast::*;
use crate::types::Type;

use super::context::Context;
use super::infer::InferResult;

pub fn infer_mem(
    infer: fn(expr: &Expr, ctx: &Context) -> InferResult,
    mem: &Member,
    ctx: &Context,
) -> InferResult {
    let Member { obj, prop, .. } = mem;

    let (obj_type, mut obj_cs) = infer(obj, ctx);
    let (prop_type, mut prop_cs) = type_of_property_on_type(obj_type, prop, ctx);

    let mut cs: Vec<Constraint> = vec![];
    cs.append(&mut obj_cs);
    cs.append(&mut prop_cs);

    (unwrap_member_type(&prop_type).to_owned(), cs)
}

fn type_of_property_on_type(ty: Type, prop: &MemberProp, ctx: &Context) -> InferResult {
    match &ty {
        Type::Var(_) => {
            // TODO: implementing this correctly should allow for the following
            // expression to be inferred:
            // let mag_square = (point) => point.x * point.x + point.y * point.y
            todo!()
        },
        Type::Lam(_) => todo!(),
        Type::Prim(_) => todo!(),
        Type::Lit(_) => todo!(),
        Type::Union(_) => todo!(),
        Type::Object(obj) => {
            // TODO: allow the use of string literals to access properties on
            // object types.
            let mem = ctx.mem(ty.clone(), &unwrap_property(prop));
            let prop = obj.props.iter().find(|p| p.name == unwrap_property(prop));

            match prop {
                Some(_) => (mem, vec![]),
                // TODO: include property name in error message
                None => panic!("Record literal doesn't contain property"),
            }
        },
        Type::Alias(_) => todo!(),
        Type::Tuple(_) => todo!(),
        Type::Member(_) => todo!(),
    }
}

fn unwrap_property(prop: &MemberProp) -> String {
    match prop {
        MemberProp::Ident(Ident { name, .. }) => name.to_owned(),
        MemberProp::Computed(_) => todo!(),
    }
}

fn unwrap_member_type(ty: &'_ Type) -> &'_ Type {
    match ty {
        Type::Member(member) => match member.obj.as_ref() {
            Type::Object(obj) => {
                let prop = obj.props.iter().find(|prop| prop.name == member.prop);
                match prop {
                    Some(prop) => unwrap_member_type(&prop.ty),
                    None => ty,
                }
            }
            _ => ty,
        },
        _ => ty,
    }
}
