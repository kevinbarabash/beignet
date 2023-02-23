use escalier_ast::types::{TObjElem, TObject, TProp, TPropKey, Type, TypeKind};
use escalier_ast::values::*;

use crate::context::Context;
use crate::infer_expr::infer_expr as infer_expr_rec;
use crate::infer_stmt::infer_stmt;
use crate::type_error::TypeError;
use crate::util::close_over;

pub fn infer_prog(prog: &mut Program, ctx: &mut Context) -> Result<Context, Vec<TypeError>> {
    // TODO: replace with Class type once it exists
    // We use {_name: "Promise"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: TPropKey::StringKey(String::from("_name")),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("Promise"), 0..0, DUMMY_LOC)),
    })];
    let promise_type = Type::from(TypeKind::Object(TObject {
        elems,
        is_interface: true,
    }));
    ctx.insert_type(String::from("Promise"), promise_type);
    // TODO: replace with Class type once it exists
    // We use {_name: "JSXElement"} to differentiate it from other
    // object types.
    let elems = vec![TObjElem::Prop(TProp {
        name: TPropKey::StringKey(String::from("_name")),
        optional: false,
        mutable: false,
        t: Type::from(Lit::str(String::from("JSXElement"), 0..0, DUMMY_LOC)),
    })];
    let jsx_element_type = Type::from(TypeKind::Object(TObject {
        elems,
        is_interface: true,
    }));
    ctx.insert_type(String::from("JSXElement"), jsx_element_type);

    let mut reports: Vec<TypeError> = vec![];

    // TODO: figure out how report multiple errors
    for stmt in &mut prog.body {
        match infer_stmt(stmt, ctx, true) {
            Ok(_) => (),
            Err(mut errors) => reports.append(&mut errors),
        }
    }

    if reports.is_empty() {
        Ok(ctx.to_owned())
    } else {
        Err(reports)
    }
}

pub fn infer_expr(ctx: &mut Context, expr: &mut Expr) -> Result<Type, Vec<TypeError>> {
    let (s, t) = infer_expr_rec(ctx, expr, false)?;
    Ok(close_over(&s, &t, ctx))
}
