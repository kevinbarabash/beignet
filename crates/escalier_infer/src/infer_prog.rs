use escalier_ast::types::{TObjElem, TObject, TProp, TPropKey, Type, TypeKind};
use escalier_ast::values::*;

use crate::context::Context;
use crate::type_error::TypeError;

use crate::checker::Checker;

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
    let mut checker = Checker {
        current_scope: ctx.to_owned(),
        parent_scopes: vec![],
    };

    // TODO: figure out how report multiple errors
    for stmt in &mut prog.body {
        match checker.infer_stmt(stmt, true) {
            Ok(_) => (),
            Err(mut errors) => reports.append(&mut errors),
        }
    }

    if reports.is_empty() {
        Ok(checker.current_scope)
    } else {
        Err(reports)
    }
}
