use std::collections::HashMap;

use nouveau_lib::syntax::{BindingIdent, Expr};
use nouveau_lib::types;
use nouveau_lib::types::*;

fn main() {
    println!("Hello, world!");

    let t = Type::Lam(types::TLam {
        args: vec![Type::Prim(Primitive::Num)],
        ret: Box::new(Type::Prim(Primitive::Num)),
    });

    if let Type::Lam(types::TLam { args, .. }) = &t {
        println!("t is a lambad and it takes {} args", args.len());
    }

    println!("t = {:?}", &t);

    let expr = Expr::Lam(
        vec![BindingIdent::Ident(String::from("x"))],
        // TODO: add helpers like Lit::from(5.0), Lit::from("hello"), etc.
        Box::new(Expr::Lit(Literal::Num(String::from("5.0")))),
        false,
    );

    let sub = HashMap::new();
    t.apply(&sub);
    t.apply(&sub);

    println!("expr = {:?}", &expr);
}
