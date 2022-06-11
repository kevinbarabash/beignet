use std::collections::HashMap;
use std::iter::Iterator;

use crate::ast::*;
use crate::types::{self, Flag, Type, Variant};

use super::constraint_solver::Constraint;
use super::context::Context;
use super::infer_pattern::infer_pattern;

type InferFn = fn(expr: &Expr, ctx: &Context) -> Result<(Type, Vec<Constraint>), String>;

pub fn infer_lambda(
    lambda: &Lambda,
    ctx: &Context,
    infer: InferFn,
) -> Result<(Type, Vec<Constraint>), String> {
    let Lambda {
        params,
        body,
        is_async,
        type_params,
        ..
    } = lambda;

    // Creates a new type variable for each arg
    let mut constraints: Vec<Constraint> = vec![];
    let mut new_ctx = ctx.clone();
    new_ctx.is_async = is_async.to_owned();

    // TODO: raise an error if there's a duplicate indentifier in the
    // list of type params.
    let type_params_map: HashMap<String, Type> = match type_params {
        Some(params) => params
            .iter()
            .map(|param| (param.name.name.to_owned(), new_ctx.fresh_var()))
            .collect(),
        None => HashMap::default(),
    };

    let param_tvs: Result<Vec<_>, String> = params
        .iter()
        .map(|param| {
            let (mut param_type, mut param_cs, new_vars) =
                infer_pattern(param, &mut new_ctx, &type_params_map)?;
            constraints.append(&mut param_cs);

            // NOTE: We may not actually need to do this.  The tests pass without it.
            // That may change as we add more test cases though so I'm going to leave
            // it here for now.
            // TODO: set the flag to Flag::Param so that we have more semantic
            // information if and when we need to resolve conflicts in the constraint
            // solver.
            param_type.flag = Some(Flag::SupertypeWins);

            // Inserts any new variables introduced by infer_pattern() into
            // the current context.
            for (name, scheme) in new_vars {
                new_ctx.values.insert(name, scheme);
            }

            Ok(param_type)
        })
        .collect();

    let (ret, mut body_cs) = infer(body, &new_ctx)?;
    constraints.append(&mut body_cs);

    // Ensures that type variable ids are unique.
    ctx.state.count.set(new_ctx.state.count.get());

    let ret = match !is_async || is_promise(&ret) {
        true => ret,
        false => ctx.alias("Promise", Some(vec![ret])),
    };

    let lam_ty = ctx.lam(param_tvs?, Box::new(ret));

    // TODO: add a constraint for the return type if it's specified

    Ok((lam_ty, constraints))
}

fn is_promise(ty: &Type) -> bool {
    matches!(&ty.variant, Variant::Alias(types::AliasType { name, .. }) if name == "Promise")
}
