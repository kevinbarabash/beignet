use generational_arena::{Arena, Index};
use std::collections::BTreeMap;

use crate::ast::{self as syntax, *};
use crate::context::*;
use crate::errors::*;
use crate::infer_pattern::*;
use crate::types::{self, *};
use crate::unify::*;
use crate::util::*;

/// Computes the type of the expression given by node.
///
/// The type of the node is computed in the context of the
/// supplied type environment env. Data types can be introduced into the
/// language simply by having a predefined set of identifiers in the initial
/// environment. environment; this way there is no need to change the syntax or, more
/// importantly, the type-checking program when extending the language.
///
/// Args:
///     node: The root of the abstract syntax tree.
///     env: The type environment is a mapping of expression identifier names
///         to type assignments.
///     non_generic: A set of non-generic variables, or None
///
/// Returns:
///     The computed type of the expression.
///
/// Raises:
///     InferenceError: The type of the expression could not be inferred, for example
///         if it is not possible to unify two types such as Integer and Bool
///     ParseError: The abstract syntax tree rooted at node could not be parsed
pub fn infer_expression<'a>(
    arena: &mut Arena<Type>,
    node: &'a mut Expr,
    ctx: &mut Context,
) -> Result<Index, Errors> {
    let t: Index = match &mut node.kind {
        ExprKind::Ident(Ident { name, .. }) => get_type(arena, name, ctx)?,
        ExprKind::Lit(literal) => new_lit_type(arena, literal),
        ExprKind::Tuple(syntax::Tuple { elems, .. }) => {
            let mut element_types = vec![];
            for element in elems.iter_mut() {
                // TODO: handle spreads
                let t = infer_expression(arena, element.expr.as_mut(), ctx)?;
                element_types.push(t);
            }
            new_tuple_type(arena, &element_types)
        }
        ExprKind::Obj(syntax::Obj { props }) => {
            let mut prop_types: Vec<types::TObjElem> = vec![];
            for prop_or_spread in props.iter_mut() {
                match prop_or_spread {
                    PropOrSpread::Spread(_) => todo!(),
                    PropOrSpread::Prop(prop) => match prop.as_mut() {
                        Prop::Shorthand(ident) => {
                            prop_types.push(types::TObjElem::Prop(types::TProp {
                                name: TPropKey::StringKey(ident.name.to_owned()),
                                t: get_type(arena, &ident.name, ctx)?,
                                mutable: false,
                                optional: false,
                            }));
                        }
                        Prop::KeyValue(KeyValueProp { key, value }) => {
                            prop_types.push(types::TObjElem::Prop(types::TProp {
                                name: TPropKey::StringKey(key.name.to_owned()),
                                t: infer_expression(arena, value.as_mut(), ctx)?,
                                mutable: false,
                                optional: false,
                            }));
                        }
                    },
                }
            }
            new_object_type(arena, &prop_types)
        }
        ExprKind::App(App {
            lam: func, args, ..
        }) => {
            let func_type = infer_expression(arena, func, ctx)?;

            let arg_types = args
                .iter_mut()
                .map(|arg| {
                    // TODO: handle spreads
                    infer_expression(arena, arg.expr.as_mut(), ctx)
                })
                .collect::<Result<Vec<_>, _>>()?;

            unify_call(arena, &arg_types, func_type)?
        }
        // TODO: Add support for explicit type parameters
        ExprKind::Lambda(Lambda {
            params,
            body,
            is_async,
            ..
        }) => {
            let mut param_types = vec![];
            let mut new_ctx = ctx.clone();
            new_ctx.is_async = *is_async;

            for EFnParam { pat: pattern, .. } in params.iter_mut() {
                let param_type = new_var_type(arena);
                if let PatternKind::Ident(BindingIdent {
                    name, mutable: _, ..
                }) = &pattern.kind
                {
                    pattern.inferred_type = Some(param_type);
                    new_ctx.env.insert(name.to_owned(), param_type);
                    new_ctx.non_generic.insert(param_type);
                    param_types.push(param_type);
                } else {
                    return Err(Errors::InferenceError(
                        "Other patterns are not yet supported as function params".to_owned(),
                    ));
                }
            }

            let mut body_t = 'outer: {
                match body {
                    BlockOrExpr::Block(Block { stmts, .. }) => {
                        for stmt in stmts.iter_mut() {
                            new_ctx = new_ctx.clone();
                            let t = infer_statement(arena, stmt, &mut new_ctx, false)?;
                            if let StmtKind::ReturnStmt(_) = stmt.kind {
                                // TODO: warn about unreachable code.
                                break 'outer t;
                            }
                        }

                        // If we don't encounter a return statement, we assume
                        // the return type is `undefined`.
                        new_constructor(arena, "undefined", &[])
                    }
                    BlockOrExpr::Expr(expr) => infer_expression(arena, expr, &mut new_ctx)?,
                }
            };

            if *is_async && !is_promise(&arena[body_t]) {
                body_t = new_constructor(arena, "Promise", &[body_t]);
            }

            new_func_type(arena, &param_types, body_t, None)
        }
        ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = infer_expression(arena, cond, ctx)?;
            let bool_type = new_constructor(arena, "boolean", &[]);
            unify(arena, cond_type, bool_type)?;
            let consequent_type = infer_block(arena, consequent, ctx)?;
            // TODO: handle the case where there is no alternate
            let alternate_type = infer_block(arena, &mut alternate.clone().unwrap(), ctx)?;
            new_union_type(arena, &[consequent_type, alternate_type])
        }
        ExprKind::Member(Member { obj, prop }) => {
            let obj_idx = infer_expression(arena, obj, ctx)?;
            let obj_type = arena[obj_idx].clone();

            match (&obj_type.kind, prop) {
                (TypeKind::Object(_), MemberProp::Ident(Ident { name, .. })) => {
                    get_prop(arena, obj_idx, name)?
                }
                (TypeKind::Constructor(union), MemberProp::Ident(Ident { name, .. }))
                    if union.name == "@@union" =>
                {
                    let mut types = vec![];
                    for idx in &union.types {
                        types.push(get_prop(arena, *idx, name)?);
                    }
                    new_union_type(arena, &types)
                }
                (
                    TypeKind::Constructor(tuple),
                    MemberProp::Computed(ComputedPropName { expr, .. }),
                ) if tuple.name == "@@tuple" => {
                    let prop_type = infer_expression(arena, expr.as_mut(), ctx)?;
                    match &arena[prop_type].kind {
                        TypeKind::Literal(Lit::Num(Num { value, .. })) => {
                            let index: usize = value.parse().unwrap();
                            if index < tuple.types.len() {
                                // TODO: update AST with the inferred type
                                return Ok(tuple.types[index]);
                            }
                            return Err(Errors::InferenceError(format!(
                                "{index} was outside the bounds 0..{} of the tuple",
                                tuple.types.len()
                            )));
                        }
                        _ => {
                            return Err(Errors::InferenceError(
                                "Can only access tuple properties with a number".to_string(),
                            ));
                        }
                    }
                }
                _ => {
                    return Err(Errors::InferenceError(
                        "Can only access properties on objects/tuples".to_string(),
                    ));
                }
            }
        }
        ExprKind::New(_) => todo!(),
        ExprKind::JSXElement(_) => todo!(),
        ExprKind::Assign(_) => todo!(),
        ExprKind::LetExpr(_) => todo!(),
        ExprKind::Keyword(_) => todo!(), // null, undefined, etc.
        ExprKind::BinaryExpr(BinaryExpr { op, left, right }) => {
            let number = new_constructor(arena, "number", &[]);
            let boolean = new_constructor(arena, "boolean", &[]);
            let left_type = infer_expression(arena, left, ctx)?;
            let right_type = infer_expression(arena, right, ctx)?;
            unify(arena, left_type, number)?;
            unify(arena, right_type, number)?;

            match op {
                BinOp::Add => number,
                BinOp::Sub => number,
                BinOp::Mul => number,
                BinOp::Div => number,
                BinOp::EqEq => boolean,
                BinOp::NotEq => boolean,
                BinOp::Gt => boolean,
                BinOp::GtEq => boolean,
                BinOp::Lt => boolean,
                BinOp::LtEq => boolean,
            }
        }
        ExprKind::UnaryExpr(UnaryExpr { op, arg }) => {
            let number = new_constructor(arena, "number", &[]);
            let arg_type = infer_expression(arena, arg, ctx)?;
            unify(arena, arg_type, number)?;

            match op {
                UnaryOp::Minus => number,
            }
        }
        ExprKind::Await(Await { expr, .. }) => {
            if !ctx.is_async {
                return Err(Errors::InferenceError(
                    "Can't use await outside of an async function".to_string(),
                ));
            }

            let expr_t = infer_expression(arena, expr, ctx)?;
            let inner_t = new_var_type(arena);
            // TODO: Merge Constructor and TypeRef
            // NOTE: This isn't quite right because we can await non-promise values.
            // That being said, we should avoid doing so.
            let promise_t = new_constructor(arena, "Promise", &[inner_t]);
            unify(arena, expr_t, promise_t)?;

            inner_t
        }
        ExprKind::Empty => todo!(),
        ExprKind::TemplateLiteral(_) => todo!(),
        ExprKind::TaggedTemplateLiteral(_) => todo!(),
        ExprKind::Match(Match { expr, arms }) => {
            let expr_idx = infer_expression(arena, expr, ctx)?;
            let mut body_types: Vec<Index> = vec![];

            for arm in arms.iter_mut() {
                let (pat_bindings, pat_idx) = infer_pattern(arena, &mut arm.pattern, ctx)?;

                // Checks that the pattern is a sub-type of expr
                unify(arena, pat_idx, expr_idx)?;

                let mut new_ctx = ctx.clone();
                for (name, binding) in pat_bindings {
                    // TODO: Update .env to store bindings so that we can handle
                    // mutability correctly
                    new_ctx.env.insert(name, binding.t);
                }

                body_types.push(infer_block(arena, &mut arm.body, &mut new_ctx)?);
            }

            let t0 = prune(arena, body_types[0]);
            eprintln!("t0 = {}", arena[t0].as_string(arena));

            let t1 = prune(arena, body_types[1]);
            eprintln!("t1 = {}", arena[t1].as_string(arena));

            new_union_type(arena, &body_types)
        }
        ExprKind::Class(_) => todo!(),
        ExprKind::Regex(_) => todo!(),
        ExprKind::DoExpr(DoExpr { body }) => infer_block(arena, body, ctx)?,
    };

    node.inferred_type = Some(t);

    Ok(t)
}

fn is_promise(t: &Type) -> bool {
    matches!(
        t,
        Type {
            kind: TypeKind::Constructor(types::Constructor { name, .. })
        } if name == "Promise"
    )
}

pub fn infer_block(
    arena: &mut Arena<Type>,
    block: &mut Block,
    ctx: &mut Context,
) -> Result<Index, Errors> {
    let mut new_ctx = ctx.clone();
    let mut result_t = new_constructor(arena, "undefined", &[]);

    for stmt in &mut block.stmts.iter_mut() {
        result_t = infer_statement(arena, stmt, &mut new_ctx, false)?;
    }

    Ok(result_t)
}

pub fn infer_type_ann(
    arena: &mut Arena<Type>,
    type_ann: &mut TypeAnn,
    _ctx: &mut Context,
) -> Result<Index, Errors> {
    // TODO: handle type params

    let idx = match &mut type_ann.kind {
        TypeAnnKind::Lam(LamType {
            params,
            ret,
            type_params,
        }) => {
            let params = params
                .iter_mut()
                .map(|param| infer_type_ann(arena, &mut param.type_ann, _ctx))
                .collect::<Result<Vec<_>, _>>()?;

            let ret_idx = infer_type_ann(arena, ret.as_mut(), _ctx)?;

            let type_params = type_params.as_mut().map(|type_params| {
                type_params
                    .iter_mut()
                    .map(|param| types::TypeParam {
                        name: param.name.name.clone(),
                    })
                    .collect()
            });

            new_func_type(arena, &params, ret_idx, type_params)
        }
        TypeAnnKind::Lit(lit) => new_lit_type(arena, lit),
        TypeAnnKind::Keyword(KeywordType { keyword }) => match keyword {
            Keyword::Number => new_constructor(arena, "number", &[]),
            Keyword::Boolean => new_constructor(arena, "boolean", &[]),
            Keyword::String => new_constructor(arena, "string", &[]),
            Keyword::Null => new_constructor(arena, "null", &[]),
            Keyword::Self_ => todo!(),
            Keyword::Symbol => new_constructor(arena, "symbol", &[]),
            Keyword::Undefined => new_constructor(arena, "undefined", &[]),
            Keyword::Never => new_constructor(arena, "never", &[]),
        },
        TypeAnnKind::Object(obj) => {
            let mut props: Vec<types::TObjElem> = Vec::new();
            for elem in obj.elems.iter_mut() {
                match elem {
                    syntax::TObjElem::Index(_) => {
                        todo!();
                    }
                    syntax::TObjElem::Prop(prop) => {
                        props.push(types::TObjElem::Prop(types::TProp {
                            name: TPropKey::StringKey(prop.name.to_owned()),
                            t: infer_type_ann(arena, &mut prop.type_ann, _ctx)?,
                            mutable: prop.mutable,
                            optional: prop.optional,
                        }));
                    }
                }
            }
            new_object_type(arena, &props)
        }
        TypeAnnKind::TypeRef(TypeRef { name, type_args }) => match type_args {
            Some(type_args) => {
                let mut type_args_idxs = Vec::new();
                for type_arg in type_args.iter_mut() {
                    type_args_idxs.push(infer_type_ann(arena, type_arg, _ctx)?);
                }
                new_constructor(arena, name, &type_args_idxs)
            }
            None => new_constructor(arena, name, &[]),
        },
        TypeAnnKind::Union(UnionType { types }) => {
            let mut idxs = Vec::new();
            for type_ann in types.iter_mut() {
                idxs.push(infer_type_ann(arena, type_ann, _ctx)?);
            }
            new_union_type(arena, &idxs)
        }
        TypeAnnKind::Intersection(IntersectionType { types }) => {
            let mut idxs = Vec::new();
            for type_ann in types.iter_mut() {
                idxs.push(infer_type_ann(arena, type_ann, _ctx)?);
            }
            new_intersection_type(arena, &idxs)
        }
        TypeAnnKind::Tuple(TupleType { types }) => {
            let mut idxs = Vec::new();
            for type_ann in types.iter_mut() {
                idxs.push(infer_type_ann(arena, type_ann, _ctx)?);
            }
            new_tuple_type(arena, &idxs)
        }
        TypeAnnKind::Array(ArrayType { elem_type }) => {
            let idx = infer_type_ann(arena, elem_type, _ctx)?;
            new_constructor(arena, "Array", &[idx])
        }
        TypeAnnKind::IndexedAccess(_) => todo!(),
        TypeAnnKind::Mutable(_) => todo!(),
        TypeAnnKind::Query(_) => todo!(),

        // TODO: Create types for all of these
        TypeAnnKind::KeyOf(_) => todo!(),
        TypeAnnKind::Mapped(_) => todo!(),
        TypeAnnKind::Conditional(_) => todo!(),
        TypeAnnKind::Infer(_) => todo!(),
    };

    type_ann.inferred_type = Some(idx);

    Ok(idx)
}

pub fn infer_statement<'a>(
    arena: &'a mut Arena<Type>,
    statement: &mut Statement,
    ctx: &mut Context,
    top_level: bool,
) -> Result<Index, Errors> {
    let t = match &mut statement.kind {
        StmtKind::VarDecl(VarDecl {
            rec,
            pattern,
            init,
            type_ann,
            declare,
            ..
        }) => {
            let (pat_bindings, pat_type) = infer_pattern(arena, pattern, ctx)?;

            match (declare, init, type_ann) {
                (false, Some(init), type_ann) => {
                    let init_idx = if *rec {
                        let mut new_ctx = ctx.clone();

                        for (name, binding) in &pat_bindings {
                            new_ctx.env.insert(name.clone(), binding.t);
                            new_ctx.non_generic.insert(binding.t);
                        }

                        infer_expression(arena, init.as_mut(), &mut new_ctx)?
                    } else {
                        infer_expression(arena, init.as_mut(), ctx)?
                    };

                    let init_type = arena.get(init_idx).unwrap().clone();
                    let init_idx = match &init_type.kind {
                        TypeKind::Function(func) if top_level => generalize_func(arena, func),
                        _ => init_idx,
                    };
                    eprintln!("init_idx = {}", arena[init_idx].as_string(arena));

                    let idx = match type_ann {
                        Some(type_ann) => {
                            let type_ann_idx = infer_type_ann(arena, type_ann, ctx)?;

                            // The initializer must conform to the type annotation's
                            // inferred type.
                            unify(arena, init_idx, type_ann_idx)?;
                            // Results in bindings introduced by the LHS pattern
                            // having their types inferred.
                            // It's okay for pat_type to be the super type here
                            // because all initializers it introduces are type
                            // variables.  It also prevents patterns from including
                            // variables that don't exist in the type annotation.
                            unify(arena, type_ann_idx, pat_type)?;

                            type_ann_idx
                        }
                        None => {
                            // Results in bindings introduced by the LHS pattern
                            // having their types inferred.
                            // It's okay for pat_type to be the super type here
                            // because all initializers it introduces are type
                            // variables.  It also prevents patterns from including
                            // variables that don't exist in the initializer.
                            unify(arena, init_idx, pat_type)?;

                            init_idx
                        }
                    };

                    for (name, binding) in &pat_bindings {
                        eprintln!(
                            "inserting binding for {name}, {}",
                            arena[binding.t].as_string(arena)
                        );
                        ctx.env.insert(name.clone(), binding.t);
                    }

                    pattern.inferred_type = Some(idx);

                    idx // TODO: Should this be unit?
                }
                (false, None, _) => {
                    return Err(Errors::InferenceError(
                        "Variable declarations not using `declare` must have an initializer"
                            .to_string(),
                    ))
                }
                (true, None, Some(type_ann)) => {
                    let idx = infer_type_ann(arena, type_ann, ctx)?;

                    unify(arena, idx, pat_type)?;

                    for (name, binding) in &pat_bindings {
                        eprintln!(
                            "inserting binding for {name}, {}",
                            arena[binding.t].as_string(arena)
                        );
                        ctx.env.insert(name.clone(), binding.t);
                    }

                    idx
                }
                (true, Some(_), _) => {
                    return Err(Errors::InferenceError(
                        "Variable declarations using `declare` cannot have an initializer"
                            .to_string(),
                    ))
                }
                (true, None, None) => {
                    return Err(Errors::InferenceError(
                        "Variable declarations using `declare` must have a type annotation"
                            .to_string(),
                    ))
                }
            }
        }
        StmtKind::ExprStmt(expr) => infer_expression(arena, expr, ctx)?,
        StmtKind::ReturnStmt(ReturnStmt { arg: expr }) => {
            // TODO: handle multiple return statements
            // TODO: warn about unreachable code after a return statement
            match expr {
                Some(expr) => infer_expression(arena, expr, ctx)?,
                None => {
                    // TODO: return `undefined` or `void`.
                    todo!()
                }
            }
        }
        StmtKind::ClassDecl(_) => todo!(),
        StmtKind::TypeDecl(_) => {
            // TODO: add support for type declarations
            todo!()
        }
        StmtKind::ForStmt(_) => todo!(),
    };

    statement.inferred_type = Some(t);

    Ok(t)
}

pub fn infer_program<'a>(
    arena: &mut Arena<Type>,
    node: &'a mut Program,
    ctx: &mut Context,
) -> Result<(), Errors> {
    for stmt in &mut node.statements.iter_mut() {
        infer_statement(arena, stmt, ctx, true)?;
    }

    Ok(())
}

// TODO:
// - find all type variables in the type
// - create type params for them
// - replace the type variables with the corresponding type params
// - return the new function type with the newly created type params

pub fn generalize_func(arena: &'_ mut Arena<Type>, func: &Function) -> Index {
    // A mapping of TypeVariables to TypeVariables
    let mut mappings = BTreeMap::default();

    // TODO: dedupe with freshrec and instrec
    fn generalize_rec(
        arena: &mut Arena<Type>,
        tp: Index,
        mappings: &mut BTreeMap<Index, Ref>,
    ) -> Index {
        let p = prune(arena, tp);
        // We clone here because we can't move out of a shared reference.
        // TODO: Consider using Rc<RefCell<Type>> to avoid unnecessary cloning.
        let a_t = arena.get(p).unwrap().clone();
        match &a_t.kind {
            TypeKind::Variable(_) => {
                // Replace with a type reference/constructor
                let name = match mappings.get(&p) {
                    Some(tref) => tref.name.clone(),
                    None => {
                        let name = ((mappings.len() as u8) + 65) as char;
                        let name = format!("{}", name);
                        // let name = format!("'{}", mappings.len());
                        mappings.insert(p, Ref { name: name.clone() });
                        name
                    }
                };
                new_constructor(arena, &name, &[])
            }
            TypeKind::Literal(lit) => new_lit_type(arena, lit),
            TypeKind::Object(object) => {
                let fields: Vec<_> = object
                    .props
                    .iter()
                    .map(|prop| match prop {
                        types::TObjElem::Index(index) => {
                            let t = generalize_rec(arena, index.t, mappings);
                            types::TObjElem::Index(types::TIndex { t, ..index.clone() })
                        }
                        types::TObjElem::Prop(prop) => {
                            let t = generalize_rec(arena, prop.t, mappings);
                            types::TObjElem::Prop(types::TProp { t, ..prop.clone() })
                        }
                    })
                    .collect();
                new_object_type(arena, &fields)
            }
            TypeKind::Function(func) => {
                let params = generalize_rec_many(arena, &func.params, mappings);
                let ret = generalize_rec(arena, func.ret, mappings);
                new_func_type(arena, &params, ret, None)
            }
            TypeKind::Constructor(con) => {
                let types = generalize_rec_many(arena, &con.types, mappings);
                new_constructor(arena, &con.name, &types)
            }
        }
    }

    pub fn generalize_rec_many(
        a: &mut Arena<Type>,
        types: &[Index],
        mappings: &mut BTreeMap<Index, Ref>,
    ) -> Vec<Index> {
        types
            .iter()
            .map(|x| generalize_rec(a, *x, mappings))
            .collect()
    }

    let params = generalize_rec_many(arena, &func.params, &mut mappings);
    let ret = generalize_rec(arena, func.ret, &mut mappings);
    let type_params: Vec<types::TypeParam> = mappings
        .iter()
        .map(|(_, v)| types::TypeParam {
            name: v.name.clone(),
        })
        .collect();

    new_func_type(arena, &params, ret, Some(type_params))
}

fn get_prop(arena: &mut Arena<Type>, obj_idx: Index, name: &str) -> Result<Index, Errors> {
    let undefined = new_constructor(arena, "undefined", &[]);
    if let TypeKind::Object(object) = &arena[obj_idx].kind {
        for prop in &object.props {
            if let types::TObjElem::Prop(prop) = &prop {
                let key = match &prop.name {
                    TPropKey::StringKey(key) => key,
                    TPropKey::NumberKey(key) => key,
                };
                if key == name {
                    let prop_t = match prop.optional {
                        true => new_union_type(arena, &[prop.t, undefined]),
                        false => prop.t,
                    };
                    return Ok(prop_t);
                }
            }
        }
        Err(Errors::InferenceError(format!(
            "Couldn't find property '{name}' on object",
        )))
    } else {
        Err(Errors::InferenceError(
            "Can't access property on non-object type".to_string(),
        ))
    }
}

fn is_object<'a>(arena: &'a Arena<Type>, t: Index) -> Result<bool, Errors> {
    match arena.get(t) {
        Some(t) => match t.kind {
            TypeKind::Object(_) => Ok(true),
            _ => Ok(false),
        },
        None => Err(Errors::InferenceError(
            "Can't find index in arena".to_string(),
        )),
    }
}
