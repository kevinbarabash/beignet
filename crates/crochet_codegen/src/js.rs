use std::rc::Rc;

use ast::LetExpr;
use swc_atoms::*;
use swc_common::comments::SingleThreadedComments;
use swc_common::hygiene::Mark;
use swc_common::source_map::{Globals, SourceMap, DUMMY_SP, GLOBALS};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;
use swc_ecma_transforms_react::{react, Options, Runtime};
use swc_ecma_visit::*;

use crochet_ast::{self as ast, is_refutable};

pub struct Context {
    pub temp_id: u32,
}

impl Context {
    pub fn new_ident(&mut self) -> Ident {
        let ident = Ident {
            span: DUMMY_SP,
            sym: JsWord::from(format!("$temp_{}", self.temp_id)),
            optional: false,
        };
        self.temp_id += 1;
        ident
    }
}

pub fn codegen_js(program: &ast::Program) -> String {
    let mut ctx = Context { temp_id: 0 };
    let program = build_js(program, &mut ctx);

    let cm = Rc::new(SourceMap::default());
    let comments: Option<SingleThreadedComments> = None;
    let options = Options {
        runtime: Some(Runtime::Automatic),
        ..Default::default()
    };

    let globals = Globals::default();
    // The call to Mark::new() must be wrapped in a GLOBALS.set() closure
    GLOBALS.set(&globals, || {
        let top_level_mark = Mark::new();
        let mut v = react(cm, comments, options, top_level_mark);
        let program = program.fold_with(&mut v);
        print_js(&program)
    })
}

fn print_js(program: &Program) -> String {
    let mut buf = vec![];
    let cm = Rc::new(SourceMap::default());

    let mut emitter = Emitter {
        cfg: swc_ecma_codegen::Config {
            ..Default::default()
        },
        cm: cm.clone(),
        comments: None,
        wr: text_writer::JsWriter::new(cm, "\n", &mut buf, None),
    };

    emitter.emit_program(program).unwrap();

    String::from_utf8_lossy(&buf).to_string()
}

fn build_js(program: &ast::Program, ctx: &mut Context) -> Program {
    let body: Vec<ModuleItem> = program
        .body
        .iter()
        .flat_map(|child| {
            let mut stmts: Vec<Stmt> = vec![];
            let result = match child {
                ast::Statement::VarDecl {
                    pattern,
                    init,
                    declare,
                    ..
                } => match declare {
                    true => ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP })),
                    false => {
                        // It should be okay to unwrap this here since any decl that isn't
                        // using `declare` should have an initial value.
                        let init = init.as_ref().unwrap();

                        match build_pattern(pattern, &mut stmts, ctx) {
                            Some(name) => {
                                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                                    span: DUMMY_SP,
                                    decl: Decl::Var(VarDecl {
                                        span: DUMMY_SP,
                                        kind: VarDeclKind::Const,
                                        declare: false,
                                        decls: vec![VarDeclarator {
                                            span: DUMMY_SP,
                                            name,
                                            init: Some(Box::from(build_expr(
                                                init, &mut stmts, ctx,
                                            ))),
                                            definite: false,
                                        }],
                                    }),
                                }))
                            }
                            None => todo!(),
                        }
                    }
                },
                ast::Statement::TypeDecl { .. } => {
                    ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP }))
                }
                ast::Statement::Expr { expr, .. } => ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: DUMMY_SP,
                    expr: Box::from(build_expr(expr, &mut stmts, ctx)),
                })),
            };

            let mut items: Vec<ModuleItem> = stmts
                .iter()
                .map(|stmt| ModuleItem::Stmt(stmt.to_owned()))
                .collect();
            items.push(result);

            items
        })
        .collect();

    Program::Module(Module {
        span: DUMMY_SP,
        body,
        shebang: None,
    })
}

fn build_pattern(pattern: &ast::Pattern, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Option<Pat> {
    match pattern {
        // unassignable patterns
        ast::Pattern::Lit(_) => None,
        ast::Pattern::Wildcard(_) => None,

        // assignable patterns
        ast::Pattern::Ident(ast::BindingIdent { id, .. }) => Some(Pat::Ident(BindingIdent {
            id: build_ident(id),
            type_ann: None,
        })),
        ast::Pattern::Rest(_) => todo!(),
        ast::Pattern::Object(ast::ObjectPat {
            props, optional, ..
        }) => {
            let props: Vec<ObjectPatProp> = props
                .iter()
                .filter_map(|p| match p {
                    ast::ObjectPatProp::KeyValue(kvp) => {
                        build_pattern(kvp.value.as_ref(), stmts, ctx).map(|value| {
                            ObjectPatProp::KeyValue(KeyValuePatProp {
                                key: PropName::Ident(Ident::from(&kvp.key)),
                                value: Box::from(value),
                            })
                        })
                    }
                    ast::ObjectPatProp::Assign(ap) => Some(ObjectPatProp::Assign(AssignPatProp {
                        span: DUMMY_SP,
                        key: Ident::from(&ap.key),
                        value: ap
                            .value
                            .clone()
                            .map(|value| Box::from(build_expr(value.as_ref(), stmts, ctx))),
                    })),
                    ast::ObjectPatProp::Rest(_) => todo!(),
                })
                .collect();

            Some(Pat::Object(ObjectPat {
                span: DUMMY_SP,
                optional: optional.to_owned(),
                type_ann: None, // because we're generating .js
                props,
            }))
        }
        ast::Pattern::Array(ast::ArrayPat {
            elems, optional, ..
        }) => {
            let elems: Vec<Option<Pat>> = elems
                .iter()
                .map(|elem| match elem {
                    Some(elem) => build_pattern(elem, stmts, ctx),
                    None => None,
                })
                .collect();

            // TODO: If all elems are None, we can drop the array pattern.

            Some(Pat::Array(ArrayPat {
                span: DUMMY_SP,
                elems,
                optional: optional.to_owned(),
                type_ann: None, // because we're generating .js.
            }))
        }
        ast::Pattern::Is(ast::IsPat { id, .. }) => Some(Pat::Ident(BindingIdent {
            id: build_ident(id),
            type_ann: None,
        })),
    }
}

// This should only be called by `build_expr_in_new_scope` or `build_fn_body`.
fn _build_expr(expr: &ast::Expr, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Expr {
    if let ast::Expr::Let(r#let) = expr {
        let child = let_to_child(r#let, stmts, ctx);
        stmts.push(child);

        let mut body = r#let.body.to_owned();
        while let ast::Expr::Let(r#let) = body.as_ref() {
            let child = let_to_child(r#let, stmts, ctx);
            stmts.push(child);
            body = r#let.body.to_owned();
        }

        build_expr(&body, stmts, ctx)
    } else {
        build_expr(expr, stmts, ctx)
    }
}

fn build_expr_in_new_scope(expr: &ast::Expr, temp_id: &Ident, ctx: &mut Context) -> BlockStmt {
    let mut stmts: Vec<Stmt> = vec![];

    let expr = _build_expr(expr, &mut stmts, ctx);

    // Assigns the result of the block to the temp variable
    stmts.push(Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::from(Expr::Assign(AssignExpr {
            span: DUMMY_SP,
            op: AssignOp::Assign,
            left: PatOrExpr::Pat(Box::from(Pat::Ident(BindingIdent {
                id: temp_id.to_owned(),
                type_ann: None,
            }))),
            right: Box::from(expr),
        })),
    }));

    BlockStmt {
        span: DUMMY_SP,
        stmts,
    }
}

fn e_fn_param_pat_to_js_pat(
    pat: &ast::EFnParamPat,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> Pat {
    match pat {
        ast::EFnParamPat::Ident(ast::EFnParamBindingIdent { id, .. }) => Pat::Ident(BindingIdent {
            id: Ident {
                span: DUMMY_SP,
                sym: JsWord::from(id.name.to_owned()),
                optional: false,
            },
            type_ann: None,
        }),
        ast::EFnParamPat::Rest(_) => todo!(),
        ast::EFnParamPat::Object(ast::EFnParamObjectPat { props, .. }) => {
            let props: Vec<ObjectPatProp> = props
                .iter()
                .map(|prop| match prop {
                    ast::EFnParamObjectPatProp::KeyValue(kv) => {
                        ObjectPatProp::KeyValue(KeyValuePatProp {
                            key: PropName::Ident(Ident {
                                span: DUMMY_SP,
                                sym: JsWord::from(kv.key.name.to_owned()),
                                optional: false,
                            }),
                            value: Box::from(e_fn_param_pat_to_js_pat(&kv.value, stmts, ctx)),
                        })
                    }
                    ast::EFnParamObjectPatProp::Assign(assign) => {
                        ObjectPatProp::Assign(AssignPatProp {
                            span: DUMMY_SP,
                            key: Ident {
                                span: DUMMY_SP,
                                sym: JsWord::from(assign.key.name.to_owned()),
                                optional: false,
                            },
                            value: assign
                                .value
                                .as_ref()
                                .map(|value| Box::from(build_expr(value.as_ref(), stmts, ctx))),
                        })
                    }
                    ast::EFnParamObjectPatProp::Rest(_) => todo!(),
                })
                .collect();
            Pat::Object(ObjectPat {
                span: DUMMY_SP,
                props,
                optional: false,
                type_ann: None,
            })
        }
        ast::EFnParamPat::Array(_) => todo!(),
    }
}

fn build_expr(expr: &ast::Expr, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Expr {
    match expr {
        ast::Expr::App(ast::App { lam, args, .. }) => {
            let callee = Callee::Expr(Box::from(build_expr(lam.as_ref(), stmts, ctx)));
            let args: Vec<ExprOrSpread> = args
                .iter()
                .map(|arg| ExprOrSpread {
                    spread: None,
                    expr: Box::from(build_expr(arg.expr.as_ref(), stmts, ctx)),
                })
                .collect();

            Expr::Call(CallExpr {
                span: DUMMY_SP,
                callee,
                args,
                type_args: None,
            })
        }
        ast::Expr::Ident(ident) => Expr::from(build_ident(ident)),
        ast::Expr::Lambda(ast::Lambda {
            params: args,
            body,
            is_async,
            ..
        }) => {
            let params: Vec<Pat> = args
                .iter()
                .map(|arg| e_fn_param_pat_to_js_pat(&arg.pat, stmts, ctx))
                .collect();

            let body = build_fn_body(body.as_ref(), ctx);

            Expr::Arrow(ArrowExpr {
                span: DUMMY_SP,
                params,
                body,
                is_async: is_async.to_owned(),
                is_generator: false,
                type_params: None,
                return_type: None,
            })
        }
        ast::Expr::Let(_) => {
            // let $temp_n;
            let temp_id = ctx.new_ident();
            let temp_decl = build_let_decl_stmt(&temp_id);
            stmts.push(temp_decl);

            // { ...; $temp_n = <result> }
            let block = build_expr_in_new_scope(expr, &temp_id, ctx);
            stmts.push(Stmt::Block(block));

            // $temp_n
            Expr::Ident(temp_id)
        }
        ast::Expr::Lit(lit) => Expr::from(lit),
        ast::Expr::Op(ast::Op {
            op, left, right, ..
        }) => {
            let op = match op {
                ast::BinOp::Add => BinaryOp::Add,
                ast::BinOp::Sub => BinaryOp::Sub,
                ast::BinOp::Mul => BinaryOp::Mul,
                ast::BinOp::Div => BinaryOp::Div,
                ast::BinOp::EqEq => BinaryOp::EqEqEq,
                ast::BinOp::NotEq => BinaryOp::NotEqEq,
                ast::BinOp::Lt => BinaryOp::Lt,
                ast::BinOp::LtEq => BinaryOp::LtEq,
                ast::BinOp::Gt => BinaryOp::Gt,
                ast::BinOp::GtEq => BinaryOp::GtEq,
            };

            let left = Box::from(build_expr(left, stmts, ctx));

            let wrap_left = match left.as_ref() {
                Expr::Bin(left) => left.op.precedence() < op.precedence(),
                _ => false,
            };

            let right = Box::from(build_expr(right, stmts, ctx));

            let wrap_right = match right.as_ref() {
                Expr::Bin(right) => match (op, right.op) {
                    (BinaryOp::Div, BinaryOp::Div) => true,
                    (BinaryOp::Sub, BinaryOp::Sub) => true,
                    _ => right.op.precedence() < op.precedence(),
                },
                _ => false,
            };

            Expr::Bin(BinExpr {
                span: DUMMY_SP,
                op,
                left: if wrap_left {
                    Box::from(Expr::Paren(ParenExpr {
                        span: DUMMY_SP,
                        expr: left,
                    }))
                } else {
                    left
                },
                right: if wrap_right {
                    Box::from(Expr::Paren(ParenExpr {
                        span: DUMMY_SP,
                        expr: right,
                    }))
                } else {
                    right
                },
            })
        }
        ast::Expr::UnaryExpr(ast::UnaryExpr { arg, op, .. }) => {
            let op = match op {
                ast::UnaryOp::Minus => UnaryOp::Minus,
            };

            Expr::Unary(UnaryExpr {
                span: DUMMY_SP,
                op,
                arg: Box::from(build_expr(arg, stmts, ctx)),
            })
        }
        ast::Expr::Fix(ast::Fix { expr, .. }) => match expr.as_ref() {
            ast::Expr::Lambda(ast::Lambda { body, .. }) => build_expr(body, stmts, ctx),
            _ => panic!("Fix should only wrap a lambda"),
        },
        ast::Expr::IfElse(ast::IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => match cond.as_ref() {
            ast::Expr::LetExpr(let_expr) => {
                build_let_expr(let_expr, consequent, alternate, stmts, ctx)
            }
            _ => {
                // let $temp_n;
                let temp_id = ctx.new_ident();
                let temp_decl = build_let_decl_stmt(&temp_id);
                stmts.push(temp_decl);

                // if (cond) { ...; $temp_n = <cons_res> } else { ...; $temp_n = <alt_res> }
                let test = Box::from(build_expr(cond.as_ref(), stmts, ctx));
                let cons = Box::from(Stmt::Block(build_expr_in_new_scope(
                    consequent.as_ref(),
                    &temp_id,
                    ctx,
                )));
                let alt = alternate.as_ref().map(|alt| {
                    Box::from(Stmt::Block(build_expr_in_new_scope(
                        alt.as_ref(),
                        &temp_id,
                        ctx,
                    )))
                });
                stmts.push(Stmt::If(IfStmt {
                    span: DUMMY_SP,
                    test,
                    cons,
                    alt,
                }));

                // $temp_n
                Expr::Ident(temp_id)
            }
        },
        ast::Expr::Obj(ast::Obj { props, .. }) => {
            let props: Vec<PropOrSpread> = props
                .iter()
                .map(|prop| match prop {
                    ast::PropOrSpread::Prop(prop) => match prop.as_ref() {
                        ast::Prop::Shorthand(ast::ident::Ident { name, .. }) => {
                            PropOrSpread::Prop(Box::from(Prop::Shorthand(Ident {
                                span: DUMMY_SP,
                                sym: JsWord::from(name.clone()),
                                optional: false,
                            })))
                        }
                        ast::Prop::KeyValue(ast::KeyValueProp { name, value, .. }) => {
                            PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
                                key: PropName::from(Ident {
                                    span: DUMMY_SP,
                                    sym: JsWord::from(name.clone()),
                                    optional: false,
                                }),
                                value: Box::from(build_expr(value, stmts, ctx)),
                            })))
                        }
                    },
                    ast::PropOrSpread::Spread(_) => todo!(),
                })
                .collect();

            Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props,
            })
        }
        ast::Expr::Await(ast::Await { expr, .. }) => Expr::Await(AwaitExpr {
            span: DUMMY_SP,
            arg: Box::from(build_expr(expr.as_ref(), stmts, ctx)),
        }),
        ast::Expr::JSXElement(elem) => {
            Expr::JSXElement(Box::from(build_jsx_element(elem, stmts, ctx)))
        }
        ast::Expr::Tuple(ast::Tuple { elems, .. }) => Expr::Array(ArrayLit {
            span: DUMMY_SP,
            elems: elems
                .iter()
                .map(|ast::ExprOrSpread { spread, expr }| {
                    Some(ExprOrSpread {
                        spread: spread.to_owned().map(|_| DUMMY_SP),
                        expr: Box::from(build_expr(expr.as_ref(), stmts, ctx)),
                    })
                })
                .collect(),
        }),
        ast::Expr::Member(ast::Member { obj, prop, .. }) => {
            let prop = match prop {
                ast::MemberProp::Ident(ident) => MemberProp::Ident(build_ident(ident)),
                ast::MemberProp::Computed(ast::ComputedPropName { expr, .. }) => {
                    MemberProp::Computed(ComputedPropName {
                        span: DUMMY_SP,
                        expr: Box::from(build_expr(expr, stmts, ctx)),
                    })
                }
            };
            Expr::Member(MemberExpr {
                span: DUMMY_SP,
                obj: Box::from(build_expr(obj, stmts, ctx)),
                prop,
            })
        }
        ast::Expr::Empty(_) => Expr::from(Ident {
            span: DUMMY_SP,
            sym: JsWord::from("undefined"),
            optional: false,
        }),
        ast::Expr::LetExpr(_) => {
            panic!("LetExpr should always be handled by the IfElse branch")
        }
        ast::Expr::TemplateLiteral(template) => {
            Expr::Tpl(build_template_literal(template, stmts, ctx))
        }
        ast::Expr::TaggedTemplateLiteral(ast::TaggedTemplateLiteral {
            span: _,
            tag,
            template,
        }) => {
            Expr::TaggedTpl(TaggedTpl {
                span: DUMMY_SP,
                tag: Box::from(Expr::Ident(build_ident(tag))),
                type_params: None, // TODO: support type params on tagged templates

                tpl: build_template_literal(template, stmts, ctx),
            })
        }
        ast::Expr::Match(ast::Match { expr, arms, .. }) => {
            // let $temp_n;
            let ret_temp_id = ctx.new_ident();
            let ret_decl = build_let_decl_stmt(&ret_temp_id);
            stmts.push(ret_decl);

            // let $temp_m = <expr>
            let temp_id = ctx.new_ident();
            let temp_decl = build_const_decl_stmt(&temp_id, build_expr(expr, stmts, ctx));
            stmts.push(temp_decl);

            // TODO: we want to stop when we encounter the first
            // irrefutable pattern since all subsequent patterns
            // shouldn't be matched.
            let mut has_catchall: bool = false;
            let mut built_arms: Vec<(_, _)> = vec![];
            for arm in arms {
                if has_catchall {
                    panic!("Catchall must appear last in match");
                }

                let (cond, block) = build_arm(arm, &temp_id, &ret_temp_id, stmts, ctx);

                if cond.is_none() {
                    has_catchall = true
                }

                built_arms.push((cond, block));
            }

            // We reverse the order of the arms because when building
            // an if/else-if/else chain we need to start with the `else`
            // and work our way back to the initial `if`.
            built_arms.reverse();
            let mut iter = built_arms.iter();
            let first = match iter.next() {
                Some((cond, block)) => match cond {
                    Some(cond) => Stmt::If(IfStmt {
                        span: DUMMY_SP,
                        test: Box::from(cond.to_owned()),
                        cons: Box::from(Stmt::Block(block.to_owned())),
                        alt: None,
                    }),
                    None => Stmt::Block(block.to_owned()),
                },
                None => panic!("No arms in match"),
            };

            let if_else = iter.fold(first, |prev, (cond, block)| {
                Stmt::If(IfStmt {
                    span: DUMMY_SP,
                    test: Box::from(cond.to_owned().unwrap()),
                    cons: Box::from(Stmt::Block(block.to_owned())),
                    alt: Some(Box::from(prev)),
                })
            });

            stmts.push(if_else);

            // $temp_n
            Expr::Ident(ret_temp_id)
        }
    }
}

fn build_let_expr(
    let_expr: &ast::LetExpr,
    consequent: &ast::Expr,
    alternate: &Option<Box<ast::Expr>>,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> Expr {
    let LetExpr { pat, expr, .. } = let_expr;

    let ret_id = ctx.new_ident();
    let ret_decl = build_let_decl_stmt(&ret_id);
    stmts.push(ret_decl);

    let temp_id = ctx.new_ident();
    let temp_decl = build_const_decl_stmt(&temp_id, build_expr(expr, stmts, ctx));
    stmts.push(temp_decl);

    let cond = build_cond_for_pat(pat, &temp_id);

    let mut block = build_expr_in_new_scope(consequent, &ret_id, ctx);

    if let Some(name) = build_pattern(pat, stmts, ctx) {
        // TODO: ignore the refutuable patterns when destructuring
        let destructure = build_const_decl_stmt_with_pat(name, Expr::from(temp_id));
        block.stmts.insert(0, destructure);
    }

    match cond {
        Some(cond) => {
            let alt = alternate.as_ref().map(|alt| {
                Box::from(Stmt::Block(build_expr_in_new_scope(
                    alt.as_ref(),
                    &ret_id,
                    ctx,
                )))
            });
            let if_else = Stmt::If(IfStmt {
                span: DUMMY_SP,
                test: Box::from(cond),
                cons: Box::from(Stmt::Block(block)),
                alt,
            });
            stmts.push(if_else);
        }
        None => {
            stmts.push(Stmt::Block(block));
        }
    };

    Expr::Ident(ret_id)
}

fn build_arm(
    arm: &ast::Arm,
    id: &Ident,
    ret_id: &Ident,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> (Option<Expr>, BlockStmt) {
    let ast::Arm {
        pattern: pat,
        expr: consequent,
        guard,
        ..
    } = arm;

    let cond = build_cond_for_pat(pat, id);

    let mut block = build_expr_in_new_scope(consequent, ret_id, ctx);

    // If pattern has assignables, assign them
    if let Some(name) = build_pattern(pat, stmts, ctx) {
        let destructure = build_const_decl_stmt_with_pat(name, Expr::from(id.to_owned()));
        block.stmts.insert(0, destructure);
    }

    let cond = match (cond, guard) {
        (Some(cond), Some(guard)) => {
            // If the pattern was refutable and there's a guard then
            // we return them logically AND-ed together.
            Some(Expr::Bin(BinExpr {
                span: DUMMY_SP,
                op: BinaryOp::LogicalAnd,
                left: Box::from(cond),
                right: Box::from(build_expr(guard, stmts, ctx)),
            }))
        }
        (Some(cond), None) => Some(cond),
        (None, Some(guard)) => Some(build_expr(guard, stmts, ctx)),
        (None, None) => None,
    };

    (cond, block)
}

fn build_jsx_element(
    elem: &ast::JSXElement,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> JSXElement {
    let name = JSXElementName::Ident(Ident {
        span: DUMMY_SP,
        sym: JsWord::from(elem.name.to_owned()),
        optional: false,
    });

    let elem = JSXElement {
        span: DUMMY_SP,
        opening: JSXOpeningElement {
            span: DUMMY_SP,
            name: name.to_owned(),
            attrs: elem
                .attrs
                .iter()
                .map(|ast::JSXAttr { value, ident, .. }| {
                    let value = Some(match value {
                        ast::JSXAttrValue::Lit(lit) => JSXAttrValue::Lit(build_lit(lit)),
                        ast::JSXAttrValue::JSXExprContainer(ast::JSXExprContainer {
                            expr, ..
                        }) => JSXAttrValue::JSXExprContainer(JSXExprContainer {
                            span: DUMMY_SP,
                            expr: JSXExpr::Expr(Box::from(build_expr(expr, stmts, ctx))),
                        }),
                    });

                    JSXAttrOrSpread::JSXAttr(JSXAttr {
                        span: DUMMY_SP,
                        name: JSXAttrName::Ident(Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(ident.name.to_owned()),
                            optional: false,
                        }),
                        value,
                    })
                })
                .collect(),
            self_closing: false,
            type_args: None,
        },
        children: elem
            .children
            .iter()
            .map(|child| {
                let result: JSXElementChild = match child {
                    ast::JSXElementChild::JSXText(ast::JSXText { value, .. }) => {
                        JSXElementChild::JSXText(JSXText {
                            span: DUMMY_SP,
                            value: Atom::new(value.to_owned()),
                            raw: Atom::new(value.to_owned()),
                        })
                    }
                    ast::JSXElementChild::JSXExprContainer(ast::JSXExprContainer {
                        expr, ..
                    }) => JSXElementChild::JSXExprContainer(JSXExprContainer {
                        span: DUMMY_SP,
                        expr: JSXExpr::Expr(Box::from(build_expr(expr, stmts, ctx))),
                    }),
                    ast::JSXElementChild::JSXElement(elem) => {
                        JSXElementChild::JSXElement(Box::from(build_jsx_element(elem, stmts, ctx)))
                    }
                };
                result
            })
            .collect(),
        closing: Some(JSXClosingElement {
            span: DUMMY_SP,
            name,
        }),
    };

    elem
}

fn build_lit(lit: &ast::Lit) -> Lit {
    match lit {
        ast::Lit::Num(n) => Lit::Num(Number {
            span: DUMMY_SP,
            value: n.value.parse().unwrap(),
            raw: None,
        }),
        ast::Lit::Bool(b) => Lit::Bool(Bool {
            span: DUMMY_SP,
            value: b.value,
        }),
        ast::Lit::Str(s) => Lit::Str(Str {
            span: DUMMY_SP,
            value: JsWord::from(s.value.to_owned()),
            raw: None,
            // Some would include the quotes around the string
            // Some(JsWord::from(s.value.to_owned())),
        }),
        ast::Lit::Null(_) => Lit::Null(Null { span: DUMMY_SP }),
        ast::Lit::Undefined(_) => todo!(),
    }
}

// TODO: have an intermediary from between the AST and what we used for
// codegen that unwraps `Let` nodes into vectors before converting them
// to statements.
fn build_fn_body(body: &ast::Expr, ctx: &mut Context) -> BlockStmtOrExpr {
    let mut stmts: Vec<Stmt> = vec![];

    let ret_expr = _build_expr(body, &mut stmts, ctx);

    if stmts.is_empty() {
        // Use fat arrow shorthand, e.g. (x) => x
        BlockStmtOrExpr::Expr(Box::from(ret_expr))
    } else {
        let ret = Stmt::Return(ReturnStmt {
            span: DUMMY_SP,
            arg: Some(Box::from(ret_expr)),
        });
        stmts.push(ret);

        BlockStmtOrExpr::BlockStmt(BlockStmt {
            span: DUMMY_SP,
            stmts,
        })
    }
}

fn let_to_child(r#let: &ast::Let, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Stmt {
    let ast::Let { pattern, init, .. } = r#let;

    // TODO: handle shadowed variables in the same scope by introducing
    // unique identifiers.
    match pattern {
        Some(pattern) => match build_pattern(pattern, stmts, ctx) {
            Some(name) => build_const_decl_stmt_with_pat(name, build_expr(init, stmts, ctx)),
            None => todo!(),
        },
        None => Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::from(build_expr(init, stmts, ctx)),
        }),
    }
}

fn build_cond_for_pat(pat: &ast::Pattern, id: &Ident) -> Option<Expr> {
    if is_refutable(pat) {
        // Right now the only refutable pattern we support is LitPat.
        // In the future there will be other refutable patterns such as
        // array length, typeof, and instanceof checks.

        let mut conds: Vec<Condition> = vec![];

        get_conds_for_pat(pat, &mut conds, &mut vec![]);

        let mut iter = conds.iter();

        let first = match iter.next() {
            Some(cond) => cond_to_expr(cond, id),
            None => return None,
        };

        Some(iter.fold(first, |prev, next| {
            Expr::Bin(BinExpr {
                span: DUMMY_SP,
                op: BinaryOp::LogicalOr,
                left: Box::from(prev),
                right: Box::from(cond_to_expr(next, id)),
            })
        }))
    } else {
        None
    }
}

fn build_template_literal(
    template: &ast::TemplateLiteral,
    stmt: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> Tpl {
    Tpl {
        span: DUMMY_SP,
        exprs: template
            .exprs
            .iter()
            .map(|expr| Box::from(build_expr(expr, stmt, ctx)))
            .collect(),
        quasis: template
            .quasis
            .iter()
            .map(|quasi| {
                let cooked = match &quasi.cooked {
                    ast::Lit::Str(ast::Str { value, .. }) => value,
                    _ => panic!("quasi.cooked must be a string"),
                };
                let raw = match &quasi.raw {
                    ast::Lit::Str(ast::Str { value, .. }) => value,
                    _ => panic!("quasi.raw must be a string"),
                };
                TplElement {
                    span: DUMMY_SP,
                    cooked: Some(Atom::new(cooked.to_owned())),
                    raw: Atom::new(raw.to_owned()),
                    tail: false, // TODO: set this to `true` if it's the last quasi
                }
            })
            .collect(),
    }
}

fn build_ident(ident: &ast::Ident) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: JsWord::from(ident.name.to_owned()),
        optional: false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PathElem {
    ObjProp(String),
    ArrayIndex(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Check {
    EqualLit(ast::Lit),
    Typeof(String), // limit this to primitives: "number", "string", "boolean"
    Instanceof(ast::Ident),
    // TODO: array length
}

type Path = Vec<PathElem>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Condition {
    path: Path,
    check: Check,
}

fn get_conds_for_pat(pat: &ast::Pattern, conds: &mut Vec<Condition>, path: &mut Path) {
    match pat {
        // irrefutable
        ast::Pattern::Ident(_) => (),
        ast::Pattern::Rest(_) => (),
        ast::Pattern::Wildcard(_) => (),

        // refutable and possibly refutable
        ast::Pattern::Object(ast::ObjectPat { props, .. }) => {
            for prop in props {
                match prop {
                    ast::ObjectPatProp::KeyValue(ast::KeyValuePatProp { value, key, .. }) => {
                        path.push(PathElem::ObjProp(key.name.clone()));
                        get_conds_for_pat(value, conds, path);
                        path.pop();
                    }
                    ast::ObjectPatProp::Rest(_) => (),
                    ast::ObjectPatProp::Assign(_) => (),
                }
            }
        }
        ast::Pattern::Array(ast::ArrayPat { elems, .. }) => {
            for (index, elem) in elems.iter().enumerate() {
                path.push(PathElem::ArrayIndex(index as u32));
                if let Some(elem) = elem {
                    get_conds_for_pat(elem, conds, path);
                }
                path.pop();
            }
        }
        ast::Pattern::Lit(ast::LitPat { lit, .. }) => {
            conds.push(Condition {
                path: path.to_owned(),
                check: Check::EqualLit(lit.to_owned()),
            });
        }
        ast::Pattern::Is(ast::IsPat { is_id, .. }) => match is_id.name.as_ref() {
            "string" | "number" | "boolean" => {
                conds.push(Condition {
                    path: path.to_owned(),
                    check: Check::Typeof(is_id.name.to_owned()),
                });
            }
            _ => {
                println!("adding Check::Instanceof condition");
                conds.push(Condition {
                    path: path.to_owned(),
                    check: Check::Instanceof(is_id.to_owned()),
                });
            }
        },
    }
}

fn cond_to_expr(cond: &Condition, id: &Ident) -> Expr {
    let Condition { check, path } = cond;

    let left = path
        .iter()
        .fold(Expr::Ident(id.to_owned()), |prev, path_elem| {
            let prop: MemberProp = match path_elem {
                PathElem::ObjProp(name) => MemberProp::Ident(Ident {
                    span: DUMMY_SP,
                    sym: JsWord::from(name.to_owned()),
                    optional: false,
                }),
                PathElem::ArrayIndex(index) => MemberProp::Computed(ComputedPropName {
                    span: DUMMY_SP,
                    expr: Box::from(Expr::Lit(Lit::Num(Number {
                        span: DUMMY_SP,
                        value: index.to_owned() as f64,
                        raw: None,
                    }))),
                }),
            };

            Expr::Member(MemberExpr {
                span: DUMMY_SP,
                obj: Box::from(prev),
                prop,
            })
        });

    match check {
        Check::EqualLit(lit) => Expr::Bin(BinExpr {
            span: DUMMY_SP,
            op: BinaryOp::EqEqEq,
            left: Box::from(left),
            right: Box::from(Expr::from(lit)),
        }),
        Check::Typeof(str) => Expr::Bin(BinExpr {
            span: DUMMY_SP,
            op: BinaryOp::EqEqEq,
            left: Box::from(Expr::Unary(UnaryExpr {
                span: DUMMY_SP,
                op: UnaryOp::TypeOf,
                arg: Box::from(left),
            })),
            right: Box::from(Expr::Lit(Lit::Str(Str {
                span: DUMMY_SP,
                value: JsWord::from(str.to_owned()),
                raw: None,
            }))),
        }),
        Check::Instanceof(id) => Expr::Bin(BinExpr {
            span: DUMMY_SP,
            op: BinaryOp::InstanceOf,
            left: Box::from(left),
            right: Box::from(Expr::Ident(Ident::from(id))),
        }),
    }
}

fn build_const_decl_stmt(id: &Ident, expr: Expr) -> Stmt {
    build_const_decl_stmt_with_pat(Pat::Ident(BindingIdent::from(id.to_owned())), expr)
}

fn build_const_decl_stmt_with_pat(name: Pat, expr: Expr) -> Stmt {
    Stmt::Decl(Decl::Var(VarDecl {
        span: DUMMY_SP,
        kind: VarDeclKind::Const,
        declare: false,
        decls: vec![VarDeclarator {
            span: DUMMY_SP,
            name,
            init: Some(Box::from(expr)),
            definite: false,
        }],
    }))
}

fn build_let_decl_stmt(id: &Ident) -> Stmt {
    Stmt::Decl(Decl::Var(VarDecl {
        span: DUMMY_SP,
        kind: VarDeclKind::Let,
        declare: false,
        decls: vec![VarDeclarator {
            span: DUMMY_SP,
            name: Pat::Ident(BindingIdent::from(id.to_owned())),
            init: None,
            definite: false,
        }],
    }))
}
