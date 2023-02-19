use std::rc::Rc;

use swc_atoms::*;
use swc_common::comments::SingleThreadedComments;
use swc_common::hygiene::Mark;
use swc_common::source_map::{
    self, DefaultSourceMapGenConfig, FilePathMapping, Globals, DUMMY_SP, GLOBALS,
};
use swc_common::{self, BytePos, FileName, SyntaxContext};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;
use swc_ecma_transforms_react::{react, Options, Runtime};
use swc_ecma_visit::*;

use escalier_ast::values;
use values::LetExpr;

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

pub fn codegen_js(src: &str, program: &values::Program) -> (String, String) {
    let mut ctx = Context { temp_id: 0 };
    let program = build_js(program, &mut ctx);

    let cm = Rc::new(source_map::SourceMap::default());
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
        print_js(src, &program)
    })
}

fn print_js(src: &str, program: &Program) -> (String, String) {
    let mut buf = vec![];
    let mut src_map = vec![];
    let cm = Rc::new(source_map::SourceMap::new(FilePathMapping::empty()));

    cm.new_source_file(FileName::Anon, String::from(src));

    {
        let wr = text_writer::JsWriter::new(cm.clone(), "\n", &mut buf, Some(&mut src_map));
        let mut emitter = Emitter {
            cfg: swc_ecma_codegen::Config {
                ..Default::default()
            },
            cm: cm.clone(),
            comments: None,
            wr,
        };
        emitter.emit_program(program).unwrap();
    }

    let output_code = String::from_utf8_lossy(&buf).to_string();
    let source_map = cm.build_source_map_with_config(&src_map, None, DefaultSourceMapGenConfig);

    let mut source_map_buf: Vec<u8> = vec![];
    source_map.to_writer(&mut source_map_buf).unwrap();

    (output_code, String::from_utf8(source_map_buf).unwrap())
}

fn build_js(program: &values::Program, ctx: &mut Context) -> Program {
    let body: Vec<ModuleItem> = program
        .body
        .iter()
        .flat_map(|child| {
            let mut stmts: Vec<Stmt> = vec![];
            let result = match child {
                values::Statement::VarDecl {
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
                                    decl: Decl::Var(Box::from(VarDecl {
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
                                    })),
                                }))
                            }
                            None => todo!(),
                        }
                    }
                },
                values::Statement::TypeDecl { .. } => {
                    ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP }))
                }
                values::Statement::Expr { expr, .. } => ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: DUMMY_SP,
                    expr: Box::from(build_expr(expr, &mut stmts, ctx)),
                })),
                values::Statement::ClassDecl { class, ident, .. } => {
                    let ident = Ident::from(ident);
                    let class = build_class(class, &mut stmts, ctx);

                    ModuleItem::Stmt(Stmt::Decl(Decl::Class(ClassDecl {
                        ident,
                        class: Box::from(class),
                        declare: false,
                    })))
                }
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

// TODO: See if we can avoid returning an Option<> here so that we don't have
// to unwrap() in when calling it from build_expr().
fn build_pattern(
    pattern: &values::Pattern,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> Option<Pat> {
    let span = swc_common::Span {
        lo: BytePos(pattern.span.start as u32 + 1),
        hi: BytePos(pattern.span.end as u32 + 1),
        ctxt: SyntaxContext::empty(),
    };

    match &pattern.kind {
        // unassignable patterns
        values::PatternKind::Lit(_) => None,

        // TODO: we need to have something we can assign `_` to when it appears
        // in object destructuring otherwise if there's a `...rest` that's also
        // in the pattern we'll end up with the wrong items assigned to `rest`.
        values::PatternKind::Wildcard => Some(Pat::Ident(BindingIdent {
            id: ctx.new_ident(),
            type_ann: None,
        })),

        // assignable patterns
        values::PatternKind::Ident(binding_ident) => Some(Pat::Ident(BindingIdent {
            id: Ident::from(binding_ident),
            type_ann: None,
        })),
        values::PatternKind::Rest(values::RestPat { arg }) => {
            let dot3_token = swc_common::Span {
                lo: BytePos(pattern.span.start as u32 + 1),
                hi: BytePos(pattern.span.start as u32 + 4),
                ctxt: SyntaxContext::empty(),
            };
            let arg = build_pattern(arg, stmts, ctx).unwrap();
            Some(Pat::Rest(RestPat {
                span,
                dot3_token,
                type_ann: None,
                arg: Box::from(arg),
            }))
        }
        values::PatternKind::Object(values::ObjectPat { props, optional }) => {
            let props: Vec<ObjectPatProp> = props
                .iter()
                .filter_map(|p| match p {
                    values::ObjectPatProp::KeyValue(kvp) => {
                        build_pattern(kvp.value.as_ref(), stmts, ctx).map(|value| {
                            ObjectPatProp::KeyValue(KeyValuePatProp {
                                key: PropName::Ident(Ident::from(&kvp.key)),
                                value: Box::from(value),
                            })
                        })
                    }
                    values::ObjectPatProp::Shorthand(values::ShorthandPatProp {
                        ident,
                        init,
                        ..
                    }) => Some(ObjectPatProp::Assign(AssignPatProp {
                        span: DUMMY_SP,
                        key: Ident::from(ident),
                        value: init
                            .clone()
                            .map(|value| Box::from(build_expr(&value, stmts, ctx))),
                    })),
                    values::ObjectPatProp::Rest(values::RestPat { arg }) => {
                        let dot3_token = swc_common::Span {
                            lo: BytePos(pattern.span.start as u32 + 1),
                            hi: BytePos(pattern.span.start as u32 + 4),
                            ctxt: SyntaxContext::empty(),
                        };
                        let span = swc_common::Span {
                            lo: BytePos(pattern.span.start as u32 + 1),
                            hi: BytePos(pattern.span.end as u32 + 1),
                            ctxt: SyntaxContext::empty(),
                        };
                        Some(ObjectPatProp::Rest(RestPat {
                            span,
                            dot3_token,
                            arg: Box::from(build_pattern(arg, stmts, ctx)?),
                            type_ann: None,
                        }))
                    }
                })
                .collect();

            Some(Pat::Object(ObjectPat {
                span,
                optional: optional.to_owned(),
                type_ann: None, // because we're generating .js
                props,
            }))
        }
        values::PatternKind::Array(values::ArrayPat { elems, optional }) => {
            let elems: Vec<Option<Pat>> = elems
                .iter()
                .map(|elem| match elem {
                    Some(elem) => build_pattern(&elem.pattern, stmts, ctx),
                    None => None,
                })
                .collect();

            // TODO: If all elems are None, we can drop the array pattern.
            Some(Pat::Array(ArrayPat {
                span,
                elems,
                optional: optional.to_owned(),
                type_ann: None, // because we're generating .js.
            }))
        }
        values::PatternKind::Is(values::IsPat { ident, .. }) => Some(Pat::Ident(BindingIdent {
            id: Ident::from(ident),
            type_ann: None,
        })),
    }
}

fn build_expr(expr: &values::Expr, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Expr {
    let span = swc_common::Span {
        lo: BytePos(expr.span.start as u32 + 1),
        hi: BytePos(expr.span.end as u32 + 1),
        ctxt: SyntaxContext::empty(),
    };

    match &expr.kind {
        values::ExprKind::App(values::App { lam, args, .. }) => {
            let callee = Callee::Expr(Box::from(build_expr(lam.as_ref(), stmts, ctx)));

            let args: Vec<ExprOrSpread> = args
                .iter()
                .map(|arg| ExprOrSpread {
                    spread: if arg.spread.is_some() {
                        Some(DUMMY_SP)
                    } else {
                        None
                    },
                    expr: Box::from(build_expr(arg.expr.as_ref(), stmts, ctx)),
                })
                .collect();

            Expr::Call(CallExpr {
                span,
                callee,
                args,
                type_args: None,
            })
        }
        values::ExprKind::New(values::New { expr, args, .. }) => {
            let callee = Box::from(build_expr(expr.as_ref(), stmts, ctx));

            let args: Vec<ExprOrSpread> = args
                .iter()
                .map(|arg| ExprOrSpread {
                    spread: if arg.spread.is_some() {
                        Some(DUMMY_SP)
                    } else {
                        None
                    },
                    expr: Box::from(build_expr(arg.expr.as_ref(), stmts, ctx)),
                })
                .collect();

            Expr::New(NewExpr {
                span,
                callee,
                args: Some(args), // JavaScript allows `new Array`, but we don't
                type_args: None,
            })
        }
        values::ExprKind::Ident(ident) => Expr::from(Ident::from(ident)),
        values::ExprKind::Lambda(values::Lambda {
            params: args,
            body,
            is_async,
            ..
        }) => {
            let params: Vec<Pat> = args
                .iter()
                .map(|arg| build_pattern(&arg.pat, stmts, ctx).unwrap())
                .collect();

            let body = build_body(body, stmts, ctx);
            // let body = build_lambda_body(body.as_ref(), ctx);

            Expr::Arrow(ArrowExpr {
                span,
                params,
                body,
                is_async: is_async.to_owned(),
                is_generator: false,
                type_params: None,
                return_type: None,
            })
        }
        values::ExprKind::Assign(values::Assign { left, right, op: _ }) => {
            // TODO: handle other operators
            Expr::Assign(AssignExpr {
                span,
                left: PatOrExpr::Expr(Box::from(build_expr(left, stmts, ctx))),
                right: Box::from(build_expr(right, stmts, ctx)),
                op: AssignOp::Assign,
            })
        }
        values::ExprKind::Lit(lit) => Expr::from(lit),
        values::ExprKind::Keyword(keyword) => match keyword {
            values::Keyword::Null => {
                swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Null(swc_ecma_ast::Null { span }))
            }
            values::Keyword::Undefined => {
                // NOTE: `undefined` is actually an identifier in JavaScript.
                swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
                    span,
                    sym: swc_atoms::JsWord::from(String::from("undefined")),
                    optional: false,
                })
            }
            _ => panic!("{keyword} should only be used as a type"),
        },
        values::ExprKind::BinaryExpr(values::BinaryExpr {
            op, left, right, ..
        }) => {
            let op = match op {
                values::BinOp::Add => BinaryOp::Add,
                values::BinOp::Sub => BinaryOp::Sub,
                values::BinOp::Mul => BinaryOp::Mul,
                values::BinOp::Div => BinaryOp::Div,
                values::BinOp::EqEq => BinaryOp::EqEqEq,
                values::BinOp::NotEq => BinaryOp::NotEqEq,
                values::BinOp::Lt => BinaryOp::Lt,
                values::BinOp::LtEq => BinaryOp::LtEq,
                values::BinOp::Gt => BinaryOp::Gt,
                values::BinOp::GtEq => BinaryOp::GtEq,
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
                span,
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
        values::ExprKind::UnaryExpr(values::UnaryExpr { arg, op, .. }) => {
            let op = match op {
                values::UnaryOp::Minus => UnaryOp::Minus,
            };

            Expr::Unary(UnaryExpr {
                span,
                op,
                arg: Box::from(build_expr(arg, stmts, ctx)),
            })
        }
        values::ExprKind::Fix(values::Fix { expr, .. }) => match &expr.kind {
            values::ExprKind::Lambda(values::Lambda { body, .. }) => {
                build_expr(&body[0], stmts, ctx)
            }
            _ => panic!("Fix should only wrap a lambda"),
        },
        values::ExprKind::IfElse(values::IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => match &cond.kind {
            values::ExprKind::LetExpr(let_expr) => {
                build_let_expr(let_expr, consequent, alternate, stmts, ctx)
            }
            _ => {
                // let $temp_n;
                let temp_id = ctx.new_ident();
                let temp_decl = build_let_decl_stmt(&temp_id);
                stmts.push(temp_decl);

                // if (cond) { ...; $temp_n = <cons_res> } else { ...; $temp_n = <alt_res> }
                let test = Box::from(build_expr(cond.as_ref(), stmts, ctx));
                let cons = Box::from(Stmt::Block(build_body_block_stmt(
                    consequent.as_ref(),
                    &temp_id,
                    ctx,
                )));
                let alt = alternate.as_ref().map(|alt| {
                    Box::from(Stmt::Block(build_body_block_stmt(
                        alt.as_ref(),
                        &temp_id,
                        ctx,
                    )))
                });
                stmts.push(Stmt::If(IfStmt {
                    span,
                    test,
                    cons,
                    alt,
                }));

                // $temp_n
                Expr::Ident(temp_id)
            }
        },
        values::ExprKind::Obj(values::Obj { props }) => {
            let props: Vec<PropOrSpread> = props
                .iter()
                .map(|prop| match prop {
                    values::PropOrSpread::Prop(prop) => match prop.as_ref() {
                        values::Prop::Shorthand(ident) => {
                            PropOrSpread::Prop(Box::from(Prop::Shorthand(Ident::from(ident))))
                        }
                        values::Prop::KeyValue(values::KeyValueProp { key, value, .. }) => {
                            PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
                                key: PropName::from(Ident::from(key)),
                                value: Box::from(build_expr(value, stmts, ctx)),
                            })))
                        }
                    },
                    values::PropOrSpread::Spread(_) => todo!(),
                })
                .collect();

            Expr::Object(ObjectLit { span, props })
        }
        values::ExprKind::Await(values::Await { expr, .. }) => Expr::Await(AwaitExpr {
            span,
            arg: Box::from(build_expr(expr.as_ref(), stmts, ctx)),
        }),
        values::ExprKind::JSXElement(elem) => {
            Expr::JSXElement(Box::from(build_jsx_element(elem, stmts, ctx)))
        }
        values::ExprKind::Tuple(values::Tuple { elems }) => Expr::Array(ArrayLit {
            span,
            elems: elems
                .iter()
                .map(|values::ExprOrSpread { spread, expr }| {
                    Some(ExprOrSpread {
                        spread: spread.to_owned().map(|_| DUMMY_SP),
                        expr: Box::from(build_expr(expr.as_ref(), stmts, ctx)),
                    })
                })
                .collect(),
        }),
        values::ExprKind::Member(values::Member { obj, prop, .. }) => {
            let prop = match prop {
                values::MemberProp::Ident(ident) => MemberProp::Ident(Ident::from(ident)),
                values::MemberProp::Computed(values::ComputedPropName { expr, .. }) => {
                    MemberProp::Computed(ComputedPropName {
                        span: DUMMY_SP,
                        expr: Box::from(build_expr(expr, stmts, ctx)),
                    })
                }
            };
            Expr::Member(MemberExpr {
                span,
                obj: Box::from(build_expr(obj, stmts, ctx)),
                prop,
            })
        }
        values::ExprKind::Empty => Expr::from(Ident {
            span,
            sym: JsWord::from("undefined"),
            optional: false,
        }),
        values::ExprKind::LetExpr(_) => {
            panic!("LetExpr should always be handled by the IfElse branch")
        }
        values::ExprKind::TemplateLiteral(template) => {
            Expr::Tpl(build_template_literal(template, stmts, ctx))
        }
        values::ExprKind::TaggedTemplateLiteral(values::TaggedTemplateLiteral {
            tag,
            template,
        }) => {
            Expr::TaggedTpl(TaggedTpl {
                span,
                tag: Box::from(Expr::Ident(Ident::from(tag))),
                type_params: None, // TODO: support type params on tagged templates

                tpl: build_template_literal(template, stmts, ctx),
            })
        }
        values::ExprKind::Match(values::Match { expr, arms, .. }) => {
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
                    span,
                    test: Box::from(cond.to_owned().unwrap()),
                    cons: Box::from(Stmt::Block(block.to_owned())),
                    alt: Some(Box::from(prev)),
                })
            });

            stmts.push(if_else);

            // $temp_n
            Expr::Ident(ret_temp_id)
        }
        values::ExprKind::Class(class) => {
            let ident = Some(Ident::from(&class.ident));
            let class = build_class(class, stmts, ctx);

            Expr::Class(ClassExpr {
                ident,
                class: Box::from(class),
            })
        }
        values::ExprKind::Regex(regex) => Expr::Lit(Lit::Regex(Regex {
            span,
            exp: Atom::new(regex.pattern.as_ref()),
            flags: match &regex.flags {
                Some(flags) => Atom::new(flags.as_ref()),
                None => Atom::new(""),
            },
        })),
        values::ExprKind::DoExpr(do_expr) => {
            let len = do_expr.body.len();

            let temp_id = ctx.new_ident();
            let temp_decl = build_let_decl_stmt(&temp_id);
            stmts.push(temp_decl);

            let mut new_stmts: Vec<Stmt> = vec![];
            for (i, expr) in do_expr.body.iter().enumerate() {
                match &expr.kind {
                    values::ExprKind::LetDecl(values::LetDecl {
                        pattern,
                        type_ann: _,
                        init,
                    }) => {
                        let stmt = match build_pattern(pattern, &mut new_stmts, ctx) {
                            Some(name) => build_const_decl_stmt_with_pat(
                                name,
                                build_expr(init, &mut new_stmts, ctx),
                            ),
                            None => todo!(),
                        };
                        new_stmts.push(stmt);
                    }
                    _ => {
                        let expr = build_expr(expr, &mut new_stmts, ctx);
                        if i == len - 1 {
                            new_stmts.push(Stmt::Expr(ExprStmt {
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
                        } else {
                            new_stmts.push(Stmt::Expr(ExprStmt {
                                span: DUMMY_SP,
                                expr: Box::from(expr),
                            }));
                        }
                    }
                }
            }

            stmts.push(Stmt::Block(BlockStmt {
                span: DUMMY_SP,
                stmts: new_stmts,
            }));

            Expr::Ident(temp_id)
        }
        values::ExprKind::LetDecl(_) => {
            // NOTE: This should always be handled by the DoExpr arm
            todo!();
        }
    }
}

fn build_body(
    body: &Vec<values::Expr>,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> BlockStmtOrExpr {
    let len = body.len();

    if body.is_empty() {
        BlockStmtOrExpr::BlockStmt(BlockStmt {
            span: DUMMY_SP,
            stmts: vec![],
        })
    } else if len == 1 {
        // TODO: Fix this - it doesn't doesn't handle expressions that introduce
        // temporary variables correctly.
        BlockStmtOrExpr::Expr(Box::from(build_expr(&body[0], stmts, ctx)))
    } else {
        BlockStmtOrExpr::BlockStmt(build_body_block_stmt_fn(body, ctx))
    }
}

fn build_body_block_stmt_fn(body: &[values::Expr], ctx: &mut Context) -> BlockStmt {
    let mut new_stmts: Vec<Stmt> = vec![];
    let len = body.len();

    for (i, expr) in body.iter().enumerate() {
        match &expr.kind {
            values::ExprKind::LetDecl(values::LetDecl {
                pattern,
                type_ann: _,
                init,
            }) => {
                let stmt = match build_pattern(pattern, &mut new_stmts, ctx) {
                    Some(name) => {
                        build_const_decl_stmt_with_pat(name, build_expr(init, &mut new_stmts, ctx))
                    }
                    None => todo!(),
                };
                new_stmts.push(stmt);
            }
            _ => {
                let expr = build_expr(expr, &mut new_stmts, ctx);

                if i == len - 1 {
                    new_stmts.push(Stmt::Return(ReturnStmt {
                        span: DUMMY_SP,
                        arg: Some(Box::from(expr)),
                    }));
                } else {
                    new_stmts.push(Stmt::Expr(ExprStmt {
                        span: DUMMY_SP,
                        expr: Box::from(expr),
                    }));
                }
            }
        }
    }

    if body.is_empty() {
        let undefined = swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
            span: DUMMY_SP,
            sym: swc_atoms::JsWord::from(String::from("undefined")),
            optional: false,
        });
        new_stmts.push(Stmt::Return(ReturnStmt {
            span: DUMMY_SP,
            arg: Some(Box::from(undefined)),
        }));
    }

    BlockStmt {
        span: DUMMY_SP,
        stmts: new_stmts,
    }
}

fn build_body_block_stmt(body: &[values::Expr], ret_id: &Ident, ctx: &mut Context) -> BlockStmt {
    let mut new_stmts: Vec<Stmt> = vec![];
    let len = body.len();

    for (i, expr) in body.iter().enumerate() {
        match &expr.kind {
            values::ExprKind::LetDecl(values::LetDecl {
                pattern,
                type_ann: _,
                init,
            }) => {
                let stmt = match build_pattern(pattern, &mut new_stmts, ctx) {
                    Some(name) => {
                        build_const_decl_stmt_with_pat(name, build_expr(init, &mut new_stmts, ctx))
                    }
                    None => todo!(),
                };
                new_stmts.push(stmt);
            }
            _ => {
                let expr = build_expr(expr, &mut new_stmts, ctx);

                if i == len - 1 {
                    new_stmts.push(Stmt::Expr(ExprStmt {
                        span: DUMMY_SP,
                        expr: Box::from(Expr::Assign(AssignExpr {
                            span: DUMMY_SP,
                            op: AssignOp::Assign,
                            left: PatOrExpr::Pat(Box::from(Pat::Ident(BindingIdent {
                                id: ret_id.to_owned(),
                                type_ann: None,
                            }))),
                            right: Box::from(expr),
                        })),
                    }));
                } else {
                    new_stmts.push(Stmt::Expr(ExprStmt {
                        span: DUMMY_SP,
                        expr: Box::from(expr),
                    }));
                }
            }
        }
    }

    if body.is_empty() {
        let undefined = swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
            span: DUMMY_SP,
            sym: swc_atoms::JsWord::from(String::from("undefined")),
            optional: false,
        });
        new_stmts.push(Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::from(Expr::Assign(AssignExpr {
                span: DUMMY_SP,
                op: AssignOp::Assign,
                left: PatOrExpr::Pat(Box::from(Pat::Ident(BindingIdent {
                    id: ret_id.to_owned(),
                    type_ann: None,
                }))),
                right: Box::from(undefined),
            })),
        }));
    }

    BlockStmt {
        span: DUMMY_SP,
        stmts: new_stmts,
    }
}

fn build_let_expr(
    let_expr: &values::LetExpr,
    consequent: &[values::Expr],
    alternate: &Option<Vec<values::Expr>>,
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

    let mut block = build_body_block_stmt(consequent, &ret_id, ctx);

    if let Some(name) = build_pattern(pat, stmts, ctx) {
        // TODO: ignore the refutuable patterns when destructuring
        let destructure = build_const_decl_stmt_with_pat(name, Expr::from(temp_id));
        block.stmts.insert(0, destructure);
    }

    match cond {
        Some(cond) => {
            let alt = alternate.as_ref().map(|alt| {
                Box::from(Stmt::Block(build_body_block_stmt(
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
    arm: &values::Arm,
    id: &Ident,
    ret_id: &Ident,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> (Option<Expr>, BlockStmt) {
    let values::Arm {
        pattern: pat,
        body,
        guard,
        ..
    } = arm;

    let cond = build_cond_for_pat(pat, id);

    let mut block = build_body_block_stmt(body, ret_id, ctx);

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
    elem: &values::JSXElement,
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
                .map(|values::JSXAttr { value, ident, .. }| {
                    let value = Some(match value {
                        values::JSXAttrValue::Lit(lit) => JSXAttrValue::Lit(build_lit(lit)),
                        values::JSXAttrValue::JSXExprContainer(values::JSXExprContainer {
                            expr,
                            ..
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
                    values::JSXElementChild::JSXText(values::JSXText { value, .. }) => {
                        JSXElementChild::JSXText(JSXText {
                            span: DUMMY_SP,
                            value: Atom::new(value.clone()),
                            raw: Atom::new(value.clone()),
                        })
                    }
                    values::JSXElementChild::JSXExprContainer(values::JSXExprContainer {
                        expr,
                        ..
                    }) => JSXElementChild::JSXExprContainer(JSXExprContainer {
                        span: DUMMY_SP,
                        expr: JSXExpr::Expr(Box::from(build_expr(expr, stmts, ctx))),
                    }),
                    values::JSXElementChild::JSXElement(elem) => {
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

fn build_lit(lit: &values::Lit) -> Lit {
    match lit {
        values::Lit::Num(n) => Lit::Num(Number {
            span: DUMMY_SP,
            value: n.value.parse().unwrap(),
            raw: None,
        }),
        values::Lit::Bool(b) => Lit::Bool(Bool {
            span: DUMMY_SP,
            value: b.value,
        }),
        values::Lit::Str(s) => Lit::Str(Str {
            span: DUMMY_SP,
            value: JsWord::from(s.value.to_owned()),
            raw: None,
            // Some would include the quotes around the string
            // Some(JsWord::from(s.value.to_owned())),
        }),
    }
}

fn build_class(class: &values::Class, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Class {
    let body: Vec<ClassMember> = class
        .body
        .iter()
        .filter_map(|member| match member {
            values::ClassMember::Constructor(constructor) => {
                let body = build_body_block_stmt_fn(&constructor.body, ctx);
                // In Crochet, `self` is always the first param in methods, but
                // it represents `this` in JavaScript which is implicit so we
                // ignore it here.
                let mut iter = constructor.params.iter();
                iter.next();
                let params: Vec<ParamOrTsParamProp> = iter
                    .map(|param| {
                        let pat = build_pattern(&param.pat, stmts, ctx).unwrap();
                        ParamOrTsParamProp::Param(Param {
                            span: DUMMY_SP,
                            decorators: vec![],
                            pat,
                        })
                    })
                    .collect();

                Some(ClassMember::Constructor(Constructor {
                    span: DUMMY_SP, // TODO
                    key: PropName::Ident(Ident {
                        span: DUMMY_SP, // TODO
                        sym: swc_atoms::JsWord::from(String::from("constructor")),
                        optional: false,
                    }),
                    params,
                    body: Some(body),
                    accessibility: None,
                    is_optional: false,
                }))
            }
            values::ClassMember::Method(method) => {
                let body = build_body_block_stmt_fn(&method.lambda.body, ctx);
                // In Crochet, `self` is always the first param in non-static
                // methods, but it represents `this` in JavaScript which is
                // implicit so we ignore it here.
                let mut iter = method.lambda.params.iter();
                if !method.is_static {
                    iter.next();
                }
                let params: Vec<Param> = iter
                    .map(|param| {
                        let pat = build_pattern(&param.pat, stmts, ctx).unwrap();
                        Param {
                            span: DUMMY_SP,
                            decorators: vec![],
                            pat,
                        }
                    })
                    .collect();

                Some(ClassMember::Method(ClassMethod {
                    span: DUMMY_SP, // TODO
                    key: PropName::Ident(Ident::from(&method.key)),
                    function: Box::from(Function {
                        params,
                        decorators: vec![],
                        span: DUMMY_SP, // TODO
                        body: Some(body),
                        is_generator: false,
                        is_async: false,   // TODO
                        type_params: None, // TODO
                        return_type: None,
                    }),
                    kind: MethodKind::Method,
                    is_static: false,
                    accessibility: None,
                    is_abstract: false,
                    is_optional: false,
                    is_override: false,
                }))
            }
            values::ClassMember::Prop(prop) => {
                if prop.value.is_some() {
                    Some(ClassMember::ClassProp(ClassProp {
                        span: DUMMY_SP, // TODO
                        value: prop
                            .value
                            .as_ref()
                            .map(|value| Box::from(build_expr(value, stmts, ctx))),
                        key: PropName::Ident(Ident::from(&prop.key)),
                        type_ann: None,
                        is_static: prop.is_static,
                        decorators: vec![],
                        accessibility: None,
                        is_abstract: false,
                        is_optional: prop.is_optional,
                        is_override: false,
                        readonly: false, // TODO
                        declare: false,
                        definite: false,
                    }))
                } else {
                    None
                }
            }
        })
        .collect();

    Class {
        span: DUMMY_SP, // TODO
        decorators: vec![],
        super_class: None,
        is_abstract: false,
        super_type_params: None,
        type_params: None,
        implements: vec![],
        body,
    }
}

fn build_cond_for_pat(pat: &values::Pattern, id: &Ident) -> Option<Expr> {
    if values::is_refutable(pat) {
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
    template: &values::TemplateLiteral,
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
                    values::Lit::Str(values::Str { value, .. }) => value,
                    _ => panic!("quasi.cooked must be a string"),
                };
                let raw = match &quasi.raw {
                    values::Lit::Str(values::Str { value, .. }) => value,
                    _ => panic!("quasi.raw must be a string"),
                };
                TplElement {
                    span: DUMMY_SP,
                    cooked: Some(Atom::new(cooked.clone())),
                    raw: Atom::new(raw.clone()),
                    tail: false, // TODO: set this to `true` if it's the last quasi
                }
            })
            .collect(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PathElem {
    ObjProp(String),
    ArrayIndex(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Check {
    EqualLit(values::Lit),
    Typeof(String), // limit this to primitives: "number", "string", "boolean"
    Instanceof(values::Ident),
    // TODO: array length
}

type Path = Vec<PathElem>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Condition {
    path: Path,
    check: Check,
}

fn get_conds_for_pat(pat: &values::Pattern, conds: &mut Vec<Condition>, path: &mut Path) {
    match &pat.kind {
        // irrefutable
        values::PatternKind::Ident(_) => (),
        values::PatternKind::Rest(_) => (),
        values::PatternKind::Wildcard => (),

        // refutable and possibly refutable
        values::PatternKind::Object(values::ObjectPat { props, .. }) => {
            for prop in props {
                match prop {
                    values::ObjectPatProp::KeyValue(values::KeyValuePatProp {
                        value, key, ..
                    }) => {
                        path.push(PathElem::ObjProp(key.name.clone()));
                        get_conds_for_pat(value, conds, path);
                        path.pop();
                    }
                    values::ObjectPatProp::Shorthand(_) => (),
                    values::ObjectPatProp::Rest(_) => (),
                }
            }
        }
        values::PatternKind::Array(values::ArrayPat { elems, .. }) => {
            for (index, elem) in elems.iter().enumerate() {
                path.push(PathElem::ArrayIndex(index as u32));
                if let Some(elem) = elem {
                    get_conds_for_pat(&elem.pattern, conds, path);
                }
                path.pop();
            }
        }
        values::PatternKind::Lit(values::LitPat { lit, .. }) => {
            conds.push(Condition {
                path: path.to_owned(),
                check: Check::EqualLit(lit.to_owned()),
            });
        }
        values::PatternKind::Is(values::IsPat { is_id, .. }) => match is_id.name.as_ref() {
            "string" | "number" | "boolean" => {
                conds.push(Condition {
                    path: path.to_owned(),
                    check: Check::Typeof(is_id.name.to_owned()),
                });
            }
            _ => {
                eprintln!("adding Check::Instanceof condition");
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
    Stmt::Decl(Decl::Var(Box::from(VarDecl {
        span: DUMMY_SP,
        kind: VarDeclKind::Const,
        declare: false,
        decls: vec![VarDeclarator {
            span: DUMMY_SP,
            name,
            init: Some(Box::from(expr)),
            definite: false,
        }],
    })))
}

fn build_let_decl_stmt(id: &Ident) -> Stmt {
    Stmt::Decl(Decl::Var(Box::from(VarDecl {
        span: DUMMY_SP,
        kind: VarDeclKind::Let,
        declare: false,
        decls: vec![VarDeclarator {
            span: DUMMY_SP,
            name: Pat::Ident(BindingIdent::from(id.to_owned())),
            init: None,
            definite: false,
        }],
    })))
}
