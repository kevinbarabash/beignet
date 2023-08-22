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

use escalier_ast::{self as values};

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
        let unresolved_mark = Mark::new();
        let mut v = react(cm, comments, options, top_level_mark, unresolved_mark);
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
        .stmts
        .iter()
        .flat_map(|child| {
            let mut stmts: Vec<Stmt> = vec![];
            let result = match &child.kind {
                values::StmtKind::VarDecl(values::VarDecl {
                    pattern,
                    expr: init,
                    is_declare: declare,
                    ..
                }) => match declare {
                    true => ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP })),
                    false => {
                        // It should be okay to unwrap this here since any decl that isn't
                        // using `declare` should have an initial value.
                        let init = init.as_ref().unwrap();

                        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                            span: DUMMY_SP,
                            decl: Decl::Var(Box::from(build_var_decl(
                                pattern,
                                Some(init),
                                &mut stmts,
                                ctx,
                            ))),
                        }))
                    }
                },
                values::StmtKind::TypeDecl { .. } => {
                    ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP }))
                }
                values::StmtKind::Expr(expr) => ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: DUMMY_SP,
                    expr: Box::from(build_expr(expr, &mut stmts, ctx)),
                })),
                // values::StmtKind::ClassDecl(values::ClassDecl { class, ident, .. }) => {
                //     let ident = Ident::from(ident);
                //     let class = build_class(class, &mut stmts, ctx);

                //     ModuleItem::Stmt(Stmt::Decl(Decl::Class(ClassDecl {
                //         ident,
                //         class: Box::from(class),
                //         declare: false,
                //     })))
                // }

                // values::StmtKind::ForStmt(for_stmt) => ModuleItem::Stmt(Stmt::ForOf(ForOfStmt {
                //     span: DUMMY_SP,
                //     is_await: false,
                //     left: ForHead::VarDecl(Box::from(build_var_decl(
                //         &for_stmt.pattern,
                //         None,
                //         &mut stmts,
                //         ctx,
                //     ))),
                //     right: Box::from(build_expr(&for_stmt.expr, &mut stmts, ctx)),
                //     body: Box::from(Stmt::Block(build_body_block_stmt(
                //         &for_stmt.body,
                //         &BlockFinalizer::ExprStmt,
                //         ctx,
                //     ))),
                // })),
                values::StmtKind::Return { .. } => {
                    panic!("return statements aren't allowed at the top level")
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

fn build_var_decl(
    pattern: &values::Pattern,
    init: Option<&values::Expr>,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> VarDecl {
    VarDecl {
        span: DUMMY_SP,
        kind: VarDeclKind::Const,
        declare: false,
        decls: vec![VarDeclarator {
            span: DUMMY_SP,
            name: build_pattern(pattern, stmts, ctx).unwrap(),
            init: init.map(|init| Box::from(build_expr(init, stmts, ctx))),
            definite: false,
        }],
    }
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
        values::PatternKind::Tuple(values::TuplePat { elems, optional }) => {
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
        values::ExprKind::Call(values::Call {
            callee: lam, args, ..
        }) => {
            let callee = Callee::Expr(Box::from(build_expr(lam.as_ref(), stmts, ctx)));

            let args: Vec<ExprOrSpread> = args
                .iter()
                // TODO: Support spreading args when calling functions
                .map(|arg| ExprOrSpread {
                    // spread: if arg.spread.is_some() {
                    //     Some(DUMMY_SP)
                    // } else {
                    //     None
                    // },
                    spread: None,
                    expr: Box::from(build_expr(arg, stmts, ctx)),
                })
                .collect();

            Expr::Call(CallExpr {
                span,
                callee,
                args,
                type_args: None,
            })
        }
        // TODO: Support `Point::new(5, 10)` -> `new Point(5, 10)`.
        // values::ExprKind::New(values::New { expr, args, .. }) => {
        //     let callee = Box::from(build_expr(expr.as_ref(), stmts, ctx));

        //     let args: Vec<ExprOrSpread> = args
        //         .iter()
        //         .map(|arg| ExprOrSpread {
        //             spread: if arg.spread.is_some() {
        //                 Some(DUMMY_SP)
        //             } else {
        //                 None
        //             },
        //             expr: Box::from(build_expr(arg.expr.as_ref(), stmts, ctx)),
        //         })
        //         .collect();

        //     Expr::New(NewExpr {
        //         span,
        //         callee,
        //         args: Some(args), // JavaScript allows `new Array`, but we don't
        //         type_args: None,
        //     })
        // }
        values::ExprKind::Ident(ident) => Expr::from(Ident::from(ident)),
        values::ExprKind::Function(values::Function {
            params: args,
            body,
            is_async,
            ..
        }) => {
            let params: Vec<Pat> = args
                .iter()
                .map(|arg| build_pattern(&arg.pattern, stmts, ctx).unwrap())
                .collect();

            let body = match body {
                values::BlockOrExpr::Block(body) => BlockStmtOrExpr::BlockStmt(
                    build_body_block_stmt(body, &BlockFinalizer::ExprStmt, ctx),
                ),
                values::BlockOrExpr::Expr(expr) => {
                    BlockStmtOrExpr::Expr(Box::from(build_expr(expr, stmts, ctx)))
                }
            };

            Expr::Arrow(ArrowExpr {
                span,
                params,
                body: Box::new(body),
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
        // values::ExprKind::Literal(lit) => Expr::from(lit),
        values::ExprKind::Str(values::Str { value, .. }) => Expr::Lit(Lit::Str(Str {
            span,
            value: swc_atoms::JsWord::from(value.as_str()),
            raw: None,
        })),
        values::ExprKind::Num(values::Num { value, .. }) => Expr::Lit(Lit::Num(Number {
            span,
            value: value.parse().unwrap(),
            raw: None,
        })),
        values::ExprKind::Bool(values::Bool { value, .. }) => Expr::Lit(Lit::Bool(Bool {
            span,
            value: *value,
        })),
        // values::ExprKind::Keyword(keyword) => match keyword {
        //     values::Keyword::Null => {
        //         swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Null(swc_ecma_ast::Null { span }))
        //     }
        //     values::Keyword::Undefined => {
        //         // NOTE: `undefined` is actually an identifier in JavaScript.
        //         swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
        //             span,
        //             sym: swc_atoms::JsWord::from(String::from("undefined")),
        //             optional: false,
        //         })
        //     }
        //     _ => panic!("{keyword} should only be used as a type"),
        // },
        values::ExprKind::Null(_) => {
            swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Null(swc_ecma_ast::Null { span }))
        }
        values::ExprKind::Undefined(_) => {
            // NOTE: `undefined` is actually an identifier in JavaScript.
            swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
                span,
                sym: swc_atoms::JsWord::from(String::from("undefined")),
                optional: false,
            })
        }
        values::ExprKind::Binary(values::Binary {
            op, left, right, ..
        }) => {
            let op = match op {
                values::BinaryOp::Plus => BinaryOp::Add,
                values::BinaryOp::Minus => BinaryOp::Sub,
                values::BinaryOp::Times => BinaryOp::Mul,
                values::BinaryOp::Divide => BinaryOp::Div,
                values::BinaryOp::Equals => BinaryOp::EqEqEq,
                values::BinaryOp::NotEquals => BinaryOp::NotEqEq,
                values::BinaryOp::LessThan => BinaryOp::Lt,
                values::BinaryOp::LessThanOrEqual => BinaryOp::LtEq,
                values::BinaryOp::GreaterThan => BinaryOp::Gt,
                values::BinaryOp::GreaterThanOrEqual => BinaryOp::GtEq,
                _ => todo!(),
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
        values::ExprKind::Unary(values::Unary { right: arg, op, .. }) => {
            let op = match op {
                values::UnaryOp::Minus => UnaryOp::Minus,
                values::UnaryOp::Not => todo!(),
                values::UnaryOp::Plus => todo!(),
            };

            Expr::Unary(UnaryExpr {
                span,
                op,
                arg: Box::from(build_expr(arg, stmts, ctx)),
            })
        }
        // values::ExprKind::Fix(values::Fix { expr, .. }) => match &expr.kind {
        //     values::ExprKind::Lambda(values::Lambda { body, .. }) => match body {
        //         values::BlockOrExpr::Expr(expr) => build_expr(expr, stmts, ctx),
        //         values::BlockOrExpr::Block(_) => panic!("Invalid recursive function"),
        //     },
        //     _ => panic!("Fix should only wrap a lambda"),
        // },
        values::ExprKind::IfElse(values::IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            // let $temp_n;
            let temp_id = ctx.new_ident();
            let temp_decl = build_let_decl_stmt(&temp_id);
            stmts.push(temp_decl);

            let finalizer = BlockFinalizer::Assign(temp_id.clone());

            // if (cond) { ...; $temp_n = <cons_res> } else { ...; $temp_n = <alt_res> }
            let test = Box::from(build_expr(cond.as_ref(), stmts, ctx));
            let cons = Box::from(Stmt::Block(build_body_block_stmt(
                consequent, &finalizer, ctx,
            )));
            let alt = alternate.as_ref().map(|alt| {
                let block = match alt {
                    values::BlockOrExpr::Block(alt) => {
                        Stmt::Block(build_body_block_stmt(alt, &finalizer, ctx))
                    }
                    values::BlockOrExpr::Expr(_) => todo!(),
                };
                Box::from(block)
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
        values::ExprKind::Object(values::Object { properties: props }) => {
            let props: Vec<PropOrSpread> = props
                .iter()
                .map(|prop| match prop {
                    values::PropOrSpread::Prop(prop) => match prop {
                        values::expr::Prop::Shorthand(ident) => {
                            PropOrSpread::Prop(Box::from(Prop::Shorthand(Ident::from(ident))))
                        }
                        values::expr::Prop::Property { key, value } => {
                            PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
                                key: prop_name_from_object_key(key, ctx),
                                value: Box::from(build_expr(value, stmts, ctx)),
                            })))
                        }
                    },
                    values::PropOrSpread::Spread(_) => todo!(),
                })
                .collect();

            Expr::Object(ObjectLit { span, props })
        }
        values::ExprKind::Await(values::Await { arg: expr, .. }) => Expr::Await(AwaitExpr {
            span,
            arg: Box::from(build_expr(expr.as_ref(), stmts, ctx)),
        }),
        values::ExprKind::JSXElement(elem) => {
            Expr::JSXElement(Box::from(build_jsx_element(elem, stmts, ctx)))
        }
        values::ExprKind::JSXFragment(_) => todo!(),
        values::ExprKind::Tuple(values::Tuple { elements: elems }) => Expr::Array(ArrayLit {
            span,
            elems: elems
                .iter()
                .map(|elem| match elem {
                    values::ExprOrSpread::Expr(expr) => Some(ExprOrSpread {
                        spread: None,
                        expr: Box::from(build_expr(expr, stmts, ctx)),
                    }),
                    values::ExprOrSpread::Spread(spread) => Some(ExprOrSpread {
                        spread: Some(DUMMY_SP),
                        expr: Box::from(build_expr(spread, stmts, ctx)),
                    }),
                })
                .collect(),
        }),
        values::ExprKind::Member(values::Member {
            object: obj,
            property: prop,
            ..
        }) => {
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
        // values::ExprKind::Empty => Expr::from(Ident {
        //     span,
        //     sym: JsWord::from("undefined"),
        //     optional: false,
        // }),
        // values::ExprKind::LetExpr(_) => {
        //     panic!("LetExpr should always be handled by the IfElse branch")
        // }
        values::ExprKind::TemplateLiteral(template) => {
            Expr::Tpl(build_template_literal(template, stmts, ctx))
        }
        // TODO: support tagged templates
        // values::ExprKind::TaggedTemplateLiteral(values::TaggedTemplateLiteral {
        //     tag,
        //     template,
        // }) => {
        //     Expr::TaggedTpl(TaggedTpl {
        //         span,
        //         tag: Box::from(Expr::Ident(Ident::from(tag))),
        //         type_params: None, // TODO: support type params on tagged templates

        //         tpl: Box::new(build_template_literal(template, stmts, ctx)),
        //     })
        // }
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
            let ident = Some(Ident::from(&values::Ident {
                name: "TODO".to_string(),
                span: values::Span { start: 0, end: 0 },
            }));
            let class = build_class(class, stmts, ctx);

            Expr::Class(ClassExpr {
                ident,
                class: Box::from(class),
            })
        }
        // values::ExprKind::Regex(regex) => Expr::Lit(Lit::Regex(Regex {
        //     span,
        //     exp: Atom::new(regex.pattern.as_ref()),
        //     flags: match &regex.flags {
        //         Some(flags) => Atom::new(flags.as_ref()),
        //         None => Atom::new(""),
        //     },
        // })),
        values::ExprKind::Do(do_expr) => {
            let temp_id = ctx.new_ident();
            let temp_decl = build_let_decl_stmt(&temp_id);
            stmts.push(temp_decl);

            let finalizer = BlockFinalizer::Assign(temp_id.clone());

            let block_stmt = build_body_block_stmt(&do_expr.body, &finalizer, ctx);
            stmts.push(Stmt::Block(block_stmt));

            Expr::Ident(temp_id)
        }
        values::ExprKind::Try(_) => todo!(),
        values::ExprKind::Yield(_) => todo!(),
        values::ExprKind::Throw(_) => todo!(),
    }
}

fn prop_name_from_object_key(key: &values::ObjectKey, ctx: &mut Context) -> PropName {
    match key {
        values::ObjectKey::Ident(ident) => PropName::Ident(Ident::from(ident)),
        values::ObjectKey::String(string) => PropName::Str(Str {
            span: DUMMY_SP,
            value: JsWord::from(string.to_owned()),
            raw: None,
        }),
        values::ObjectKey::Number(number) => PropName::Num(Number {
            span: DUMMY_SP,
            value: number.parse().unwrap(),
            raw: None,
        }),
        values::ObjectKey::Computed(expr) => PropName::Computed(ComputedPropName {
            span: DUMMY_SP,
            expr: Box::from(build_expr(expr, &mut vec![], ctx)),
        }),
    }
}

enum BlockFinalizer {
    ExprStmt,
    Assign(Ident),
}

fn build_finalizer(expr: &Expr, finalizer: &BlockFinalizer) -> Stmt {
    match &finalizer {
        BlockFinalizer::Assign(id) => Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::from(Expr::Assign(AssignExpr {
                span: DUMMY_SP,
                op: AssignOp::Assign,
                left: PatOrExpr::Pat(Box::from(Pat::Ident(BindingIdent {
                    id: id.to_owned(),
                    type_ann: None,
                }))),
                right: Box::from(expr.to_owned()),
            })),
        }),
        BlockFinalizer::ExprStmt => Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::from(expr.to_owned()),
        }),
    }
}

// NOTE: If an identifier has been specified in `assign_id` the last statement
// in the block will assign the final expression to that identifier.  If it's
// `None`, the last statement will be an actual return statement returning the
// final expression.
fn build_body_block_stmt(
    body: &values::Block,
    finalizer: &BlockFinalizer,
    ctx: &mut Context,
) -> BlockStmt {
    let mut new_stmts: Vec<Stmt> = vec![];
    let len = body.stmts.len();

    for (i, stmt) in body.stmts.iter().enumerate() {
        match &stmt.kind {
            values::StmtKind::VarDecl(values::VarDecl {
                pattern,
                type_ann: _,
                expr: Some(init),
                is_declare: _,
                ..
            }) => {
                let stmt = match build_pattern(pattern, &mut new_stmts, ctx) {
                    Some(name) => {
                        build_const_decl_stmt_with_pat(name, build_expr(init, &mut new_stmts, ctx))
                    }
                    None => todo!(),
                };
                new_stmts.push(stmt);
            }
            values::StmtKind::Expr(expr) => {
                let expr = build_expr(expr, &mut new_stmts, ctx);
                let stmt = if i == len - 1 {
                    build_finalizer(&expr, finalizer)
                } else {
                    Stmt::Expr(ExprStmt {
                        span: DUMMY_SP,
                        expr: Box::from(expr),
                    })
                };
                new_stmts.push(stmt);
            }
            // values::StmtKind::Class { class, ident, .. } => {
            //     let ident = Ident::from(ident);
            //     let class = build_class(class, &mut new_stmts, ctx);
            //     let stmt = Stmt::Decl(Decl::Class(ClassDecl {
            //         ident,
            //         class: Box::from(class),
            //         declare: false,
            //     }));
            //     new_stmts.push(stmt);
            // }
            // values::StmtKind::ForStmt(for_stmt) => {
            //     let stmt = Stmt::ForOf(ForOfStmt {
            //         span: DUMMY_SP,
            //         is_await: false,
            //         left: ForHead::VarDecl(Box::from(build_var_decl(
            //             &for_stmt.pattern,
            //             None,
            //             &mut new_stmts,
            //             ctx,
            //         ))),
            //         right: Box::from(build_expr(&for_stmt.expr, &mut new_stmts, ctx)),
            //         body: Box::from(Stmt::Block(build_body_block_stmt(
            //             &for_stmt.body,
            //             &BlockFinalizer::ExprStmt,
            //             ctx,
            //         ))),
            //     });
            //     new_stmts.push(stmt);
            // }
            values::StmtKind::Return(values::ReturnStmt { arg }) => {
                let stmt = Stmt::Return(ReturnStmt {
                    span: DUMMY_SP,
                    arg: arg
                        .as_ref()
                        .map(|arg| Box::from(build_expr(arg, &mut new_stmts, ctx))),
                });
                new_stmts.push(stmt);
            }

            // Types are ignored when generating .js code.
            values::StmtKind::TypeDecl { .. } => (),
            // Variable declarations that use `declare` are ignored as well.
            values::StmtKind::VarDecl { .. } => (),
        }
    }

    if body.stmts.is_empty() {
        let undefined = swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
            span: DUMMY_SP,
            sym: swc_atoms::JsWord::from(String::from("undefined")),
            optional: false,
        });
        match finalizer {
            BlockFinalizer::ExprStmt => (),
            _ => new_stmts.push(build_finalizer(&undefined, finalizer)),
        }
    }

    BlockStmt {
        span: DUMMY_SP,
        stmts: new_stmts,
    }
}

// fn build_let_expr(
//     let_expr: &values::LetExpr,
//     consequent: &values::Block,
//     alternate: Option<&values::Block>,
//     stmts: &mut Vec<Stmt>,
//     ctx: &mut Context,
// ) -> Expr {
//     let LetExpr { pat, expr, .. } = let_expr;

//     let ret_id = ctx.new_ident();
//     let ret_decl = build_let_decl_stmt(&ret_id);
//     stmts.push(ret_decl);

//     let temp_id = ctx.new_ident();
//     let temp_decl = build_const_decl_stmt(&temp_id, build_expr(expr, stmts, ctx));
//     stmts.push(temp_decl);

//     let cond = build_cond_for_pat(pat, &temp_id);

//     let finalizer = BlockFinalizer::Assign(ret_id.clone());
//     let mut block = build_body_block_stmt(consequent, &finalizer, ctx);

//     if let Some(name) = build_pattern(pat, stmts, ctx) {
//         // TODO: ignore the refutuable patterns when destructuring
//         let destructure = build_const_decl_stmt_with_pat(name, Expr::from(temp_id));
//         block.stmts.insert(0, destructure);
//     }

//     match cond {
//         Some(cond) => {
//             let alt = alternate
//                 .as_ref()
//                 .map(|alt| Box::from(Stmt::Block(build_body_block_stmt(alt, &finalizer, ctx))));
//             let if_else = Stmt::If(IfStmt {
//                 span: DUMMY_SP,
//                 test: Box::from(cond),
//                 cons: Box::from(Stmt::Block(block)),
//                 alt,
//             });
//             stmts.push(if_else);
//         }
//         None => {
//             stmts.push(Stmt::Block(block));
//         }
//     };

//     Expr::Ident(ret_id)
// }

fn build_arm(
    arm: &values::MatchArm,
    id: &Ident,
    ret_id: &Ident,
    stmts: &mut Vec<Stmt>,
    ctx: &mut Context,
) -> (Option<Expr>, BlockStmt) {
    let values::MatchArm {
        pattern: pat,
        body,
        guard,
        ..
    } = arm;

    let cond = build_cond_for_pat(pat, id);

    let mut block = match body {
        values::BlockOrExpr::Block(body) => {
            build_body_block_stmt(body, &BlockFinalizer::Assign(ret_id.to_owned()), ctx)
        }
        values::BlockOrExpr::Expr(expr) => {
            let mut stmts = vec![];
            let expr = build_expr(expr, &mut stmts, ctx);
            stmts.push(build_finalizer(
                &expr,
                &BlockFinalizer::Assign(ret_id.to_owned()),
            ));

            BlockStmt {
                span: DUMMY_SP,
                stmts,
            }
        }
    };

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
    let name = match &elem.opening.name {
        values::JSXElementName::Ident(name) => JSXElementName::Ident(Ident {
            span: DUMMY_SP,
            sym: JsWord::from(name.name.to_owned()),
            optional: false,
        }),
        values::JSXElementName::JSXMemberExpr(_) => todo!(),
    };

    let elem = JSXElement {
        span: DUMMY_SP,
        opening: JSXOpeningElement {
            span: DUMMY_SP,
            name: name.to_owned(),
            attrs: elem
                .opening
                .attrs
                .iter()
                .map(|values::JSXAttr { value, name, .. }| {
                    let value = value.as_ref().map(|val| match val {
                        values::JSXAttrValue::Str(value) => JSXAttrValue::Lit(Lit::Str(Str {
                            span: DUMMY_SP,
                            value: JsWord::from(value.to_owned()),
                            raw: None,
                            // Some would include the quotes around the string
                            // Some(JsWord::from(s.value.to_owned())),
                        })),
                        values::JSXAttrValue::ExprContainer(values::JSXExprContainer {
                            expr,
                            ..
                        }) => JSXAttrValue::JSXExprContainer(JSXExprContainer {
                            span: DUMMY_SP,
                            expr: JSXExpr::Expr(Box::from(build_expr(expr.as_ref(), stmts, ctx))),
                        }),
                    });

                    JSXAttrOrSpread::JSXAttr(JSXAttr {
                        span: DUMMY_SP,
                        name: JSXAttrName::Ident(Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(name.to_owned()),
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
                    values::JSXElementChild::Text(values::JSXText { value, .. }) => {
                        JSXElementChild::JSXText(JSXText {
                            span: DUMMY_SP,
                            value: Atom::new(value.clone()),
                            raw: Atom::new(value.clone()),
                        })
                    }
                    values::JSXElementChild::ExprContainer(values::JSXExprContainer {
                        expr,
                        ..
                    }) => JSXElementChild::JSXExprContainer(JSXExprContainer {
                        span: DUMMY_SP,
                        expr: JSXExpr::Expr(Box::from(build_expr(expr, stmts, ctx))),
                    }),
                    values::JSXElementChild::Element(elem) => {
                        JSXElementChild::JSXElement(Box::from(build_jsx_element(elem, stmts, ctx)))
                    }
                    values::JSXElementChild::SpreadChild(_) => todo!(),
                    values::JSXElementChild::Fragment(_) => todo!(),
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

// fn build_lit(lit: &values::Lit) -> Lit {
//     match lit {
//         values::Lit::Num(n) => Lit::Num(Number {
//             span: DUMMY_SP,
//             value: n.value.parse().unwrap(),
//             raw: None,
//         }),
//         values::Lit::Bool(b) => Lit::Bool(Bool {
//             span: DUMMY_SP,
//             value: b.value,
//         }),
//         values::Lit::Str(s) => Lit::Str(Str {
//             span: DUMMY_SP,
//             value: JsWord::from(s.value.to_owned()),
//             raw: None,
//             // Some would include the quotes around the string
//             // Some(JsWord::from(s.value.to_owned())),
//         }),
//     }
// }

fn build_class(class: &values::Class, stmts: &mut Vec<Stmt>, ctx: &mut Context) -> Class {
    let body: Vec<ClassMember> = class
        .body
        .iter()
        .filter_map(|member| match member {
            values::ClassMember::Constructor(constructor) => {
                let body = build_body_block_stmt(&constructor.body, &BlockFinalizer::ExprStmt, ctx);
                // In Crochet, `self` is always the first param in methods, but
                // it represents `this` in JavaScript which is implicit so we
                // ignore it here.
                let mut iter = constructor.params.iter();
                iter.next();
                let params: Vec<ParamOrTsParamProp> = iter
                    .map(|param| {
                        let pat = build_pattern(&param.pattern, stmts, ctx).unwrap();
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
                let body = build_body_block_stmt(&method.body, &BlockFinalizer::ExprStmt, ctx);

                // In Escalier, `self` is always the first param in non-static
                // methods, but it represents `this` in JavaScript which is
                // implicit so we ignore it here.
                let mut iter = method.params.iter();
                // TODO: Check if the first param is `self` and skip over it if it is
                // if !method.is_static {
                //     iter.next();
                // }
                let params: Vec<Param> = iter
                    .map(|param| {
                        let pat = build_pattern(&param.pattern, stmts, ctx).unwrap();
                        Param {
                            span: DUMMY_SP,
                            decorators: vec![],
                            pat,
                        }
                    })
                    .collect();

                Some(ClassMember::Method(ClassMethod {
                    span: DUMMY_SP, // TODO
                    key: prop_name_from_prop_name(&method.name, ctx),
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
            values::ClassMember::Field(prop) => {
                if prop.init.is_some() {
                    Some(ClassMember::ClassProp(ClassProp {
                        span: DUMMY_SP, // TODO
                        value: prop
                            .init
                            .as_ref()
                            .map(|value| Box::from(build_expr(value, stmts, ctx))),
                        key: PropName::Ident(Ident::from(&prop.name)),
                        type_ann: None,
                        is_static: false, // TODO,
                        decorators: vec![],
                        accessibility: None,
                        is_abstract: false,
                        is_optional: false, // TODO,
                        is_override: false,
                        readonly: false, // TODO
                        declare: false,
                        definite: false,
                    }))
                } else {
                    None
                }
            }
            values::ClassMember::Getter(_) => todo!(),
            values::ClassMember::Setter(_) => todo!(),
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

fn prop_name_from_prop_name(prop_name: &values::PropName, ctx: &mut Context) -> PropName {
    match prop_name {
        values::PropName::Ident(ident) => PropName::Ident(Ident::from(ident)),
        values::PropName::Computed(expr) => PropName::Computed(ComputedPropName {
            span: DUMMY_SP,
            expr: Box::from(build_expr(expr, &mut vec![], ctx)),
        }),
    }
}

fn build_cond_for_pat(pat: &values::Pattern, id: &Ident) -> Option<Expr> {
    // TODO: implmenent `is_refutable`
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

fn is_refutable(pat: &values::Pattern) -> bool {
    match &pat.kind {
        // irrefutable
        values::PatternKind::Ident(_) => false,
        values::PatternKind::Rest(_) => false,
        values::PatternKind::Wildcard => false,

        // refutable
        values::PatternKind::Lit(_) => true,
        values::PatternKind::Is(_) => true,

        // refutable if at least one sub-pattern is refutable
        values::PatternKind::Object(values::ObjectPat { props, .. }) => {
            props.iter().any(|prop| match prop {
                values::ObjectPatProp::KeyValue(values::KeyValuePatProp { value, .. }) => {
                    is_refutable(value)
                }
                values::ObjectPatProp::Shorthand(_) => false, // corresponds to {x} or {x = 5}
                values::ObjectPatProp::Rest(values::RestPat { arg, .. }) => is_refutable(arg),
            })
        }
        values::PatternKind::Tuple(values::TuplePat { elems, .. }) => {
            elems.iter().any(|elem| {
                match elem {
                    Some(elem) => is_refutable(&elem.pattern),
                    // FixMe: this should probably be true since it's equivalent
                    // to having an element with the value `undefined`
                    None => false,
                }
            })
        }
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
            .parts
            .iter()
            .map(|quasi| {
                // let cooked = match &quasi.cooked {
                //     values::Lit::Str(values::Str { value, .. }) => value,
                //     _ => panic!("quasi.cooked must be a string"),
                // };
                // let raw = match &quasi.raw {
                //     values::Lit::Str(values::Str { value, .. }) => value,
                //     _ => panic!("quasi.raw must be a string"),
                // };
                TplElement {
                    span: DUMMY_SP,
                    cooked: Some(Atom::new(quasi.value.clone())),
                    raw: Atom::new(quasi.value.clone()),
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
    EqualLit(values::Literal),
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
        values::PatternKind::Tuple(values::TuplePat { elems, .. }) => {
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
