use std::rc::Rc;

use swc_atoms::*;
use swc_common::comments::SingleThreadedComments;
use swc_common::hygiene::Mark;
use swc_common::source_map::{Globals, SourceMap, DUMMY_SP, GLOBALS};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;
use swc_ecma_transforms_react::{react, Options, Runtime};
use swc_ecma_visit::*;

use crate::ast;

pub fn codegen_js(program: &ast::Program) -> String {
    let program = build_js(program);

    let cm = Rc::new(SourceMap::default());
    let comments: Option<SingleThreadedComments> = None;
    let mut options = Options::default();
    options.runtime = Some(Runtime::Automatic);

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
        wr: text_writer::JsWriter::new(cm.clone(), "\n", &mut buf, None),
    };

    emitter.emit_program(program).unwrap();

    String::from_utf8_lossy(&buf).to_string()
}

fn build_js(program: &ast::Program) -> Program {
    let body: Vec<ModuleItem> = program
        .body
        .iter()
        .map(|child| match child {
            ast::Statement::Decl { pattern, value, .. } => {
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                    span: DUMMY_SP,
                    decl: Decl::Var(VarDecl {
                        span: DUMMY_SP,
                        kind: VarDeclKind::Const,
                        declare: false,
                        decls: vec![VarDeclarator {
                            span: DUMMY_SP,
                            name: build_pattern(&pattern),
                            init: Some(Box::from(build_expr(&value))),
                            definite: false,
                        }],
                    }),
                }))
            }
            ast::Statement::Expr { expr, .. } => ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                span: DUMMY_SP,
                expr: Box::from(build_expr(&expr)),
            })),
        })
        .collect();

    Program::Module(Module {
        span: DUMMY_SP,
        body,
        shebang: None,
    })
}

pub fn build_pattern(pattern: &ast::Pattern) -> Pat {
    match pattern {
        ast::Pattern::Ident(ident) => Pat::Ident(BindingIdent {
            id: Ident {
                span: DUMMY_SP,
                sym: JsWord::from(ident.name.to_owned()),
                optional: false,
            },
            type_ann: None,
        }),
    }
}

pub fn build_return_block(body: &ast::Expr) -> BlockStmt {
    match body {
        // Avoids wrapping in an IIFE when it isn't necessary.
        ast::Expr::Let { .. } => BlockStmt {
            span: DUMMY_SP,
            stmts: let_to_children(body),
        },
        _ => BlockStmt {
            span: DUMMY_SP,
            stmts: vec![Stmt::Return(ReturnStmt {
                span: DUMMY_SP,
                arg: Some(Box::from(build_expr(body))),
            })],
        },
    }
}

pub fn build_expr(expr: &ast::Expr) -> Expr {
    match expr {
        ast::Expr::App(ast::App { lam, args, .. }) => {
            let callee = Callee::Expr(Box::from(build_expr(&lam.as_ref())));
            let args: Vec<ExprOrSpread> = args
                .iter()
                .map(|arg| ExprOrSpread {
                    spread: None,
                    expr: Box::from(build_expr(&arg)),
                })
                .collect();

            Expr::Call(CallExpr {
                span: DUMMY_SP,
                callee,
                args,
                type_args: None,
            })
        }
        ast::Expr::Ident(ident) => Expr::from(Ident {
            span: DUMMY_SP,
            sym: JsWord::from(ident.name.to_owned()),
            optional: false,
        }),
        ast::Expr::Lambda(ast::Lambda {
            args,
            body,
            is_async,
            ..
        }) => {
            let params: Vec<Pat> = args
                .iter()
                .map(|ident| {
                    let name = match ident {
                        ast::BindingIdent::Ident(ident) => ident.name.to_owned(),
                        ast::BindingIdent::Rest { name, .. } => name.to_owned(),
                    };
                    Pat::Ident(BindingIdent {
                        id: Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(name),
                            optional: false,
                        },
                        type_ann: None,
                    })
                })
                .collect();

            let body: BlockStmtOrExpr = match &body.as_ref() {
                // TODO: Avoid wrapping in an IIFE when it isn't necessary.
                ast::Expr::Let { .. } => BlockStmtOrExpr::BlockStmt(BlockStmt {
                    span: DUMMY_SP,
                    stmts: let_to_children(body),
                }),
                _ => BlockStmtOrExpr::Expr(Box::from(build_expr(body))),
            };

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
        ast::Expr::Let { .. } => {
            // Return an IIFE
            let arrow = Expr::Arrow(ArrowExpr {
                span: DUMMY_SP,
                params: vec![],
                body: BlockStmtOrExpr::BlockStmt(BlockStmt {
                    span: DUMMY_SP,
                    stmts: let_to_children(expr),
                }),
                is_async: false,
                is_generator: false,
                type_params: None,
                return_type: None,
            });

            let callee = Callee::Expr(Box::from(Expr::Paren(ParenExpr {
                span: DUMMY_SP,
                expr: Box::from(arrow),
            })));

            Expr::Call(CallExpr {
                span: DUMMY_SP,
                callee,
                args: vec![],
                type_args: None,
            })
        }
        ast::Expr::Lit(lit) => {
            match lit {
                ast::Lit::Num(n) => {
                    let lit = Lit::Num(Number {
                        span: DUMMY_SP,
                        // TODO: include the parsed value in the source AST node
                        value: n.value.parse().unwrap(),
                        // Use `None` value only for transformations to avoid recalculate
                        // characters in number literal
                        raw: Some(JsWord::from(n.value.clone())),
                    });
                    Expr::Lit(lit)
                }
                ast::Lit::Bool(b) => {
                    let lit = Lit::Bool(Bool {
                        span: DUMMY_SP,
                        value: b.value,
                    });
                    Expr::Lit(lit)
                }
                ast::Lit::Str(s) => {
                    let lit = Lit::Str(Str {
                        span: DUMMY_SP,
                        value: JsWord::from(s.value.clone()),
                        // Use `None` value only for transformations to avoid recalculate escaped
                        // characters in strings
                        raw: None,
                    });
                    Expr::Lit(lit)
                }
                ast::Lit::Null(_) => {
                    let lit = Lit::Null(Null { span: DUMMY_SP });
                    Expr::Lit(lit)
                }
                ast::Lit::Undefined(_) => Expr::from(Ident {
                    span: DUMMY_SP,
                    sym: JsWord::from("undefined"),
                    optional: false,
                }),
            }
        }
        ast::Expr::Op(ast::Op {
            op, left, right, ..
        }) => {
            let op = match op {
                ast::BinOp::Add => BinaryOp::Add,
                ast::BinOp::Sub => BinaryOp::Sub,
                ast::BinOp::Mul => BinaryOp::Mul,
                ast::BinOp::Div => BinaryOp::Div,
                ast::BinOp::Eq => BinaryOp::EqEqEq,
                ast::BinOp::NotEq => BinaryOp::NotEqEq,
                ast::BinOp::Lt => BinaryOp::Lt,
                ast::BinOp::LtEq => BinaryOp::LtEq,
                ast::BinOp::Gt => BinaryOp::Gt,
                ast::BinOp::GtEq => BinaryOp::GtEq,
            };

            let left = Box::from(build_expr(&left));

            let wrap_left = match left.as_ref() {
                Expr::Bin(left) => left.op.precedence() < op.precedence(),
                _ => false,
            };

            let right = Box::from(build_expr(&right));

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
        ast::Expr::Fix(ast::Fix { expr, .. }) => match expr.as_ref() {
            ast::Expr::Lambda(ast::Lambda { body, .. }) => build_expr(&body),
            _ => panic!("Fix should only wrap a lambda"),
        },
        ast::Expr::IfElse(ast::IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            // Returns an IIFE that looks like:
            // (() => {
            //    if (cond) {
            //        return consequent;
            //    } else {
            //        return alternate;
            //    }
            // })();
            let body = BlockStmtOrExpr::BlockStmt(BlockStmt {
                span: DUMMY_SP,
                stmts: vec![Stmt::If(IfStmt {
                    span: DUMMY_SP,
                    test: Box::from(build_expr(&cond.as_ref())),
                    cons: Box::from(Stmt::Block(build_return_block(&consequent.as_ref()))),
                    alt: Some(Box::from(Stmt::Block(build_return_block(
                        &alternate.as_ref(),
                    )))),
                })],
            });

            let arrow = Expr::Arrow(ArrowExpr {
                span: DUMMY_SP,
                params: vec![],
                body,
                is_async: false,
                is_generator: false,
                type_params: None,
                return_type: None,
            });

            let callee = Callee::Expr(Box::from(Expr::Paren(ParenExpr {
                span: DUMMY_SP,
                expr: Box::from(arrow),
            })));

            Expr::Call(CallExpr {
                span: DUMMY_SP,
                callee,
                args: vec![],
                type_args: None,
            })
        }
        ast::Expr::Obj(ast::Obj { properties, .. }) => {
            let props: Vec<PropOrSpread> = properties
                .iter()
                .map(|prop| {
                    PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
                        key: PropName::from(Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(prop.name.clone()),
                            optional: false,
                        }),
                        value: Box::from(build_expr(&prop.value)),
                    })))
                })
                .collect();

            Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props,
            })
        }
        ast::Expr::Await(ast::Await { expr, .. }) => Expr::Await(AwaitExpr {
            span: DUMMY_SP,
            arg: Box::from(build_expr(&expr.as_ref())),
        }),
        ast::Expr::JSXElement(elem) => Expr::JSXElement(Box::from(build_jsx_element(elem))),
    }
}

pub fn build_jsx_element(elem: &ast::JSXElement) -> JSXElement {
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
                        ast::JSXAttrValue::Lit(lit) => JSXAttrValue::Lit(build_lit(&lit)),
                        ast::JSXAttrValue::JSXExprContainer(ast::JSXExprContainer {
                            expr, ..
                        }) => JSXAttrValue::JSXExprContainer(JSXExprContainer {
                            span: DUMMY_SP,
                            expr: JSXExpr::Expr(Box::from(build_expr(expr))),
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
                            value: JsWord::from(value.to_owned()),
                            raw: JsWord::from(value.to_owned()),
                        })
                    }
                    ast::JSXElementChild::JSXExprContainer(ast::JSXExprContainer {
                        expr, ..
                    }) => JSXElementChild::JSXExprContainer(JSXExprContainer {
                        span: DUMMY_SP,
                        expr: JSXExpr::Expr(Box::from(build_expr(expr))),
                    }),
                    ast::JSXElementChild::JSXElement(elem) => {
                        JSXElementChild::JSXElement(Box::from(build_jsx_element(elem)))
                    }
                };
                result
            })
            .collect(),
        closing: Some(JSXClosingElement {
            span: DUMMY_SP,
            name: name.to_owned(),
        }),
    };

    elem
}

pub fn build_lit(lit: &ast::Lit) -> Lit {
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

pub fn let_to_children(expr: &ast::Expr) -> Vec<Stmt> {
    if let ast::Expr::Let(ast::Let {
        pattern,
        value,
        body,
        ..
    }) = expr
    {
        // TODO: handle shadowed variables in the same scope by introducing
        // unique identifiers.
        let decl = Stmt::Decl(Decl::Var(VarDecl {
            span: DUMMY_SP,
            kind: VarDeclKind::Const,
            declare: false,
            decls: vec![VarDeclarator {
                span: DUMMY_SP,
                name: build_pattern(&pattern),
                init: Some(Box::from(build_expr(&value))),
                definite: false,
            }],
        }));

        let mut children: Vec<Stmt> = vec![decl];
        let mut body = body.to_owned();

        while let ast::Expr::Let(ast::Let {
            pattern,
            value,
            body: next_body,
            ..
        }) = body.as_ref()
        {
            let decl = Stmt::Decl(Decl::Var(VarDecl {
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: false,
                decls: vec![VarDeclarator {
                    span: DUMMY_SP,
                    name: build_pattern(&pattern),
                    init: Some(Box::from(build_expr(&value))),
                    definite: false,
                }],
            }));
            children.push(decl);
            body = next_body.to_owned();
        }

        children.push(Stmt::Return(ReturnStmt {
            span: DUMMY_SP,
            arg: Some(Box::from(build_expr(&body))),
        }));

        children
    } else {
        panic!("was expecting an ast::Expr::Let")
    }
}
