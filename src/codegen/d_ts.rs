use std::rc::Rc;

use swc_atoms::*;
use swc_common::{DUMMY_SP, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;

use crate::ast;
use crate::infer::Env;
use crate::types::{Scheme, TLam, Type, TypeKind};

pub fn codegen_d_ts(program: &ast::Program, env: &Env) -> String {
    print_d_ts(&build_d_ts(program, env))
}

fn print_d_ts(program: &Program) -> String {
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

fn build_d_ts(program: &ast::Program, env: &Env) -> Program {
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
                        declare: true,
                        decls: vec![VarDeclarator {
                            span: DUMMY_SP,
                            name: build_pattern(&pattern, &value, env),
                            init: None,
                            definite: false,
                        }],
                    }),
                }))
            }
            _ => ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP })),
        })
        .collect();

    Program::Module(Module {
        span: DUMMY_SP,
        body,
        shebang: None,
    })
}

pub fn build_pattern(pattern: &ast::Pattern, value: &ast::Expr, env: &Env) -> Pat {
    match pattern {
        ast::Pattern::Ident(ident) => {
            let scheme = env.get(&ident.name).unwrap();
            let type_params = build_type_params(&scheme);

            Pat::Ident(BindingIdent {
                id: Ident {
                    span: DUMMY_SP,
                    sym: JsWord::from(ident.name.to_owned()),
                    optional: false,
                },
                type_ann: Some(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: Box::from(build_type(&scheme.ty, Some(value), type_params)),
                }),
            })
        }
    }
}

pub fn build_type_params(scheme: &Scheme) -> Option<TsTypeParamDecl> {
    let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        .chars()
        .collect();

    if scheme.qualifiers.len() > 0 {
        Some(TsTypeParamDecl {
            span: DUMMY_SP,
            params: scheme
                .qualifiers
                .iter()
                .map(|id| {
                    let id = chars.get(id.to_owned() as usize).unwrap();

                    TsTypeParam {
                        span: DUMMY_SP,
                        name: Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(format!("{id}")),
                            optional: false,
                        },
                        is_in: false,
                        is_out: false,
                        constraint: None,
                        default: None,
                    }
                })
                .collect(),
        })
    } else {
        None
    }
}

/// Converts an internal Type to a TsType for eventual export to .d.ts.
///
/// `expr` should be the original expression that `ty` was inferred
/// from if it exists.
///
/// TODO: pass in `type_params` as an optional param
pub fn build_type(
    ty: &Type,
    expr: Option<&ast::Expr>,
    type_params: Option<TsTypeParamDecl>,
) -> TsType {
    match &ty.kind {
        TypeKind::Var => {
            let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                .chars()
                .collect();
            let id = chars.get(ty.id.to_owned() as usize).unwrap();

            TsType::TsTypeRef(TsTypeRef {
                span: DUMMY_SP,
                type_name: TsEntityName::from(Ident {
                    span: DUMMY_SP,
                    sym: JsWord::from(format!("{id}")),
                    optional: false,
                }),
                type_params: None,
            })
        }
        TypeKind::Prim(prim) => {
            let kind = match prim {
                crate::types::Primitive::Num => TsKeywordTypeKind::TsNumberKeyword,
                crate::types::Primitive::Bool => TsKeywordTypeKind::TsBooleanKeyword,
                crate::types::Primitive::Str => TsKeywordTypeKind::TsStringKeyword,
                crate::types::Primitive::Undefined => TsKeywordTypeKind::TsUndefinedKeyword,
                crate::types::Primitive::Null => TsKeywordTypeKind::TsNullKeyword,
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        TypeKind::Lit(lit) => {
            let lit = match lit {
                crate::types::Lit::Num(n) => TsLit::Number(Number {
                    span: DUMMY_SP,
                    value: n.parse().unwrap(),
                    raw: Some(JsWord::from(n.to_owned())),
                }),
                crate::types::Lit::Bool(b) => TsLit::Bool(Bool {
                    span: DUMMY_SP,
                    value: b.to_owned(),
                }),
                crate::types::Lit::Str(s) => TsLit::Str(Str {
                    span: DUMMY_SP,
                    value: JsWord::from(s.clone()),
                    raw: None,
                }),
                // TODO: remove these frmo types::Lit since they're covered
                // by primitives.
                // crate::types::Lit::Null => todo!(),
                // crate::types::Lit::Undefined => todo!(),
                _ => panic!("TODO: model null and undefined as keywords"),
            };

            TsType::TsLitType(TsLitType {
                span: DUMMY_SP,
                lit,
            })
        }
        // This is used to copy the names of args from the expression
        // over to the lambda's type.
        TypeKind::Lam(TLam { args, ret }) => {
            match expr {
                // TODO: handle is_async
                Some(ast::Expr::Lambda(ast::Lambda {
                    args: expr_args, ..
                })) => {
                    if args.len() != expr_args.len() {
                        panic!("number of args don't match")
                    } else {
                        let params: Vec<TsFnParam> = args
                            .iter()
                            .zip(expr_args)
                            .map(|(arg, binding)| {
                                let type_ann = Some(TsTypeAnn {
                                    span: DUMMY_SP,
                                    type_ann: Box::from(build_type(arg, None, None)),
                                });

                                match binding {
                                    ast::BindingIdent::Ident(ident) => {
                                        TsFnParam::Ident(BindingIdent {
                                            id: Ident {
                                                span: DUMMY_SP,
                                                sym: JsWord::from(ident.name.to_owned()),
                                                optional: false,
                                            },
                                            type_ann,
                                        })
                                    }
                                    ast::BindingIdent::Rest { name, .. } => {
                                        TsFnParam::Rest(RestPat {
                                            span: DUMMY_SP,
                                            dot3_token: DUMMY_SP,
                                            arg: Box::from(Pat::from(Ident {
                                                span: DUMMY_SP,
                                                sym: JsWord::from(name.to_owned()),
                                                optional: false,
                                            })),
                                            type_ann,
                                        })
                                    }
                                }
                            })
                            .collect();

                        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
                            span: DUMMY_SP,
                            params,
                            type_params,
                            type_ann: TsTypeAnn {
                                span: DUMMY_SP,
                                type_ann: Box::from(build_type(&ret, None, None)),
                            },
                        }))
                    }
                }
                // Fix nodes are assumed to wrap a lambda where the body of
                // the lambda is recursive function.
                Some(ast::Expr::Fix(ast::Fix { expr, .. })) => match expr.as_ref() {
                    ast::Expr::Lambda(ast::Lambda { body, .. }) => {
                        build_type(ty, Some(&body), type_params)
                    }
                    _ => panic!("mismatch"),
                },
                None => {
                    let params: Vec<TsFnParam> = args
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| {
                            let type_ann = Some(TsTypeAnn {
                                span: DUMMY_SP,
                                type_ann: Box::from(build_type(arg, None, None)),
                            });

                            TsFnParam::Ident(BindingIdent {
                                id: Ident {
                                    span: DUMMY_SP,
                                    sym: JsWord::from(format!("arg{}", i)),
                                    optional: false,
                                },
                                type_ann,
                            })
                        })
                        .collect();

                    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
                        span: DUMMY_SP,
                        params,
                        type_params,
                        type_ann: TsTypeAnn {
                            span: DUMMY_SP,
                            type_ann: Box::from(build_type(&ret, None, None)),
                        },
                    }))
                }
                _ => panic!("mismatch"),
            }
        }
        TypeKind::Union(types) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: types
                    .iter()
                    .map(|ty| Box::from(build_type(ty, None, None)))
                    .collect(),
            }))
        }
        TypeKind::Obj(props) => {
            let members: Vec<TsTypeElement> = props
                .iter()
                .map(|prop| {
                    TsTypeElement::TsPropertySignature(TsPropertySignature {
                        span: DUMMY_SP,
                        readonly: false,
                        key: Box::from(Expr::from(Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(prop.name.to_owned()),
                            optional: false,
                        })),
                        computed: false,
                        optional: false,
                        init: None,
                        params: vec![],
                        type_ann: Some(TsTypeAnn {
                            span: DUMMY_SP,
                            type_ann: Box::from(build_type(&prop.ty, None, None)),
                        }),
                        type_params: None,
                    })
                })
                .collect();

            TsType::TsTypeLit(TsTypeLit {
                span: DUMMY_SP,
                members,
            })
        }
        TypeKind::Alias { name, type_params } => TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::from(Ident {
                span: DUMMY_SP,
                sym: JsWord::from(name.to_owned()),
                optional: false,
            }),
            type_params: Some(TsTypeParamInstantiation {
                span: DUMMY_SP,
                params: type_params
                    .iter()
                    .map(|ty| Box::from(build_type(ty, None, None)))
                    .collect(),
            }),
        }),
    }
}
