use std::rc::Rc;

use swc_atoms::*;
use swc_common::{SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;

use crochet_ast as ast;
use crochet_infer::types::{self, Scheme, Type, FnParam, Variant};
use crochet_infer::Context;

pub fn codegen_d_ts(program: &ast::Program, ctx: &Context) -> String {
    print_d_ts(&build_d_ts(program, ctx))
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
        wr: text_writer::JsWriter::new(cm, "\n", &mut buf, None),
    };

    emitter.emit_program(program).unwrap();

    String::from_utf8_lossy(&buf).to_string()
}

fn build_d_ts(program: &ast::Program, ctx: &Context) -> Program {
    let body: Vec<ModuleItem> = program
        .body
        .iter()
        .map(|child| match child {
            ast::Statement::VarDecl { pattern, init, .. } => {
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                    span: DUMMY_SP,
                    decl: Decl::Var(VarDecl {
                        span: DUMMY_SP,
                        kind: VarDeclKind::Const,
                        declare: true,
                        decls: vec![VarDeclarator {
                            span: DUMMY_SP,
                            name: build_pattern(pattern, init.as_ref(), ctx),
                            init: None,
                            definite: false,
                        }],
                    }),
                }))
            }
            ast::Statement::TypeDecl { declare, id, .. } => match ctx.types.get(&id.name) {
                Some(scheme) => ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(TsTypeAliasDecl {
                    span: DUMMY_SP,
                    declare: declare.to_owned(),
                    id: build_ident(id),
                    type_params: None,
                    type_ann: Box::from(build_type(&scheme.ty, None, None)),
                }))),
                None => panic!("Couldn't find type in ctx.types"),
            },
            _ => ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP })),
        })
        .collect();

    Program::Module(Module {
        span: DUMMY_SP,
        body,
        shebang: None,
    })
}

// TODO: create a trait for this and then provide multiple implementations
pub fn build_ident(id: &ast::Ident) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: JsWord::from(id.name.to_owned()),
        optional: false,
    }
}

pub fn build_param(r#type: &Type, pattern: &ast::Pattern) -> TsFnParam {
    let type_ann = Some(TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::from(build_type(r#type, None, None)),
    });

    match pattern {
        ast::Pattern::Ident(ast::BindingIdent { id, .. }) => TsFnParam::Ident(BindingIdent {
            id: build_ident(id),
            type_ann,
        }),
        ast::Pattern::Wildcard(_) => todo!(),
        ast::Pattern::Rest(ast::RestPat { arg, .. }) => TsFnParam::Rest(RestPat {
            span: DUMMY_SP,
            dot3_token: DUMMY_SP,
            arg: Box::from(build_pattern_rec(arg.as_ref())),
            type_ann,
        }),
        ast::Pattern::Object(_) => todo!(),
        ast::Pattern::Array(_) => todo!(),
        ast::Pattern::Lit(_) => todo!(),
        ast::Pattern::Is(_) => todo!(),
    }
}

pub fn build_ts_fn_type_with_params(params: &[FnParam], ret: &Type, type_params: Option<TsTypeParamDecl>) -> TsType {
    let params: Vec<TsFnParam> = params
        .iter()
        .map(|param| {
            let type_ann = Some(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(&param.get_type(), None, None)),
            });

            match param {
                FnParam::Ident(bi) => {
                    TsFnParam::Ident(BindingIdent {
                        id: Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(bi.name.to_owned()),
                            optional: false,
                        },
                        type_ann,
                    })
                },
            }
        })
        .collect();

    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
        span: DUMMY_SP,
        params,
        type_params,
        type_ann: TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::from(build_type(ret, None, None)),
        },
    }))
}

pub fn build_ts_fn_type_with_args(args: &[Type], ret: &Type, type_params: Option<TsTypeParamDecl>) -> TsType {
    let args: Vec<TsFnParam> = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            let type_ann = Some(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(arg, None, None)),
            });

            TsFnParam::Ident(BindingIdent {
                id: Ident {
                    span: DUMMY_SP,
                    sym: JsWord::from(format!("arg{}", index)),
                    optional: false,
                },
                type_ann,
            })
        })
        .collect();

    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
        span: DUMMY_SP,
        params: args,
        type_params,
        type_ann: TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::from(build_type(ret, None, None)),
        },
    }))
}

pub fn build_pattern(pattern: &ast::Pattern, value: Option<&ast::Expr>, ctx: &Context) -> Pat {
    match pattern {
        ast::Pattern::Ident(ast::BindingIdent { id, .. }) => {
            let scheme = ctx.values.get(&id.name).unwrap();
            let type_params = build_type_params(scheme);

            Pat::Ident(BindingIdent {
                id: build_ident(id),
                type_ann: Some(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: Box::from(build_type(&scheme.ty, value, type_params)),
                }),
            })
        }
        ast::Pattern::Wildcard(_) => todo!(),
        ast::Pattern::Rest(_) => todo!(),
        ast::Pattern::Object(_) => todo!(),
        ast::Pattern::Array(_) => todo!(),
        ast::Pattern::Lit(_) => todo!(),
        ast::Pattern::Is(_) => todo!(),
    }
}

pub fn build_pattern_rec(pattern: &ast::Pattern) -> Pat {
    match pattern {
        ast::Pattern::Ident(ast::BindingIdent { id, .. }) => Pat::Ident(BindingIdent {
            id: build_ident(id),
            type_ann: None,
        }),
        ast::Pattern::Wildcard(_) => todo!(),
        ast::Pattern::Rest(ast::RestPat { arg, .. }) => Pat::Rest(RestPat {
            span: DUMMY_SP,
            dot3_token: DUMMY_SP,
            arg: Box::from(build_pattern_rec(arg.as_ref())),
            type_ann: None,
        }),
        ast::Pattern::Object(_) => todo!(),
        ast::Pattern::Array(_) => todo!(),
        ast::Pattern::Lit(_) => todo!(),
        ast::Pattern::Is(_) => todo!(),
    }
}

pub fn build_type_params(scheme: &Scheme) -> Option<TsTypeParamDecl> {
    let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        .chars()
        .collect();

    if !scheme.qualifiers.is_empty() {
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
pub fn build_type(
    ty: &Type,
    expr: Option<&ast::Expr>,
    type_params: Option<TsTypeParamDecl>,
) -> TsType {
    match &ty.variant {
        Variant::Var => {
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
        Variant::Prim(prim) => {
            let kind = match prim {
                crochet_infer::types::Primitive::Num => TsKeywordTypeKind::TsNumberKeyword,
                crochet_infer::types::Primitive::Bool => TsKeywordTypeKind::TsBooleanKeyword,
                crochet_infer::types::Primitive::Str => TsKeywordTypeKind::TsStringKeyword,
                crochet_infer::types::Primitive::Undefined => TsKeywordTypeKind::TsUndefinedKeyword,
                crochet_infer::types::Primitive::Null => TsKeywordTypeKind::TsNullKeyword,
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        Variant::Lit(lit) => {
            let lit = match lit {
                crochet_infer::types::Lit::Num(n) => TsLit::Number(Number {
                    span: DUMMY_SP,
                    value: n.parse().unwrap(),
                    raw: Some(Atom::new(n.to_owned())),
                }),
                crochet_infer::types::Lit::Bool(b) => TsLit::Bool(Bool {
                    span: DUMMY_SP,
                    value: b.to_owned(),
                }),
                crochet_infer::types::Lit::Str(s) => TsLit::Str(Str {
                    span: DUMMY_SP,
                    value: JsWord::from(s.clone()),
                    raw: None,
                }),
                // TODO: remove these frmo types::Lit since they're covered
                // by primitives.
                // crochet_infer::types::Lit::Null => todo!(),
                // crochet_infer::types::Lit::Undefined => todo!(),
                _ => panic!("TODO: model null and undefined as keywords"),
            };

            TsType::TsLitType(TsLitType {
                span: DUMMY_SP,
                lit,
            })
        }
        Variant::App(types::AppType { args, ret, .. }) => {
            // This can happen when a function type is inferred by usage
            match expr {
                // TODO: handle is_async
                Some(ast::Expr::Lambda(ast::Lambda { params, .. })) => {
                    if args.len() != params.len() {
                        panic!("number of args don't match")
                    } else {
                        let args: Vec<TsFnParam> = args
                            .iter()
                            .zip(params)
                            .map(|(arg, pattern)| build_param(arg, pattern))
                            .collect();

                        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
                            span: DUMMY_SP,
                            params: args,
                            type_params,
                            type_ann: TsTypeAnn {
                                span: DUMMY_SP,
                                type_ann: Box::from(build_type(ret, None, None)),
                            },
                        }))
                    }
                }
                // Fix nodes are assumed to wrap a lambda where the body of
                // the lambda is recursive function.
                Some(ast::Expr::Fix(ast::Fix { expr, .. })) => match expr.as_ref() {
                    ast::Expr::Lambda(ast::Lambda { body, .. }) => {
                        build_type(ty, Some(body), type_params)
                    }
                    _ => panic!("mismatch"),
                },
                _ => build_ts_fn_type_with_args(args, ret, type_params),
            }
        }
        // This is used to copy the names of args from the expression
        // over to the lambda's type.
        Variant::Lam(types::LamType { params, ret, .. }) => {
            match expr {
                // TODO: handle is_async
                Some(ast::Expr::Lambda(other_lam)) => {
                    if params.len() != other_lam.params.len() {
                        panic!("number of params don't match")
                    } else {
                        let params: Vec<TsFnParam> = params
                            .iter()
                            .zip(&other_lam.params)
                            .map(|(param, pattern)| build_param(&param.get_type(), pattern))
                            .collect();

                        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
                            span: DUMMY_SP,
                            params,
                            type_params,
                            type_ann: TsTypeAnn {
                                span: DUMMY_SP,
                                type_ann: Box::from(build_type(ret, None, None)),
                            },
                        }))
                    }
                }
                // Fix nodes are assumed to wrap a lambda where the body of
                // the lambda is recursive function.
                Some(ast::Expr::Fix(ast::Fix { expr, .. })) => match expr.as_ref() {
                    ast::Expr::Lambda(ast::Lambda { body, .. }) => {
                        build_type(ty, Some(body), type_params)
                    }
                    _ => panic!("mismatch"),
                },
                _ => build_ts_fn_type_with_params(params, ret, type_params),
            }
        }
        Variant::Union(types) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: types
                    .iter()
                    .map(|ty| Box::from(build_type(ty, None, None)))
                    .collect(),
            }))
        }
        Variant::Intersection(types) => TsType::TsUnionOrIntersectionType(
            TsUnionOrIntersectionType::TsIntersectionType(TsIntersectionType {
                span: DUMMY_SP,
                types: types
                    .iter()
                    .map(|ty| Box::from(build_type(ty, None, None)))
                    .collect(),
            }),
        ),
        Variant::Object(props) => {
            let members: Vec<TsTypeElement> = props
                .iter()
                .map(|prop| {
                    TsTypeElement::TsPropertySignature(TsPropertySignature {
                        span: DUMMY_SP,
                        readonly: !prop.mutable,
                        key: Box::from(Expr::from(Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(prop.name.to_owned()),
                            optional: false,
                        })),
                        computed: false,
                        optional: prop.optional,
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
        Variant::Alias(types::AliasType {
            name, type_params, ..
        }) => TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::from(Ident {
                span: DUMMY_SP,
                sym: JsWord::from(name.to_owned()),
                optional: false,
            }),
            type_params: type_params.clone().map(|params| TsTypeParamInstantiation {
                span: DUMMY_SP,
                params: params
                    .iter()
                    .map(|ty| Box::from(build_type(ty, None, None)))
                    .collect(),
            }),
        }),
        Variant::Tuple(types) => TsType::TsTupleType(TsTupleType {
            span: DUMMY_SP,
            elem_types: types
                .iter()
                .map(|ty| TsTupleElement {
                    span: DUMMY_SP,
                    label: None,
                    ty: build_type(ty, None, None),
                })
                .collect(),
        }),
        Variant::Array(t) => TsType::TsArrayType(TsArrayType {
            span: DUMMY_SP,
            elem_type: Box::from(build_type(t, None, None)),
        }),
        Variant::Rest(_) => todo!(),
        Variant::Member(_) => todo!(),
        Variant::Wildcard => todo!(),
    }
}
