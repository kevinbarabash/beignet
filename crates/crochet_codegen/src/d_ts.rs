use itertools::Itertools;
use std::rc::Rc;

use swc_atoms::*;
use swc_common::{SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;

use crochet_ast as ast;
use crochet_infer::Context;
use crochet_types::{self as types, Scheme, TFnParam, TPat, Type};

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

fn build_d_ts(_program: &ast::Program, ctx: &Context) -> Program {
    let current_scope = ctx.scopes.last().unwrap();

    let mut body: Vec<ModuleItem> = vec![];

    for (name, scheme) in current_scope.types.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
        let id = Ident {
            span: DUMMY_SP,
            sym: JsWord::from(name.to_owned()),
            optional: false,
        };
        let decl = ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(TsTypeAliasDecl {
            span: DUMMY_SP,
            declare: true,
            id,
            type_params: build_type_params(scheme),
            type_ann: Box::from(build_type(&scheme.t, None)),
        })));

        body.push(decl);
    }

    for (name, scheme) in current_scope.values.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
        let type_params = build_type_params(scheme);
        let id = Ident {
            span: DUMMY_SP,
            sym: JsWord::from(name.to_owned()),
            optional: false,
        };
        let pat = Pat::Ident(BindingIdent {
            id,
            type_ann: Some(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(&scheme.t, type_params)),
            }),
        });

        let decl = ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::Var(VarDecl {
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: true,
                decls: vec![VarDeclarator {
                    span: DUMMY_SP,
                    name: pat,
                    init: None,
                    definite: false,
                }],
            }),
        }));

        body.push(decl);
    }

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

pub fn build_param(r#type: &Type, e_param: &ast::EFnParam) -> TsFnParam {
    let type_ann = Some(TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::from(build_type(r#type, None)),
    });

    let pat = build_param_pat_rec(&e_param.pat, type_ann);

    match pat {
        Pat::Ident(bi) => {
            let id = Ident {
                optional: e_param.optional,
                ..bi.id
            };
            TsFnParam::Ident(BindingIdent { id, ..bi })
        }
        Pat::Array(array) => TsFnParam::Array(array),
        Pat::Rest(rest) => TsFnParam::Rest(rest),
        Pat::Object(obj) => TsFnParam::Object(obj),
        Pat::Assign(_) => todo!(),
        Pat::Invalid(_) => todo!(),
        Pat::Expr(_) => todo!(),
    }
}

pub fn build_param_pat_rec(pattern: &ast::EFnParamPat, type_ann: Option<TsTypeAnn>) -> Pat {
    match pattern {
        ast::EFnParamPat::Ident(ast::EFnParamBindingIdent { id, .. }) => Pat::Ident(BindingIdent {
            id: build_ident(id),
            type_ann,
        }),
        ast::EFnParamPat::Rest(ast::EFnParamRestPat { arg, .. }) => Pat::Rest(RestPat {
            span: DUMMY_SP,
            dot3_token: DUMMY_SP,
            arg: Box::from(build_param_pat_rec(arg.as_ref(), None)),
            type_ann,
        }),
        ast::EFnParamPat::Object(ast::EFnParamObjectPat { props, .. }) => {
            let props: Vec<ObjectPatProp> = props
                .iter()
                .map(|prop| match prop {
                    ast::EFnParamObjectPatProp::KeyValue(kv) => {
                        ObjectPatProp::KeyValue(KeyValuePatProp {
                            key: PropName::Ident(build_ident(&kv.key)),
                            value: Box::from(build_param_pat_rec(kv.value.as_ref(), None)),
                        })
                    }
                    ast::EFnParamObjectPatProp::Assign(assign) => {
                        ObjectPatProp::Assign(AssignPatProp {
                            span: DUMMY_SP,
                            key: build_ident(&assign.key),
                            // TODO: handle default values
                            value: None,
                        })
                    }
                    ast::EFnParamObjectPatProp::Rest(rest) => ObjectPatProp::Rest(RestPat {
                        span: DUMMY_SP,
                        dot3_token: DUMMY_SP,
                        arg: Box::from(build_param_pat_rec(rest.arg.as_ref(), None)),
                        type_ann: None,
                    }),
                })
                .collect();
            Pat::Object(ObjectPat {
                span: DUMMY_SP,
                props,
                optional: false,
                type_ann,
            })
        }
        ast::EFnParamPat::Array(array) => {
            let elems = array
                .elems
                .iter()
                .map(|elem| elem.as_ref().map(|elem| build_param_pat_rec(elem, None)))
                .collect();
            Pat::Array(ArrayPat {
                span: DUMMY_SP,
                elems,
                optional: false,
                type_ann,
            })
        }
    }
}

pub fn build_ts_pattern(pat: &TPat) -> Pat {
    match pat {
        TPat::Ident(bi) => Pat::Ident(BindingIdent {
            id: Ident {
                span: DUMMY_SP,
                sym: JsWord::from(bi.name.to_owned()),
                optional: false,
            },
            type_ann: None,
        }),
        _ => todo!(),
    }
}

fn tpat_to_pat(pat: &TPat, type_ann: Option<TsTypeAnn>) -> Pat {
    match pat {
        TPat::Ident(bi) => Pat::Ident(BindingIdent {
            id: Ident {
                span: DUMMY_SP,
                sym: JsWord::from(bi.name.to_owned()),
                optional: false,
            },
            type_ann,
        }),
        TPat::Rest(rest) => Pat::Rest(RestPat {
            span: DUMMY_SP,
            dot3_token: DUMMY_SP,
            arg: Box::from(tpat_to_pat(rest.arg.as_ref(), None)),
            type_ann,
        }),
        TPat::Array(array) => Pat::Array(ArrayPat {
            span: DUMMY_SP,
            elems: array
                .elems
                .iter()
                .map(|elem| elem.as_ref().map(|elem| tpat_to_pat(elem, None)))
                .collect(),
            optional: false,
            type_ann,
        }),
        TPat::Object(obj) => {
            let props: Vec<ObjectPatProp> = obj
                .props
                .iter()
                .map(|prop| {
                    match prop {
                        types::TObjectPatProp::KeyValue(kv) => {
                            ObjectPatProp::KeyValue(KeyValuePatProp {
                                key: PropName::Ident(Ident {
                                    span: DUMMY_SP,
                                    sym: JsWord::from(kv.key.clone()),
                                    optional: false,
                                }),
                                value: Box::from(tpat_to_pat(&kv.value, None)),
                            })
                        }
                        types::TObjectPatProp::Assign(assign) => {
                            ObjectPatProp::Assign(AssignPatProp {
                                span: DUMMY_SP,
                                key: Ident {
                                    span: DUMMY_SP,
                                    sym: JsWord::from(assign.key.clone()),
                                    optional: false,
                                },
                                // TODO: handle default values
                                value: None,
                            })
                        }
                        types::TObjectPatProp::Rest(rest) => ObjectPatProp::Rest(RestPat {
                            span: DUMMY_SP,
                            dot3_token: DUMMY_SP,
                            arg: Box::from(tpat_to_pat(rest.arg.as_ref(), None)),
                            type_ann: None,
                        }),
                    }
                })
                .collect();
            Pat::Object(ObjectPat {
                span: DUMMY_SP,
                props,
                optional: false,
                type_ann,
            })
        }
    }
}

pub fn build_ts_fn_type_with_params(
    params: &[TFnParam],
    ret: &Type,
    type_params: Option<TsTypeParamDecl>,
) -> TsType {
    let params: Vec<TsFnParam> = params
        .iter()
        .map(|param| {
            let type_ann = Some(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(&param.t, None)),
            });

            let pat = tpat_to_pat(&param.pat, type_ann);

            let result: TsFnParam = match pat {
                Pat::Ident(bi) => {
                    let id = Ident {
                        optional: param.optional,
                        ..bi.id
                    };
                    TsFnParam::Ident(BindingIdent { id, ..bi })
                }
                Pat::Array(array) => TsFnParam::Array(array),
                Pat::Rest(rest) => TsFnParam::Rest(rest),
                Pat::Object(obj) => TsFnParam::Object(obj),
                Pat::Assign(_) => todo!(),
                Pat::Invalid(_) => todo!(),
                Pat::Expr(_) => todo!(),
            };

            result
        })
        .collect();

    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
        span: DUMMY_SP,
        params,
        type_params,
        type_ann: TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::from(build_type(ret, None)),
        },
    }))
}

pub fn build_ts_fn_type_with_args(
    args: &[Type],
    ret: &Type,
    type_params: Option<TsTypeParamDecl>,
) -> TsType {
    let args: Vec<TsFnParam> = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            let type_ann = Some(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(arg, None)),
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
            type_ann: Box::from(build_type(ret, None)),
        },
    }))
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
/// `expr` should be the original expression that `t` was inferred
/// from if it exists.
pub fn build_type(t: &Type, type_params: Option<TsTypeParamDecl>) -> TsType {
    match &t {
        Type::Var(id) => {
            let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                .chars()
                .collect();
            let id = chars.get(id.to_owned() as usize).unwrap();

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
        Type::Prim(prim) => {
            let kind = match prim {
                types::TPrim::Num => TsKeywordTypeKind::TsNumberKeyword,
                types::TPrim::Bool => TsKeywordTypeKind::TsBooleanKeyword,
                types::TPrim::Str => TsKeywordTypeKind::TsStringKeyword,
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        Type::Keyword(keyword) => {
            let kind = match keyword {
                types::TKeyword::Null => TsKeywordTypeKind::TsNullKeyword,
                types::TKeyword::Symbol => TsKeywordTypeKind::TsSymbolKeyword,
                types::TKeyword::Undefined => TsKeywordTypeKind::TsUndefinedKeyword,
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        Type::Lit(lit) => {
            let lit = match lit {
                types::TLit::Num(n) => TsLit::Number(Number {
                    span: DUMMY_SP,
                    value: n.parse().unwrap(),
                    raw: Some(Atom::new(n.to_owned())),
                }),
                types::TLit::Bool(b) => TsLit::Bool(Bool {
                    span: DUMMY_SP,
                    value: b.to_owned(),
                }),
                types::TLit::Str(s) => TsLit::Str(Str {
                    span: DUMMY_SP,
                    value: JsWord::from(s.clone()),
                    raw: None,
                }),
            };

            TsType::TsLitType(TsLitType {
                span: DUMMY_SP,
                lit,
            })
        }
        Type::App(types::TApp { args, ret, .. }) => {
            // This can happen when a function type is inferred by usage
            build_ts_fn_type_with_args(args, ret, type_params)
        }
        Type::Lam(types::TLam { params, ret, .. }) => {
            build_ts_fn_type_with_params(params, ret, type_params)
        }
        Type::Union(types) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, None)))
                    .collect(),
            }))
        }
        Type::Intersection(types) => TsType::TsUnionOrIntersectionType(
            TsUnionOrIntersectionType::TsIntersectionType(TsIntersectionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, None)))
                    .collect(),
            }),
        ),
        Type::Object(obj) => {
            let members: Vec<TsTypeElement> = obj
                .elems
                .iter()
                .map(|elem| {
                    match elem {
                        types::TObjElem::Call(_) => todo!(),
                        types::TObjElem::Constructor(_) => todo!(),
                        types::TObjElem::Index(_) => todo!(),
                        types::TObjElem::Prop(prop) => {
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
                                    // TODO: add build_type_from_scheme() helper that handles type params
                                    type_ann: Box::from(build_type(&prop.scheme.t, None)),
                                }),
                                type_params: None,
                            })
                        }
                    }
                })
                .collect();

            TsType::TsTypeLit(TsTypeLit {
                span: DUMMY_SP,
                members,
            })
        }
        Type::Alias(types::TAlias {
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
                    .map(|t| Box::from(build_type(t, None)))
                    .collect(),
            }),
        }),
        Type::Tuple(types) => TsType::TsTupleType(TsTupleType {
            span: DUMMY_SP,
            elem_types: types
                .iter()
                .map(|t| TsTupleElement {
                    span: DUMMY_SP,
                    label: None,
                    ty: build_type(t, None),
                })
                .collect(),
        }),
        Type::Array(t) => TsType::TsArrayType(TsArrayType {
            span: DUMMY_SP,
            elem_type: Box::from(build_type(t, None)),
        }),
        Type::Rest(_) => todo!(),
        Type::This => TsType::TsThisType(TsThisType { span: DUMMY_SP }),
    }
}

fn sort_types(types: &[Type]) -> Vec<Type> {
    let mut sorted_types = types.to_owned();
    sorted_types.sort_by(|a, b| {
        let a = format!("{a}");
        let b = format!("{b}");
        a.cmp(&b)
    });
    sorted_types
}
