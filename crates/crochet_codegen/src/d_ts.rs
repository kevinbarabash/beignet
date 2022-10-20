use itertools::Itertools;
use std::rc::Rc;

use swc_atoms::*;
use swc_common::{SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;

use crochet_ast::types::{
    TFnParam, TGeneric, TIndex, TObjElem, TObject, TPat, TProp, TVar, Type, TypeKind,
};
use crochet_ast::{types, values};
use crochet_infer::{get_type_params, Context};

pub fn codegen_d_ts(program: &values::Program, ctx: &Context) -> String {
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

fn build_d_ts(_program: &values::Program, ctx: &Context) -> Program {
    let current_scope = ctx.scopes.last().unwrap();

    let mut body: Vec<ModuleItem> = vec![];

    for (name, t) in current_scope.types.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
        let id = Ident {
            span: DUMMY_SP,
            sym: JsWord::from(name.to_owned()),
            optional: false,
        };
        let decl = ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(Box::from(TsTypeAliasDecl {
            span: DUMMY_SP,
            declare: true,
            id,
            type_params: build_type_params(t),
            type_ann: Box::from(build_type(t, None)),
        }))));

        body.push(decl);
    }

    for (name, t) in current_scope.values.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
        let type_params = build_type_params(t);
        let id = Ident {
            span: DUMMY_SP,
            sym: JsWord::from(name.to_owned()),
            optional: false,
        };
        let pat = Pat::Ident(BindingIdent {
            id,
            type_ann: Some(Box::from(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(t, type_params)),
            })),
        });

        let decl = ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::Var(Box::from(VarDecl {
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: true,
                decls: vec![VarDeclarator {
                    span: DUMMY_SP,
                    name: pat,
                    init: None,
                    definite: false,
                }],
            })),
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
pub fn build_ident(id: &values::Ident) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: JsWord::from(id.name.to_owned()),
        optional: false,
    }
}

fn build_param(param: &TFnParam) -> TsFnParam {
    let type_ann = TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::from(build_type(&param.t, None)),
    };
    match &param.pat {
        TPat::Ident(bi) => {
            let id = Ident {
                span: DUMMY_SP,
                optional: param.optional,
                sym: JsWord::from(bi.name.to_owned()),
            };
            TsFnParam::Ident(BindingIdent {
                id,
                type_ann: Some(Box::from(type_ann)),
            })
        }
        TPat::Array(_) => todo!(),
        TPat::Rest(_) => todo!(),
        TPat::Object(_) => todo!(),
    }
}

pub fn _build_param(r#type: &Type, e_param: &values::EFnParam) -> TsFnParam {
    let type_ann = Some(Box::from(TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::from(build_type(r#type, None)),
    }));

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

pub fn build_param_pat_rec(pattern: &values::Pattern, type_ann: Option<Box<TsTypeAnn>>) -> Pat {
    match &pattern.kind {
        values::PatternKind::Ident(values::BindingIdent { id, .. }) => Pat::Ident(BindingIdent {
            id: build_ident(id),
            type_ann,
        }),
        values::PatternKind::Rest(values::RestPat { arg, .. }) => Pat::Rest(RestPat {
            span: DUMMY_SP,
            dot3_token: DUMMY_SP,
            arg: Box::from(build_param_pat_rec(arg.as_ref(), None)),
            type_ann,
        }),
        values::PatternKind::Object(values::ObjectPat { props, .. }) => {
            let props: Vec<ObjectPatProp> = props
                .iter()
                .map(|prop| match prop {
                    values::ObjectPatProp::KeyValue(kv) => {
                        ObjectPatProp::KeyValue(KeyValuePatProp {
                            key: PropName::Ident(build_ident(&kv.key)),
                            value: Box::from(build_param_pat_rec(kv.value.as_ref(), None)),
                        })
                    }
                    values::ObjectPatProp::Assign(assign) => {
                        ObjectPatProp::Assign(AssignPatProp {
                            span: DUMMY_SP,
                            key: build_ident(&assign.key),
                            // TODO: handle default values
                            value: None,
                        })
                    }
                    values::ObjectPatProp::Rest(rest) => ObjectPatProp::Rest(RestPat {
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
        values::PatternKind::Array(array) => {
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
        values::PatternKind::Lit(_) => panic!("Literal patterns are not allowed in params"),
        values::PatternKind::Is(_) => panic!("'is' patterns are not allowed in params"),
        values::PatternKind::Wildcard(_) => {
            panic!("Wildcard patterns are not allowed in params")
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

fn tpat_to_pat(pat: &TPat, type_ann: Option<Box<TsTypeAnn>>) -> Pat {
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
    type_params: Option<Box<TsTypeParamDecl>>,
) -> TsType {
    let params: Vec<TsFnParam> = params
        .iter()
        .map(|param| {
            let type_ann = Some(Box::from(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(&param.t, None)),
            }));

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
        type_ann: Box::from(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::from(build_type(ret, None)),
        }),
    }))
}

pub fn build_ts_fn_type_with_args(
    args: &[Type],
    ret: &Type,
    type_params: Option<Box<TsTypeParamDecl>>,
) -> TsType {
    let args: Vec<TsFnParam> = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            let type_ann = Some(Box::from(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(arg, None)),
            }));

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
        type_ann: Box::from(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::from(build_type(ret, None)),
        }),
    }))
}

pub fn build_type_params(t: &Type) -> Option<Box<TsTypeParamDecl>> {
    let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        .chars()
        .collect();

    let type_params = get_type_params(t);

    if !type_params.is_empty() {
        Some(Box::from(TsTypeParamDecl {
            span: DUMMY_SP,
            params: type_params
                .iter()
                .map(|tv| {
                    let id = chars.get(tv.id.to_owned() as usize).unwrap();

                    TsTypeParam {
                        span: DUMMY_SP,
                        name: Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(format!("{id}")),
                            optional: false,
                        },
                        is_in: false,
                        is_out: false,
                        constraint: tv
                            .constraint
                            .as_ref()
                            .map(|t| Box::from(build_type(t, None))),
                        default: None,
                    }
                })
                .collect(),
        }))
    } else {
        None
    }
}

/// Converts an internal Type to a TsType for eventual export to .d.ts.
///
/// `expr` should be the original expression that `t` was inferred
/// from if it exists.
pub fn build_type(t: &Type, type_params: Option<Box<TsTypeParamDecl>>) -> TsType {
    match &t.kind {
        TypeKind::Generic(TGeneric { t, .. }) => {
            // TODO: combine the return value from the `build_type_params()` call
            // with the `type_params` passed into this function.
            let _ = build_type_params(t);
            build_type(t, type_params)
        }
        TypeKind::Var(TVar { id, constraint: _ }) => {
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
        TypeKind::Keyword(keyword) => {
            let kind = match keyword {
                types::TKeyword::Number => TsKeywordTypeKind::TsNumberKeyword,
                types::TKeyword::String => TsKeywordTypeKind::TsStringKeyword,
                types::TKeyword::Boolean => TsKeywordTypeKind::TsBooleanKeyword,
                types::TKeyword::Null => TsKeywordTypeKind::TsNullKeyword,
                types::TKeyword::Symbol => TsKeywordTypeKind::TsSymbolKeyword,
                types::TKeyword::Undefined => TsKeywordTypeKind::TsUndefinedKeyword,
                types::TKeyword::Never => TsKeywordTypeKind::TsNeverKeyword,
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        TypeKind::Lit(lit) => {
            let lit = match lit {
                types::TLit::Num(n) => TsLit::Number(Number {
                    span: DUMMY_SP,
                    value: n.parse().unwrap(),
                    raw: Some(Atom::new(n.clone())),
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
        TypeKind::App(types::TApp { args, ret, .. }) => {
            // This can happen when a function type is inferred by usage
            build_ts_fn_type_with_args(args, ret, type_params)
        }
        TypeKind::Lam(types::TLam { params, ret, .. }) => {
            build_ts_fn_type_with_params(params, ret, type_params)
        }
        TypeKind::Union(types) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, None)))
                    .collect(),
            }))
        }
        TypeKind::Intersection(types) => TsType::TsUnionOrIntersectionType(
            TsUnionOrIntersectionType::TsIntersectionType(TsIntersectionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, None)))
                    .collect(),
            }),
        ),
        TypeKind::Object(obj) => {
            let members: Vec<TsTypeElement> = obj
                .elems
                .iter()
                .map(|elem| match elem {
                    TObjElem::Call(_) => todo!(),
                    TObjElem::Constructor(_) => todo!(),
                    TObjElem::Index(index) => TsTypeElement::TsIndexSignature(TsIndexSignature {
                        span: DUMMY_SP,
                        readonly: !index.mutable,
                        params: vec![build_param(&index.key)],
                        type_ann: Some(Box::from(TsTypeAnn {
                            span: DUMMY_SP,
                            type_ann: Box::from(build_type(&index.t, None)),
                        })),
                        is_static: false,
                    }),
                    TObjElem::Prop(prop) => {
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
                            type_ann: Some(Box::from(TsTypeAnn {
                                span: DUMMY_SP,
                                type_ann: Box::from(build_type(&prop.t, None)),
                            })),
                            type_params: None,
                        })
                    }
                })
                .collect();

            TsType::TsTypeLit(TsTypeLit {
                span: DUMMY_SP,
                members,
            })
        }
        TypeKind::Ref(types::TRef {
            name, type_args, ..
        }) => TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::from(Ident {
                span: DUMMY_SP,
                sym: JsWord::from(name.to_owned()),
                optional: false,
            }),
            // swc's AST calls these type params when really they're type args
            type_params: type_args.clone().map(|params| {
                Box::from(TsTypeParamInstantiation {
                    span: DUMMY_SP,
                    params: params
                        .iter()
                        .map(|t| Box::from(build_type(t, None)))
                        .collect(),
                })
            }),
        }),
        TypeKind::Tuple(types) => TsType::TsTupleType(TsTupleType {
            span: DUMMY_SP,
            elem_types: types
                .iter()
                .map(|t| TsTupleElement {
                    span: DUMMY_SP,
                    label: None,
                    ty: Box::from(build_type(t, None)),
                })
                .collect(),
        }),
        TypeKind::Array(t) => TsType::TsTypeOperator(TsTypeOperator {
            span: DUMMY_SP,
            op: TsTypeOperatorOp::ReadOnly,
            type_ann: Box::from(TsType::TsArrayType(TsArrayType {
                span: DUMMY_SP,
                elem_type: Box::from(build_type(t, None)),
            })),
        }),
        TypeKind::Rest(_) => todo!(),
        TypeKind::This => TsType::TsThisType(TsThisType { span: DUMMY_SP }),
        TypeKind::KeyOf(_) => todo!(),
        TypeKind::IndexAccess(_) => todo!(),
        TypeKind::Mutable(t) => {
            if let TypeKind::Object(TObject { elems }) = &t.kind {
                let elems = elems
                    .iter()
                    .map(|elem| match elem {
                        TObjElem::Call(_) => elem.to_owned(),
                        TObjElem::Constructor(_) => elem.to_owned(),
                        TObjElem::Index(index) => TObjElem::Index(TIndex {
                            mutable: true,
                            ..index.to_owned()
                        }),
                        TObjElem::Prop(prop) => TObjElem::Prop(TProp {
                            mutable: true,
                            ..prop.to_owned()
                        }),
                    })
                    .collect();

                return build_type(
                    &Type {
                        kind: TypeKind::Object(TObject { elems }),
                    },
                    type_params,
                );
            }

            let ts_type = build_type(t, type_params);

            // By default, Crochet data structures are mapped to `readonly`
            // data structures in TypeScript.  Since we're processing a `mutable`
            // Crochet data structure here, we need to remove the `readonly`-ness
            // of the TypeScript node.
            if let TsType::TsTypeOperator(TsTypeOperator {
                op: TsTypeOperatorOp::ReadOnly,
                type_ann,
                ..
            }) = ts_type
            {
                return type_ann.as_ref().to_owned();
            }

            ts_type
        }
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
