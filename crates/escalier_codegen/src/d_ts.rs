use std::collections::BTreeSet;
use std::rc::Rc;

use swc_atoms::*;
use swc_common::{SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;

use escalier_ast::types::{
    TConditionalType, TFnParam, TGetter, TIndexAccess, TInferType, TMappedType, TObjElem, TObject,
    TPat, TPropKey, TSetter, TVar, Type, TypeKind, TypeParam,
};
use escalier_ast::{types, values};
use escalier_infer::{immutable_obj_type, Context, Scope, TypeError};

pub fn codegen_d_ts(
    program: &values::Program,
    scope: &Scope,
) -> core::result::Result<String, Vec<TypeError>> {
    Ok(print_d_ts(&build_d_ts(program, scope)?))
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

fn build_type_params_from_type_params(
    type_params: Option<&Vec<TypeParam>>,
    scope: &Scope,
) -> Option<Box<TsTypeParamDecl>> {
    type_params.as_ref().map(|type_params| {
        Box::from(TsTypeParamDecl {
            span: DUMMY_SP,
            params: type_params
                .iter()
                .map(|type_param| {
                    let constraint = type_param
                        .constraint
                        .as_ref()
                        .map(|constraint| Box::from(build_type(constraint, None, scope)));
                    TsTypeParam {
                        span: DUMMY_SP,
                        name: build_ident(&type_param.name),
                        is_in: false,
                        is_out: false,
                        constraint,
                        default: None, // TODO
                    }
                })
                .collect(),
        })
    })
}

fn build_d_ts(
    program: &values::Program,
    scope: &Scope,
) -> core::result::Result<Program, Vec<TypeError>> {
    // TODO: Create a common `Export` type
    let mut type_exports: BTreeSet<String> = BTreeSet::new();
    let mut value_exports: BTreeSet<String> = BTreeSet::new();

    for stmt in &program.body {
        match &stmt.kind {
            values::StmtKind::ClassDecl(class_decl) => {
                let name = class_decl.ident.name.to_owned();
                value_exports.insert(name.to_owned());
                type_exports.insert(name.to_owned());
                type_exports.insert(format!("{name}Constructor"));

                // NOTE: The Readonly version of the interface type is generated
                // when we process `type_exports`.
            }
            values::StmtKind::VarDecl(var_decl) => {
                let bindings = values::pattern::get_binding(&var_decl.pattern);
                for name in bindings {
                    value_exports.insert(name);
                }
            }
            values::StmtKind::TypeDecl(type_decl) => {
                type_exports.insert(type_decl.id.name.to_owned());
            }
            values::StmtKind::ExprStmt(_) => (), // nothing is exported
            values::StmtKind::ForStmt(_) => (),  // nothing is exported
            values::StmtKind::ReturnStmt(_) => (), // nothing is exported
        }
    }

    let mut body: Vec<ModuleItem> = vec![];

    for name in type_exports {
        let scheme = scope.lookup_scheme(&name)?;

        let type_params = build_type_params_from_type_params(scheme.type_params.as_ref(), scope);

        if let TypeKind::Object(obj) = &scheme.t.kind {
            let mutable_decl =
                ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(Box::from(TsTypeAliasDecl {
                    span: DUMMY_SP,
                    declare: true,
                    id: build_ident(&name),
                    type_params: type_params.clone(),
                    type_ann: Box::from(build_obj_type(obj, scope)),
                }))));
            body.push(mutable_decl);

            if !name.ends_with("Constructor") {
                if let Some(obj) = immutable_obj_type(obj) {
                    let immutable_decl = ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(
                        Box::from(TsTypeAliasDecl {
                            span: DUMMY_SP,
                            declare: true,
                            id: build_ident(format!("Readonly{name}").as_str()),
                            type_params,
                            type_ann: Box::from(build_obj_type(&obj, scope)),
                        }),
                    )));

                    body.push(immutable_decl);
                }
            }
        } else {
            let decl =
                ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(Box::from(TsTypeAliasDecl {
                    span: DUMMY_SP,
                    declare: true,
                    id: build_ident(&name),
                    type_params,
                    type_ann: Box::from(build_type(&scheme.t, None, scope)),
                }))));

            body.push(decl);
        }
    }

    for name in value_exports {
        let binding = scope.lookup_binding(&name)?;
        let pat = Pat::Ident(BindingIdent {
            id: build_ident(&name),
            type_ann: Some(Box::from(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(&binding.t, None, scope)),
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

    Ok(Program::Module(Module {
        span: DUMMY_SP,
        body,
        shebang: None,
    }))
}

// TODO: create a trait for this and then provide multiple implementations
pub fn build_ident(name: &str) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: JsWord::from(name.to_owned()),
        optional: false,
    }
}

pub fn build_ts_pattern(pat: &TPat) -> Pat {
    match pat {
        TPat::Ident(bi) => Pat::Ident(BindingIdent {
            id: build_ident(&bi.name),
            type_ann: None,
        }),
        _ => todo!(),
    }
}

fn tpat_to_pat(pat: &TPat, type_ann: Option<Box<TsTypeAnn>>) -> Pat {
    match pat {
        TPat::Ident(bi) => Pat::Ident(BindingIdent {
            id: build_ident(&bi.name),
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
                                key: PropName::Ident(build_ident(&kv.key)),
                                value: Box::from(tpat_to_pat(&kv.value, None)),
                            })
                        }
                        types::TObjectPatProp::Assign(assign) => {
                            ObjectPatProp::Assign(AssignPatProp {
                                span: DUMMY_SP,
                                key: build_ident(&assign.key),
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

pub fn pat_to_fn_param(param: &TFnParam, pat: Pat) -> TsFnParam {
    match pat {
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
    }
}

pub fn build_ts_fn_type_with_params(
    params: &[TFnParam],
    ret: &Type,
    type_params: Option<Box<TsTypeParamDecl>>,
    scope: &Scope,
) -> TsType {
    let params: Vec<TsFnParam> = params
        .iter()
        .map(|param| {
            let type_ann = Some(Box::from(build_type_ann(&param.t, scope)));
            let pat = tpat_to_pat(&param.pat, type_ann);
            pat_to_fn_param(param, pat)
        })
        .collect();

    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
        span: DUMMY_SP,
        params,
        type_params,
        type_ann: Box::from(build_type_ann(ret, scope)),
    }))
}

pub fn build_ts_fn_type_with_args(
    args: &[Type],
    ret: &Type,
    type_params: Option<&TsTypeParamDecl>,
    scope: &Scope,
) -> TsType {
    let args: Vec<TsFnParam> = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            TsFnParam::Ident(BindingIdent {
                id: build_ident(&format!("arg{}", index)),
                type_ann: Some(Box::from(build_type_ann(arg, scope))),
            })
        })
        .collect();

    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
        span: DUMMY_SP,
        params: args,
        type_params: type_params.map(|type_params| Box::from(type_params.to_owned())),
        type_ann: Box::from(build_type_ann(ret, scope)),
    }))
}

/// Converts an internal Type to a TsType for eventual export to .d.ts.
///
/// `expr` should be the original expression that `t` was inferred
/// from if it exists.
pub fn build_type(t: &Type, type_params: Option<&TsTypeParamDecl>, scope: &Scope) -> TsType {
    let mutable = t.mutable;
    match &t.kind {
        TypeKind::Var(TVar {
            id,
            constraint: _,
            solution: _,
        }) => {
            // TODO: handle constraints on type variables
            // This will likely be easier if we stop using type variables for
            // type parameters.
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
                types::TKeyword::Object => TsKeywordTypeKind::TsObjectKeyword,
                types::TKeyword::Self_ => return TsType::TsThisType(TsThisType { span: DUMMY_SP }),
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
            build_ts_fn_type_with_args(args, ret, type_params, scope)
        }
        TypeKind::Lam(types::TLam {
            params,
            ret,
            type_params,
        }) => {
            let type_params = build_type_params_from_type_params(type_params.as_ref(), scope);
            build_ts_fn_type_with_params(params, ret, type_params, scope)
        }
        TypeKind::Union(types) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, None, scope)))
                    .collect(),
            }))
        }
        TypeKind::Intersection(types) => TsType::TsUnionOrIntersectionType(
            TsUnionOrIntersectionType::TsIntersectionType(TsIntersectionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, None, scope)))
                    .collect(),
            }),
        ),
        TypeKind::Object(obj) => build_obj_type(obj, scope),
        TypeKind::Ref(types::TRef {
            name, type_args, ..
        }) => {
            let mut sym = JsWord::from(name.to_owned());
            let use_readonly_utility = false;

            if !mutable && !name.ends_with("Constructor") {
                if let Ok(scheme) = scope.lookup_scheme(name) {
                    if let TypeKind::Object(obj) = scheme.t.kind {
                        if immutable_obj_type(&obj).is_some() {
                            if name == "RegExp"
                                || name == "RegExpExecArray"
                                || name == "RegExpMatchArray"
                            {
                                // Don't pre-pend "Readonly" on to these types
                            } else {
                                // TODO: keep track of which types are derived from
                                // .d.ts files and whether or not they have Readonly
                                // variants like ReadonlyArray, ReadonlySet, etc.
                                sym = JsWord::from(format!("Readonly{name}"));
                            }
                        }
                    }
                }
            }

            // Reverse overrides when exporting types
            let type_args = if name == "RegExpMatchArray" || name == "RegExp" {
                None
            } else {
                // swc's AST calls these type params when really they're type args
                type_args.clone().map(|params| {
                    Box::from(TsTypeParamInstantiation {
                        span: DUMMY_SP,
                        params: params
                            .iter()
                            .map(|t| Box::from(build_type(t, None, scope)))
                            .collect(),
                    })
                })
            };

            let t = TsType::TsTypeRef(TsTypeRef {
                span: DUMMY_SP,
                type_name: TsEntityName::from(Ident {
                    span: DUMMY_SP,
                    sym,
                    optional: false,
                }),
                type_params: type_args,
            });

            // TODO: Write a test that uses this code
            if use_readonly_utility {
                TsType::TsTypeRef(TsTypeRef {
                    span: DUMMY_SP,
                    type_name: TsEntityName::from(Ident {
                        span: DUMMY_SP,
                        sym: JsWord::from("Readonly".to_string()),
                        optional: false,
                    }),
                    type_params: Some(Box::from(TsTypeParamInstantiation {
                        span: DUMMY_SP,
                        params: vec![Box::from(t)],
                    })),
                })
            } else {
                t
            }
        }
        TypeKind::Tuple(types) => {
            let type_ann = TsType::TsTupleType(TsTupleType {
                span: DUMMY_SP,
                elem_types: types
                    .iter()
                    .map(|t| TsTupleElement {
                        span: DUMMY_SP,
                        label: None,
                        ty: Box::from(build_type(t, None, scope)),
                    })
                    .collect(),
            });

            if mutable {
                type_ann
            } else {
                TsType::TsTypeOperator(TsTypeOperator {
                    span: DUMMY_SP,
                    op: TsTypeOperatorOp::ReadOnly,
                    type_ann: Box::from(type_ann),
                })
            }
        }
        TypeKind::Array(t) => {
            let type_ann = TsType::TsArrayType(TsArrayType {
                span: DUMMY_SP,
                elem_type: Box::from(build_type(t, None, scope)),
            });

            if mutable {
                type_ann
            } else {
                TsType::TsTypeOperator(TsTypeOperator {
                    span: DUMMY_SP,
                    op: TsTypeOperatorOp::ReadOnly,
                    type_ann: Box::from(type_ann),
                })
            }
        }
        TypeKind::Rest(_) => todo!(),
        TypeKind::This => TsType::TsThisType(TsThisType { span: DUMMY_SP }),
        TypeKind::KeyOf(t) => TsType::TsTypeOperator(TsTypeOperator {
            span: DUMMY_SP,
            op: TsTypeOperatorOp::KeyOf,
            type_ann: Box::from(build_type(t.as_ref(), type_params, scope)),
        }),
        TypeKind::IndexAccess(TIndexAccess { object, index }) => {
            TsType::TsIndexedAccessType(TsIndexedAccessType {
                span: DUMMY_SP,
                readonly: false,
                obj_type: Box::from(build_type(object, type_params, scope)),
                index_type: Box::from(build_type(index, type_params, scope)),
            })
        }
        TypeKind::MappedType(TMappedType {
            mutable,
            type_param,
            optional,
            t,
        }) => {
            TsType::TsMappedType(TsMappedType {
                span: DUMMY_SP,
                readonly: mutable.as_ref().map(|value| match value {
                    types::TMappedTypeChangeProp::Plus => TruePlusMinus::Minus,
                    types::TMappedTypeChangeProp::Minus => TruePlusMinus::Plus,
                }),
                type_param: TsTypeParam {
                    span: DUMMY_SP,
                    name: build_ident(&type_param.name),
                    is_in: true,
                    is_out: false,
                    constraint: type_param.constraint.as_ref().map(|constraint| {
                        Box::from(build_type(
                            &constraint.as_ref().to_owned(),
                            type_params,
                            scope,
                        ))
                    }),
                    default: None, // NOTE: This is always None for mapped types
                },
                name_type: None, // TODO: update when support for constraint aliases is added
                optional: optional.as_ref().map(|value| match value {
                    types::TMappedTypeChangeProp::Plus => TruePlusMinus::Plus,
                    types::TMappedTypeChangeProp::Minus => TruePlusMinus::Minus,
                }),
                type_ann: Some(Box::from(build_type(t.as_ref(), type_params, scope))),
            })
        }
        TypeKind::ConditionalType(TConditionalType {
            check_type,
            extends_type,
            true_type,
            false_type,
        }) => TsType::TsConditionalType(TsConditionalType {
            span: DUMMY_SP,
            check_type: Box::from(build_type(check_type.as_ref(), type_params, scope)),
            extends_type: Box::from(build_type(extends_type.as_ref(), type_params, scope)),
            true_type: Box::from(build_type(true_type.as_ref(), type_params, scope)),
            false_type: Box::from(build_type(false_type.as_ref(), type_params, scope)),
        }),
        TypeKind::InferType(TInferType { name }) => TsType::TsInferType(TsInferType {
            span: DUMMY_SP,
            type_param: TsTypeParam {
                span: DUMMY_SP,
                name: Ident {
                    span: DUMMY_SP,
                    sym: JsWord::from(name.to_string()),
                    optional: false,
                },
                is_in: false,
                is_out: false,
                constraint: None,
                default: None,
            },
        }),
    }
}

fn build_obj_type(obj: &TObject, scope: &Scope) -> TsType {
    let members: Vec<TsTypeElement> = obj
        .elems
        .iter()
        .filter_map(|elem| match elem {
            TObjElem::Call(_) => todo!(),
            TObjElem::Constructor(types::TCallable {
                params,
                ret,
                type_params,
            }) => {
                let type_params = build_type_params_from_type_params(type_params.as_ref(), scope);
                let params: Vec<TsFnParam> = params
                    .iter()
                    .map(|param| {
                        let type_ann = Some(Box::from(build_type_ann(&param.t, scope)));
                        let pat = tpat_to_pat(&param.pat, type_ann);
                        pat_to_fn_param(param, pat)
                    })
                    .collect();

                Some(TsTypeElement::TsConstructSignatureDecl(
                    TsConstructSignatureDecl {
                        span: DUMMY_SP,
                        params,
                        type_ann: Some(Box::from(build_type_ann(ret, scope))),
                        type_params,
                    },
                ))
            }
            TObjElem::Method(types::TMethod {
                name,
                params,
                ret,
                type_params,
                is_mutating: _, // TODO
            }) => {
                let key = match name {
                    TPropKey::StringKey(key) => key.to_owned(),
                    TPropKey::NumberKey(key) => key.to_owned(),
                };
                // TODO: dedupe with build_ts_fn_type_with_params
                let type_params = build_type_params_from_type_params(type_params.as_ref(), scope);
                let params: Vec<TsFnParam> = params
                    .iter()
                    .map(|param| {
                        let type_ann = Some(Box::from(build_type_ann(&param.t, scope)));
                        let pat = tpat_to_pat(&param.pat, type_ann);
                        pat_to_fn_param(param, pat)
                    })
                    .collect();

                Some(TsTypeElement::TsMethodSignature(TsMethodSignature {
                    span: DUMMY_SP,
                    readonly: false, // `readonly` modifier can't appear on methods
                    key: Box::from(Ident {
                        span: DUMMY_SP,
                        sym: JsWord::from(key),
                        optional: false,
                    }),
                    computed: false,
                    optional: false,
                    params,
                    type_ann: Some(Box::from(build_type_ann(ret, scope))),
                    type_params,
                }))
            }
            TObjElem::Getter(TGetter { name, ret }) => {
                let key = match name {
                    TPropKey::StringKey(key) => key.to_owned(),
                    TPropKey::NumberKey(key) => key.to_owned(),
                };
                Some(TsTypeElement::TsGetterSignature(TsGetterSignature {
                    span: DUMMY_SP,
                    readonly: false,
                    key: Box::from(Ident {
                        span: DUMMY_SP,
                        sym: JsWord::from(key),
                        optional: false,
                    }),
                    computed: false,
                    optional: false,
                    type_ann: Some(Box::from(build_type_ann(ret, scope))),
                }))
            }
            TObjElem::Setter(TSetter { name, param }) => {
                let key = match name {
                    TPropKey::StringKey(key) => key.to_owned(),
                    TPropKey::NumberKey(key) => key.to_owned(),
                };

                let type_ann = Some(Box::from(build_type_ann(&param.t, scope)));
                let pat = tpat_to_pat(&param.pat, type_ann);
                let param = pat_to_fn_param(param, pat);

                Some(TsTypeElement::TsSetterSignature(TsSetterSignature {
                    span: DUMMY_SP,
                    readonly: false,
                    key: Box::from(Ident {
                        span: DUMMY_SP,
                        sym: JsWord::from(key),
                        optional: false,
                    }),
                    param,
                    computed: false,
                    optional: false,
                }))
            }
            TObjElem::Index(index) => Some(TsTypeElement::TsIndexSignature(TsIndexSignature {
                span: DUMMY_SP,
                readonly: !index.mutable,
                params: vec![TsFnParam::Ident(BindingIdent {
                    id: build_ident(&index.key.name),
                    type_ann: Some(Box::from(build_type_ann(&index.key.t, scope))),
                })],
                type_ann: Some(Box::from(build_type_ann(&index.t, scope))),
                is_static: false,
            })),
            TObjElem::Prop(prop) => {
                let key = match &prop.name {
                    TPropKey::StringKey(key) => key.to_owned(),
                    TPropKey::NumberKey(key) => key.to_owned(),
                };
                Some(TsTypeElement::TsPropertySignature(TsPropertySignature {
                    span: DUMMY_SP,
                    readonly: !prop.mutable,
                    key: Box::from(Expr::from(build_ident(&key))),
                    computed: false,
                    optional: prop.optional,
                    init: None,
                    params: vec![],
                    type_ann: Some(Box::from(build_type_ann(&prop.t, scope))),
                    type_params: None,
                }))
            }
        })
        .collect();

    TsType::TsTypeLit(TsTypeLit {
        span: DUMMY_SP,
        members,
    })
}

fn build_type_ann(t: &Type, scope: &Scope) -> TsTypeAnn {
    TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::from(build_type(t, None, scope)),
    }
}

fn sort_types(types: &[Type]) -> Vec<Type> {
    let mut sorted_types = types.to_owned();
    sorted_types.sort_by_key(|a| a.to_string());
    sorted_types
}
