use generational_arena::Index;
use std::collections::BTreeSet;
use std::rc::Rc;
use swc_atoms::*;
use swc_common::{SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::*;

use escalier_ast::visitor::*;
use escalier_ast::{self as values};
use escalier_hm::checker::Checker;
use escalier_hm::context::Context;
use escalier_hm::type_error::TypeError;
use escalier_hm::types;

pub fn codegen_d_ts(
    program: &values::Script,
    ctx: &Context,
    checker: &Checker,
) -> core::result::Result<String, TypeError> {
    Ok(print_d_ts(&build_d_ts(program, ctx, checker)?))
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
    type_params: Option<&Vec<types::TypeParam>>,
    ctx: &Context,
    checker: &Checker,
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
                        .map(|constraint| Box::from(build_type(constraint, ctx, checker)));
                    TsTypeParam {
                        span: DUMMY_SP,
                        name: build_ident(&type_param.name),
                        is_in: false,
                        is_out: false,
                        is_const: false, // TODO: find ways to leverage this
                        constraint,
                        default: None, // TODO
                    }
                })
                .collect(),
        })
    })
}

fn build_d_ts(
    program: &values::Script,
    ctx: &Context,
    checker: &Checker,
) -> core::result::Result<Program, TypeError> {
    // TODO: Create a common `Export` type
    let mut type_exports: BTreeSet<String> = BTreeSet::new();
    let mut value_exports: BTreeSet<String> = BTreeSet::new();

    for stmt in &program.stmts {
        match &stmt.kind {
            // values::StmtKind::ClassDecl(class_decl) => {
            //     let name = class_decl.ident.name.to_owned();
            //     value_exports.insert(name.to_owned());
            //     type_exports.insert(name.to_owned());
            //     type_exports.insert(format!("{name}Constructor"));

            //     // NOTE: The Readonly version of the interface type is generated
            //     // when we process `type_exports`.
            // }
            values::StmtKind::Decl(decl) => match &decl.kind {
                values::DeclKind::TypeDecl(values::TypeDecl { name, .. }) => {
                    type_exports.insert(name.to_owned());
                }
                values::DeclKind::VarDecl(values::VarDecl { pattern, .. }) => {
                    let bindings = get_bindings(pattern);
                    for name in bindings {
                        value_exports.insert(name);
                    }
                }
            },
            values::StmtKind::Expr(_) => (),   // nothing is exported
            values::StmtKind::For(_) => (),    // nothing is exported
            values::StmtKind::Return(_) => (), // nothing is exported
        }
    }

    let mut body: Vec<ModuleItem> = vec![];

    for name in type_exports {
        let scheme = ctx.get_scheme(&name)?;

        let type_params =
            build_type_params_from_type_params(scheme.type_params.as_ref(), ctx, checker);

        if let types::TypeKind::Object(obj) = &checker.arena[scheme.t].kind {
            let mutable_decl =
                ModuleItem::Stmt(Stmt::Decl(Decl::TsTypeAlias(Box::from(TsTypeAliasDecl {
                    span: DUMMY_SP,
                    declare: true,
                    id: build_ident(&name),
                    type_params: type_params.clone(),
                    type_ann: Box::from(build_obj_type(obj, ctx, checker)),
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
                            type_ann: Box::from(build_obj_type(&obj, ctx, checker)),
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
                    type_ann: Box::from(build_type(&scheme.t, ctx, checker)),
                }))));

            body.push(decl);
        }
    }

    for name in value_exports {
        let binding = ctx.get_binding(&name)?;

        let pat = Pat::Ident(BindingIdent {
            id: build_ident(&name),
            type_ann: Some(Box::from(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::from(build_type(&binding.index, ctx, checker)),
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

pub fn build_ts_pattern(pat: &types::TPat) -> Pat {
    match pat {
        types::TPat::Ident(bi) => Pat::Ident(BindingIdent {
            id: build_ident(&bi.name),
            type_ann: None,
        }),
        _ => todo!(),
    }
}

fn tpat_to_pat(pat: &types::TPat, type_ann: Option<Box<TsTypeAnn>>) -> Pat {
    match pat {
        types::TPat::Ident(bi) => Pat::Ident(BindingIdent {
            id: build_ident(&bi.name),
            type_ann,
        }),
        types::TPat::Rest(rest) => Pat::Rest(RestPat {
            span: DUMMY_SP,
            dot3_token: DUMMY_SP,
            arg: Box::from(tpat_to_pat(rest.arg.as_ref(), None)),
            type_ann,
        }),
        types::TPat::Tuple(tuple) => Pat::Array(ArrayPat {
            span: DUMMY_SP,
            elems: tuple
                .elems
                .iter()
                .map(|elem| elem.as_ref().map(|elem| tpat_to_pat(elem, None)))
                .collect(),
            optional: false,
            type_ann,
        }),
        types::TPat::Object(obj) => {
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
        types::TPat::Lit(_) => todo!(),
        types::TPat::Is(_) => todo!(),
        types::TPat::Wildcard => todo!(),
    }
}

pub fn pat_to_fn_param(param: &types::FuncParam, pat: Pat) -> TsFnParam {
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
    params: &[types::FuncParam],
    ret: &Index,
    type_params: Option<Box<TsTypeParamDecl>>,
    ctx: &Context,
    checker: &Checker,
) -> TsType {
    let params: Vec<TsFnParam> = params
        .iter()
        .map(|param| {
            let type_ann = Some(Box::from(build_type_ann(&param.t, ctx, checker)));
            let pat = tpat_to_pat(&param.pattern, type_ann);
            pat_to_fn_param(param, pat)
        })
        .collect();

    TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
        span: DUMMY_SP,
        params,
        type_params,
        type_ann: Box::from(build_type_ann(ret, ctx, checker)),
    }))
}

/// Converts an internal Type to a TsType for eventual export to .d.ts.
///
/// `expr` should be the original expression that `t` was inferred
/// from if it exists.
pub fn build_type(
    t: &Index,
    // type_params: Option<&TsTypeParamDecl>,
    ctx: &Context,
    checker: &Checker,
) -> TsType {
    let t = &checker.arena[*t];
    let mutable = false;
    // let mutable = t.mutable;
    match &t.kind {
        types::TypeKind::TypeVar(types::TypeVar {
            id,
            constraint: _,
            instance,
        }) => {
            if let Some(instance) = instance {
                return build_type(instance, ctx, checker);
            }

            // TODO: handle constraints on type variables
            // This will likely be easier if we stop using type variables for
            // type parameters.
            let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                .chars()
                .collect();
            let id = chars.get(*id).unwrap();

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
        types::TypeKind::Keyword(keyword) => {
            let kind = match keyword {
                types::Keyword::Never => TsKeywordTypeKind::TsNeverKeyword,
                types::Keyword::Object => TsKeywordTypeKind::TsObjectKeyword,
                types::Keyword::Unknown => TsKeywordTypeKind::TsUnknownKeyword,
                // TODO:
                // types::Keyword::Object => TsKeywordTypeKind::TsObjectKeyword,
                // types::Keyword::Self_ => return TsType::TsThisType(TsThisType { span: DUMMY_SP }),
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        types::TypeKind::Primitive(primitive) => {
            let kind = match primitive {
                types::Primitive::Number => TsKeywordTypeKind::TsNumberKeyword,
                types::Primitive::Boolean => TsKeywordTypeKind::TsBooleanKeyword,
                types::Primitive::String => TsKeywordTypeKind::TsStringKeyword,
                types::Primitive::Symbol => TsKeywordTypeKind::TsSymbolKeyword,
            };

            TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind,
            })
        }
        types::TypeKind::Literal(lit) => {
            let lit = match lit {
                values::Literal::Number(n) => TsLit::Number(Number {
                    span: DUMMY_SP,
                    value: n.parse().unwrap(),
                    raw: Some(Atom::new(n.clone())),
                }),
                values::Literal::Boolean(b) => TsLit::Bool(Bool {
                    span: DUMMY_SP,
                    value: b.to_owned(),
                }),
                values::Literal::String(s) => TsLit::Str(Str {
                    span: DUMMY_SP,
                    value: JsWord::from(s.clone()),
                    raw: None,
                }),
                values::Literal::Null => {
                    return TsType::TsKeywordType(TsKeywordType {
                        span: DUMMY_SP,
                        kind: TsKeywordTypeKind::TsNullKeyword,
                    })
                }
                values::Literal::Undefined => {
                    return TsType::TsKeywordType(TsKeywordType {
                        span: DUMMY_SP,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    })
                }
            };

            TsType::TsLitType(TsLitType {
                span: DUMMY_SP,
                lit,
            })
        }
        types::TypeKind::Function(types::Function {
            params,
            ret,
            type_params,
            throws: _,
        }) => {
            let type_params =
                build_type_params_from_type_params(type_params.as_ref(), ctx, checker);
            build_ts_fn_type_with_params(params, ret, type_params, ctx, checker)
        }
        types::TypeKind::Union(types::Union { types }) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: sort_types(types)
                    .iter()
                    .map(|t| Box::from(build_type(t, ctx, checker)))
                    .collect(),
            }))
        }
        types::TypeKind::Intersection(types::Intersection { types }) => {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
                TsIntersectionType {
                    span: DUMMY_SP,
                    types: sort_types(types)
                        .iter()
                        .map(|t| Box::from(build_type(t, ctx, checker)))
                        .collect(),
                },
            ))
        }
        types::TypeKind::Object(obj) => build_obj_type(obj, ctx, checker),
        types::TypeKind::TypeRef(types::TypeRef {
            name,
            types: type_args,
            ..
        }) => {
            let mut sym = JsWord::from(name.to_owned());
            let use_readonly_utility = false;

            if !mutable && !name.ends_with("Constructor") {
                if let Ok(scheme) = ctx.get_scheme(name) {
                    if let types::TypeKind::Object(obj) = &checker.arena[scheme.t].kind {
                        if immutable_obj_type(obj).is_some() {
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
            let type_args =
                if type_args.is_empty() || name == "RegExpMatchArray" || name == "RegExp" {
                    None
                } else {
                    // swc's AST calls these type params when really they're type args
                    Some(Box::from(TsTypeParamInstantiation {
                        span: DUMMY_SP,
                        params: type_args
                            .iter()
                            .map(|t| Box::from(build_type(t, ctx, checker)))
                            .collect(),
                    }))
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
        types::TypeKind::Tuple(types::Tuple { types }) => {
            let type_ann = TsType::TsTupleType(TsTupleType {
                span: DUMMY_SP,
                elem_types: types
                    .iter()
                    .map(|t| TsTupleElement {
                        span: DUMMY_SP,
                        label: None,
                        ty: Box::from(build_type(t, ctx, checker)),
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
        types::TypeKind::Array(types::Array { t }) => {
            let type_ann = TsType::TsArrayType(TsArrayType {
                span: DUMMY_SP,
                elem_type: Box::from(build_type(t, ctx, checker)),
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
        types::TypeKind::Rest(_) => todo!(),
        // types::TypeKind::This => TsType::TsThisType(TsThisType { span: DUMMY_SP }),
        types::TypeKind::KeyOf(types::KeyOf { t }) => TsType::TsTypeOperator(TsTypeOperator {
            span: DUMMY_SP,
            op: TsTypeOperatorOp::KeyOf,
            type_ann: Box::from(build_type(t, ctx, checker)),
        }),
        types::TypeKind::IndexedAccess(types::IndexedAccess { obj: object, index }) => {
            TsType::TsIndexedAccessType(TsIndexedAccessType {
                span: DUMMY_SP,
                readonly: false,
                obj_type: Box::from(build_type(object, ctx, checker)),
                index_type: Box::from(build_type(index, ctx, checker)),
            })
        }
        types::TypeKind::Conditional(types::Conditional {
            check: check_type,
            extends: extends_type,
            true_type,
            false_type,
        }) => TsType::TsConditionalType(TsConditionalType {
            span: DUMMY_SP,
            check_type: Box::from(build_type(check_type, ctx, checker)),
            extends_type: Box::from(build_type(extends_type, ctx, checker)),
            true_type: Box::from(build_type(true_type, ctx, checker)),
            false_type: Box::from(build_type(false_type, ctx, checker)),
        }),
        types::TypeKind::Infer(types::Infer { name }) => TsType::TsInferType(TsInferType {
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
                is_const: false, // TODO: find ways to leverage this
                constraint: None,
                default: None,
            },
        }),
        types::TypeKind::Wildcard => TsType::TsKeywordType(TsKeywordType {
            span: DUMMY_SP,
            kind: TsKeywordTypeKind::TsAnyKeyword,
        }),
        types::TypeKind::Binary(_) => {
            // Depending on the operator, we'll need to convert this to either
            // a `number` or `boolean` type.
            todo!()
        }
    }
}

// TODO: generate separate types for immutable and mutable object types
fn build_obj_type(obj: &types::Object, ctx: &Context, checker: &Checker) -> TsType {
    let mut members: Vec<TsTypeElement> = vec![];
    let mut mapped_types: Vec<TsType> = vec![];

    for elem in &obj.elems {
        match elem {
            types::TObjElem::Call(_) => todo!(),
            types::TObjElem::Constructor(types::Function {
                params,
                ret,
                type_params,
                throws: _, // TODO
            }) => {
                let type_params =
                    build_type_params_from_type_params(type_params.as_ref(), ctx, checker);
                let params: Vec<TsFnParam> = params
                    .iter()
                    .map(|param| {
                        let type_ann = Some(Box::from(build_type_ann(&param.t, ctx, checker)));
                        let pat = tpat_to_pat(&param.pattern, type_ann);
                        pat_to_fn_param(param, pat)
                    })
                    .collect();

                let type_elem = TsTypeElement::TsConstructSignatureDecl(TsConstructSignatureDecl {
                    span: DUMMY_SP,
                    params,
                    type_ann: Some(Box::from(build_type_ann(ret, ctx, checker))),
                    type_params,
                });

                members.push(type_elem);
            }
            types::TObjElem::Prop(prop) => {
                let key = match &prop.name {
                    types::TPropKey::StringKey(key) => key.to_owned(),
                    types::TPropKey::NumberKey(key) => key.to_owned(),
                };

                match prop.modifier {
                    // TODO: have separate node types so that we don't have to
                    // check whether props with modifiers are functions or not
                    Some(_) => todo!(),
                    None => {
                        let type_elem = TsTypeElement::TsPropertySignature(TsPropertySignature {
                            span: DUMMY_SP,
                            readonly: prop.readonly,
                            key: Box::from(Expr::from(build_ident(&key))),
                            computed: false,
                            optional: prop.optional,
                            init: None,
                            params: vec![],
                            type_ann: Some(Box::from(build_type_ann(&prop.t, ctx, checker))),
                            type_params: None,
                        });
                        members.push(type_elem);
                    }
                };
            }
            types::TObjElem::Mapped(types::MappedType {
                key,
                value,
                target, // TODO: make this an Ident
                source,
                optional: _, // TODO
                // TODO:
                check: _,
                extends: _,
            }) => {
                let mapped = TsType::TsMappedType(TsMappedType {
                    span: DUMMY_SP,
                    readonly: None, // TODO
                    optional: None, // TODO
                    name_type: Some(Box::new(build_type(key, ctx, checker))),
                    type_ann: Some(Box::new(build_type(value, ctx, checker))),
                    type_param: TsTypeParam {
                        span: DUMMY_SP,
                        name: Ident {
                            span: DUMMY_SP,
                            sym: JsWord::from(target.to_owned()),
                            optional: false,
                        },
                        is_in: true,
                        is_out: false,
                        is_const: false,
                        constraint: Some(Box::new(build_type(source, ctx, checker))),
                        default: None, // TODO
                    },
                });

                mapped_types.push(mapped);
            }
        }
    }

    if mapped_types.is_empty() {
        TsType::TsTypeLit(TsTypeLit {
            span: DUMMY_SP,
            members,
        })
    } else if members.is_empty() {
        if mapped_types.len() == 1 {
            mapped_types.pop().unwrap()
        } else {
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                span: DUMMY_SP,
                types: mapped_types
                    .iter()
                    .map(|t| Box::new(t.to_owned()))
                    .collect(),
            }))
        }
    } else {
        let mut types = vec![Box::new(TsType::TsTypeLit(TsTypeLit {
            span: DUMMY_SP,
            members,
        }))];

        for mapped_type in mapped_types {
            types.push(Box::new(mapped_type));
        }

        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
            TsIntersectionType {
                span: DUMMY_SP,
                types,
            },
        ))
    }
}

fn build_type_ann(t: &Index, ctx: &Context, checker: &Checker) -> TsTypeAnn {
    TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::from(build_type(t, ctx, checker)),
    }
}

// TODO: implement this for real
fn sort_types(types: &[Index]) -> Vec<Index> {
    types.to_owned()
    // let mut sorted_types = types.to_owned();
    // sorted_types.sort_by_key(|a| a.to_string());
    // sorted_types
}

pub fn immutable_obj_type(obj: &types::Object) -> Option<types::Object> {
    let mut changed = false;
    let elems: Vec<types::TObjElem> = obj
        .elems
        .iter()
        .map(|elem| match elem {
            types::TObjElem::Call(_) => elem.to_owned(),
            types::TObjElem::Constructor(_) => elem.to_owned(),
            types::TObjElem::Mapped(_) => elem.to_owned(),
            // TODO: Remove mutating methods.
            types::TObjElem::Prop(prop) => {
                if !prop.readonly {
                    changed = true;
                }
                types::TObjElem::Prop(types::TProp {
                    readonly: true,
                    ..prop.to_owned()
                })
            }
        })
        .collect();

    if changed {
        Some(types::Object {
            elems,
            // is_interface: obj.is_interface,
        })
    } else {
        None
    }
}

struct BindingsVisitor {
    pub bindings: Vec<String>,
}

impl Visitor for BindingsVisitor {
    fn visit_pattern(&mut self, pattern: &values::Pattern) {
        if let values::PatternKind::Ident(ident) = &pattern.kind {
            self.bindings.push(ident.name.to_owned())
        }
        values::walk_pattern(self, pattern)
    }
}

fn get_bindings(pattern: &values::Pattern) -> Vec<String> {
    let mut visitor = BindingsVisitor { bindings: vec![] };
    visitor.visit_pattern(pattern);
    visitor.bindings
}
