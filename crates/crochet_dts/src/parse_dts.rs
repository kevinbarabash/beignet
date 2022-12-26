use std::sync::Arc;
use types::{
    TCallable, TConditionalType, TIndex, TIndexAccess, TIndexKey, TMappedType, TObjElem, TObject,
    TPropKey, TRef, TypeKind,
};

use swc_common::{comments::SingleThreadedComments, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

use crate::util;
use crochet_ast::types::{
    self as types, RestPat, TFnParam, TGenLam, TKeyword, TMappedTypeChangeProp, TPat, TProp, Type,
    TypeParam,
};
use crochet_ast::values::Lit;
use crochet_infer::{
    generalize, get_sub_and_type_params, normalize, Context, Env, Scheme, Subst, Substitutable,
};

#[derive(Debug, Clone)]
pub struct InterfaceCollector {
    pub ctx: Context,
    pub comments: SingleThreadedComments,
    pub namespace: Vec<String>,
}

pub fn infer_ts_type_ann(type_ann: &TsType, ctx: &Context) -> Result<Type, String> {
    match type_ann {
        TsType::TsKeywordType(keyword) => match &keyword.kind {
            TsKeywordTypeKind::TsAnyKeyword => Ok(ctx.fresh_var()),
            TsKeywordTypeKind::TsUnknownKeyword => Ok(ctx.fresh_var()),
            TsKeywordTypeKind::TsNumberKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::Number)))
            }
            TsKeywordTypeKind::TsObjectKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::Object)))
            }
            TsKeywordTypeKind::TsBooleanKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::Boolean)))
            }
            TsKeywordTypeKind::TsBigIntKeyword => Err(String::from("can't parse BigInt yet")),
            TsKeywordTypeKind::TsStringKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::String)))
            }
            TsKeywordTypeKind::TsSymbolKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::Symbol)))
            }
            // NOTE: `void` is treated the same as `undefined` ...for now.
            TsKeywordTypeKind::TsVoidKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::Undefined)))
            }
            TsKeywordTypeKind::TsUndefinedKeyword => {
                Ok(Type::from(TypeKind::Keyword(TKeyword::Undefined)))
            }
            TsKeywordTypeKind::TsNullKeyword => Ok(Type::from(TypeKind::Keyword(TKeyword::Null))),
            TsKeywordTypeKind::TsNeverKeyword => Ok(Type::from(TypeKind::Keyword(TKeyword::Never))),
            TsKeywordTypeKind::TsIntrinsicKeyword => {
                Err(String::from("can't parse Intrinsics yet"))
            }
        },
        TsType::TsThisType(_) => Ok(Type::from(TypeKind::This)),
        TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
            TsFnOrConstructorType::TsFnType(fn_type) => {
                let lam = types::TLam {
                    params: infer_fn_params(&fn_type.params, ctx)?,
                    ret: Box::from(infer_ts_type_ann(&fn_type.type_ann.type_ann, ctx)?),
                };

                match &fn_type.type_params {
                    Some(type_param_decl) => {
                        let type_params = type_param_decl
                            .params
                            .iter()
                            .map(|type_param| {
                                let constraint = match &type_param.constraint {
                                    Some(constraint) => {
                                        let t = infer_ts_type_ann(constraint, ctx)?;
                                        Some(Box::from(t))
                                    }
                                    None => None,
                                };

                                let default = match &type_param.default {
                                    Some(default) => {
                                        let t = infer_ts_type_ann(default, ctx)?;
                                        Some(Box::from(t))
                                    }
                                    None => None,
                                };

                                Ok(TypeParam {
                                    name: type_param.name.sym.to_string(),
                                    constraint,
                                    default,
                                })
                            })
                            .collect::<Result<Vec<TypeParam>, String>>()?;

                        let t = Type::from(TypeKind::GenLam(TGenLam {
                            lam: Box::from(lam),
                            type_params,
                        }));

                        Ok(t)
                    }
                    None => Ok(Type::from(TypeKind::Lam(lam))),
                }
            }
            TsFnOrConstructorType::TsConstructorType(_) => {
                // NOTE: This is only used by `bind` in NewableFunction so it's
                // okay to ignore for now.
                Err(String::from("can't parse constructor yet"))
            }
        },
        TsType::TsTypeRef(ref_type) => {
            let name = match &ref_type.type_name {
                TsEntityName::Ident(name) => name.sym.to_string(),
                TsEntityName::TsQualifiedName(q_name) => {
                    // TODO: handle qualified names properly
                    let id = &q_name.as_ref().right;
                    id.sym.to_string()
                }
            };
            match &ref_type.type_params {
                Some(type_params) => {
                    let result: Result<Vec<_>, String> = type_params
                        .params
                        .iter()
                        .map(|t| infer_ts_type_ann(t, ctx))
                        .collect();
                    Ok(Type::from(TypeKind::Ref(types::TRef {
                        name,
                        type_args: result.ok(),
                    })))
                }
                None => Ok(Type::from(TypeKind::Ref(types::TRef {
                    name,
                    type_args: None,
                }))),
            }
        }
        TsType::TsTypeQuery(_) => Err(String::from("can't parse type query yet")),
        TsType::TsTypeLit(TsTypeLit { span: _, members }) => {
            let elems: Vec<TObjElem> = members
                .iter()
                .filter_map(|elem| {
                    let prop = infer_ts_type_element(elem, ctx);

                    match prop {
                        Ok(prop) => Some(prop),
                        Err(msg) => {
                            println!("Err: {msg}");
                            None
                        }
                    }
                })
                .collect();

            let t = Type::from(TypeKind::Object(TObject { elems }));

            Ok(t)
        }
        TsType::TsArrayType(array) => {
            let elem_type = infer_ts_type_ann(&array.elem_type, ctx)?;
            Ok(Type {
                kind: TypeKind::Array(Box::from(elem_type)),
                mutable: true,
                provenance: None, // TODO: link back to the TypeScript source type annotation
            })
        }
        TsType::TsTupleType(_) => Err(String::from("can't parse tuple type yet")),
        TsType::TsOptionalType(_) => Err(String::from("can't parse optional type yet")),
        TsType::TsRestType(_) => Err(String::from("can't parse rest type yet")),
        TsType::TsUnionOrIntersectionType(union_or_intersection) => match union_or_intersection {
            TsUnionOrIntersectionType::TsUnionType(union) => {
                let types: Result<Vec<_>, String> = union
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(ts_type, ctx))
                    .collect();
                Ok(Type::from(TypeKind::Union(types?)))
            }
            TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                let types: Result<Vec<_>, String> = intersection
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(ts_type, ctx))
                    .collect();
                Ok(Type::from(TypeKind::Intersection(types?)))
            }
        },
        TsType::TsConditionalType(TsConditionalType {
            span: _,
            check_type,
            extends_type,
            true_type,
            false_type,
        }) => {
            let t = Type::from(TypeKind::ConditionalType(TConditionalType {
                check_type: Box::from(infer_ts_type_ann(check_type, ctx)?),
                extends_type: Box::from(infer_ts_type_ann(extends_type, ctx)?),
                true_type: Box::from(infer_ts_type_ann(true_type, ctx)?),
                false_type: Box::from(infer_ts_type_ann(false_type, ctx)?),
            }));
            Ok(t)
        }
        TsType::TsInferType(_) => Err(String::from("can't parse infer type yet")),
        TsType::TsParenthesizedType(_) => Err(String::from("can't parse parenthesized yet")),
        TsType::TsTypeOperator(TsTypeOperator {
            op,
            type_ann,
            span: _,
        }) => {
            // TODO: If type_ann is a Type::Mutable(_) then we have to unwrap it here
            // let type_ann = ;
            match op {
                TsTypeOperatorOp::KeyOf => match type_ann.as_ref() {
                    TsType::TsKeywordType(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        let t = Type::from(TypeKind::Union(vec![
                            Type::from(TypeKind::Keyword(TKeyword::Number)),
                            Type::from(TypeKind::Keyword(TKeyword::String)),
                            Type::from(TypeKind::Keyword(TKeyword::Symbol)),
                        ]));
                        Ok(t)
                    }
                    _ => {
                        let type_ann = infer_ts_type_ann(type_ann, ctx)?;
                        Ok(Type::from(TypeKind::KeyOf(Box::from(type_ann))))
                    }
                },
                TsTypeOperatorOp::Unique => todo!(),
                TsTypeOperatorOp::ReadOnly => {
                    let type_ann = infer_ts_type_ann(type_ann, ctx)?;
                    Ok(type_ann)
                }
            }
        }
        TsType::TsIndexedAccessType(TsIndexedAccessType {
            span: _,
            readonly: _, // What does `readonly` mean in this context?
            obj_type,
            index_type,
        }) => {
            let t = Type::from(TypeKind::IndexAccess(TIndexAccess {
                object: Box::from(infer_ts_type_ann(obj_type, ctx)?),
                index: Box::from(infer_ts_type_ann(index_type, ctx)?),
            }));
            Ok(t)
        }
        TsType::TsMappedType(TsMappedType {
            span: _,
            readonly,
            type_param,
            optional,
            type_ann,
            ..
        }) => {
            let type_ann = infer_ts_type_ann(type_ann.as_ref().unwrap(), ctx)?;
            let t = Type::from(TypeKind::MappedType(TMappedType {
                type_param: TypeParam {
                    name: type_param.name.sym.to_string(),
                    constraint: match &type_param.constraint {
                        Some(constraint) => Some(Box::from(infer_ts_type_ann(constraint, ctx)?)),
                        None => None,
                    },
                    default: match &type_param.default {
                        Some(default) => Some(Box::from(infer_ts_type_ann(default, ctx)?)),
                        None => None,
                    },
                },
                optional: match optional {
                    Some(change) => match change {
                        TruePlusMinus::True => Some(TMappedTypeChangeProp::Plus),
                        TruePlusMinus::Plus => Some(TMappedTypeChangeProp::Plus),
                        TruePlusMinus::Minus => Some(TMappedTypeChangeProp::Minus),
                    },
                    None => None,
                },
                mutable: match readonly {
                    Some(change) => match change {
                        // NOTE: We reverse plus/minus here because we converting
                        // from `readonly` to `mutable`.
                        TruePlusMinus::True => Some(TMappedTypeChangeProp::Minus),
                        TruePlusMinus::Plus => Some(TMappedTypeChangeProp::Minus),
                        TruePlusMinus::Minus => Some(TMappedTypeChangeProp::Plus),
                    },
                    None => None,
                },
                t: Box::from(type_ann),
            }));
            Ok(t)
        }
        TsType::TsLitType(lit) => match &lit.lit {
            TsLit::Number(num) => Ok(Type::from(Lit::num(format!("{}", num.value), 0..0))),
            TsLit::Str(str) => Ok(Type::from(Lit::str(str.value.to_string(), 0..0))),
            TsLit::Bool(b) => Ok(Type::from(Lit::bool(b.value, 0..0))),
            TsLit::BigInt(_) => Err(String::from("can't parse BigInt literal yet")),
            TsLit::Tpl(_) => Err(String::from("can't parse Tpl literal yet")),
        },
        TsType::TsTypePredicate(_) => Err(String::from("can't parse type predicate yet")),
        TsType::TsImportType(_) => Err(String::from("can't parse import type yet")),
    }
}

fn infer_fn_params(params: &[TsFnParam], ctx: &Context) -> Result<Vec<TFnParam>, String> {
    let params: Vec<TFnParam> = params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| match param {
            TsFnParam::Ident(ident) => {
                let type_ann = ident.type_ann.clone().unwrap();
                let param = TFnParam {
                    pat: TPat::Ident(types::BindingIdent {
                        name: ident.id.sym.to_string(),
                        mutable: false,
                    }),
                    t: infer_ts_type_ann(&type_ann.type_ann, ctx).ok()?,
                    optional: ident.optional,
                };
                Some(param)
            }
            TsFnParam::Array(_) => {
                // TODO: create a tuple pattern
                println!("skipping TsFnParam::Array(_)");
                None
            }
            TsFnParam::Rest(rest) => {
                let type_ann = rest.type_ann.clone().unwrap();
                let name = match rest.arg.as_ref() {
                    Pat::Ident(BindingIdent { id, .. }) => id.sym.to_string(),
                    _ => format!("arg{index}"),
                };
                let param = TFnParam {
                    pat: TPat::Rest(RestPat {
                        arg: Box::from(TPat::Ident(types::BindingIdent {
                            name,
                            mutable: false,
                        })),
                    }),
                    t: infer_ts_type_ann(&type_ann.type_ann, ctx).ok()?,
                    optional: false,
                };
                Some(param)
            }
            TsFnParam::Object(_) => {
                // TODO: create an object pattern
                println!("skipping TsFnParam::Object(_)");
                None
            }
        })
        .collect();

    Ok(params)
}

fn infer_method_sig(sig: &TsMethodSignature, ctx: &Context) -> Result<Type, String> {
    if sig.computed {
        panic!("unexpected computed property in TypElement")
    }

    let params = infer_fn_params(&sig.params, ctx)?;
    let ret = match &sig.type_ann {
        Some(type_ann) => infer_ts_type_ann(&type_ann.type_ann, ctx),
        None => Err(String::from("method has no return type")),
    };

    let lam = types::TLam {
        params,
        ret: Box::from(ret?),
    };

    let t = match &sig.type_params {
        Some(type_param_decl) => {
            let type_params = type_param_decl
                .params
                .iter()
                .map(|type_param| {
                    let constraint = match &type_param.constraint {
                        Some(constraint) => {
                            let t = infer_ts_type_ann(constraint, ctx)?;
                            Some(Box::from(t))
                        }
                        None => None,
                    };

                    let default = match &type_param.default {
                        Some(default) => {
                            let t = infer_ts_type_ann(default, ctx)?;
                            Some(Box::from(t))
                        }
                        None => None,
                    };

                    Ok(TypeParam {
                        name: type_param.name.sym.to_string(),
                        constraint,
                        default,
                    })
                })
                .collect::<Result<Vec<TypeParam>, String>>()?;

            Type::from(TypeKind::GenLam(TGenLam {
                lam: Box::from(lam),
                type_params,
            }))
        }
        None => Type::from(TypeKind::Lam(lam)),
    };

    Ok(t)
    // TODO: maintain param names
    // Ok(generalize(&HashMap::default(), &t))
}

fn get_key_name(key: &Expr) -> Result<String, String> {
    match key {
        Expr::Ident(Ident { sym, .. }) => Ok(sym.to_string()),
        Expr::Lit(swc_ecma_ast::Lit::Str(Str { value, .. })) => Ok(value.to_string()),
        _ => Err(format!("get_key_name: {key:#?}")),
    }
}

fn get_type_params(
    type_params: &Option<Box<TsTypeParamDecl>>,
    ctx: &Context,
) -> Result<Vec<TypeParam>, String> {
    match type_params {
        Some(type_params) => type_params
            .params
            .iter()
            .map(|type_param| {
                let constraint = match &type_param.constraint {
                    Some(constraint) => {
                        let t = infer_ts_type_ann(constraint, ctx)?;
                        Some(Box::from(t))
                    }
                    None => None,
                };

                let default = match &type_param.default {
                    Some(default) => {
                        let t = infer_ts_type_ann(default, ctx)?;
                        Some(Box::from(t))
                    }
                    None => None,
                };

                Ok(TypeParam {
                    name: type_param.name.sym.to_string(),
                    constraint,
                    default,
                })
            })
            .collect(),
        None => Ok(vec![]),
    }
}

fn infer_callable(
    params: &[TsFnParam],
    type_ann: &TsType,
    type_params: &Option<Box<TsTypeParamDecl>>,
    ctx: &Context,
) -> Result<TCallable, String> {
    let mut params = infer_fn_params(params, ctx)?;
    let mut ret = infer_ts_type_ann(type_ann, ctx)?;
    let mut type_params = get_type_params(type_params, ctx)?;

    let mut tvars = params.ftv();
    tvars.append(&mut ret.ftv());

    let (sub, mut more_type_params) = get_sub_and_type_params(&tvars);
    type_params.append(&mut more_type_params);

    params.apply(&sub);
    ret.apply(&sub);

    Ok(TCallable {
        params,
        ret: Box::from(ret),
        type_params,
    })
}

fn infer_ts_type_element(elem: &TsTypeElement, ctx: &Context) -> Result<TObjElem, String> {
    match elem {
        TsTypeElement::TsCallSignatureDecl(decl) => match &decl.type_ann {
            Some(type_ann) => Ok(TObjElem::Call(infer_callable(
                &decl.params,
                &type_ann.type_ann,
                &decl.type_params,
                ctx,
            )?)),
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsConstructSignatureDecl(decl) => match &decl.type_ann {
            Some(type_ann) => Ok(TObjElem::Constructor(infer_callable(
                &decl.params,
                &type_ann.type_ann,
                &decl.type_params,
                ctx,
            )?)),
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsPropertySignature(sig) => match &sig.type_ann {
            Some(type_ann) => {
                let t = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                let name = get_key_name(sig.key.as_ref())?;
                Ok(TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(name),
                    optional: sig.optional,
                    mutable: !sig.readonly,
                    t,
                }))
            }
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsGetterSignature(sig) => {
            let key = get_key_name(sig.key.as_ref())?;
            Err(format!("TsGetterSignature: {key}"))
        }
        TsTypeElement::TsSetterSignature(sig) => {
            let key = get_key_name(sig.key.as_ref())?;
            Err(format!("TsSetterSignature: {key}"))
        }
        TsTypeElement::TsMethodSignature(sig) => {
            let t = infer_method_sig(sig, ctx)?;
            let tvs = t.ftv();
            let name = get_key_name(sig.key.as_ref())?;

            let t = if tvs.is_empty() {
                t
            } else {
                match &t.kind {
                    TypeKind::Lam(lam) => {
                        let mut type_params: Vec<TypeParam> = vec![];
                        let mut sub: Subst = Subst::default();
                        let mut char_code: u32 = 65;
                        for tv in tvs {
                            let c = char::from_u32(char_code).unwrap();
                            let t = Type::from(TypeKind::Ref(TRef {
                                name: c.to_string(),
                                type_args: None,
                            }));
                            sub.insert(tv.id, t);
                            type_params.push(TypeParam {
                                name: c.to_string(),
                                constraint: tv.constraint,
                                default: None,
                            });
                            char_code += 1;
                        }

                        let mut t = Type::from(TypeKind::GenLam(TGenLam {
                            lam: Box::from(lam.to_owned()),
                            type_params,
                        }));
                        t.apply(&sub);

                        t
                    }
                    TypeKind::GenLam(gen_lam) => {
                        let TGenLam { type_params, lam } = gen_lam;
                        let mut type_params = type_params.clone();

                        let mut sub: Subst = Subst::default();
                        let mut char_code: u32 = 65;
                        for tv in tvs {
                            // TODO: avoid collisions with existing type params
                            let c = char::from_u32(char_code).unwrap();
                            let t = Type::from(TypeKind::Ref(TRef {
                                name: c.to_string(),
                                type_args: None,
                            }));
                            sub.insert(tv.id, t);
                            type_params.push(TypeParam {
                                name: c.to_string(),
                                constraint: tv.constraint,
                                default: None,
                            });
                            char_code += 1;
                        }

                        let mut t = Type::from(TypeKind::GenLam(TGenLam {
                            lam: lam.to_owned(),
                            type_params,
                        }));
                        t.apply(&sub);

                        t
                    }
                    _ => panic!("methods can only be a Lam or GenLam"),
                }
            };

            Ok(TObjElem::Prop(TProp {
                name: TPropKey::StringKey(name),
                optional: sig.optional,
                mutable: false, // All methods on interfaces are readonly
                t,
            }))
        }
        TsTypeElement::TsIndexSignature(sig) => match &sig.type_ann {
            Some(type_ann) => {
                let t = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                let params = infer_fn_params(&sig.params, ctx)?;
                let key = params.get(0).unwrap();

                if let TPat::Ident(crochet_ast::types::BindingIdent { name, .. }) = &key.pat {
                    Ok(TObjElem::Index(TIndex {
                        key: TIndexKey {
                            name: name.to_owned(),
                            t: Box::from(key.t.to_owned()),
                        },
                        mutable: !sig.readonly,
                        t,
                    }))
                } else {
                    Err(String::from("Invalid key in index signature"))
                }
            }
            None => Err(String::from("Index is missing type annotation")),
        },
    }
}

fn infer_type_alias_decl(decl: &TsTypeAliasDecl, ctx: &Context) -> Result<Scheme, String> {
    let t = infer_ts_type_ann(&decl.type_ann, ctx)?;

    let type_params = match &decl.type_params {
        Some(type_params) => type_params
            .params
            .iter()
            .map(|type_param| {
                let constraint = match &type_param.constraint {
                    Some(constraint) => {
                        let t = infer_ts_type_ann(constraint, ctx)?;
                        Some(Box::from(t))
                    }
                    None => None,
                };

                let default = match &type_param.default {
                    Some(default) => {
                        let t = infer_ts_type_ann(default, ctx)?;
                        Some(Box::from(t))
                    }
                    None => None,
                };

                Ok(TypeParam {
                    name: type_param.name.sym.to_string(),
                    constraint,
                    default,
                })
            })
            .collect::<Result<Vec<TypeParam>, String>>()?,
        None => vec![],
    };

    let scheme = Scheme {
        t: Box::from(t),
        type_params,
    };

    Ok(scheme)
}

fn infer_interface_decl(decl: &TsInterfaceDecl, ctx: &Context) -> Result<Scheme, String> {
    // TODO: skip properties we don't know how to deal with instead of return an error for the whole map
    let elems: Vec<TObjElem> = decl
        .body
        .body
        .iter()
        .filter_map(|elem| {
            let prop = infer_ts_type_element(elem, ctx);

            match prop {
                Ok(prop) => Some(prop),
                Err(msg) => {
                    println!("Err: {msg}");
                    None
                }
            }
        })
        .collect();

    let t = Type::from(TypeKind::Object(TObject { elems }));

    let type_params = match &decl.type_params {
        Some(type_params) => type_params
            .params
            .iter()
            .map(|type_param| {
                let constraint = match &type_param.constraint {
                    Some(constraint) => {
                        let t = infer_ts_type_ann(constraint, ctx)?;
                        Some(Box::from(t))
                    }
                    None => None,
                };

                let default = match &type_param.default {
                    Some(default) => {
                        let t = infer_ts_type_ann(default, ctx)?;
                        Some(Box::from(t))
                    }
                    None => None,
                };

                Ok(TypeParam {
                    name: type_param.name.sym.to_string(),
                    constraint,
                    default,
                })
            })
            .collect::<Result<Vec<TypeParam>, String>>()?,
        None => vec![],
    };

    let scheme = Scheme {
        t: Box::from(t),
        type_params,
    };

    Ok(scheme)
}

impl Visit for InterfaceCollector {
    fn visit_ts_type_alias_decl(&mut self, decl: &TsTypeAliasDecl) {
        let name = decl.id.sym.to_string();
        match infer_type_alias_decl(decl, &self.ctx) {
            Ok(scheme) => {
                // println!("inferring: {name} as type: {t}");
                self.ctx.insert_scheme(name, scheme)
            }
            Err(_err) => {
                // println!("couldn't infer {name}, {err:#?}")
            }
        }
    }

    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        let name = decl.id.sym.to_string();
        match infer_interface_decl(decl, &self.ctx) {
            Ok(scheme) => {
                match self.ctx.lookup_scheme(&name).ok() {
                    Some(existing_scheme) => {
                        let merged_t = util::merge_types(&existing_scheme.t, &scheme.t);
                        let merged_scheme = Scheme {
                            t: Box::from(normalize(&merged_t, &self.ctx)),
                            // TODO: check if the type_params match
                            type_params: existing_scheme.type_params,
                        };
                        // let merged_t = normalize(&merged_t, &self.ctx);
                        self.ctx.insert_scheme(name, merged_scheme);
                    }
                    None => self.ctx.insert_scheme(name, scheme),
                };
            }
            Err(_) => println!("couldn't infer {name}"),
        }
    }

    fn visit_ts_module_decl(&mut self, decl: &TsModuleDecl) {
        match &decl.id {
            TsModuleName::Ident(id) => {
                let name = id.sym.to_string();
                println!("module: {name}");
                self.namespace.push(name);
                decl.visit_children_with(self);
                self.namespace.pop();
            }
            TsModuleName::Str(_) => todo!(),
        }
    }

    fn visit_var_decl(&mut self, decl: &VarDecl) {
        if !decl.declare {
            return;
        }
        for d in &decl.decls {
            match &d.name {
                Pat::Ident(bi) => {
                    match &bi.type_ann {
                        Some(type_ann) => {
                            // TODO: capture errors and store them in self.errors
                            let t = infer_ts_type_ann(&type_ann.type_ann, &self.ctx).unwrap();
                            let t = generalize(&Env::new(), &t);
                            let name = bi.id.sym.to_string();
                            self.ctx.insert_value(name, t)
                        }
                        None => todo!(),
                    }
                }
                Pat::Array(_) => todo!(),
                Pat::Rest(_) => todo!(),
                Pat::Object(_) => todo!(),
                Pat::Assign(_) => todo!(),
                Pat::Invalid(_) => todo!(),
                Pat::Expr(_) => todo!(),
            }
        }
    }
}

impl InterfaceCollector {}

pub fn parse_dts(d_ts_source: &str) -> Result<Context, Error> {
    let cm = Arc::<SourceMap>::default();
    let fm = cm.new_source_file(FileName::Anon, d_ts_source.to_owned());

    let mut errors: Vec<Error> = vec![];
    let comments = SingleThreadedComments::default();

    let module = parse_file_as_module(
        &fm,
        Syntax::Typescript(TsConfig {
            tsx: false,
            dts: true,
            decorators: false,
            no_early_errors: false,
        }),
        EsVersion::Es2020,
        Some(&comments),
        &mut errors,
    )?;

    let mut collector = InterfaceCollector {
        ctx: Context::default(),
        comments,
        namespace: vec![],
    };

    module.visit_with(&mut collector);

    Ok(collector.ctx)
}
