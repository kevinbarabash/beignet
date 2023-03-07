use std::collections::HashMap;
use std::sync::Arc;

use swc_common::{comments::SingleThreadedComments, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

use escalier_ast::types::{
    self as types, RestPat, TCallable, TConditionalType, TFnParam, TIndex, TIndexAccess, TIndexKey,
    TKeyword, TLam, TMappedType, TMappedTypeChangeProp, TObjElem, TObject, TPat, TProp, TPropKey,
    TRef, Type, TypeKind, TypeParam,
};
use escalier_ast::values::{Lit, DUMMY_LOC};
use escalier_infer::{self, get_sub_and_type_params, Scheme, Subst, Substitutable};

use crate::checker::Checker;
use crate::overrides::maybe_override_string_methods;
use crate::util;

impl Checker {
    pub fn infer_ts_type_ann(&mut self, type_ann: &TsType) -> Result<Type, String> {
        match type_ann {
            TsType::TsKeywordType(keyword) => match &keyword.kind {
                TsKeywordTypeKind::TsAnyKeyword => Ok(self.fresh_var(None)),
                TsKeywordTypeKind::TsUnknownKeyword => Ok(self.fresh_var(None)),
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
                TsKeywordTypeKind::TsNullKeyword => {
                    Ok(Type::from(TypeKind::Keyword(TKeyword::Null)))
                }
                TsKeywordTypeKind::TsNeverKeyword => {
                    Ok(Type::from(TypeKind::Keyword(TKeyword::Never)))
                }
                TsKeywordTypeKind::TsIntrinsicKeyword => {
                    Err(String::from("can't parse Intrinsics yet"))
                }
            },
            TsType::TsThisType(_) => Ok(Type::from(TypeKind::This)),
            TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
                TsFnOrConstructorType::TsFnType(fn_type) => {
                    let params = self.infer_fn_params(&fn_type.params)?;
                    let ret = Box::from(self.infer_ts_type_ann(&fn_type.type_ann.type_ann)?);

                    let type_params = match &fn_type.type_params {
                        Some(type_param_decl) => {
                            let type_params = type_param_decl
                                .params
                                .iter()
                                .map(|type_param| {
                                    let constraint = match &type_param.constraint {
                                        Some(constraint) => {
                                            let t = self.infer_ts_type_ann(constraint)?;
                                            Some(Box::from(t))
                                        }
                                        None => None,
                                    };

                                    let default = match &type_param.default {
                                        Some(default) => {
                                            let t = self.infer_ts_type_ann(default)?;
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

                            if type_params.is_empty() {
                                None
                            } else {
                                Some(type_params)
                            }
                        }
                        None => None,
                    };

                    let t = Type::from(TypeKind::Lam(TLam {
                        type_params,
                        params,
                        ret,
                    }));

                    Ok(t)
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
                            .map(|t| self.infer_ts_type_ann(t))
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
                        // TODO: double check that this is correct
                        let obj_is_mutable = false;
                        let prop = self.infer_ts_type_element(elem, obj_is_mutable);

                        match prop {
                            Ok(prop) => Some(prop),
                            Err(msg) => {
                                eprintln!("Err: {msg}");
                                None
                            }
                        }
                    })
                    .collect();

                let t = Type::from(TypeKind::Object(TObject {
                    elems,
                    is_interface: false,
                }));

                Ok(t)
            }
            TsType::TsArrayType(array) => {
                let elem_type = self.infer_ts_type_ann(&array.elem_type)?;
                Ok(Type {
                    kind: TypeKind::Array(Box::from(elem_type)),
                    mutable: true,
                    provenance: None, // TODO: link back to the TypeScript source type annotation
                })
            }
            TsType::TsTupleType(_) => Err(String::from("can't parse tuple type yet")),
            TsType::TsOptionalType(_) => Err(String::from("can't parse optional type yet")),
            TsType::TsRestType(_) => Err(String::from("can't parse rest type yet")),
            TsType::TsUnionOrIntersectionType(union_or_intersection) => match union_or_intersection
            {
                TsUnionOrIntersectionType::TsUnionType(union) => {
                    let types: Result<Vec<_>, String> = union
                        .types
                        .iter()
                        .map(|ts_type| self.infer_ts_type_ann(ts_type))
                        .collect();
                    Ok(Type::from(TypeKind::Union(types?)))
                }
                TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                    let types: Result<Vec<_>, String> = intersection
                        .types
                        .iter()
                        .map(|ts_type| self.infer_ts_type_ann(ts_type))
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
                    check_type: Box::from(self.infer_ts_type_ann(check_type)?),
                    extends_type: Box::from(self.infer_ts_type_ann(extends_type)?),
                    true_type: Box::from(self.infer_ts_type_ann(true_type)?),
                    false_type: Box::from(self.infer_ts_type_ann(false_type)?),
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
                            let type_ann = self.infer_ts_type_ann(type_ann)?;
                            Ok(Type::from(TypeKind::KeyOf(Box::from(type_ann))))
                        }
                    },
                    TsTypeOperatorOp::Unique => todo!(),
                    TsTypeOperatorOp::ReadOnly => {
                        let type_ann = self.infer_ts_type_ann(type_ann)?;
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
                    object: Box::from(self.infer_ts_type_ann(obj_type)?),
                    index: Box::from(self.infer_ts_type_ann(index_type)?),
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
                let type_ann = self.infer_ts_type_ann(type_ann.as_ref().unwrap())?;
                let t = Type::from(TypeKind::MappedType(TMappedType {
                    type_param: TypeParam {
                        name: type_param.name.sym.to_string(),
                        constraint: match &type_param.constraint {
                            Some(constraint) => {
                                Some(Box::from(self.infer_ts_type_ann(constraint)?))
                            }
                            None => None,
                        },
                        default: match &type_param.default {
                            Some(default) => Some(Box::from(self.infer_ts_type_ann(default)?)),
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
                TsLit::Number(num) => Ok(Type::from(Lit::num(
                    format!("{}", num.value),
                    0..0,
                    DUMMY_LOC,
                ))),
                TsLit::Str(str) => Ok(Type::from(Lit::str(str.value.to_string(), 0..0, DUMMY_LOC))),
                TsLit::Bool(b) => Ok(Type::from(Lit::bool(b.value, 0..0, DUMMY_LOC))),
                TsLit::BigInt(_) => Err(String::from("can't parse BigInt literal yet")),
                TsLit::Tpl(_) => Err(String::from("can't parse Tpl literal yet")),
            },
            TsType::TsTypePredicate(_) => Err(String::from("can't parse type predicate yet")),
            TsType::TsImportType(_) => Err(String::from("can't parse import type yet")),
        }
    }

    fn infer_fn_params(&mut self, params: &[TsFnParam]) -> Result<Vec<TFnParam>, String> {
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
                        t: self.infer_ts_type_ann(&type_ann.type_ann).ok()?,
                        optional: ident.optional,
                    };
                    Some(param)
                }
                TsFnParam::Array(_) => {
                    // TODO: create a tuple pattern
                    eprintln!("skipping TsFnParam::Array(_)");
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
                        t: self.infer_ts_type_ann(&type_ann.type_ann).ok()?,
                        optional: false,
                    };
                    Some(param)
                }
                TsFnParam::Object(_) => {
                    // TODO: create an object pattern
                    eprintln!("skipping TsFnParam::Object(_)");
                    None
                }
            })
            .collect();

        Ok(params)
    }

    fn infer_method_sig(
        &mut self,
        sig: &TsMethodSignature,
        obj_is_mutable: bool,
    ) -> Result<TObjElem, String> {
        if sig.computed {
            panic!("unexpected computed property in TypElement")
        }

        let params = self.infer_fn_params(&sig.params)?;
        let ret = match &sig.type_ann {
            Some(type_ann) => self.infer_ts_type_ann(&type_ann.type_ann),
            None => Err(String::from("method has no return type")),
        }?;

        let mut type_params = match &sig.type_params {
            Some(type_param_decl) => type_param_decl
                .params
                .iter()
                .map(|type_param| {
                    let constraint = match &type_param.constraint {
                        Some(constraint) => {
                            let t = self.infer_ts_type_ann(constraint)?;
                            Some(Box::from(t))
                        }
                        None => None,
                    };

                    let default = match &type_param.default {
                        Some(default) => {
                            let t = self.infer_ts_type_ann(default)?;
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

        let name = get_key_name(sig.key.as_ref())?;

        // If there are any free type variables, add them to `type_params`.
        let mut tvs = params.ftv();
        tvs.append(&mut ret.ftv());

        let mut sub: Subst = Subst::default();
        let mut char_code: u32 = 65; // TODO: avoid naming collisions
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

        let elem = types::TObjElem::Method(types::TMethod {
            name: TPropKey::StringKey(name),
            params,
            ret: Box::from(ret),
            type_params: if type_params.is_empty() {
                None
            } else {
                Some(type_params)
            },
            // Assume that all methods in mutable object can mutate the object.
            // If there's a ReadonlyFoo and Foo pair, duplicates will be merged such
            // that `is_mutating: false`.
            is_mutating: obj_is_mutable,
        });

        let t = elem.apply(&sub);

        Ok(t)
    }

    fn get_type_params(
        &mut self,
        type_params: &Option<Box<TsTypeParamDecl>>,
    ) -> Result<Vec<TypeParam>, String> {
        match type_params {
            Some(type_params) => type_params
                .params
                .iter()
                .map(|type_param| {
                    let constraint = match &type_param.constraint {
                        Some(constraint) => {
                            let t = self.infer_ts_type_ann(constraint)?;
                            Some(Box::from(t))
                        }
                        None => None,
                    };

                    let default = match &type_param.default {
                        Some(default) => {
                            let t = self.infer_ts_type_ann(default)?;
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
        &mut self,
        params: &[TsFnParam],
        type_ann: &TsType,
        type_params: &Option<Box<TsTypeParamDecl>>,
    ) -> Result<TCallable, String> {
        let params = self.infer_fn_params(params)?;
        let ret = self.infer_ts_type_ann(type_ann)?;
        let mut type_params = self.get_type_params(type_params)?;

        let mut tvars = params.ftv();
        tvars.append(&mut ret.ftv());

        let (sub, more_type_params) = get_sub_and_type_params(&tvars);
        if let Some(mut more_type_params) = more_type_params {
            type_params.append(&mut more_type_params);
        }

        let params = params.apply(&sub);
        let ret = ret.apply(&sub);

        Ok(TCallable {
            params,
            ret: Box::from(ret),
            type_params: if type_params.is_empty() {
                None
            } else {
                Some(type_params)
            },
        })
    }

    fn infer_ts_type_element(
        &mut self,
        elem: &TsTypeElement,
        obj_is_mutable: bool,
    ) -> Result<TObjElem, String> {
        match elem {
            TsTypeElement::TsCallSignatureDecl(decl) => match &decl.type_ann {
                Some(type_ann) => Ok(TObjElem::Call(self.infer_callable(
                    &decl.params,
                    &type_ann.type_ann,
                    &decl.type_params,
                )?)),
                None => Err(String::from("Property is missing type annotation")),
            },
            TsTypeElement::TsConstructSignatureDecl(decl) => match &decl.type_ann {
                Some(type_ann) => Ok(TObjElem::Constructor(self.infer_callable(
                    &decl.params,
                    &type_ann.type_ann,
                    &decl.type_params,
                )?)),
                None => Err(String::from("Property is missing type annotation")),
            },
            TsTypeElement::TsPropertySignature(sig) => match &sig.type_ann {
                Some(type_ann) => {
                    let t = self.infer_ts_type_ann(&type_ann.type_ann)?;
                    let name = get_key_name(sig.key.as_ref())?;
                    Ok(TObjElem::Prop(TProp {
                        name: TPropKey::StringKey(name),
                        optional: sig.optional,
                        // TODO: warn about mutable props inside of a readonly object
                        mutable: !sig.readonly,
                        t,
                    }))
                }
                None => Err(String::from("Property is missing type annotation")),
            },
            TsTypeElement::TsGetterSignature(sig) => {
                // TODO: warn about setters inside of a readonly object
                let key = get_key_name(sig.key.as_ref())?;
                Err(format!("TsGetterSignature: {key}"))
            }
            TsTypeElement::TsSetterSignature(sig) => {
                let key = get_key_name(sig.key.as_ref())?;
                Err(format!("TsSetterSignature: {key}"))
            }
            TsTypeElement::TsMethodSignature(sig) => self.infer_method_sig(sig, obj_is_mutable),
            TsTypeElement::TsIndexSignature(sig) => match &sig.type_ann {
                Some(type_ann) => {
                    let t = self.infer_ts_type_ann(&type_ann.type_ann)?;
                    let params = self.infer_fn_params(&sig.params)?;
                    let key = params.get(0).unwrap();

                    if let TPat::Ident(escalier_ast::types::BindingIdent { name, .. }) = &key.pat {
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

    fn infer_type_alias_decl(&mut self, decl: &TsTypeAliasDecl) -> Result<Scheme, String> {
        let t = self.infer_ts_type_ann(&decl.type_ann)?;

        let type_params = match &decl.type_params {
            Some(type_params) => Some(
                type_params
                    .params
                    .iter()
                    .map(|type_param| {
                        let constraint = match &type_param.constraint {
                            Some(constraint) => {
                                let t = self.infer_ts_type_ann(constraint)?;
                                Some(Box::from(t))
                            }
                            None => None,
                        };

                        let default = match &type_param.default {
                            Some(default) => {
                                let t = self.infer_ts_type_ann(default)?;
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
            ),
            None => None,
        };

        let scheme = Scheme {
            t: Box::from(t),
            type_params,
        };

        Ok(scheme)
    }

    fn infer_interface_decl(
        &mut self,
        decl: &TsInterfaceDecl,
        obj_is_mutable: bool,
    ) -> Result<Scheme, String> {
        // TODO: skip properties we don't know how to deal with instead of return an error for the whole map
        let elems: Vec<TObjElem> = decl
            .body
            .body
            .iter()
            .filter_map(|elem| {
                let elem = self.infer_ts_type_element(elem, obj_is_mutable);

                match elem {
                    Ok(elem) => match maybe_override_string_methods(decl, &elem) {
                        Some(override_elem) => Some(override_elem),
                        None => Some(elem),
                    },
                    Err(msg) => {
                        eprintln!("Err: {msg}");
                        None
                    }
                }
            })
            .collect();

        let t = Type::from(TypeKind::Object(TObject {
            elems,
            is_interface: false,
        }));

        let mut type_params = match &decl.type_params {
            Some(type_params) => Some(
                type_params
                    .params
                    .iter()
                    .map(|type_param| {
                        let constraint = match &type_param.constraint {
                            Some(constraint) => {
                                let t = self.infer_ts_type_ann(constraint)?;
                                Some(Box::from(t))
                            }
                            None => None,
                        };

                        let default = match &type_param.default {
                            Some(default) => {
                                let t = self.infer_ts_type_ann(default)?;
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
            ),
            None => None,
        };

        // Add `TPattern` and `TFlags` type params to `RegExp`, `RegExpExecArray`,
        // and `RegExpMatchArray`.
        let interface_name = decl.id.sym.to_string();
        if interface_name == "RegExp"
            || interface_name == "RegExpExecArray"
            || interface_name == "RegExpMatchArray"
        {
            type_params = Some(vec![
                TypeParam {
                    name: "TPattern".to_string(),
                    constraint: None,
                    default: None,
                },
                TypeParam {
                    name: "TFlags".to_string(),
                    constraint: None,
                    default: None,
                },
            ])
        }

        let scheme = Scheme {
            t: Box::from(t),
            type_params,
        };

        Ok(scheme)
    }
}

fn get_key_name(key: &Expr) -> Result<String, String> {
    match key {
        Expr::Ident(Ident { sym, .. }) => Ok(sym.to_string()),
        Expr::Lit(swc_ecma_ast::Lit::Str(Str { value, .. })) => Ok(value.to_string()),
        _ => Err(format!("get_key_name: {key:#?}")),
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceCollector {
    pub checker: Checker,
    pub comments: SingleThreadedComments,
    pub namespace: Vec<String>,
    pub interfaces: HashMap<String, Vec<TsInterfaceDecl>>,
}

impl Visit for InterfaceCollector {
    fn visit_ts_type_alias_decl(&mut self, decl: &TsTypeAliasDecl) {
        let name = decl.id.sym.to_string();
        match self.checker.infer_type_alias_decl(decl) {
            Ok(scheme) => {
                eprintln!("inferring: {name} as scheme: {scheme}");
                self.checker.insert_scheme(name, scheme)
            }
            Err(err) => {
                eprintln!("couldn't infer {name}, {err:#?}")
            }
        }
    }

    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        let mut name = decl.id.sym.to_string();
        if name.starts_with("Readonly") {
            name = name.replace("Readonly", "");
        }

        match self.interfaces.get_mut(&name) {
            Some(decls) => decls.push(decl.to_owned()),
            None => {
                self.interfaces
                    .insert(name.to_owned(), vec![decl.to_owned()]);
            }
        }
    }

    fn visit_ts_module_decl(&mut self, decl: &TsModuleDecl) {
        match &decl.id {
            TsModuleName::Ident(id) => {
                let name = id.sym.to_string();
                eprintln!("module: {name}");
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
                            let t = self.checker.infer_ts_type_ann(&type_ann.type_ann).unwrap();
                            let name = bi.id.sym.to_string();
                            self.checker.insert_value(name, t)
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

pub fn parse_dts(d_ts_source: &str) -> Result<escalier_infer::Checker, Error> {
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
        checker: Checker::default(),
        comments,
        namespace: vec![],
        interfaces: HashMap::new(),
    };

    module.visit_with(&mut collector);

    for (name, decls) in collector.interfaces {
        let has_readonly = decls
            .iter()
            .any(|decl| decl.id.sym.to_string().starts_with("Readonly"));
        for decl in decls {
            let readonly = decl.id.sym.to_string().starts_with("Readonly");
            // NOTE: If there is no interface prefixed with `Readonly` then we
            // assume the interface is unsafe (since there's no way to know which
            // methods are mutable or not).  In this situation we set `mutable`
            // to `false` so that all methods are accessible from both mutable
            // and immutable references.
            let mutable = has_readonly && !readonly;

            let name = name.to_owned();
            match collector.checker.infer_interface_decl(&decl, mutable) {
                Ok(new_scheme) => {
                    match collector.checker.lookup_scheme(&name).ok() {
                        Some(old_scheme) => {
                            let merged_scheme = util::merge_schemes(&old_scheme, &new_scheme);
                            collector.checker.insert_scheme(name, merged_scheme);
                        }
                        None => collector.checker.insert_scheme(name, new_scheme),
                    };
                }
                Err(_) => eprintln!("couldn't infer {name}"),
            }
        }
    }

    let checker = escalier_infer::Checker {
        current_scope: collector.checker.current_scope,
        next_id: collector.checker.next_id,
        parent_scopes: vec![],
    };
    Ok(checker)
}
