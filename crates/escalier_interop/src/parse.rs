use generational_arena::Index;
use std::collections::HashMap;
use std::sync::Arc;

use swc_common::{comments::SingleThreadedComments, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

use escalier_ast::identifier;
use escalier_ast::literal::Literal as Lit;
use escalier_ast::span::Span;
use escalier_hm::checker::Checker;
use escalier_hm::context::{Binding, Context};
use escalier_hm::types::{
    self, Conditional, FuncParam, Function, IndexedAccess, Keyword, MappedType, Object, Primitive,
    RestPat, Scheme, TCallable, TObjElem, TPat, TProp, TPropKey, TypeKind, TypeParam, TypeRef,
};

// use crate::overrides::maybe_override_string_methods;
use crate::util;

pub fn infer_ts_type_ann(checker: &'_ mut Checker, type_ann: &TsType) -> Result<Index, String> {
    match type_ann {
        TsType::TsKeywordType(keyword) => match &keyword.kind {
            TsKeywordTypeKind::TsAnyKeyword => Ok(checker.new_var_type(None)),
            TsKeywordTypeKind::TsUnknownKeyword => Ok(checker.new_var_type(None)),
            TsKeywordTypeKind::TsNumberKeyword => {
                Ok(checker.from_type_kind(TypeKind::Primitive(Primitive::Number)))
            }
            TsKeywordTypeKind::TsObjectKeyword => {
                Ok(checker.from_type_kind(TypeKind::Keyword(Keyword::Object)))
            }
            TsKeywordTypeKind::TsBooleanKeyword => {
                Ok(checker.from_type_kind(TypeKind::Primitive(Primitive::Boolean)))
            }
            TsKeywordTypeKind::TsBigIntKeyword => Err(String::from("can't parse BigInt yet")),
            TsKeywordTypeKind::TsStringKeyword => {
                Ok(checker.from_type_kind(TypeKind::Primitive(Primitive::String)))
            }
            TsKeywordTypeKind::TsSymbolKeyword => {
                Ok(checker.from_type_kind(TypeKind::Primitive(Primitive::Symbol)))
            }
            // NOTE: `void` is treated the same as `undefined` ...for now.
            TsKeywordTypeKind::TsVoidKeyword => {
                Ok(checker.from_type_kind(TypeKind::Keyword(Keyword::Undefined)))
            }
            TsKeywordTypeKind::TsUndefinedKeyword => {
                Ok(checker.from_type_kind(TypeKind::Keyword(Keyword::Undefined)))
            }
            TsKeywordTypeKind::TsNullKeyword => {
                Ok(checker.from_type_kind(TypeKind::Keyword(Keyword::Null)))
            }
            TsKeywordTypeKind::TsNeverKeyword => {
                Ok(checker.from_type_kind(TypeKind::Keyword(Keyword::Never)))
            }
            TsKeywordTypeKind::TsIntrinsicKeyword => {
                Err(String::from("can't parse Intrinsics yet"))
            }
        },
        TsType::TsThisType(_) => {
            // TODO: create a new type variable for `Self`
            Ok(checker.new_constructor("This", &[]))
        }
        TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
            TsFnOrConstructorType::TsFnType(fn_type) => {
                let params = infer_fn_params(checker, &fn_type.params)?;
                let ret = infer_ts_type_ann(checker, &fn_type.type_ann.type_ann)?;

                let type_params = match &fn_type.type_params {
                    Some(type_param_decl) => {
                        let type_params = type_param_decl
                            .params
                            .iter()
                            .map(|type_param| {
                                let constraint = match &type_param.constraint {
                                    Some(constraint) => {
                                        let t = infer_ts_type_ann(checker, constraint)?;
                                        Some(t)
                                    }
                                    None => None,
                                };

                                let default = match &type_param.default {
                                    Some(default) => {
                                        let t = infer_ts_type_ann(checker, default)?;
                                        Some(t)
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

                let t = checker.from_type_kind(TypeKind::Function(Function {
                    type_params,
                    params,
                    ret,
                    throws: None,
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
                        .map(|t| infer_ts_type_ann(checker, t))
                        .collect();
                    Ok(checker.from_type_kind(TypeKind::TypeRef(TypeRef {
                        name,
                        // TODO: Update TypeRef's .types field to be Option<Vec<Index>>
                        types: result.ok().unwrap(),
                    })))
                }
                None => Ok(checker.from_type_kind(TypeKind::TypeRef(TypeRef {
                    name,
                    types: vec![],
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
                    let prop = infer_ts_type_element(checker, elem, obj_is_mutable);

                    match prop {
                        Ok(prop) => Some(prop),
                        Err(msg) => {
                            eprintln!("Err: {msg}");
                            None
                        }
                    }
                })
                .collect();

            let t = checker.from_type_kind(TypeKind::Object(Object {
                elems,
                // is_interface: false,
            }));

            Ok(t)
        }
        TsType::TsArrayType(array) => {
            let elem_type = infer_ts_type_ann(checker, &array.elem_type)?;
            // TODO: link back to the TypeScript source type annotation
            let array_type = checker.new_array_type(elem_type);
            // t.mutable = true;
            Ok(array_type)
        }
        TsType::TsTupleType(_) => Err(String::from("can't parse tuple type yet")),
        TsType::TsOptionalType(_) => Err(String::from("can't parse optional type yet")),
        TsType::TsRestType(_) => Err(String::from("can't parse rest type yet")),
        TsType::TsUnionOrIntersectionType(union_or_intersection) => match union_or_intersection {
            TsUnionOrIntersectionType::TsUnionType(union) => {
                let types = union
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(checker, ts_type))
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(checker.new_union_type(&types))
            }
            TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                let types = intersection
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(checker, ts_type))
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(checker.new_intersection_type(&types))
            }
        },
        TsType::TsConditionalType(TsConditionalType {
            span: _,
            check_type,
            extends_type,
            true_type,
            false_type,
        }) => {
            let check = infer_ts_type_ann(checker, check_type)?;
            let extends = infer_ts_type_ann(checker, extends_type)?;
            let true_type = infer_ts_type_ann(checker, true_type)?;
            let false_type = infer_ts_type_ann(checker, false_type)?;
            let t = checker.from_type_kind(TypeKind::Conditional(Conditional {
                check,
                extends,
                true_type,
                false_type,
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
                        let number = checker.from_type_kind(TypeKind::Primitive(Primitive::Number));
                        let string = checker.from_type_kind(TypeKind::Primitive(Primitive::String));
                        let symbol = checker.from_type_kind(TypeKind::Primitive(Primitive::Symbol));
                        let t = checker.new_union_type(&[number, string, symbol]);
                        Ok(t)
                    }
                    _ => {
                        let type_ann = infer_ts_type_ann(checker, type_ann)?;
                        Ok(checker.new_keyof_type(type_ann))
                    }
                },
                TsTypeOperatorOp::Unique => todo!(),
                TsTypeOperatorOp::ReadOnly => {
                    let type_ann = infer_ts_type_ann(checker, type_ann)?;
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
            let object_t = infer_ts_type_ann(checker, obj_type)?;
            let index_t = infer_ts_type_ann(checker, index_type.as_ref())?;
            let t = checker.from_type_kind(TypeKind::IndexedAccess(IndexedAccess {
                obj: object_t,
                index: index_t,
            }));
            Ok(t)
        }
        TsType::TsMappedType(TsMappedType {
            span: _,
            type_param,
            type_ann,
            readonly: _,
            optional: _,
            ..
        }) => {
            let type_ann = infer_ts_type_ann(checker, type_ann.as_ref().unwrap())?;
            let constraint = match &type_param.constraint {
                Some(constraint) => infer_ts_type_ann(checker, constraint)?,
                None => {
                    return Err(String::from(
                        "MappedType is missing a type parameter constraint",
                    ))
                }
            };

            // TODO: handle readonly modifier
            // TODO: handle optional modifier

            let name = type_param.name.sym.to_string();

            let elems = vec![TObjElem::Mapped(MappedType {
                key: checker.new_constructor(&name, &[]),
                target: name,
                source: constraint,
                value: type_ann,
                check: None,
                extends: None,
            })];

            let t = checker.new_object_type(&elems);

            Ok(t)
        }
        TsType::TsLitType(lit) => match &lit.lit {
            TsLit::Number(num) => Ok(checker.new_lit_type(&Lit::Number(format!("{}", num.value)))),
            TsLit::Str(str) => Ok(checker.new_lit_type(&Lit::String(str.value.to_string()))),
            TsLit::Bool(b) => Ok(checker.new_lit_type(&Lit::Boolean(b.value))),
            TsLit::BigInt(_) => Err(String::from("can't parse BigInt literal yet")),
            TsLit::Tpl(_) => Err(String::from("can't parse Tpl literal yet")),
        },
        TsType::TsTypePredicate(_) => Err(String::from("can't parse type predicate yet")),
        TsType::TsImportType(_) => Err(String::from("can't parse import type yet")),
    }
}

fn infer_fn_params(
    checker: &'_ mut Checker,
    params: &[TsFnParam],
) -> Result<Vec<FuncParam>, String> {
    let params: Vec<FuncParam> = params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| match param {
            TsFnParam::Ident(ident) => {
                let type_ann = ident.type_ann.clone().unwrap();
                let param = FuncParam {
                    pattern: TPat::Ident(identifier::BindingIdent {
                        span: Span { start: 0, end: 0 },
                        name: ident.id.sym.to_string(),
                        mutable: false,
                    }),
                    t: infer_ts_type_ann(checker, &type_ann.type_ann).ok()?,
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
                let param = FuncParam {
                    pattern: TPat::Rest(RestPat {
                        arg: Box::from(TPat::Ident(identifier::BindingIdent {
                            span: Span { start: 0, end: 0 },
                            name,
                            mutable: false,
                        })),
                    }),
                    t: infer_ts_type_ann(checker, &type_ann.type_ann).ok()?,
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
    checker: &'_ mut Checker,
    sig: &TsMethodSignature,
    _obj_is_mutable: bool,
) -> Result<TObjElem, String> {
    if sig.computed {
        panic!("unexpected computed property in TypElement")
    }

    let params = infer_fn_params(checker, &sig.params)?;
    let ret = match &sig.type_ann {
        Some(type_ann) => infer_ts_type_ann(checker, &type_ann.type_ann),
        None => Err(String::from("method has no return type")),
    }?;

    // TODO: check to see if there are any type variables in the signature
    // that can be turned into type params.
    let type_params = match &sig.type_params {
        Some(type_param_decl) => type_param_decl
            .params
            .iter()
            .map(|type_param| {
                let constraint = match &type_param.constraint {
                    Some(constraint) => {
                        let t = infer_ts_type_ann(checker, constraint)?;
                        Some(t)
                    }
                    None => None,
                };

                let default = match &type_param.default {
                    Some(default) => {
                        let t = infer_ts_type_ann(checker, default)?;
                        Some(t)
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

    let t = checker.from_type_kind(TypeKind::Function(Function {
        type_params: if type_params.is_empty() {
            None
        } else {
            Some(type_params)
        },
        params,
        ret,
        throws: None,
    }));

    let elem = types::TObjElem::Prop(TProp {
        name: TPropKey::StringKey(name),
        modifier: None,
        optional: false,
        mutable: false,
        t,
    });

    Ok(elem)
}

fn get_type_params(
    checker: &'_ mut Checker,
    type_params: &Option<Box<TsTypeParamDecl>>,
) -> Result<Vec<TypeParam>, String> {
    match type_params {
        Some(type_params) => type_params
            .params
            .iter()
            .map(|type_param| {
                let constraint = match &type_param.constraint {
                    Some(constraint) => {
                        let t = infer_ts_type_ann(checker, constraint)?;
                        Some(t)
                    }
                    None => None,
                };

                let default = match &type_param.default {
                    Some(default) => {
                        let t = infer_ts_type_ann(checker, default)?;
                        Some(t)
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
    checker: &mut Checker,
    params: &[TsFnParam],
    type_ann: &TsType,
    type_params: &Option<Box<TsTypeParamDecl>>,
) -> Result<TCallable, String> {
    let params = infer_fn_params(checker, params)?;
    let ret = infer_ts_type_ann(checker, type_ann)?;
    let type_params = get_type_params(checker, type_params)?;

    // TODO: check signature to see if there are any free type variables that
    // can be converted to additional type params.

    Ok(TCallable {
        params,
        ret,
        type_params: if type_params.is_empty() {
            None
        } else {
            Some(type_params)
        },
    })
}

fn infer_ts_type_element(
    checker: &'_ mut Checker,
    elem: &TsTypeElement,
    obj_is_mutable: bool,
) -> Result<TObjElem, String> {
    match elem {
        TsTypeElement::TsCallSignatureDecl(decl) => match &decl.type_ann {
            Some(type_ann) => Ok(TObjElem::Call(infer_callable(
                checker,
                &decl.params,
                &type_ann.type_ann,
                &decl.type_params,
            )?)),
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsConstructSignatureDecl(decl) => match &decl.type_ann {
            Some(type_ann) => Ok(TObjElem::Constructor(infer_callable(
                checker,
                &decl.params,
                &type_ann.type_ann,
                &decl.type_params,
            )?)),
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsPropertySignature(sig) => match &sig.type_ann {
            Some(type_ann) => {
                let t = infer_ts_type_ann(checker, &type_ann.type_ann)?;
                let name = get_key_name(sig.key.as_ref())?;
                Ok(TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(name),
                    optional: sig.optional,
                    // TODO: warn about mutable props inside of a readonly object
                    mutable: !sig.readonly,
                    t,
                    modifier: None,
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
        TsTypeElement::TsMethodSignature(sig) => infer_method_sig(checker, sig, obj_is_mutable),
        TsTypeElement::TsIndexSignature(sig) => match &sig.type_ann {
            Some(type_ann) => {
                let t = infer_ts_type_ann(checker, &type_ann.type_ann)?;
                let params = infer_fn_params(checker, &sig.params)?;
                let key = params.get(0).unwrap();

                if let TPat::Ident(identifier::BindingIdent { name, .. }) = &key.pattern {
                    Ok(TObjElem::Mapped(MappedType {
                        key: checker.new_constructor(name.as_str(), &[]),
                        target: name.to_owned(),
                        value: t,
                        source: key.t,
                        check: None,
                        extends: None,
                    }))
                } else {
                    Err(String::from("Invalid key in index signature"))
                }
            }
            None => Err(String::from("Index is missing type annotation")),
        },
    }
}

fn infer_type_alias_decl(
    checker: &'_ mut Checker,
    decl: &TsTypeAliasDecl,
) -> Result<Scheme, String> {
    let t = infer_ts_type_ann(checker, &decl.type_ann)?;

    let type_params = match &decl.type_params {
        Some(type_params) => Some(
            type_params
                .params
                .iter()
                .map(|type_param| {
                    let constraint = match &type_param.constraint {
                        Some(constraint) => {
                            let t = infer_ts_type_ann(checker, constraint)?;
                            Some(t)
                        }
                        None => None,
                    };

                    let default = match &type_param.default {
                        Some(default) => {
                            let t = infer_ts_type_ann(checker, default)?;
                            Some(t)
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

    let scheme = Scheme { t, type_params };

    Ok(scheme)
}

fn infer_interface_decl(
    checker: &'_ mut Checker,
    decl: &TsInterfaceDecl,
    obj_is_mutable: bool,
) -> Result<Scheme, String> {
    // TODO: skip properties we don't know how to deal with instead of return an error for the whole map
    let elems: Vec<TObjElem> = decl
        .body
        .body
        .iter()
        .filter_map(|elem| {
            let elem = infer_ts_type_element(checker, elem, obj_is_mutable);

            match elem {
                Ok(elem) => Some(elem),
                Err(msg) => {
                    eprintln!("Err: {msg}");
                    None
                }
            }
        })
        .collect();

    let t = checker.from_type_kind(TypeKind::Object(Object {
        elems,
        // is_interface: false,
    }));

    let mut type_params = match &decl.type_params {
        Some(type_params) => Some(
            type_params
                .params
                .iter()
                .map(|type_param| {
                    let constraint = match &type_param.constraint {
                        Some(constraint) => {
                            let t = infer_ts_type_ann(checker, constraint)?;
                            Some(t)
                        }
                        None => None,
                    };

                    let default = match &type_param.default {
                        Some(default) => {
                            let t = infer_ts_type_ann(checker, default)?;
                            Some(t)
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

    let scheme = Scheme { t, type_params };

    Ok(scheme)
}

fn get_key_name(key: &Expr) -> Result<String, String> {
    match key {
        Expr::Ident(Ident { sym, .. }) => Ok(sym.to_string()),
        Expr::Lit(swc_ecma_ast::Lit::Str(Str { value, .. })) => Ok(value.to_string()),
        _ => Err(format!("get_key_name: {key:#?}")),
    }
}

#[derive(Debug)]
pub struct InterfaceCollector {
    pub checker: Checker,
    pub ctx: Context,
    pub comments: SingleThreadedComments,
    pub namespace: Vec<String>,
    pub interfaces: HashMap<String, Vec<TsInterfaceDecl>>,
}

impl Visit for InterfaceCollector {
    fn visit_ts_type_alias_decl(&mut self, decl: &TsTypeAliasDecl) {
        let name = decl.id.sym.to_string();
        match infer_type_alias_decl(&mut self.checker, decl) {
            Ok(scheme) => {
                // eprintln!("inferring: {name} as scheme: {scheme}");
                self.ctx.schemes.insert(name, scheme);
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
                            let t =
                                infer_ts_type_ann(&mut self.checker, &type_ann.type_ann).unwrap();
                            let name = bi.id.sym.to_string();
                            let binding = Binding {
                                index: t.to_owned(),
                                is_mut: false,
                            };
                            self.ctx.values.insert(name, binding);
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

pub fn parse_dts(d_ts_source: &str) -> Result<(Checker, Context), Error> {
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
            disallow_ambiguous_jsx_like: false,
        }),
        EsVersion::Es2020,
        Some(&comments),
        &mut errors,
    )?;

    let mut collector = InterfaceCollector {
        checker: Checker::default(),
        ctx: Context::default(),
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
            match infer_interface_decl(&mut collector.checker, &decl, mutable) {
                Ok(new_scheme) => {
                    match collector.ctx.get_scheme(&name).ok() {
                        Some(old_scheme) => {
                            let merged_scheme = util::merge_schemes(
                                &old_scheme,
                                &new_scheme,
                                &mut collector.checker,
                            );
                            collector.ctx.schemes.insert(name, merged_scheme)
                        }
                        None => collector.ctx.schemes.insert(name, new_scheme),
                    };
                }
                Err(_) => eprintln!("couldn't infer {name}"),
            }
        }
    }

    Ok((collector.checker, collector.ctx))
}
