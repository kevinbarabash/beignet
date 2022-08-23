use std::sync::Arc;
use types::Scheme;

use swc_common::{comments::SingleThreadedComments, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

// TODO: have crochet_infer re-export Lit
use crochet_ast::Lit;
use crochet_infer::{close_over, generalize, Context, Env, Subst};
use crochet_types::{self as types, RestPat, TFnParam, TPat, TPrim, TProp, Type};

#[derive(Debug, Clone)]
pub struct InterfaceCollector {
    pub ctx: Context,
    pub comments: SingleThreadedComments,
    pub namespace: Vec<String>,
}

fn infer_ts_type_ann(type_ann: &TsType, ctx: &Context) -> Result<Type, String> {
    match type_ann {
        TsType::TsKeywordType(keyword) => match &keyword.kind {
            TsKeywordTypeKind::TsAnyKeyword => Ok(ctx.fresh_var()),
            TsKeywordTypeKind::TsUnknownKeyword => {
                Err(String::from("can't parse unknown keyword yet"))
            }
            TsKeywordTypeKind::TsNumberKeyword => Ok(Type::Prim(TPrim::Num)),
            TsKeywordTypeKind::TsObjectKeyword => Err(String::from("can't parse Objects yet")),
            TsKeywordTypeKind::TsBooleanKeyword => Ok(Type::Prim(TPrim::Bool)),
            TsKeywordTypeKind::TsBigIntKeyword => Err(String::from("can't parse BigInt yet")),
            TsKeywordTypeKind::TsStringKeyword => Ok(Type::Prim(TPrim::Str)),
            TsKeywordTypeKind::TsSymbolKeyword => Err(String::from("can't parse Symbols yet")),
            TsKeywordTypeKind::TsVoidKeyword => Err(String::from("can't parse void keyword yet")),
            TsKeywordTypeKind::TsUndefinedKeyword => Ok(Type::Prim(TPrim::Undefined)),
            TsKeywordTypeKind::TsNullKeyword => Ok(Type::Prim(TPrim::Null)),
            TsKeywordTypeKind::TsNeverKeyword => Err(String::from("can't parse never keyword yet")),
            TsKeywordTypeKind::TsIntrinsicKeyword => {
                Err(String::from("can't parse Intrinsics yet"))
            }
        },
        TsType::TsThisType(_) => Err(String::from("can't parse this type yet")),
        TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
            TsFnOrConstructorType::TsFnType(fn_type) => {
                let params: Vec<TFnParam> = fn_type
                    .params
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
                                ty: infer_ts_type_ann(&type_ann.type_ann, ctx).ok()?,
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
                                ty: infer_ts_type_ann(&type_ann.type_ann, ctx).ok()?,
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
                let ret = infer_ts_type_ann(&fn_type.type_ann.type_ann, ctx)?;
                Ok(Type::Lam(types::TLam {
                    params,
                    ret: Box::from(ret),
                }))
            }
            TsFnOrConstructorType::TsConstructorType(_) => {
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
                    Ok(Type::Alias(types::TAlias {
                        name,
                        type_params: result.ok(),
                    }))
                }
                None => Ok(Type::Alias(types::TAlias {
                    name,
                    type_params: None,
                })),
            }
        }
        TsType::TsTypeQuery(_) => Err(String::from("can't parse type query yet")),
        TsType::TsTypeLit(_) => Err(String::from("can't parse type literal yet")),
        TsType::TsArrayType(array) => {
            let elem_type = infer_ts_type_ann(&array.elem_type, ctx)?;
            Ok(Type::Array(Box::from(elem_type)))
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
                Ok(Type::Union(types?))
            }
            TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                let types: Result<Vec<_>, String> = intersection
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(ts_type, ctx))
                    .collect();
                Ok(Type::Intersection(types?))
            }
        },
        TsType::TsConditionalType(_) => Err(String::from("can't parse conditional type yet")),
        TsType::TsInferType(_) => Err(String::from("can't parse infer type yet")),
        TsType::TsParenthesizedType(_) => Err(String::from("can't parse parenthesized yet")),
        TsType::TsTypeOperator(_) => Err(String::from("can't parse type operator yet")),
        TsType::TsIndexedAccessType(_) => Err(String::from("can't parse indexed type yet")),
        TsType::TsMappedType(_) => Err(String::from("can't parse mapped type yet")),
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

fn infer_method_sig(sig: &TsMethodSignature, ctx: &Context) -> Result<Type, String> {
    if sig.computed {
        panic!("unexpected computed property in TypElement")
    }

    let params: Vec<TFnParam> = sig
        .params
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
                    ty: infer_ts_type_ann(&type_ann.type_ann, ctx).ok()?,
                    optional: ident.optional,
                };
                Some(param)
            }
            TsFnParam::Array(_) => {
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
                    ty: infer_ts_type_ann(&type_ann.type_ann, ctx).ok()?,
                    optional: false,
                };
                Some(param)
            }
            TsFnParam::Object(_) => {
                println!("skipping TsFnParam::Object(_)");
                None
            }
        })
        .collect();

    let ret = match &sig.type_ann {
        Some(type_ann) => infer_ts_type_ann(&type_ann.type_ann, ctx),
        None => Err(String::from("method has no return type")),
    };

    // TODO: maintain param names
    Ok(Type::Lam(types::TLam {
        params,
        ret: Box::from(ret?),
    }))
}

fn get_key_name(key: &Expr) -> Result<String, String> {
    if let Expr::Ident(Ident { sym, .. }) = key {
        Ok(sym.to_string())
    } else {
        Err(format!("get_key_name: {key:#?}"))
    }
}

fn infer_ts_type_element(elem: &TsTypeElement, ctx: &Context) -> Result<TProp, String> {
    match elem {
        TsTypeElement::TsCallSignatureDecl(_decl) => Err(String::from("TsCallSignatureDecl")),
        TsTypeElement::TsConstructSignatureDecl(_decl) => {
            Err(String::from("TsConstructSignatureDecl"))
        }
        TsTypeElement::TsPropertySignature(sig) => {
            // TODO: update TProp to include a readonly flag
            match &sig.type_ann {
                Some(type_ann) => {
                    // TODO: do a better job of handling this
                    let t = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                    let name = get_key_name(sig.key.as_ref())?;
                    Ok(TProp {
                        name,
                        optional: sig.optional,
                        mutable: !sig.readonly,
                        scheme: Scheme::from(t),
                    })
                }
                None => Err(String::from("Property is missing type annotation")),
            }
        }
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
            let name = get_key_name(sig.key.as_ref())?;
            let scheme = generalize(&Env::new(), &t);
            Ok(TProp {
                name,
                optional: sig.optional,
                mutable: false, // All methods on interfaces are readonly
                scheme,
            })
        }
        TsTypeElement::TsIndexSignature(_sig) => Err(String::from("TsIndexSignature")),
    }
}

fn infer_interface_decl(decl: &TsInterfaceDecl, ctx: &Context) -> Result<Type, String> {
    // TODO: skip properties we don't know how to deal with instead of return an error for the whole map
    let props: Vec<_> = decl
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

    Ok(Type::Object(props))
}

fn merge_schemes(s1: &Scheme, s2: &Scheme) -> Scheme {
    match (&s1.ty, &s2.ty) {
        (Type::Object(props1), Type::Object(props2)) => {
            let props: Vec<_> = props1
                .iter()
                .cloned()
                .chain(props2.iter().cloned())
                .collect();
            let merged_t = Type::Object(props);
            // TODO: merge qualifiers
            Scheme::from(merged_t)
        }
        (_, _) => todo!(),
    }
}

impl Visit for InterfaceCollector {
    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        let name = decl.id.sym.to_string();
        println!("inferring: {name}");
        let t = infer_interface_decl(decl, &self.ctx);
        match t {
            Ok(t) => {
                let empty_s = Subst::default();
                let scheme = close_over(&empty_s, &t, &self.ctx);
                match self.ctx.lookup_type_scheme(&name).ok() {
                    Some(existing_scheme) => self
                        .ctx
                        .insert_type(name, merge_schemes(&existing_scheme, &scheme)),
                    None => self.ctx.insert_type(name, scheme),
                };
            }
            Err(_) => {
                println!("couldn't infer {name}");
            }
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
