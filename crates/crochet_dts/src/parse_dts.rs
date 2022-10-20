use std::collections::HashMap;
use std::sync::Arc;
use types::{TCallable, TIndex, TObjElem, TObject, TypeKind};

use swc_common::{comments::SingleThreadedComments, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

// TODO: have crochet_infer re-export Lit
use crochet_ast::common;
use crochet_ast::types::{self as types, RestPat, TFnParam, TKeyword, TPat, TProp, Type};
use crochet_ast::values::Lit;
use crochet_infer::{close_over, generalize, normalize, Context, Env, Subst, Substitutable};

use crate::util;

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
            TsKeywordTypeKind::TsNumberKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Number),
            }),
            TsKeywordTypeKind::TsObjectKeyword => Err(String::from("can't parse Objects yet")),
            TsKeywordTypeKind::TsBooleanKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Boolean),
            }),
            TsKeywordTypeKind::TsBigIntKeyword => Err(String::from("can't parse BigInt yet")),
            TsKeywordTypeKind::TsStringKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::String),
            }),
            TsKeywordTypeKind::TsSymbolKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Symbol),
            }),
            // NOTE: `void` is treated the same as `undefined` ...for now.
            TsKeywordTypeKind::TsVoidKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Undefined),
            }),
            TsKeywordTypeKind::TsUndefinedKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Undefined),
            }),
            TsKeywordTypeKind::TsNullKeyword => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Null),
            }),
            TsKeywordTypeKind::TsNeverKeyword => Err(String::from("can't parse never keyword yet")),
            TsKeywordTypeKind::TsIntrinsicKeyword => {
                Err(String::from("can't parse Intrinsics yet"))
            }
        },
        TsType::TsThisType(_) => Ok(Type {
            kind: TypeKind::This,
        }),
        TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
            TsFnOrConstructorType::TsFnType(fn_type) => {
                let params: Vec<TFnParam> = infer_fn_params(&fn_type.params, ctx)?;
                let ret = infer_ts_type_ann(&fn_type.type_ann.type_ann, ctx)?;

                let t = Type {
                    kind: TypeKind::Lam(types::TLam {
                        params,
                        ret: Box::from(ret),
                    }),
                };

                match &fn_type.type_params {
                    Some(type_param_decl) => util::replace_aliases(&t, type_param_decl, ctx),
                    None => Ok(t),
                }
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
                    Ok(Type {
                        kind: TypeKind::Ref(types::TRef {
                            name,
                            type_args: result.ok(),
                        }),
                    })
                }
                None => Ok(Type {
                    kind: TypeKind::Ref(types::TRef {
                        name,
                        type_args: None,
                    }),
                }),
            }
        }
        TsType::TsTypeQuery(_) => Err(String::from("can't parse type query yet")),
        TsType::TsTypeLit(_) => Err(String::from("can't parse type literal yet")),
        TsType::TsArrayType(array) => {
            let elem_type = infer_ts_type_ann(&array.elem_type, ctx)?;
            Ok(Type {
                kind: TypeKind::Array(Box::from(elem_type)),
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
                Ok(Type {
                    kind: TypeKind::Union(types?),
                })
            }
            TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                let types: Result<Vec<_>, String> = intersection
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(ts_type, ctx))
                    .collect();
                Ok(Type {
                    kind: TypeKind::Intersection(types?),
                })
            }
        },
        TsType::TsConditionalType(_) => Err(String::from("can't parse conditional type yet")),
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
                TsTypeOperatorOp::KeyOf => {
                    let type_ann = infer_ts_type_ann(type_ann, ctx)?;
                    Ok(Type {
                        kind: TypeKind::KeyOf(Box::from(type_ann)),
                    })
                }
                TsTypeOperatorOp::Unique => todo!(),
                TsTypeOperatorOp::ReadOnly => {
                    let type_ann = infer_ts_type_ann(type_ann, ctx)?;
                    Ok(type_ann)
                }
            }
        }
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

fn infer_fn_params(params: &[TsFnParam], ctx: &Context) -> Result<Vec<TFnParam>, String> {
    let params: Vec<TFnParam> = params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| match param {
            TsFnParam::Ident(ident) => {
                let type_ann = ident.type_ann.clone().unwrap();
                let param = TFnParam {
                    pat: TPat::Ident(common::BindingIdent {
                        name: ident.id.sym.to_string(),
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
                        arg: Box::from(TPat::Ident(common::BindingIdent { name })),
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

    let t = Type {
        kind: TypeKind::Lam(types::TLam {
            params,
            ret: Box::from(ret?),
        }),
    };

    let t = match &sig.type_params {
        Some(type_param_decl) => util::replace_aliases(&t, type_param_decl, ctx)?,
        None => t,
    };

    // TODO: maintain param names
    Ok(generalize(&HashMap::default(), &t))
}

fn get_key_name(key: &Expr) -> Result<String, String> {
    match key {
        Expr::Ident(Ident { sym, .. }) => Ok(sym.to_string()),
        Expr::Lit(swc_ecma_ast::Lit::Str(Str { value, .. })) => Ok(value.to_string()),
        _ => Err(format!("get_key_name: {key:#?}")),
    }
}

fn infer_ts_type_element(elem: &TsTypeElement, ctx: &Context) -> Result<TObjElem, String> {
    match elem {
        TsTypeElement::TsCallSignatureDecl(decl) => match &decl.type_ann {
            Some(type_ann) => {
                let params = infer_fn_params(&decl.params, ctx)?;
                let ret = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                let mut qualifiers = params.ftv();
                qualifiers.extend(ret.ftv());

                Ok(TObjElem::Call(TCallable {
                    params,
                    ret: Box::from(ret),
                    type_params: qualifiers.clone().into_iter().collect(),
                }))
            }
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsConstructSignatureDecl(decl) => match &decl.type_ann {
            Some(type_ann) => {
                let params = infer_fn_params(&decl.params, ctx)?;
                let ret = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                let mut qualifiers = params.ftv();
                qualifiers.extend(ret.ftv());

                Ok(TObjElem::Constructor(TCallable {
                    params,
                    ret: Box::from(ret),
                    type_params: qualifiers.clone().into_iter().collect(),
                }))
            }
            None => Err(String::from("Property is missing type annotation")),
        },
        TsTypeElement::TsPropertySignature(sig) => {
            match &sig.type_ann {
                Some(type_ann) => {
                    // TODO: do a better job of handling this
                    let t = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                    let gen_t = generalize(&HashMap::default(), &t);
                    let name = get_key_name(sig.key.as_ref())?;
                    Ok(TObjElem::Prop(TProp {
                        name,
                        optional: sig.optional,
                        mutable: !sig.readonly,
                        t: gen_t,
                    }))
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
            let gen_t = generalize(&HashMap::default(), &t);
            // println!("t = {t}, gen_t = {gen_t}");
            let name = get_key_name(sig.key.as_ref())?;
            Ok(TObjElem::Prop(TProp {
                name,
                optional: sig.optional,
                mutable: false, // All methods on interfaces are readonly
                t: gen_t,
            }))
        }
        TsTypeElement::TsIndexSignature(sig) => match &sig.type_ann {
            Some(type_ann) => {
                let t = infer_ts_type_ann(&type_ann.type_ann, ctx)?;
                let params = infer_fn_params(&sig.params, ctx)?;
                let key = params.get(0).unwrap();
                Ok(TObjElem::Index(TIndex {
                    key: key.to_owned(),
                    mutable: !sig.readonly,
                    t,
                }))
            }
            None => Err(String::from("Index is missing type annotation")),
        },
    }
}

fn infer_interface_decl(decl: &TsInterfaceDecl, ctx: &Context) -> Result<Type, String> {
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

    // TODO: make this generic if the decl has any type params
    let t = Type {
        kind: TypeKind::Object(TObject { elems }),
    };

    let t = match &decl.type_params {
        Some(type_param_decl) => util::replace_aliases(&t, type_param_decl, ctx)?,
        None => t,
    };

    let empty_s = Subst::default();
    Ok(close_over(&empty_s, &t, ctx))
}

impl Visit for InterfaceCollector {
    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        let name = decl.id.sym.to_string();
        println!("inferring: {name}");
        match infer_interface_decl(decl, &self.ctx) {
            Ok(t) => {
                match self.ctx.lookup_type(&name).ok() {
                    Some(existing_t) => {
                        let merged_t = util::merge_types(&existing_t, &t);
                        let merged_t = normalize(&merged_t, &self.ctx);
                        self.ctx.insert_type(name, merged_t)
                    }
                    None => self.ctx.insert_type(name, t),
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
