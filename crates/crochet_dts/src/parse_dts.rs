use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use types::{TIndex, TLam, TObjElem, TObject};

use swc_common::{comments::SingleThreadedComments, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

// TODO: have crochet_infer re-export Lit
use crochet_ast::Lit;
use crochet_infer::{
    close_over, generalize_type, get_type_params, normalize, Context, Env, Subst, Substitutable,
};
use crochet_types::{self as types, RestPat, TFnParam, TKeyword, TPat, TPrim, TProp, Type};

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
            TsKeywordTypeKind::TsUnknownKeyword => Ok(ctx.fresh_var()),
            TsKeywordTypeKind::TsNumberKeyword => Ok(Type::Prim(TPrim::Num)),
            TsKeywordTypeKind::TsObjectKeyword => Err(String::from("can't parse Objects yet")),
            TsKeywordTypeKind::TsBooleanKeyword => Ok(Type::Prim(TPrim::Bool)),
            TsKeywordTypeKind::TsBigIntKeyword => Err(String::from("can't parse BigInt yet")),
            TsKeywordTypeKind::TsStringKeyword => Ok(Type::Prim(TPrim::Str)),
            TsKeywordTypeKind::TsSymbolKeyword => Ok(Type::Keyword(TKeyword::Symbol)),
            // NOTE: `void` is treated the same as `undefined` ...for now.
            TsKeywordTypeKind::TsVoidKeyword => Ok(Type::Keyword(TKeyword::Undefined)),
            TsKeywordTypeKind::TsUndefinedKeyword => Ok(Type::Keyword(TKeyword::Undefined)),
            TsKeywordTypeKind::TsNullKeyword => Ok(Type::Keyword(TKeyword::Null)),
            TsKeywordTypeKind::TsNeverKeyword => Err(String::from("can't parse never keyword yet")),
            TsKeywordTypeKind::TsIntrinsicKeyword => {
                Err(String::from("can't parse Intrinsics yet"))
            }
        },
        TsType::TsThisType(_) => Ok(Type::This),
        TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
            TsFnOrConstructorType::TsFnType(fn_type) => {
                let params: Vec<TFnParam> = infer_fn_params(&fn_type.params, ctx)?;
                let ret = infer_ts_type_ann(&fn_type.type_ann.type_ann, ctx)?;

                match &fn_type.type_params {
                    Some(type_param_decl) => {
                        let mut type_params: Vec<i32> = vec![];
                        let type_param_map: HashMap<String, Type> = type_param_decl
                            .params
                            .iter()
                            .map(|tp| {
                                let id = ctx.fresh_id();
                                type_params.push(id);
                                (tp.name.sym.to_string(), Type::Var(id))
                            })
                            .collect();

                        let t = Type::Lam(types::TLam {
                            params,
                            ret: Box::from(ret),
                            type_params,
                        });

                        Ok(replace_aliases(&t, &type_param_map))
                    }
                    None => {
                        let t = Type::Lam(types::TLam {
                            params,
                            ret: Box::from(ret),
                            type_params: vec![],
                        });
                        Ok(t)
                    }
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
                    Ok(Type::Alias(types::TAlias {
                        name,
                        type_args: result.ok(),
                    }))
                }
                None => Ok(Type::Alias(types::TAlias {
                    name,
                    type_args: None,
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

    // TODO: maintain param names
    Ok(Type::Lam(types::TLam {
        params,
        ret: Box::from(ret?),
        // TODO: handle generic method signatures
        type_params: vec![],
    }))
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
                let mut qualifiers: HashSet<_> = params.ftv();
                qualifiers.extend(ret.ftv());

                Ok(TObjElem::Call(TLam {
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
                let mut qualifiers: HashSet<_> = params.ftv();
                qualifiers.extend(ret.ftv());

                Ok(TObjElem::Constructor(TLam {
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
                    let gen_t = generalize_type(&HashMap::default(), &t);
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
            let gen_t = generalize_type(&HashMap::default(), &t);
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
                    // TODO: handle generic indexers
                    type_params: vec![],
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

    let t = Type::Object(TObject {
        elems,
        // TODO: parse type params on interfaces
        type_params: vec![],
    });

    let scheme = match &decl.type_params {
        Some(type_params) => {
            let type_param_map: HashMap<String, Type> = type_params
                .params
                .iter()
                .map(|tp| (tp.name.sym.to_string(), ctx.fresh_var()))
                .collect();

            let t = replace_aliases(&t, &type_param_map);
            generalize_type(&HashMap::default(), &t)
        }
        None => {
            let empty_s = Subst::default();
            close_over(&empty_s, &t, ctx)
        }
    };

    Ok(scheme)
}

fn replace_aliases(t: &Type, map: &HashMap<String, Type>) -> Type {
    match t {
        Type::Var(_) => t.to_owned(),
        Type::App(types::TApp { args, ret }) => Type::App(types::TApp {
            args: args.iter().map(|t| replace_aliases(t, map)).collect(),
            ret: Box::from(replace_aliases(ret, map)),
        }),
        Type::Lam(types::TLam {
            params,
            ret,
            type_params,
        }) => Type::Lam(types::TLam {
            params: params
                .iter()
                .map(|param| TFnParam {
                    t: replace_aliases(&param.t, map),
                    ..param.to_owned()
                })
                .collect(),
            ret: Box::from(replace_aliases(ret, map)),
            type_params: type_params.to_owned(),
        }),
        Type::Prim(_) => t.to_owned(),
        Type::Lit(_) => t.to_owned(),
        Type::Keyword(_) => t.to_owned(),
        Type::Union(types) => Type::Union(types.iter().map(|t| replace_aliases(t, map)).collect()),
        Type::Intersection(types) => {
            Type::Intersection(types.iter().map(|t| replace_aliases(t, map)).collect())
        }
        Type::Object(obj) => {
            let elems: Vec<TObjElem> = obj
                .elems
                .iter()
                .map(|elem| {
                    match elem {
                        TObjElem::Call(lam) => {
                            let params: Vec<TFnParam> = lam
                                .params
                                .iter()
                                .map(|t| TFnParam {
                                    t: replace_aliases(&t.t, map),
                                    ..t.to_owned()
                                })
                                .collect();
                            let ret = replace_aliases(lam.ret.as_ref(), map);

                            TObjElem::Call(TLam {
                                params,
                                ret: Box::from(ret),
                                type_params: lam.type_params.to_owned(),
                            })
                        }
                        TObjElem::Constructor(lam) => {
                            let params: Vec<TFnParam> = lam
                                .params
                                .iter()
                                .map(|t| TFnParam {
                                    t: replace_aliases(&t.t, map),
                                    ..t.to_owned()
                                })
                                .collect();
                            let ret = replace_aliases(lam.ret.as_ref(), map);

                            TObjElem::Constructor(TLam {
                                params,
                                ret: Box::from(ret),
                                type_params: lam.type_params.to_owned(),
                            })
                        }
                        TObjElem::Index(index) => {
                            // TODO: handle generic indexers
                            let t = replace_aliases(&index.t, map);
                            TObjElem::Index(types::TIndex {
                                t,
                                ..index.to_owned()
                            })
                        }
                        TObjElem::Prop(prop) => {
                            // TODO: handle generic properties
                            let t = replace_aliases(&prop.t, map);
                            TObjElem::Prop(types::TProp {
                                t,
                                ..prop.to_owned()
                            })
                        }
                    }
                })
                .collect();
            Type::Object(TObject {
                elems,
                ..obj.to_owned()
            })
        }
        Type::Alias(alias) => match map.get(&alias.name) {
            Some(replacement) => replacement.to_owned(),
            None => t.to_owned(),
        },
        Type::Tuple(types) => Type::Tuple(types.iter().map(|t| replace_aliases(t, map)).collect()),
        Type::Array(t) => Type::Array(Box::from(replace_aliases(t, map))),
        Type::Rest(t) => Type::Rest(Box::from(replace_aliases(t, map))),
        Type::This => Type::This,
    }
}

fn merge_types(t1: &Type, t2: &Type) -> Type {
    let tp1 = get_type_params(t1);
    let tp2 = get_type_params(t2);

    if tp1.len() != tp2.len() {
        panic!("Mismatch in type param count when attempting to merge type");
    }

    // Creates a mapping from type params in t2 to those in t1
    let subs: Subst = tp2
        .into_iter()
        .zip(tp1.iter().map(|id| Type::Var(*id)))
        .collect();

    // Updates type variables for type params to match t1
    let t2 = t2.apply(&subs);

    match (t1, t2) {
        (Type::Object(obj1), Type::Object(obj2)) => {
            let elems: Vec<_> = obj1
                .elems
                .iter()
                .cloned()
                .chain(obj2.elems.iter().cloned())
                .collect();

            Type::Object(TObject {
                elems,
                type_params: obj1.type_params.to_owned(),
            })
        }
        (_, _) => todo!(),
    }
}

impl Visit for InterfaceCollector {
    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        let name = decl.id.sym.to_string();
        println!("inferring: {name}");
        match infer_interface_decl(decl, &self.ctx) {
            Ok(t) => {
                match self.ctx.lookup_type(&name).ok() {
                    Some(existing_t) => {
                        let merged_t = merge_types(&existing_t, &t);
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
                            let t = generalize_type(&Env::new(), &t);
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
