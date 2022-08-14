use defaultmap::*;
use std::collections::HashSet;
use std::sync::Arc;

use swc_common::{FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

// TODO: have crochet_infer re-export Lit
use crochet_ast::{Lit, Primitive};
use crochet_infer::{types, types::FnParam, types::Scheme, Context, TProp, Type};

type Interface = Vec<TProp>;

#[derive(Debug, Clone)]
pub struct InterfaceCollector {
    pub ctx: Context,
    pub current_interface: Option<String>,
    pub to_parse: HashSet<String>,
    pub interfaces: DefaultHashMap<String, Interface>,
}

impl InterfaceCollector {
    pub fn get_interface(&self, name: &str) -> Type {
        let props = &self.interfaces[name.to_owned()];
        self.ctx.object(props.to_owned())
    }
}

fn infer_ts_type_ann(type_ann: &TsType, ctx: &Context) -> Type {
    match type_ann {
        TsType::TsKeywordType(keyword) => match &keyword.kind {
            TsKeywordTypeKind::TsAnyKeyword => ctx.fresh_var(),
            TsKeywordTypeKind::TsUnknownKeyword => todo!(),
            TsKeywordTypeKind::TsNumberKeyword => ctx.prim(Primitive::Num),
            TsKeywordTypeKind::TsObjectKeyword => todo!(),
            TsKeywordTypeKind::TsBooleanKeyword => ctx.prim(Primitive::Bool),
            TsKeywordTypeKind::TsBigIntKeyword => todo!(),
            TsKeywordTypeKind::TsStringKeyword => ctx.prim(Primitive::Str),
            TsKeywordTypeKind::TsSymbolKeyword => todo!(),
            TsKeywordTypeKind::TsVoidKeyword => todo!(),
            TsKeywordTypeKind::TsUndefinedKeyword => ctx.prim(Primitive::Undefined),
            TsKeywordTypeKind::TsNullKeyword => ctx.prim(Primitive::Null),
            TsKeywordTypeKind::TsNeverKeyword => todo!(),
            TsKeywordTypeKind::TsIntrinsicKeyword => todo!(),
        },
        TsType::TsThisType(_) => todo!(),
        TsType::TsFnOrConstructorType(fn_or_constructor) => match &fn_or_constructor {
            TsFnOrConstructorType::TsFnType(fn_type) => {
                let params: Vec<FnParam> = fn_type
                    .params
                    .iter()
                    .enumerate()
                    .filter_map(|(index, param)| match param {
                        TsFnParam::Ident(ident) => {
                            let type_ann = ident.type_ann.clone().unwrap();
                            let param = FnParam::Ident(types::BindingIdent {
                                name: ident.id.sym.to_string(),
                                optional: ident.optional,
                                mutable: false,
                                ty: infer_ts_type_ann(&type_ann.type_ann, ctx),
                            });
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
                            // TODO: replace this with FnParam::Rest
                            let param = FnParam::Ident(types::BindingIdent {
                                name,
                                optional: false,
                                mutable: false,
                                ty: infer_ts_type_ann(&type_ann.type_ann, ctx),
                            });
                            Some(param)
                        }
                        TsFnParam::Object(_) => {
                            println!("skipping TsFnParam::Object(_)");
                            None
                        }
                    })
                    .collect();
                let ret = infer_ts_type_ann(&fn_type.type_ann.type_ann, ctx);
                ctx.lam(params, Box::from(ret))
            }
            TsFnOrConstructorType::TsConstructorType(_) => todo!(),
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
            let type_params = ref_type.type_params.as_ref().map(|type_params| {
                type_params
                    .params
                    .iter()
                    .map(|t| infer_ts_type_ann(t, ctx))
                    .collect()
            });
            ctx.alias(&name, type_params)
        }
        TsType::TsTypeQuery(_) => todo!(),
        TsType::TsTypeLit(_) => todo!(),
        TsType::TsArrayType(array) => {
            let elem_type = infer_ts_type_ann(&array.elem_type, ctx);
            ctx.array(elem_type)
        }
        TsType::TsTupleType(_) => todo!(),
        TsType::TsOptionalType(_) => todo!(),
        TsType::TsRestType(_) => todo!(),
        TsType::TsUnionOrIntersectionType(union_or_intersection) => match union_or_intersection {
            TsUnionOrIntersectionType::TsUnionType(union) => {
                let types: Vec<_> = union
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(ts_type, ctx))
                    .collect();
                ctx.union(types)
            }
            TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                let types: Vec<_> = intersection
                    .types
                    .iter()
                    .map(|ts_type| infer_ts_type_ann(ts_type, ctx))
                    .collect();
                ctx.intersection(types)
            }
        },
        TsType::TsConditionalType(_) => todo!(),
        TsType::TsInferType(_) => todo!(),
        TsType::TsParenthesizedType(_) => todo!(),
        TsType::TsTypeOperator(_) => todo!(),
        TsType::TsIndexedAccessType(_) => todo!(),
        TsType::TsMappedType(_) => todo!(),
        TsType::TsLitType(lit) => match &lit.lit {
            TsLit::Number(num) => ctx.lit(Lit::num(format!("{}", num.value), 0..0)),
            TsLit::Str(str) => ctx.lit(Lit::str(str.value.to_string(), 0..0)),
            TsLit::Bool(b) => ctx.lit(Lit::bool(b.value, 0..0)),
            TsLit::BigInt(_) => todo!(),
            TsLit::Tpl(_) => todo!(),
        },
        TsType::TsTypePredicate(_) => todo!(),
        TsType::TsImportType(_) => todo!(),
    }
}

impl Visit for InterfaceCollector {
    // call this if we don't want to visit TypeScript type nodes
    // noop_visit_type!();

    // fn visit_ident(&mut self, ident: &Ident) {
    //     self.names.insert(ident.span.lo, ident.sym.clone());
    // }

    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        self.current_interface = Some(decl.id.sym.to_string());
        decl.visit_children_with(self);
        self.current_interface = None;
    }

    fn visit_ts_type_element(&mut self, elem: &TsTypeElement) {
        let name = if let Some(name) = &self.current_interface {
            if !self.to_parse.contains(name) {
                return;
            } else {
                name.clone()
            }
        } else {
            // This can happen when visiting types like Awaited<T>
            // that aren't interfaces
            return;
        };
        match elem {
            TsTypeElement::TsCallSignatureDecl(_decl) => {}
            TsTypeElement::TsConstructSignatureDecl(_decl) => {}
            TsTypeElement::TsPropertySignature(sig) => {
                // TODO: update TProp to include a readonly flag
                match &sig.type_ann {
                    Some(type_ann) => {
                        let t = infer_ts_type_ann(&type_ann.type_ann, &self.ctx);
                        if let Expr::Ident(Ident { sym, .. }) = sig.key.as_ref() {
                            let prop = TProp {
                                name: sym.to_string(),
                                optional: sig.optional,
                                mutable: !sig.readonly,
                                ty: t,
                            };
                            self.interfaces[name].push(prop);
                        }
                    }
                    None => panic!("Property is missing type annotation"),
                }
            }
            TsTypeElement::TsGetterSignature(_sig) => {}
            TsTypeElement::TsSetterSignature(_sig) => {}
            TsTypeElement::TsMethodSignature(sig) => {
                let t = self.infer_method_sig(sig);
                if let Expr::Ident(Ident { sym, .. }) = sig.key.as_ref() {
                    let prop = TProp {
                        name: sym.to_string(),
                        optional: sig.optional,
                        mutable: false, // All methods on interfaces are readonly
                        ty: t,
                    };
                    self.interfaces[name].push(prop);
                }
            }
            TsTypeElement::TsIndexSignature(_sig) => {}
        }
    }
}

impl InterfaceCollector {
    fn infer_method_sig(&mut self, sig: &TsMethodSignature) -> Type {
        if sig.computed {
            panic!("unexpected computed property in TypElement")
        }

        let params: Vec<FnParam> = sig
            .params
            .iter()
            .enumerate()
            .filter_map(|(index, param)| match param {
                TsFnParam::Ident(ident) => {
                    let type_ann = ident.type_ann.clone().unwrap();
                    let param = FnParam::Ident(types::BindingIdent {
                        name: ident.id.sym.to_string(),
                        optional: ident.optional,
                        mutable: false,
                        ty: infer_ts_type_ann(&type_ann.type_ann, &self.ctx),
                    });
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
                    // TODO: replace this with FnParam::Rest
                    let param = FnParam::Ident(types::BindingIdent {
                        name,
                        optional: false,
                        mutable: false,
                        ty: infer_ts_type_ann(&type_ann.type_ann, &self.ctx),
                    });
                    Some(param)
                }
                TsFnParam::Object(_) => {
                    println!("skipping TsFnParam::Object(_)");
                    None
                }
            })
            .collect();
        let ret = match &sig.type_ann {
            Some(type_ann) => infer_ts_type_ann(&type_ann.type_ann, &self.ctx),
            None => panic!("method has no return type"),
        };
        // TODO: maintain param names
        self.ctx.lam(params, Box::from(ret))
    }
}

pub fn parse_dts(d_ts_source: &str) -> Result<Context, Error> {
    let cm = Arc::<SourceMap>::default();
    let fm = cm.new_source_file(FileName::Anon, d_ts_source.to_owned());

    let mut errors: Vec<Error> = vec![];

    let module = parse_file_as_module(
        &fm,
        Syntax::Typescript(TsConfig {
            tsx: false,
            dts: true,
            decorators: false,
            no_early_errors: false,
        }),
        EsVersion::Es2020,
        None, // comments
        &mut errors,
    )?;

    // TODO: add more items from lib.es5.d.ts
    let to_parse = HashSet::from([
        String::from("Math"),
        String::from("String"),
        String::from("StringConstructor"),
        String::from("Number"),
        String::from("NumberConstructor"),
        String::from("Boolean"),
        String::from("BooleanConstructor"),
    ]);
    let mut collector = InterfaceCollector {
        ctx: Context::default(),
        current_interface: None,
        to_parse,
        interfaces: defaulthashmap!(),
    };
    module.visit_with(&mut collector);

    for name in collector.interfaces.keys() {
        let t = collector.get_interface(name);
        collector.ctx.types.insert(name.to_owned(), Scheme::from(t));
    }

    Ok(collector.ctx)
}
