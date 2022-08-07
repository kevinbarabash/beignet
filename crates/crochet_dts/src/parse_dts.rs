use std::collections::HashMap;
use std::{path::Path, sync::Arc};
use memoize::memoize;

use swc_atoms::{js_word, JsWord};
use swc_common::{BytePos, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{error::Error, parse_file_as_module, Syntax, TsConfig};
use swc_ecma_visit::*;

#[derive(Debug, Clone)]
pub struct InterfaceCollector {
    names: HashMap<BytePos, JsWord>,
    current_interface: Option<JsWord>,
    array_methods: Vec<JsWord>,
}

impl Visit for InterfaceCollector {
    // call this if we don't want to visit TypeScript type nodes
    // noop_visit_type!();

    // fn visit_ident(&mut self, ident: &Ident) {
    //     self.names.insert(ident.span.lo, ident.sym.clone());
    // }

    fn visit_ts_interface_decl(&mut self, decl: &TsInterfaceDecl) {
        self.names.insert(decl.id.span.lo, decl.id.sym.clone());
        self.current_interface = Some(decl.id.sym.clone());
        decl.visit_children_with(self);
        self.current_interface = None;
    }

    fn visit_ts_type_element(&mut self, elem: &TsTypeElement) {
        if let Some(word) = &self.current_interface {
            if word == &js_word!("Array") {
                match elem {
                    TsTypeElement::TsCallSignatureDecl(_decl) => {}
                    TsTypeElement::TsConstructSignatureDecl(_decl) => {}
                    TsTypeElement::TsPropertySignature(_sig) => {}
                    TsTypeElement::TsGetterSignature(_sig) => {}
                    TsTypeElement::TsSetterSignature(_sig) => {}
                    TsTypeElement::TsMethodSignature(sig) => {
                        if sig.computed {
                            panic!("unexpected computed property in TypElement")
                        }
                        if let Expr::Ident(Ident {sym, ..}) = sig.key.as_ref() {
                            self.array_methods.push(sym.clone())
                        };
                        println!("key: {:#?}", sig.key.as_ref());
                        // TODO: convert sig.params to crochet type
                        for param in &sig.params {
                            match param {
                                TsFnParam::Ident(ident) => {
                                    let word = &ident.id.sym;
                                    let type_ann = ident.type_ann.clone().unwrap();
                                    println!("{word}: {:?}", type_ann);
                                },
                                TsFnParam::Array(_) => todo!(),
                                TsFnParam::Rest(_) => todo!(),
                                TsFnParam::Object(_) => todo!(),
                            }
                        }
                    }
                    TsTypeElement::TsIndexSignature(_sig) => {}
                }
            }
        }
    }
}

#[memoize]
pub fn parse_dts(dts: &'static str) -> Result<InterfaceCollector, Error> {
    let cm = Arc::<SourceMap>::default();
     let fm = cm
         .load_file(Path::new(dts))
         .expect("failed to load file");

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

    let mut collector = InterfaceCollector {
        names: Default::default(),
        current_interface: None,
        array_methods: vec![],
    };
    module.visit_with(&mut collector);

    Ok(collector)
}
