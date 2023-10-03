use generational_arena::Index;

use escalier_ast::{self as syntax, *};

use crate::checker::Checker;
use crate::context::*;
use crate::infer_pattern::pattern_to_tpat;
use crate::type_error::TypeError;
use crate::types::{self, *};

impl Checker {
    pub fn infer_class(
        &mut self,
        class: &mut Class,
        ctx: &mut Context,
    ) -> Result<Index, TypeError> {
        let interface = self.infer_class_interface(class, ctx)?;

        eprintln!("interface = {:?}", interface);

        let mut _static_elems: Vec<TObjElem> = vec![];
        let mut _instance_elems: Vec<TObjElem> = vec![];

        for member in &mut class.body {
            match member {
                ClassMember::Method(_) => todo!(),
                ClassMember::Getter(_) => todo!(),
                ClassMember::Setter(_) => todo!(),
                ClassMember::Constructor(_) => todo!(),
                ClassMember::Field(_) => todo!(),
            }
        }

        todo!()
    }

    fn infer_class_interface(
        &mut self,
        class: &mut Class,
        ctx: &mut Context,
    ) -> Result<(Scheme, Index), TypeError> {
        let mut instance_elems: Vec<TObjElem> = vec![];
        let mut static_elems: Vec<TObjElem> = vec![];

        // TODO:
        // - add `Self` type to `sig_ctx` so that it can be referenced in
        //   type annotations
        // - `Self` should expand to whatever the class name is + type params

        for member in &mut class.body {
            match member {
                // Constructors are part of statics and thus not part of the interface
                ClassMember::Constructor(Constructor {
                    span: _,
                    is_public: _,
                    params,
                    body: _,
                }) => {
                    let mut sig_ctx = ctx.clone();

                    let func_params = self.infer_func_params(params, &mut sig_ctx)?;

                    let ret = self.new_type_ref("Self", &[]);

                    let constructor = TObjElem::Constructor(types::Function {
                        params: func_params,
                        ret,
                        type_params: None,
                        throws: None, // TODO: constructors can throw
                    });

                    static_elems.push(constructor);
                }
                // TODO: update Method {} to contain `name` and `function` fields
                // so that we can reuse some of the logic around function inference
                ClassMember::Method(Method {
                    span: _,
                    name,
                    is_public: _,
                    is_async: _, // return type is a promise
                    is_gen: _,   // return type is a generator
                    is_mutating,
                    type_params,
                    params,
                    body: _,
                    type_ann: return_type,
                }) => {
                    let mut sig_ctx = ctx.clone();

                    let type_params = self.infer_type_params(type_params, &mut sig_ctx)?;
                    let func_params = self.infer_func_params(params, &mut sig_ctx)?;

                    let ret = match return_type {
                        Some(return_type) => self.infer_type_ann(return_type, &mut sig_ctx)?,
                        None => self.new_type_var(None),
                    };

                    let name: TPropKey = match name {
                        PropName::Ident(Ident { name, span: _ }) => {
                            TPropKey::StringKey(name.to_string())
                        }
                        PropName::Computed(_) => todo!(),
                    };
                    let method = TObjElem::Method(TMethod {
                        name,
                        // replace with `fucntion: Fucntion` - START>
                        type_params,
                        params: func_params,
                        ret,
                        throws: None, // TODO: methods can throw
                        // <END - replace with `fucntion: Fucntion`
                        mutates: *is_mutating,
                    });

                    // TODO:
                    // - handle static types
                    // - filter out `self` from params
                    instance_elems.push(method);
                }
                ClassMember::Getter(_) => todo!(),
                ClassMember::Setter(_) => todo!(),
                ClassMember::Field(_) => todo!(),
            }
        }

        let instance_scheme = Scheme {
            t: self.new_object_type(&instance_elems),
            // TODO: add type params
            // I don't think this is something that can be inferred, by
            // default, each function gets its own type params
            type_params: None,
        };

        let static_type = self.new_object_type(&static_elems);

        // TODO: How do we keep track of the relationship between these two?
        Ok((instance_scheme, static_type))
    }

    fn infer_func_params(
        &mut self,
        params: &mut [syntax::FuncParam],
        sig_ctx: &mut Context,
    ) -> Result<Vec<types::FuncParam>, TypeError> {
        let mut func_params: Vec<types::FuncParam> = vec![];

        for param in params.iter_mut() {
            let type_ann_t = match &mut param.type_ann {
                Some(type_ann) => self.infer_type_ann(type_ann, sig_ctx)?,
                None => self.new_type_var(None),
            };
            param.pattern.inferred_type = Some(type_ann_t);

            let (assumps, param_t) = self.infer_pattern(&mut param.pattern, sig_ctx)?;
            self.unify(sig_ctx, param_t, type_ann_t)?;

            for (name, binding) in assumps {
                sig_ctx.non_generic.insert(binding.index);
                sig_ctx.values.insert(name.to_owned(), binding);
            }

            func_params.push(types::FuncParam {
                pattern: pattern_to_tpat(&param.pattern, true),
                t: type_ann_t,
                optional: param.optional,
            });
        }

        Ok(func_params)
    }
}
