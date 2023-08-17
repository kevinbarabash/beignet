use generational_arena::Index;
use std::collections::HashMap;

use escalier_ast::{self as ast, *};

use crate::checker::Checker;
use crate::context::{Binding, Context};
use crate::errors::*;
use crate::types::{self, *};

// #[derive(Clone, Debug)]
// pub struct Binding {
//     pub mutable: bool,
//     pub t: Index,
// }

type Assump = HashMap<String, Binding>;

impl Checker {
    // TODO: Use a Folder for this.
    pub fn infer_pattern(
        &mut self,
        pattern: &mut Pattern,
        ctx: &Context,
    ) -> Result<(Assump, Index), Errors> {
        fn infer_pattern_rec(
            checker: &mut Checker,
            pattern: &mut Pattern,
            assump: &mut Assump,
            ctx: &Context,
        ) -> Result<Index, Errors> {
            let t = match &mut pattern.kind {
                PatternKind::Ident(BindingIdent { name, mutable, .. }) => {
                    let t = new_var_type(&mut checker.arena, None);
                    if assump
                        .insert(
                            name.to_owned(),
                            Binding {
                                index: t,
                                is_mut: *mutable,
                            },
                        )
                        .is_some()
                    {
                        return Err(Errors::InferenceError(
                            "Duplicate identifier in pattern".to_string(),
                        ));
                    }
                    t
                }
                PatternKind::Rest(ast::RestPat { arg }) => {
                    let arg_type = infer_pattern_rec(checker, arg.as_mut(), assump, ctx)?;
                    new_rest_type(&mut checker.arena, arg_type)
                }
                PatternKind::Object(ObjectPat { props, .. }) => {
                    let mut rest_opt_ty: Option<Index> = None;
                    let mut elems: Vec<types::TObjElem> = vec![];

                    for prop in props.iter_mut() {
                        match prop {
                            // re-assignment, e.g. {x: new_x, y: new_y} = point
                            ObjectPatProp::KeyValue(KeyValuePatProp { key, value, .. }) => {
                                // We ignore `init` for now, we can come back later to handle
                                // default values.
                                // TODO: handle default values

                                // TODO: bubble the error up from infer_patter_rec() if there is one.
                                let value_type =
                                    infer_pattern_rec(checker, value.as_mut(), assump, ctx)?;

                                elems.push(types::TObjElem::Prop(types::TProp {
                                    name: TPropKey::StringKey(key.name.to_owned()),
                                    modifier: None,
                                    optional: false,
                                    mutable: false,
                                    t: value_type,
                                }))
                            }
                            ObjectPatProp::Shorthand(ShorthandPatProp { ident, .. }) => {
                                // We ignore `init` for now, we can come back later to handle
                                // default values.
                                // TODO: handle default values

                                let t = new_var_type(&mut checker.arena, None);
                                if assump
                                    .insert(
                                        ident.name.to_owned(),
                                        Binding {
                                            index: t,
                                            is_mut: false,
                                        },
                                    )
                                    .is_some()
                                {
                                    todo!("return an error");
                                }

                                elems.push(types::TObjElem::Prop(types::TProp {
                                    name: TPropKey::StringKey(ident.name.to_owned()),
                                    modifier: None,
                                    optional: false,
                                    mutable: false,
                                    t,
                                }))
                            }
                            ObjectPatProp::Rest(rest) => {
                                if rest_opt_ty.is_some() {
                                    return Err(Errors::InferenceError(
                                        "Maximum one rest pattern allowed in object patterns"
                                            .to_string(),
                                    ));
                                }
                                // TypeScript doesn't support spreading/rest in types so instead we
                                // do the following conversion:
                                // {x, y, ...rest} -> {x: A, y: B} & C
                                // TODO: bubble the error up from infer_patter_rec() if there is one.
                                rest_opt_ty =
                                    Some(infer_pattern_rec(checker, &mut rest.arg, assump, ctx)?);
                            }
                        }
                    }

                    let obj_type = new_object_type(&mut checker.arena, &elems);

                    match rest_opt_ty {
                        // TODO: Replace this with a proper Rest/Spread type
                        // See https://github.com/microsoft/TypeScript/issues/10727
                        Some(rest_ty) => {
                            new_intersection_type(&mut checker.arena, &[obj_type, rest_ty])
                        }
                        None => obj_type,
                    }
                }
                PatternKind::Tuple(ast::TuplePat { elems, optional: _ }) => {
                    let mut elem_types = vec![];
                    for elem in elems.iter_mut() {
                        let t = match elem {
                            Some(elem) => {
                                // TODO:
                                // - handle elem.init
                                // - check for multiple rest patterns
                                infer_pattern_rec(checker, &mut elem.pattern, assump, ctx)?
                            }
                            None => new_keyword(&mut checker.arena, Keyword::Undefined),
                        };
                        elem_types.push(t);
                    }

                    new_tuple_type(&mut checker.arena, &elem_types)
                }
                PatternKind::Lit(LitPat { lit }) => new_lit_type(&mut checker.arena, lit),
                PatternKind::Is(IsPat { ident, is_id }) => {
                    let t = match is_id.name.as_str() {
                        "number" => new_primitive(&mut checker.arena, Primitive::Number),
                        "string" => new_primitive(&mut checker.arena, Primitive::String),
                        "boolean" => new_primitive(&mut checker.arena, Primitive::Boolean),
                        name => checker.get_type(name, ctx)?,
                    };

                    assump.insert(
                        ident.name.to_owned(),
                        Binding {
                            index: t,
                            is_mut: false,
                        },
                    );

                    t
                }
                PatternKind::Wildcard => new_var_type(&mut checker.arena, None),
            };

            Ok(t)
        }

        let mut assump = Assump::default();
        let pat_type = infer_pattern_rec(self, pattern, &mut assump, ctx)?;

        Ok((assump, pat_type))
    }
}

pub fn pattern_to_tpat(pattern: &Pattern, is_func_param: bool) -> TPat {
    match &pattern.kind {
        PatternKind::Ident(binding_ident) => TPat::Ident(ast::BindingIdent {
            name: binding_ident.name.to_owned(),
            mutable: binding_ident.mutable.to_owned(),
            span: Span { start: 0, end: 0 },
        }),
        PatternKind::Rest(e_rest) => TPat::Rest(types::RestPat {
            arg: Box::from(pattern_to_tpat(e_rest.arg.as_ref(), is_func_param)),
        }),
        PatternKind::Object(e_obj) => {
            // TODO: replace TProp with the type equivalent of EFnParamObjectPatProp
            let props: Vec<types::TObjectPatProp> = e_obj
                .props
                .iter()
                .map(|e_prop| {
                    match e_prop {
                        ObjectPatProp::KeyValue(kv) => {
                            types::TObjectPatProp::KeyValue(types::TObjectKeyValuePatProp {
                                key: kv.key.name.to_owned(),
                                value: pattern_to_tpat(&kv.value, is_func_param),
                            })
                        }
                        ObjectPatProp::Shorthand(ShorthandPatProp { ident, .. }) => {
                            types::TObjectPatProp::Assign(types::TObjectAssignPatProp {
                                key: ident.name.to_owned(),
                                // TODO: figure when/how to set this to a non-None value
                                value: None,
                            })
                        }
                        ObjectPatProp::Rest(rest) => types::TObjectPatProp::Rest(types::RestPat {
                            arg: Box::from(pattern_to_tpat(rest.arg.as_ref(), is_func_param)),
                        }),
                    }
                })
                .collect();
            TPat::Object(types::TObjectPat { props })
        }
        PatternKind::Tuple(e_array) => {
            TPat::Tuple(types::TuplePat {
                // TODO: fill in gaps in array patterns with types from the corresponding
                // type annotation if one exists.
                elems: e_array
                    .elems
                    .iter()
                    .map(|elem| {
                        elem.as_ref()
                            .map(|elem| pattern_to_tpat(&elem.pattern, is_func_param))
                    })
                    .collect(),
            })
        }
        PatternKind::Lit(LitPat { lit }) => {
            if is_func_param {
                panic!("Literal patterns not allowed in function params")
            } else {
                TPat::Lit(TLitPat {
                    lit: lit.to_owned(),
                })
            }
        }
        PatternKind::Is(IsPat { ident, is_id }) => {
            if is_func_param {
                panic!("'is' patterns not allowed in function params")
            } else {
                TPat::Is(TIsPat {
                    ident: ident.name.to_owned(),
                    is_id: is_id.name.to_owned(),
                })
            }
        }
        PatternKind::Wildcard => {
            if is_func_param {
                panic!("Wildcard patterns not allowed in function params")
            } else {
                TPat::Wildcard
            }
        }
    }
}
