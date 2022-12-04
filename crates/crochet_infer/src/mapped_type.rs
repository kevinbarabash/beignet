use crochet_ast::types::{
    BindingIdent, TFnParam, TIndex, TIndexAccess, TMappedType, TObjElem, TObject, TPat, TProp,
    Type, TypeKind,
};
use error_stack::{Report, Result};
use std::collections::HashMap;

use crate::context::Context;
use crate::key_of::key_of;
use crate::type_error::TypeError;
use crate::util::replace_aliases_rec;

// unwraps `t` recursively until it finds an object type or something that is
// definitely not an object type
// TODO: update `infer_property_type` to use this helper function
fn unwrap_obj_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    let t = match &t.kind {
        TypeKind::Var(_) => todo!(),
        TypeKind::App(_) => todo!(),
        TypeKind::Lam(_) => todo!(),
        TypeKind::Lit(_) => todo!(), // TODO: lookup Number, String, Boolean in Context
        TypeKind::Keyword(_) => todo!(), // TODO: lookup Number, String, Boolean in Context
        TypeKind::Union(_) => todo!(),
        TypeKind::Intersection(_) => todo!(),
        TypeKind::Object(_) => t.to_owned(),
        TypeKind::Ref(alias) => unwrap_obj_type(&ctx.lookup_ref_and_instantiate(alias)?, ctx)?,
        TypeKind::Tuple(_) => todo!(), // TODO: lookup Array in Context
        TypeKind::Array(_) => todo!(), // TODO: lookup Array in Context
        TypeKind::Rest(_) => todo!(),
        TypeKind::This => todo!(),
        TypeKind::KeyOf(_) => todo!(),
        TypeKind::IndexAccess(_) => todo!(),
        TypeKind::MappedType(_) => todo!(),
        TypeKind::ConditionalType(_) => todo!(),
        TypeKind::Generic(_) => todo!(),
    };
    Ok(t)
}

fn get_obj_type_from_mapped_type(mapped: &TMappedType, ctx: &Context) -> Result<Type, TypeError> {
    if let Some(constraint) = &mapped.type_param.constraint {
        if let TypeKind::KeyOf(t) = &constraint.kind {
            return unwrap_obj_type(t.as_ref(), ctx);
        }

        // TODO: look at constraint.provenence to see if it's a keyof type
    }

    if let TypeKind::IndexAccess(access) = &mapped.t.kind {
        return unwrap_obj_type(access.object.as_ref(), ctx);
    }

    Err(Report::new(TypeError::Unhandled))
}

fn get_prop(obj: &TObject, key: &str) -> Result<TProp, TypeError> {
    for elem in &obj.elems {
        if let TObjElem::Prop(prop) = elem {
            if prop.name == key {
                return Ok(prop.to_owned());
            }
        }
    }

    Err(Report::new(TypeError::MissingKey(key.to_owned())))
}

fn get_index(obj: &TObject, key_type: &Type) -> Result<TIndex, TypeError> {
    for elem in &obj.elems {
        if let TObjElem::Index(index) = elem {
            if &index.key.t == key_type {
                return Ok(index.to_owned());
            }
        }
    }

    Err(Report::new(TypeError::Unhandled))
}

fn computed_indexed_access(access: &TIndexAccess, ctx: &Context) -> Result<Type, TypeError> {
    let obj = unwrap_obj_type(access.object.as_ref(), ctx)?;
    let index = access.index.as_ref();

    match &index.kind {
        TypeKind::Lit(lit) => {
            let key = match lit {
                crochet_ast::types::TLit::Num(num) => num,
                crochet_ast::types::TLit::Bool(_) => {
                    return Err(Report::new(TypeError::InvalidIndex(
                        access.object.to_owned(),
                        access.index.to_owned(),
                    )));
                }
                crochet_ast::types::TLit::Str(str) => str,
            };

            if let TypeKind::Object(obj) = &obj.kind {
                let prop = get_prop(obj, key)?;
                return Ok(prop.t);
            }
        }
        TypeKind::Keyword(_) => {
            if let TypeKind::Object(obj) = &obj.kind {
                let index = get_index(obj, index)?;
                return Ok(index.t);
            }
        }
        _ => {
            return Err(Report::new(TypeError::InvalidIndex(
                access.object.to_owned(),
                access.index.to_owned(),
            )));
        }
    };

    Err(Report::new(TypeError::Unhandled))
}

fn get_prop_by_name(elems: &[TObjElem], name: &str) -> Result<TProp, TypeError> {
    for elem in elems {
        match elem {
            TObjElem::Call(_) => (),
            TObjElem::Constructor(_) => (),
            TObjElem::Index(_) => (),
            TObjElem::Prop(prop) => {
                if prop.name == name {
                    return Ok(prop.to_owned());
                }
            }
        }
    }

    Err(Report::new(TypeError::MissingKey(name.to_owned())))
}

pub fn compute_mapped_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            compute_mapped_type(&t, ctx)
        }
        TypeKind::MappedType(mapped) => {
            let constraint = mapped.type_param.constraint.as_ref().unwrap();
            let keys = match &constraint.kind {
                TypeKind::KeyOf(_) => key_of(constraint, ctx)?,
                _ => constraint.as_ref().to_owned(),
            };
            let obj = get_obj_type_from_mapped_type(mapped, ctx)?;

            let elems = match &obj.kind {
                TypeKind::Object(TObject { elems }) => elems.to_owned(),
                _ => vec![],
            };

            let keys = match &keys.kind {
                TypeKind::Union(keys) => keys.to_owned(),
                _ => vec![keys],
            };

            // keys is either a union of all the prop keys/indexer types or
            // is a single prop key/indexer type.
            let elems = keys
                .iter()
                .map(|key| {
                    let value = mapped.t.clone();

                    // NOTE: This assumes that `key` has already been expanded
                    // TODO: Add a call to expand_type here so that we don't have
                    // rely on this assumption
                    let mut type_arg_map: HashMap<String, Type> = HashMap::default();
                    let name = mapped.type_param.name.to_owned();
                    type_arg_map.insert(name, key.to_owned());
                    let value = replace_aliases_rec(value.as_ref(), &type_arg_map);

                    let value = match &value.kind {
                        TypeKind::IndexAccess(access) => computed_indexed_access(access, ctx)?,
                        _ => value,
                    };

                    match &key.kind {
                        TypeKind::Lit(lit) => match lit {
                            crochet_ast::types::TLit::Num(name) => {
                                let prop = get_prop_by_name(&elems, name)?;
                                let optional = match &mapped.optional {
                                    Some(change) => match change {
                                        crochet_ast::types::TMappedTypeChangeProp::Plus => true,
                                        crochet_ast::types::TMappedTypeChangeProp::Minus => false,
                                    },
                                    None => prop.optional,
                                };
                                let mutable = match &mapped.mutable {
                                    Some(change) => match change {
                                        crochet_ast::types::TMappedTypeChangeProp::Plus => true,
                                        crochet_ast::types::TMappedTypeChangeProp::Minus => false,
                                    },
                                    None => prop.mutable,
                                };
                                Ok(TObjElem::Prop(TProp {
                                    name: name.to_owned(),
                                    optional,
                                    mutable,
                                    t: value,
                                }))
                            }
                            crochet_ast::types::TLit::Bool(_) => {
                                Err(Report::new(TypeError::Unhandled))
                            }
                            crochet_ast::types::TLit::Str(name) => {
                                let prop = get_prop_by_name(&elems, name)?;
                                let optional = match &mapped.optional {
                                    Some(change) => match change {
                                        crochet_ast::types::TMappedTypeChangeProp::Plus => true,
                                        crochet_ast::types::TMappedTypeChangeProp::Minus => false,
                                    },
                                    None => prop.optional,
                                };
                                let mutable = match &mapped.mutable {
                                    Some(change) => match change {
                                        crochet_ast::types::TMappedTypeChangeProp::Plus => true,
                                        crochet_ast::types::TMappedTypeChangeProp::Minus => false,
                                    },
                                    None => prop.mutable,
                                };
                                Ok(TObjElem::Prop(TProp {
                                    name: name.to_owned(),
                                    optional,
                                    mutable,
                                    t: value,
                                }))
                            }
                        },
                        // TODO: get indexer(s), you can mix symbol + number
                        // OR symbol + string, but not number + string.
                        TypeKind::Keyword(_) => Ok(TObjElem::Index(TIndex {
                            // TODO: stop using TFnParam for an indexer's key
                            // since it's confusing.
                            key: TFnParam {
                                pat: TPat::Ident(BindingIdent {
                                    name: String::from("key"),
                                    mutable: false, // what does this even mean?
                                }),
                                t: key.to_owned(),
                                optional: false, // what does this even mean?
                            },
                            // How do we maintain the optionality of each property
                            // when we aren't setting it explicitly
                            mutable: false, // TODO
                            t: value,
                        })),
                        _ => Err(Report::new(TypeError::Unhandled)),
                    }
                })
                .collect::<Result<Vec<_>, TypeError>>()?;

            let t = Type {
                kind: TypeKind::Object(TObject { elems }),
                mutable: false,
                provenance: None, // TODO: fill this in
            };

            Ok(t)
        }
        _ => Err(Report::new(TypeError::Unhandled)),
    }
}
