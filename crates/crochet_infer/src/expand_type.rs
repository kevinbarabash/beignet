use crochet_ast::types::*;
use crochet_ast::types::{
    BindingIdent, TFnParam, TIndex, TObjElem, TObject, TPat, TProp, TPropKey, Type, TypeKind,
};
use error_stack::{Report, Result};
use std::collections::HashMap;

use crate::context::Context;
use crate::type_error::TypeError;
use crate::unify::unify;
use crate::util::{replace_aliases_rec, union_many_types};

pub fn expand_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Var(_) => Ok(t.to_owned()),
        TypeKind::App(_) => Ok(t.to_owned()),
        TypeKind::Lam(_) => Ok(t.to_owned()),
        TypeKind::Lit(_) => Ok(t.to_owned()),
        TypeKind::Keyword(_) => Ok(t.to_owned()),
        TypeKind::Union(_) => Ok(t.to_owned()),
        TypeKind::Intersection(_) => Ok(t.to_owned()),
        TypeKind::Object(_) => Ok(t.to_owned()),
        TypeKind::Ref(alias) => {
            let alias = TRef {
                name: alias.name.to_owned(),
                type_args: match &alias.type_args {
                    Some(args) => {
                        let args: Result<Vec<_>, TypeError> =
                            args.iter().map(|arg| expand_type(arg, ctx)).collect();
                        Some(args?)
                    }
                    None => None,
                },
            };
            let t = ctx.lookup_ref_and_instantiate(&alias)?;
            expand_type(&t, ctx)
        }
        TypeKind::Tuple(_) => Ok(t.to_owned()),
        TypeKind::Array(_) => Ok(t.to_owned()),
        TypeKind::Rest(_) => Ok(t.to_owned()),
        TypeKind::This => todo!(),
        // for keyof we want to lookup the keys on literals and keywords
        TypeKind::KeyOf(t) => {
            // TODO: update this to do:
            // keyof(get_obj_type(expand(t)))
            let result = keyof(t, ctx)?;
            println!("keyof {t} = {result}");

            Ok(result)
        }
        TypeKind::IndexAccess(_) => todo!(),
        // should only be used to process object types and arrays/tuples
        // all other types should be passed through
        TypeKind::MappedType(mapped) => {
            let constraint = mapped.type_param.constraint.as_ref().unwrap();
            let keys = match &constraint.kind {
                TypeKind::KeyOf(t) => keyof(t, ctx)?,
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
                                    name: TPropKey::NumberKey(name.to_owned()),
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
                                    name: TPropKey::StringKey(name.to_owned()),
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

            expand_type(&t, ctx)
        }
        TypeKind::ConditionalType(TConditionalType {
            check_type,
            extends_type,
            true_type,
            false_type,
        }) => {
            let t = match unify(check_type, extends_type, ctx) {
                Ok(_) => true_type,
                Err(_) => false_type,
            };
            expand_type(t.as_ref(), ctx)
        }
        TypeKind::Generic(_) => todo!(),
    }
}

fn get_obj_type_from_mapped_type(mapped: &TMappedType, ctx: &Context) -> Result<Type, TypeError> {
    if let Some(constraint) = &mapped.type_param.constraint {
        if let TypeKind::KeyOf(t) = &constraint.kind {
            return get_obj_type(t.as_ref(), ctx);
        }

        // TODO: look at constraint.provenence to see if it's a keyof type
    }

    if let TypeKind::IndexAccess(access) = &mapped.t.kind {
        return get_obj_type(access.object.as_ref(), ctx);
    }

    Err(Report::new(TypeError::Unhandled))
}

fn computed_indexed_access(access: &TIndexAccess, ctx: &Context) -> Result<Type, TypeError> {
    let obj = get_obj_type(access.object.as_ref(), ctx)?;
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
                let key = match &prop.name {
                    TPropKey::StringKey(key) => key,
                    TPropKey::NumberKey(key) => key,
                };
                if key == name {
                    return Ok(prop.to_owned());
                }
            }
        }
    }

    Err(Report::new(TypeError::MissingKey(name.to_owned())))
}

fn get_prop(obj: &TObject, key: &str) -> Result<TProp, TypeError> {
    for elem in &obj.elems {
        if let TObjElem::Prop(prop) = elem {
            if prop.name == TPropKey::StringKey(key.to_owned()) {
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

const NEVER_TYPE: Type = Type {
    kind: TypeKind::Keyword(TKeyword::Never),
    provenance: None,
    mutable: false,
};

fn keyof(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    if let TypeKind::KeyOf(t) = &t.kind {
        return keyof(t, ctx);
    }

    let obj_t = get_obj_type(t, ctx)?;

    match &obj_t.kind {
        TypeKind::Object(TObject { elems }) => {
            let elems: Vec<_> = elems
                .iter()
                .filter_map(|elem| -> Option<Type> {
                    match elem {
                        TObjElem::Call(_) => None,
                        TObjElem::Constructor(_) => None,
                        TObjElem::Index(TIndex {
                            key: TFnParam { t, .. },
                            ..
                        }) => Some(t.to_owned()),
                        TObjElem::Prop(prop) => match &prop.name {
                            crochet_ast::types::TPropKey::StringKey(str) => {
                                Some(Type::from(TypeKind::Lit(TLit::Str(str.to_owned()))))
                            }
                            crochet_ast::types::TPropKey::NumberKey(num) => {
                                Some(Type::from(TypeKind::Lit(TLit::Num(num.to_owned()))))
                            }
                        },
                    }
                })
                .collect();

            Ok(union_many_types(&elems))
        }
        _ => Ok(NEVER_TYPE),
    }
}

fn get_obj_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Generic(TGeneric { t, type_params: _ }) => get_obj_type(t, ctx),
        TypeKind::Var(_) => Err(Report::new(TypeError::CantInferTypeFromItKeys)),
        TypeKind::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            get_obj_type(&t, ctx)
        }
        TypeKind::Object(_) => Ok(t.to_owned()),
        TypeKind::Lit(lit) => {
            let t = match lit {
                TLit::Num(_) => ctx.lookup_type_and_instantiate("Number", false)?,
                TLit::Bool(_) => ctx.lookup_type_and_instantiate("Boolean", false)?,
                TLit::Str(_) => ctx.lookup_type_and_instantiate("String", false)?,
            };
            Ok(t)
        }
        TypeKind::Keyword(keyword) => {
            let t = match keyword {
                TKeyword::Number => ctx.lookup_type_and_instantiate("Number", false)?,
                TKeyword::Boolean => ctx.lookup_type_and_instantiate("Boolean", false)?,
                TKeyword::String => ctx.lookup_type_and_instantiate("String", false)?,
                TKeyword::Symbol => ctx.lookup_type_and_instantiate("Symbol", false)?,
                // TODO: return TypeError::NotAnObject here
                TKeyword::Null => return Ok(NEVER_TYPE),
                TKeyword::Undefined => return Ok(NEVER_TYPE),
                TKeyword::Never => return Ok(NEVER_TYPE),
            };
            Ok(t)
        }
        TypeKind::Tuple(tuple) => {
            let mut elems: Vec<TObjElem> = vec![];
            // TODO: change this to a for_each
            for i in 0..tuple.len() {
                let elem_t = tuple.get(i).unwrap();
                elems.push(TObjElem::Prop(TProp {
                    name: TPropKey::NumberKey(i.to_string()),
                    optional: false,
                    // If the type we're processing is mutable, then we can
                    // read/write each element in the tuple
                    mutable: t.mutable,
                    t: elem_t.to_owned(),
                }));
            }
            // TODO: filter out the indexer since we only want indexes from
            // 0 to tuple.len()
            let array_t = ctx.lookup_type_and_instantiate("Array", t.mutable)?;
            if let TypeKind::Object(TObject { elems: array_elems }) = &array_t.kind {
                for array_elem in array_elems {
                    match array_elem {
                        TObjElem::Call(_) => (),
                        TObjElem::Constructor(_) => (),
                        // We intentionally ignore index elements since we only
                        // want to include valid indexes which is done above
                        TObjElem::Index(_) => (),
                        TObjElem::Prop(_) => elems.push(array_elem.to_owned()),
                    };
                }
            };
            let t = Type::from(TypeKind::Object(TObject { elems }));
            Ok(t)
        }
        TypeKind::Array(_) => {
            let t = ctx.lookup_type_and_instantiate("Array", t.mutable)?;
            Ok(t)
        }
        TypeKind::Lam(_) => {
            let t = ctx.lookup_type_and_instantiate("Function", false)?;
            Ok(t)
        }
        TypeKind::App(_) => todo!(), // What does this even mean?
        TypeKind::Union(_) => todo!(),
        TypeKind::Intersection(_) => {
            // How do we differentiate between object types vs things like
            // number, string, array, etc. which have a prototype which contains
            // properties.
            todo!();
        }
        TypeKind::Rest(_) => todo!(), // What does this even mean?
        TypeKind::This => todo!(),    // Depends on what this is referencing
        TypeKind::KeyOf(t) => keyof(t, ctx),
        TypeKind::IndexAccess(_) => {
            todo!() // We have to evaluate the IndexAccess first
        }
        TypeKind::MappedType(_) => {
            todo!() // We have to evaluate the MappedType first
        }
        TypeKind::ConditionalType(_) => {
            todo!() // We have to evaluate the ConditionalType first
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::infer;
    use crochet_parser::*;

    fn infer_prog(input: &str) -> Context {
        let mut prog = parse(input).unwrap();
        let mut ctx: Context = Context::default();
        infer::infer_prog(&mut prog, &mut ctx).unwrap()
    }

    fn get_keyof(name: &str, ctx: &Context) -> String {
        match ctx.lookup_type(name, true) {
            Ok(t) => {
                let t = keyof(&t, ctx).unwrap();
                format!("{t}")
            }
            Err(_) => panic!("Couldn't find type with name '{name}'"),
        }
    }

    #[test]
    fn test_object() {
        let src = r#"
        type t = {x: number, y: number};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_keyof("t", &ctx), r#""x" | "y""#);
    }

    #[test]
    fn test_intersection() {
        let src = r#"
        type t = {a: number, b: boolean} & {b: string, c: number};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_keyof("t", &ctx), r#""a" | "b" | "c""#);
    }

    #[test]
    fn test_number() {
        let src = r#"
        type Number = {
            toFixed: () => string,
            toString: () => string,
        };
        type t = number;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_keyof("t", &ctx), r#""toFixed" | "toString""#);
    }

    #[test]
    fn test_string() {
        let src = r#"
        type String = {
            length: () => number,
            toLowerCase: () => string,
            toUpperCase: () => string,
        };
        type t = string;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_keyof("t", &ctx),
            r#""length" | "toLowerCase" | "toUpperCase""#
        );
    }

    #[test]
    fn test_array() {
        let src = r#"
        type ReadonlyArray<T> = {
            [key: number]: T;
            length: number;
            map: (item: T, index: number, array: ReadonlyArray<T>) => null;
        };
        type t = number[];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_keyof("t", &ctx), r#""length" | "map" | number"#);
    }

    #[test]
    fn test_mutable_array() {
        let src = r#"
        type Array<T> = {
            [key: number]: T;
            length: number;
            map: (item: T, index: number, array: ReadonlyArray<T>) => null;
            sort: () => mut T[];
        };
        type t = mut number[];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_keyof("t", &ctx),
            r#""length" | "map" | "sort" | number"#
        );
    }

    #[test]
    fn test_tuple() {
        let src = r#"
        type ReadonlyArray<T> = {
            length: number,
            map: (item: T, index: number, array: ReadonlyArray<T>) => null,
        };
        type t = [1, 2, 3];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_keyof("t", &ctx), r#""length" | "map" | 0 | 1 | 2"#);
    }

    #[test]
    fn test_function() {
        let src = r#"
        type Function = {
            call: () => null,
            apply: () => null,
            bind: () => null,
        };
        type t = () => boolean;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_keyof("t", &ctx), r#""apply" | "bind" | "call""#);
    }
}
