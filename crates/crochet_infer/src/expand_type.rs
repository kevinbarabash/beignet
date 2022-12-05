use crochet_ast::types::*;
use crochet_ast::types::{
    BindingIdent, TFnParam, TIndex, TLit, TObjElem, TObject, TPat, TProp, TPropKey, Type, TypeKind,
};
use error_stack::{Report, Result};
use std::collections::HashMap;

use crate::context::Context;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::unify::unify;
use crate::util::{
    get_type_params, replace_aliases_rec, union_many_types, union_types, unwrap_generic,
};

// `expand_type` is used to expand types that `unify` doesn't know how to unify
// into something that it does know how to unify.
pub fn expand_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    println!("expand_type t = {t}");
    match &t.kind {
        TypeKind::Var(_) => Ok(t.to_owned()),
        TypeKind::App(_) => Ok(t.to_owned()),
        TypeKind::Lam(_) => Ok(t.to_owned()),
        TypeKind::Lit(_) => Ok(t.to_owned()),
        TypeKind::Keyword(_) => Ok(t.to_owned()),
        TypeKind::Union(_) => Ok(t.to_owned()),
        TypeKind::Intersection(_) => Ok(t.to_owned()),
        TypeKind::Object(_) => Ok(t.to_owned()),
        TypeKind::Ref(alias) => expand_alias_type(alias, ctx),
        TypeKind::Tuple(_) => Ok(t.to_owned()),
        TypeKind::Array(_) => Ok(t.to_owned()),
        TypeKind::Rest(_) => Ok(t.to_owned()),
        TypeKind::This => todo!(),
        TypeKind::KeyOf(t) => expand_keyof(t, ctx),
        TypeKind::IndexAccess(access) => expand_index_access(access, ctx),
        TypeKind::MappedType(mapped) => expand_mapped_type(mapped, ctx),
        TypeKind::ConditionalType(cond) => expand_conditional_type(cond, ctx),
        TypeKind::Generic(_) => todo!(),
    }
}

fn expand_alias_type(alias: &TRef, ctx: &Context) -> Result<Type, TypeError> {
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

    let name = &alias.name;
    let t = ctx._lookup_type(name)?;
    let type_params = get_type_params(&t);

    // Replaces qualifiers in the type with the corresponding type params
    // from the alias type.
    let ids = type_params.iter().map(|tv| tv.id.to_owned());
    let subs: Subst = match &alias.type_args {
        Some(type_args) => {
            if type_args.len() != type_params.len() {
                return Err(
                    Report::new(TypeError::TypeInstantiationFailure).attach_printable(
                        "mismatch between the number of qualifiers and type params",
                    ),
                );
            }
            let inner_t = unwrap_generic(&t);
            match &inner_t.kind {
                // When conditional types act on a generic type, they
                // become distributive when given a union type.
                TypeKind::ConditionalType(_) => {
                    // TODO: determine which type arg is being used
                    // for the `check_type` and use that as the type
                    // that we distribute.  For now we assume the first
                    // type arg is one being used as the `check_type`.
                    let check_args = match &type_args[0].kind {
                        TypeKind::Union(types) => types.to_owned(),
                        _ => vec![type_args[0].to_owned()],
                    };
                    let mut type_args = type_args.clone();

                    let mut types = vec![];
                    for check_arg in check_args {
                        type_args[0] = check_arg;
                        let subs: Subst = ids.clone().zip(type_args.iter().cloned()).collect();
                        let inner_t = inner_t.apply(&subs);
                        let t = expand_type(&inner_t, ctx)?;
                        types.push(t);
                    }

                    let t = union_many_types(&types);
                    return expand_type(&t, ctx);
                }
                _ => ids.zip(type_args.iter().cloned()).collect(),
            }
        }
        None => {
            if !type_params.is_empty() {
                println!("type = {t}");
                println!("no type params");
                return Err(
                    Report::new(TypeError::TypeInstantiationFailure).attach_printable(
                        "mismatch between the number of qualifiers and type params",
                    ),
                );
            }
            Subst::default()
        }
    };

    expand_type(&t.apply(&subs), ctx)
}

fn expand_conditional_type(cond: &TConditionalType, ctx: &Context) -> Result<Type, TypeError> {
    let TConditionalType {
        check_type,
        extends_type,
        true_type,
        false_type,
    } = cond;

    let t = match unify(check_type, extends_type, ctx) {
        Ok(_) => true_type,
        Err(_) => false_type,
    };
    expand_type(t.as_ref(), ctx)
}

fn expand_index_access(access: &TIndexAccess, ctx: &Context) -> Result<Type, TypeError> {
    let obj = get_obj_type(access.object.as_ref(), ctx)?;
    let index = expand_type(access.index.as_ref(), ctx)?;

    match &index.kind {
        TypeKind::Lit(lit) => {
            if let TypeKind::Object(obj) = &obj.kind {
                for elem in &obj.elems {
                    match elem {
                        TObjElem::Call(_) => (),
                        TObjElem::Constructor(_) => (),
                        TObjElem::Index(index) => match &index.key.t.kind {
                            TypeKind::Keyword(keyword) => match (keyword, lit) {
                                (TKeyword::Number, TLit::Num(_)) => {
                                    let undefined =
                                        Type::from(TypeKind::Keyword(TKeyword::Undefined));
                                    return Ok(union_types(&index.t, &undefined));
                                }
                                (TKeyword::String, TLit::Str(_)) => {
                                    let undefined =
                                        Type::from(TypeKind::Keyword(TKeyword::Undefined));
                                    return Ok(union_types(&index.t, &undefined));
                                }
                                _ => (),
                            },
                            _ => {
                                todo!("Return an error that object indexer's key is invalid");
                            }
                        },
                        TObjElem::Prop(prop) => match (&prop.name, lit) {
                            (TPropKey::StringKey(key), TLit::Str(str)) if key == str => {
                                return Ok(prop.t.to_owned());
                            }
                            // NOTE: `get_obj_type` add numeric keys for each element
                            // in a tuple.
                            (TPropKey::NumberKey(key), TLit::Num(num)) if key == num => {
                                return Ok(prop.t.to_owned());
                            }
                            _ => (),
                        },
                    }
                }

                return Err(Report::new(TypeError::MissingKey(format!("{lit}"))));
            }
        }
        TypeKind::Keyword(_) => {
            if let TypeKind::Object(obj) = &obj.kind {
                for elem in &obj.elems {
                    if let TObjElem::Index(TIndex { key, t, .. }) = elem {
                        if key.t == index {
                            return Ok(t.to_owned());
                        }
                    }
                }
            }

            return Err(Report::new(TypeError::Unhandled));
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

fn get_obj_type_from_mapped_type(mapped: &TMappedType, ctx: &Context) -> Result<Type, TypeError> {
    if let Some(constraint) = &mapped.type_param.constraint {
        if let TypeKind::KeyOf(t) = &constraint.kind {
            return get_obj_type(t.as_ref(), ctx);
        }

        // TODO: look at constraint.provenence to see if it's a keyof type
    }

    if let TypeKind::IndexAccess(access) = &mapped.t.kind {
        get_obj_type(access.object.as_ref(), ctx)
    } else {
        Err(Report::new(TypeError::Unhandled))
    }
}

// TODO: This should only be used to process object types and arrays/tuples
// all other types (number, string, etc.) should be passed through.
fn expand_mapped_type(mapped: &TMappedType, ctx: &Context) -> Result<Type, TypeError> {
    let constraint = mapped.type_param.constraint.as_ref().unwrap();
    let keys = expand_type(constraint, ctx)?;
    let obj = get_obj_type_from_mapped_type(mapped, ctx)?;

    let elems = match &obj.kind {
        TypeKind::Object(TObject { elems }) => elems.to_owned(),
        _ => vec![],
    };

    let keys = match &keys.kind {
        TypeKind::Union(keys) => keys.to_owned(),
        _ => vec![keys],
    };

    let elems = keys
        .iter()
        .map(|key| {
            let name = mapped.type_param.name.to_owned();
            let type_arg_map = HashMap::from([(name, key.to_owned())]);
            let value = replace_aliases_rec(mapped.t.as_ref(), &type_arg_map);

            // TODO: recursively replace all indexed access types in `value`,
            // only process a top-level indexed access is insufficient.
            let value = match &value.kind {
                TypeKind::IndexAccess(access) => expand_index_access(access, ctx)?,
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
                    crochet_ast::types::TLit::Bool(_) => Err(Report::new(TypeError::Unhandled)),
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

    Ok(t)
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

const NEVER_TYPE: Type = Type {
    kind: TypeKind::Keyword(TKeyword::Never),
    provenance: None,
    mutable: false,
};

fn expand_keyof(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    if let TypeKind::KeyOf(t) = &t.kind {
        return expand_keyof(t, ctx);
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

pub fn get_obj_type(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Generic(TGeneric { t, type_params: _ }) => get_obj_type(t, ctx),
        TypeKind::Var(_) => Err(Report::new(TypeError::CantInferTypeFromItKeys)),
        TypeKind::Ref(alias) => {
            let t = expand_alias_type(alias, ctx)?;
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
            let mut elems: Vec<TObjElem> = tuple
                .iter()
                .enumerate()
                .map(|(i, elem_t)| {
                    TObjElem::Prop(TProp {
                        name: TPropKey::NumberKey(i.to_string()),
                        optional: false,
                        // If the type we're processing is mutable, then we can
                        // read/write each element in the tuple
                        mutable: t.mutable,
                        t: elem_t.to_owned(),
                    })
                })
                .collect();

            // TODO: Provide a type arg when instantiating "Array".  It should
            // be the union of all element types in the tuple.
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
        TypeKind::Array(type_param) => {
            // TODO: Update lookup_type_and_instantiate() to take type args so
            // that we don't have to handle them here manually.
            let t = ctx.lookup_type("Array", t.mutable)?;
            let type_params = get_type_params(&t);
            // TODO: Instead of instantiating the whole interface for one method, do
            // the lookup call first and then instantiate the method.
            let s: Subst =
                Subst::from([(type_params[0].id.to_owned(), type_param.as_ref().to_owned())]);
            let t = t.apply(&s);

            Ok(t)
        }
        TypeKind::Lam(_) => ctx.lookup_type_and_instantiate("Function", false),
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
        TypeKind::KeyOf(t) => expand_keyof(t, ctx),
        TypeKind::IndexAccess(access) => {
            let t = expand_index_access(access, ctx)?;
            get_obj_type(&t, ctx)
        }
        TypeKind::MappedType(mapped) => {
            let t = expand_mapped_type(mapped, ctx)?;
            get_obj_type(&t, ctx)
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
                let t = expand_keyof(&t, ctx).unwrap();
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

    #[test]
    fn test_index_access() {
        let src = r#"
        type Obj = {a: number, b: string, c: boolean};
        type A = Obj["a"];
        type Key = "b"
        type B = Obj[Key];
        let b: Obj[Key] = "hello";
        "#;
        let ctx = infer_prog(src);

        let a = ctx.lookup_type("A", false).unwrap();
        let a = expand_type(&a, &ctx).unwrap();
        let result = format!("{a}");
        assert_eq!(result, r#"number"#);

        let b = ctx.lookup_type("B", false).unwrap();
        let b = expand_type(&b, &ctx).unwrap();
        let result = format!("{b}");
        assert_eq!(result, r#"string"#);
    }
}
