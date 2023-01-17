use std::collections::BTreeMap;

use crochet_ast::types::{TObjElem, TObject, TPropKey, Type, TypeKind};
use crochet_infer::Scheme;

pub fn merge_schemes(old_scheme: &Scheme, new_scheme: &Scheme) -> Scheme {
    let type_params_1 = &old_scheme.type_params;
    let type_params_2 = &new_scheme.type_params;

    if type_params_1.len() != type_params_2.len() {
        panic!("Mismatch in type param count when attempting to merge type");
    }

    // TODO: check if the type params are in the same order
    for (type_param_1, type_param_2) in type_params_1.iter().zip(type_params_2.iter()) {
        if type_param_1.name != type_param_2.name {
            panic!("Type params must have the same names when merging schemes");
        }
    }

    let t = match (&old_scheme.t.kind, &new_scheme.t.kind) {
        (TypeKind::Object(old_obj), TypeKind::Object(new_obj)) => {
            // TODO: Handle function overloads.  `map` only allows a single entry
            // per key which means that we end up with only the first declaration
            // instead of all of them.  This is complicated by the fact that the
            // Readonly variant may define the methods in slightly different ways.
            // See `reduce` in `Array` and `ReadonlyArray`.  The callback is
            // passed `T[]` in the first on and `readonly T[]` in the second one.
            let mut map: BTreeMap<String, TObjElem> = BTreeMap::new();

            let mut calls: Vec<TObjElem> = vec![];
            let mut constructors: Vec<TObjElem> = vec![];
            let mut indexes: Vec<TObjElem> = vec![];

            for elem in &old_obj.elems {
                match elem {
                    TObjElem::Call(_) => {
                        calls.push(elem.to_owned());
                    }
                    TObjElem::Constructor(_) => {
                        constructors.push(elem.to_owned());
                    }
                    TObjElem::Index(_) => {
                        indexes.push(elem.to_owned());
                    }

                    TObjElem::Method(method) => {
                        let key = match &method.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        map.insert(key, elem.to_owned());
                    }
                    TObjElem::Getter(getter) => {
                        let key = match &getter.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        map.insert(key, elem.to_owned());
                    }
                    TObjElem::Setter(setter) => {
                        let key = match &setter.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        map.insert(key, elem.to_owned());
                    }
                    TObjElem::Prop(prop) => {
                        let key = match &prop.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        map.insert(key, elem.to_owned());
                    }
                }
            }

            for elem in &new_obj.elems {
                match elem {
                    TObjElem::Call(_) => {
                        calls.push(elem.to_owned());
                    }
                    TObjElem::Constructor(_) => {
                        constructors.push(elem.to_owned());
                    }
                    TObjElem::Index(_) => {
                        indexes.push(elem.to_owned());
                    }

                    TObjElem::Method(method) => {
                        let key = match &method.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        match map.get(&key) {
                            Some(old_elem) => {
                                // NOTE: When an individual interface decl is
                                // inferred we don't have enough info to know
                                // whether individual methods are mutating so
                                // we either infer all methods ad mutating or
                                // not.  If one interface has a mutating version
                                // and one doesn't, we can infer that the method
                                // isn't mutating.
                                if let TObjElem::Method(old_method) = old_elem {
                                    if old_method.is_mutating && !method.is_mutating {
                                        map.insert(key, elem.to_owned());
                                    }
                                }
                            }
                            None => {
                                map.insert(key, elem.to_owned());
                            }
                        };
                    }
                    TObjElem::Getter(getter) => {
                        let key = match &getter.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        // Even if there is a getter with the same name, it
                        // should return the same type.
                        map.insert(key, elem.to_owned());
                    }
                    TObjElem::Setter(setter) => {
                        let key = match &setter.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        // In practice there should never be an existing setter
                        // with the same name.
                        map.insert(key, elem.to_owned());
                    }
                    TObjElem::Prop(prop) => {
                        let key = match &prop.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        match map.get(&key) {
                            Some(old_elem) => {
                                // NOTE: Properties can be marked as `readonly`
                                // in TS interfaces.  If the old properties is
                                // not mutable, but the new one is, we use the
                                // new one.
                                if let TObjElem::Prop(old_prop) = old_elem {
                                    if !old_prop.mutable && prop.mutable {
                                        map.insert(key, elem.to_owned());
                                    }
                                }
                            }
                            None => {
                                map.insert(key, elem.to_owned());
                            }
                        };
                    }
                }
            }

            let mut elems: Vec<TObjElem> = vec![];

            // TODO: Dedupe these while handling overloads.
            elems.append(&mut calls);
            elems.append(&mut constructors);
            elems.append(&mut indexes);

            for (_, elem) in map {
                elems.push(elem.to_owned());
            }

            Type::from(TypeKind::Object(TObject { elems }))
        }
        (_, _) => todo!(),
    };

    Scheme {
        t: Box::from(t),
        type_params: type_params_1.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crochet_ast::types::{TCallable, TIndex, TIndexKey, TKeyword, TMethod};

    #[test]
    fn if_obj_is_not_mutating_all_methods_are_not_mutating() {
        let old_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("foo")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: false,
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("bar")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: false,
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(
            result.to_string(),
            "{bar(self): boolean, foo(self): boolean}"
        );
    }

    #[test]
    fn new_is_mutating_and_old_is_mutating_with_different_method_names() {
        let old_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("foo")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: true,
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("bar")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: true,
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(
            result.to_string(),
            "{bar(mut self): boolean, foo(mut self): boolean}"
        );
    }

    #[test]
    fn old_is_mutating_and_new_is_not_mutating_with_same_method_names() {
        let old_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("foo")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: true,
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("foo")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: false,
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(result.to_string(), "{foo(self): boolean}");
    }

    #[test]
    fn old_is_not_mutating_and_new_is_mutating_with_same_method_names() {
        let old_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("foo")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: false,
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Method(TMethod {
            name: TPropKey::StringKey(String::from("foo")),
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
            is_mutating: true,
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(result.to_string(), "{foo(self): boolean}");
    }

    #[test]
    fn old_index_mut_new_index_immut() {
        let old_elems = vec![TObjElem::Index(TIndex {
            key: TIndexKey {
                name: "key".to_string(),
                t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
            },
            t: Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            mutable: true,
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Index(TIndex {
            key: TIndexKey {
                name: "key".to_string(),
                t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
            },
            t: Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            mutable: false,
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(
            result.to_string(),
            "{mut [key: number]: boolean, [key: number]: boolean}"
        );
    }

    #[test]
    fn old_index_immut_new_index_mut() {
        let old_elems = vec![TObjElem::Index(TIndex {
            key: TIndexKey {
                name: "key".to_string(),
                t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
            },
            t: Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            mutable: false,
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Index(TIndex {
            key: TIndexKey {
                name: "key".to_string(),
                t: Box::from(Type::from(TypeKind::Keyword(TKeyword::Number))),
            },
            t: Type::from(TypeKind::Keyword(TKeyword::Boolean)),
            mutable: true,
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(
            result.to_string(),
            "{[key: number]: boolean, mut [key: number]: boolean}"
        );
    }

    #[test]
    fn callables() {
        let old_elems = vec![TObjElem::Call(TCallable {
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Call(TCallable {
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(result.to_string(), "{() => boolean, () => boolean}");
    }

    #[test]
    fn constructors() {
        let old_elems = vec![TObjElem::Constructor(TCallable {
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
        })];
        let old_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: old_elems }))),
            type_params: vec![],
        };
        let new_elems = vec![TObjElem::Constructor(TCallable {
            params: vec![],
            ret: Box::from(Type::from(TypeKind::Keyword(TKeyword::Boolean))),
            type_params: vec![],
        })];
        let new_scheme = Scheme {
            t: Box::from(Type::from(TypeKind::Object(TObject { elems: new_elems }))),
            type_params: vec![],
        };

        let result = merge_schemes(&old_scheme, &new_scheme);
        assert_eq!(result.to_string(), "{new () => boolean, new () => boolean}");
    }
}
