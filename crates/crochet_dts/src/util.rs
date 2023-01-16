use std::collections::HashMap;

use crochet_ast::types::{TObjElem, TObject, TPropKey, Type, TypeKind};
use crochet_infer::Scheme;

pub fn merge_schemes(old_scheme: &Scheme, new_scheme: &Scheme, _obj_is_mutating: bool) -> Scheme {
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
            let mut map: HashMap<String, TObjElem> = HashMap::new();
            for elem in &old_obj.elems {
                match elem {
                    TObjElem::Call(_) => todo!(),
                    TObjElem::Constructor(_) => todo!(),
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
                    TObjElem::Index(_) => {
                        // TODO
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

            // TODO:
            // - loop through `new_elems` and update `map`
            // - generate output `elems` from `map`

            // NOTE: right now this results in duplicate methods appearing when
            // mergin `Array` and `ReadonlyArray`.
            let elems: Vec<_> = old_obj
                .elems
                .iter()
                .cloned()
                .chain(new_obj.elems.iter().cloned())
                .collect();

            Type::from(TypeKind::Object(TObject { elems }))
        }
        (_, _) => todo!(),
    };

    Scheme {
        t: Box::from(t),
        type_params: type_params_1.clone(),
    }
}
