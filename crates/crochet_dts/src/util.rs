use crochet_ast::types::{TObject, Type, TypeKind};
use crochet_infer::Scheme;

pub fn merge_schemes(scheme1: &Scheme, scheme2: &Scheme) -> Scheme {
    let type_params_1 = &scheme1.type_params;
    let type_params_2 = &scheme2.type_params;

    if type_params_1.len() != type_params_2.len() {
        panic!("Mismatch in type param count when attempting to merge type");
    }

    // TODO: check if the type params are in the same order
    for (type_param_1, type_param_2) in type_params_1.iter().zip(type_params_2.iter()) {
        if type_param_1.name != type_param_2.name {
            panic!("Type params must have the same names when merging schemes");
        }
    }

    let t = match (&scheme1.t.kind, &scheme2.t.kind) {
        (TypeKind::Object(obj1), TypeKind::Object(obj2)) => {
            let elems: Vec<_> = obj1
                .elems
                .iter()
                .cloned()
                .chain(obj2.elems.iter().cloned())
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
