use crochet_ast::types::{TGeneric, TObject, Type, TypeKind};
use crochet_infer::{get_type_params, Subst, Substitutable};

// TODO: rename to merge_interface_types
pub fn merge_types(t1: &Type, t2: &Type) -> Type {
    let tp1 = get_type_params(t1);
    let tp2 = get_type_params(t2);

    if tp1.len() != tp2.len() {
        panic!("Mismatch in type param count when attempting to merge type");
    }

    // Creates a mapping from type params in t2 to those in t1
    let subs: Subst = tp2
        .into_iter()
        .map(|tv| tv.id.to_owned())
        .zip(
            tp1.iter()
                .map(|tv| Type::from(TypeKind::Var(tv.to_owned()))),
        )
        .collect();

    // Unwrap qualified types since we return a qualified
    // type if there are any type qualifiers.
    let t1 = if let TypeKind::Generic(TGeneric { t, .. }) = &t1.kind {
        t
    } else {
        t1
    };
    let t2 = if let TypeKind::Generic(TGeneric { t, .. }) = &t2.kind {
        t
    } else {
        t2
    };

    // Updates type variables for type params to match t1
    let mut t2 = t2.to_owned();
    t2.apply(&subs);

    let type_params = tp1;
    let t = match (&t1.kind, &t2.kind) {
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

    if type_params.is_empty() {
        t
    } else {
        Type::from(TypeKind::Generic(TGeneric {
            t: Box::from(t),
            type_params,
        }))
    }
}
