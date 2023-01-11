use defaultmap::*;
use std::collections::{BTreeSet, HashMap};
use std::iter::Iterator;

use crochet_ast::types::*;

use crate::context::Context;
use crate::substitutable::{Subst, Substitutable};

fn get_mapping(t: &Type) -> HashMap<i32, Type> {
    let mapping: HashMap<i32, Type> = t
        .ftv()
        .iter()
        .enumerate()
        .map(|(index, key)| {
            (
                key.id,
                Type::from(TypeKind::Var(TVar {
                    id: index as i32,
                    constraint: key.constraint.clone(),
                })),
            )
        })
        .collect();

    mapping
}

pub fn close_over(s: &Subst, t: &Type, ctx: &Context) -> Type {
    let mut t = t.clone();
    t.apply(s);

    let tvs = t.ftv();

    let t = if tvs.is_empty() {
        t
    } else {
        match &t.kind {
            TypeKind::Lam(lam) => {
                let mut type_params: Vec<TypeParam> = vec![];
                let mut sub: Subst = Subst::default();
                let mut char_code: u32 = 65;
                for tv in tvs {
                    let c = char::from_u32(char_code).unwrap();
                    let t = Type::from(TypeKind::Ref(TRef {
                        name: c.to_string(),
                        type_args: None,
                    }));
                    sub.insert(tv.id, t);
                    type_params.push(TypeParam {
                        name: c.to_string(),
                        constraint: tv.constraint,
                        default: None,
                    });
                    char_code += 1;
                }

                let mut t = Type::from(TypeKind::GenLam(TGenLam {
                    lam: Box::from(lam.to_owned()),
                    type_params,
                }));

                t.apply(&sub);

                t
            }
            TypeKind::GenLam(_) => {
                panic!("We shouldn't be closing over TypeKind::GenLam");
            }
            _ => {
                panic!("We shouldn't have any free type variables when closing over non-lambdas")
            }
        }
    };

    normalize(&t, ctx)
}

pub fn normalize(t: &Type, ctx: &Context) -> Type {
    let mapping = get_mapping(t);

    // TODO: add norm_type as a method on Type, Vec<Type>, etc. similar to what we do for Substitutable
    // We should also add it to TObjElem (and structs used by its enums.  This will help us filter out
    // type variables that are bound to the object element as opposed to the encompassing object type.
    fn norm_type(t: &Type, mapping: &HashMap<i32, Type>, _ctx: &Context) -> Type {
        let kind = match &t.kind {
            TypeKind::Var(tv) => {
                match mapping.get(&tv.id) {
                    Some(t) => return t.to_owned(),
                    // If `id` doesn't exist in `mapping` we return the original type variable.
                    // In this situation, it should appear in some other list of qualifiers.
                    None => TypeKind::Var(TVar {
                        id: tv.id,
                        constraint: tv
                            .constraint
                            .as_ref()
                            .map(|constraint| Box::from(norm_type(constraint, mapping, _ctx))),
                    }),
                }
            }
            TypeKind::App(app) => {
                let args: Vec<_> = app
                    .args
                    .iter()
                    .map(|arg| norm_type(arg, mapping, _ctx))
                    .collect();
                let ret = Box::from(norm_type(&app.ret, mapping, _ctx));
                TypeKind::App(TApp { args, ret })
            }
            TypeKind::Lam(lam) => {
                let params: Vec<_> = lam
                    .params
                    .iter()
                    .map(|param| TFnParam {
                        t: norm_type(&param.t, mapping, _ctx),
                        ..param.to_owned()
                    })
                    .collect();
                let ret = Box::from(norm_type(&lam.ret, mapping, _ctx));
                TypeKind::Lam(TLam { params, ret })
            }
            TypeKind::GenLam(TGenLam { type_params, lam }) => {
                let params: Vec<_> = lam
                    .params
                    .iter()
                    .map(|param| TFnParam {
                        t: norm_type(&param.t, mapping, _ctx),
                        ..param.to_owned()
                    })
                    .collect();
                let ret = Box::from(norm_type(&lam.ret, mapping, _ctx));

                TypeKind::GenLam(TGenLam {
                    lam: Box::from(TLam { params, ret }),
                    type_params: type_params.to_owned(),
                })
            }
            TypeKind::Lit(_) => return t.to_owned(),
            TypeKind::Keyword(_) => return t.to_owned(),
            TypeKind::Union(types) => {
                // TODO: update union_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types = types.iter().map(|t| norm_type(t, mapping, _ctx)).collect();
                TypeKind::Union(types)
            }
            TypeKind::Intersection(types) => {
                // TODO: update intersection_types from constraint_solver.rs to handle
                // any number of types instead of just two and then call it here.
                let types: Vec<_> = types.iter().map(|t| norm_type(t, mapping, _ctx)).collect();

                let mut result = simplify_intersection(&types);
                result.provenance = t.provenance.to_owned();
                result.mutable = t.mutable;
                return result;
            }
            TypeKind::Object(obj) => {
                let elems = obj
                    .elems
                    .iter()
                    .map(|elem| match elem {
                        TObjElem::Call(call) => {
                            let params: Vec<_> = call
                                .params
                                .iter()
                                .map(|param| TFnParam {
                                    t: norm_type(&param.t, mapping, _ctx),
                                    ..param.to_owned()
                                })
                                .collect();
                            let ret = Box::from(norm_type(&call.ret, mapping, _ctx));
                            TObjElem::Call(TCallable {
                                params,
                                ret,
                                // TODO: normalize type_params?
                                type_params: call.type_params.to_owned(),
                            })
                        }
                        TObjElem::Constructor(call) => {
                            let params: Vec<_> = call
                                .params
                                .iter()
                                .map(|param| TFnParam {
                                    t: norm_type(&param.t, mapping, _ctx),
                                    ..param.to_owned()
                                })
                                .collect();
                            let ret = Box::from(norm_type(&call.ret, mapping, _ctx));
                            TObjElem::Constructor(TCallable {
                                params,
                                ret,
                                // TODO: normalize type_params?
                                type_params: call.type_params.to_owned(),
                            })
                        }
                        TObjElem::Index(index) => TObjElem::Index(TIndex {
                            key: index.key.to_owned(),
                            mutable: index.mutable,
                            t: norm_type(&index.t, mapping, _ctx),
                        }),
                        TObjElem::Prop(prop) => TObjElem::Prop(TProp {
                            name: prop.name.to_owned(),
                            optional: prop.optional,
                            mutable: prop.mutable,
                            t: norm_type(&prop.t, mapping, _ctx),
                        }),
                        TObjElem::Method(_) => todo!(),
                        TObjElem::Getter(_) => todo!(),
                        TObjElem::Setter(_) => todo!(),
                    })
                    .collect();
                TypeKind::Object(TObject { elems })
            }
            TypeKind::Ref(TRef { name, type_args }) => {
                let type_args = type_args
                    .clone()
                    .map(|params| params.iter().map(|t| norm_type(t, mapping, _ctx)).collect());
                TypeKind::Ref(TRef {
                    name: name.to_owned(),
                    type_args,
                })
            }
            TypeKind::Tuple(types) => {
                let types = types.iter().map(|t| norm_type(t, mapping, _ctx)).collect();
                TypeKind::Tuple(types)
            }
            TypeKind::Array(t) => TypeKind::Array(Box::from(norm_type(t, mapping, _ctx))),
            TypeKind::Rest(arg) => TypeKind::Rest(Box::from(norm_type(arg, mapping, _ctx))),
            TypeKind::This => TypeKind::This,
            TypeKind::KeyOf(t) => TypeKind::KeyOf(Box::from(norm_type(t, mapping, _ctx))),
            TypeKind::IndexAccess(TIndexAccess { object, index }) => {
                TypeKind::IndexAccess(TIndexAccess {
                    object: Box::from(norm_type(object, mapping, _ctx)),
                    index: Box::from(norm_type(index, mapping, _ctx)),
                })
            }
            TypeKind::MappedType(mapped) => TypeKind::MappedType(TMappedType {
                t: Box::from(norm_type(&mapped.t, mapping, _ctx)),
                type_param: TypeParam {
                    name: mapped.type_param.name.to_owned(),
                    constraint: mapped
                        .type_param
                        .constraint
                        .as_ref()
                        .map(|constraint| Box::from(norm_type(constraint, mapping, _ctx))),
                    default: mapped
                        .type_param
                        .default
                        .as_ref()
                        .map(|default| Box::from(norm_type(default, mapping, _ctx))),
                },
                ..mapped.to_owned()
            }),
            TypeKind::ConditionalType(TConditionalType {
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => TypeKind::ConditionalType(TConditionalType {
                check_type: Box::from(norm_type(check_type, mapping, _ctx)),
                extends_type: Box::from(norm_type(extends_type, mapping, _ctx)),
                true_type: Box::from(norm_type(true_type, mapping, _ctx)),
                false_type: Box::from(norm_type(false_type, mapping, _ctx)),
            }),
        };

        Type {
            kind,
            ..t.to_owned()
        }
    }

    // NOTE: normalize() is usually called on a type that's already been generalized so we
    // don't need to re-generalize the result type here.
    norm_type(t, &mapping, ctx)
}

// TODO: make this recursive
// TODO: handle optional properties correctly
// Maybe we can have a function that will canonicalize objects by converting
// `x: T | undefined` to `x?: T`
pub fn simplify_intersection(in_types: &[Type]) -> Type {
    let obj_types: Vec<_> = in_types
        .iter()
        .filter_map(|t| match &t.kind {
            TypeKind::Object(elems) => Some(elems),
            _ => None,
        })
        .collect();

    // The use of HashSet<Type> here is to avoid duplicate types
    let mut props_map: DefaultHashMap<String, BTreeSet<Type>> = defaulthashmap!();
    for obj in obj_types {
        for elem in &obj.elems {
            match elem {
                // What do we do with Call and Index signatures
                TObjElem::Call(_) => todo!(),
                TObjElem::Constructor(_) => todo!(),
                TObjElem::Method(_) => todo!(),
                TObjElem::Getter(_) => todo!(),
                TObjElem::Setter(_) => todo!(),
                TObjElem::Index(_) => todo!(),
                TObjElem::Prop(prop) => {
                    let key = match &prop.name {
                        TPropKey::StringKey(key) => key.to_owned(),
                        TPropKey::NumberKey(key) => key.to_owned(),
                    };
                    props_map[key].insert(prop.t.clone());
                }
            }
        }
    }

    let mut elems: Vec<TObjElem> = props_map
        .iter()
        .map(|(name, types)| {
            let types: Vec<_> = types.iter().cloned().collect();
            let t: Type = if types.len() == 1 {
                types[0].clone()
            } else {
                Type::from(TypeKind::Intersection(types))
            };
            TObjElem::Prop(TProp {
                name: TPropKey::StringKey(name.to_owned()),
                // TODO: determine this field from all of the TProps with
                // the same name.  This should only be optional if all of
                // the TProps with the current name are optional.
                optional: false,
                mutable: false,
                t,
            })
        })
        .collect();
    // How do we sort call and index signatures?
    elems.sort_by_key(|elem| match elem {
        TObjElem::Call(_) => todo!(),
        TObjElem::Constructor(_) => todo!(),
        TObjElem::Method(_) => todo!(),
        TObjElem::Getter(_) => todo!(),
        TObjElem::Setter(_) => todo!(),
        TObjElem::Index(_) => todo!(),
        TObjElem::Prop(prop) => prop.name.clone(),
    }); // ensure a stable order

    let mut not_obj_types: Vec<_> = in_types
        .iter()
        .filter(|t| !matches!(&t.kind, TypeKind::Object(_)))
        .cloned()
        .collect();

    let mut out_types = vec![];
    out_types.append(&mut not_obj_types);
    if !elems.is_empty() {
        out_types.push(Type::from(TypeKind::Object(TObject { elems })));
    }
    // TODO: figure out a consistent way to sort types
    // out_types.sort_by_key(|t| t.id); // ensure a stable order

    if out_types.len() == 1 {
        out_types[0].clone()
    } else {
        Type::from(TypeKind::Intersection(out_types))
    }
}

pub fn flatten_types(t: &Type) -> Vec<Type> {
    match &t.kind {
        TypeKind::Union(types) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![t.to_owned()],
    }
}

pub fn union_types(t1: &Type, t2: &Type) -> Type {
    union_many_types(&[t1.to_owned(), t2.to_owned()])
}

// TODO: remove any extraneous 'never' types
pub fn union_many_types(ts: &[Type]) -> Type {
    let types: Vec<_> = ts.iter().flat_map(flatten_types).collect();

    let types_set: BTreeSet<_> = types.iter().cloned().collect();

    let keyword_types: BTreeSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| matches!(&t.kind, TypeKind::Keyword(_)))
        .collect();

    let lit_types: BTreeSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| match &t.kind {
            // Primitive types subsume corresponding literal types
            TypeKind::Lit(lit) => {
                let kind = match lit {
                    TLit::Num(_) => TypeKind::Keyword(TKeyword::Number),
                    TLit::Bool(_) => TypeKind::Keyword(TKeyword::Boolean),
                    TLit::Str(_) => TypeKind::Keyword(TKeyword::String),
                };
                !keyword_types.contains(&Type::from(kind))
            }
            _ => false,
        })
        .collect();

    let other_types: BTreeSet<_> = types_set
        .iter()
        .cloned()
        .filter(|t| !matches!(&t.kind, TypeKind::Lit(_) | TypeKind::Keyword(_)))
        .collect();

    let types: Vec<_> = keyword_types
        .iter()
        .chain(lit_types.iter())
        .chain(other_types.iter())
        .cloned()
        .filter(|t| match &t.kind {
            TypeKind::Keyword(keyword) => !matches!(keyword, TKeyword::Never),
            _ => true,
        })
        .collect();

    match types.len() {
        0 => Type::from(TypeKind::Keyword(TKeyword::Never)),
        1 => types[0].clone(),
        _ => Type::from(TypeKind::Union(types)),
    }
}

pub fn compose_subs(s2: &Subst, s1: &Subst) -> Subst {
    let mut result: Subst = s1
        .iter()
        .map(|(tv, t)| {
            let mut t = t.clone();
            t.apply(s2);
            (*tv, t)
        })
        .collect();

    // TODO: detect any entries with a TVar that points to itself and remove them.
    result.extend(s2.to_owned());
    result
}

// subs are composed from left to right with ones to the right
// being applied to all of the ones to the left.
pub fn compose_many_subs(subs: &[Subst]) -> Subst {
    subs.iter()
        .fold(Subst::new(), |accum, next| compose_subs(&accum, next))
}

// If are multiple entries for the same type variable, this function merges
// them into a union type (simplifying the type if possible).
fn compose_subs_with_context(s1: &Subst, s2: &Subst) -> Subst {
    let mut result: Subst = s2
        .iter()
        .map(|(tv, t)| {
            let mut t = t.clone();
            t.apply(s1);
            (*tv, t)
        })
        .collect();
    for (tv, t) in s1 {
        match result.get(tv) {
            Some(t1) => {
                let t = union_types(t, t1);
                result.insert(*tv, t)
            }
            None => result.insert(*tv, t.to_owned()),
        };
    }
    result
}

pub fn compose_many_subs_with_context(subs: &[Subst]) -> Subst {
    subs.iter().fold(Subst::new(), |accum, next| {
        compose_subs_with_context(&accum, next)
    })
}

pub fn get_property_type(prop: &TProp) -> Type {
    let t = prop.t.to_owned();
    match prop.optional {
        true => Type::from(TypeKind::Union(vec![
            t,
            Type::from(TypeKind::Keyword(TKeyword::Undefined)),
        ])),
        false => t,
    }
}

// TODO: update this to use Visitor from crochet_infer, which should probably
// be moved into the crochet_ast crate.
// TODO: rename this replace_refs_rec
// NOTE: This is only used externally by replace_aliases_rec.
pub fn replace_aliases_rec(t: &Type, type_param_map: &HashMap<String, Type>) -> Type {
    let kind = match &t.kind {
        TypeKind::Var(tvar) => match &tvar.constraint {
            Some(constraint) => TypeKind::Var(TVar {
                constraint: Some(Box::from(replace_aliases_rec(constraint, type_param_map))),
                ..tvar.to_owned()
            }),
            None => t.kind.to_owned(),
        },
        TypeKind::App(TApp { args, ret }) => TypeKind::App(TApp {
            args: args
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
            ret: Box::from(replace_aliases_rec(ret, type_param_map)),
        }),
        TypeKind::Lam(TLam { params, ret }) => TypeKind::Lam(TLam {
            params: params
                .iter()
                .map(|param| TFnParam {
                    t: replace_aliases_rec(&param.t, type_param_map),
                    ..param.to_owned()
                })
                .collect(),
            ret: Box::from(replace_aliases_rec(ret, type_param_map)),
        }),
        TypeKind::GenLam(TGenLam { lam, type_params }) => {
            // Removes any shadowed type variables.
            let mut type_param_map = type_param_map.clone();
            for type_param in type_params {
                type_param_map.remove(&type_param.name);
            }

            let params: Vec<_> = lam
                .params
                .iter()
                .map(|param| TFnParam {
                    t: replace_aliases_rec(&param.t, &type_param_map),
                    ..param.to_owned()
                })
                .collect();
            let ret = replace_aliases_rec(&lam.ret, &type_param_map);

            TypeKind::GenLam(TGenLam {
                lam: Box::from(TLam {
                    params,
                    ret: Box::from(ret),
                }),
                type_params: type_params.to_owned(),
            })
        }
        TypeKind::Lit(_) => return t.to_owned(),
        TypeKind::Keyword(_) => return t.to_owned(),
        TypeKind::Union(types) => TypeKind::Union(
            types
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
        ),
        TypeKind::Intersection(types) => TypeKind::Intersection(
            types
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
        ),
        TypeKind::Object(obj) => {
            let elems: Vec<TObjElem> = obj
                .elems
                .iter()
                .map(|elem| match elem {
                    TObjElem::Call(lam) => {
                        let params: Vec<TFnParam> = lam
                            .params
                            .iter()
                            .map(|t| TFnParam {
                                t: replace_aliases_rec(&t.t, type_param_map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(lam.ret.as_ref(), type_param_map);

                        TObjElem::Call(TCallable {
                            params,
                            ret: Box::from(ret),
                            type_params: lam.type_params.to_owned(),
                        })
                    }
                    TObjElem::Constructor(lam) => {
                        let params: Vec<TFnParam> = lam
                            .params
                            .iter()
                            .map(|t| TFnParam {
                                t: replace_aliases_rec(&t.t, type_param_map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(lam.ret.as_ref(), type_param_map);

                        TObjElem::Constructor(TCallable {
                            params,
                            ret: Box::from(ret),
                            type_params: lam.type_params.to_owned(),
                        })
                    }
                    TObjElem::Method(method) => {
                        let params: Vec<TFnParam> = method
                            .params
                            .iter()
                            .map(|t| TFnParam {
                                t: replace_aliases_rec(&t.t, type_param_map),
                                ..t.to_owned()
                            })
                            .collect();
                        let ret = replace_aliases_rec(method.ret.as_ref(), type_param_map);

                        TObjElem::Method(TMethod {
                            params,
                            ret: Box::from(ret),
                            ..method.to_owned()
                        })
                    }
                    TObjElem::Getter(_) => todo!(),
                    TObjElem::Setter(_) => todo!(),
                    TObjElem::Index(index) => {
                        let t = replace_aliases_rec(&index.t, type_param_map);
                        TObjElem::Index(TIndex {
                            t,
                            ..index.to_owned()
                        })
                    }
                    TObjElem::Prop(prop) => {
                        let t = replace_aliases_rec(&prop.t, type_param_map);
                        TObjElem::Prop(TProp {
                            t,
                            ..prop.to_owned()
                        })
                    }
                })
                .collect();
            TypeKind::Object(TObject { elems })
        }
        TypeKind::Ref(alias) => match type_param_map.get(&alias.name) {
            Some(t) => {
                return t.to_owned();
            }
            None => match &alias.type_args {
                Some(type_args) => {
                    let type_args: Vec<_> = type_args
                        .iter()
                        .map(|arg| replace_aliases_rec(arg, type_param_map))
                        .collect();

                    return Type {
                        kind: TypeKind::Ref(TRef {
                            name: alias.name.to_owned(),
                            type_args: Some(type_args),
                        }),
                        provenance: None,
                        mutable: t.mutable,
                    };
                }
                None => return t.to_owned(),
            },
        },
        TypeKind::Tuple(types) => TypeKind::Tuple(
            types
                .iter()
                .map(|t| replace_aliases_rec(t, type_param_map))
                .collect(),
        ),
        TypeKind::Array(t) => TypeKind::Array(Box::from(replace_aliases_rec(t, type_param_map))),
        TypeKind::Rest(t) => TypeKind::Rest(Box::from(replace_aliases_rec(t, type_param_map))),
        TypeKind::This => TypeKind::This,
        TypeKind::KeyOf(t) => TypeKind::KeyOf(Box::from(replace_aliases_rec(t, type_param_map))),
        TypeKind::IndexAccess(TIndexAccess { object, index }) => {
            TypeKind::IndexAccess(TIndexAccess {
                object: Box::from(replace_aliases_rec(object, type_param_map)),
                index: Box::from(replace_aliases_rec(index, type_param_map)),
            })
        }
        TypeKind::MappedType(mapped) => {
            TypeKind::MappedType(TMappedType {
                t: Box::from(replace_aliases_rec(&mapped.t, type_param_map)),
                type_param: TypeParam {
                    name: mapped.type_param.name.to_owned(),
                    constraint: mapped.type_param.constraint.as_ref().map(|constraint| {
                        Box::from(replace_aliases_rec(constraint, type_param_map))
                    }),
                    default: mapped
                        .type_param
                        .default
                        .as_ref()
                        .map(|default| Box::from(replace_aliases_rec(default, type_param_map))),
                },
                ..mapped.to_owned()
            })
        }
        TypeKind::ConditionalType(TConditionalType {
            check_type,
            extends_type,
            true_type,
            false_type,
        }) => TypeKind::ConditionalType(TConditionalType {
            check_type: Box::from(replace_aliases_rec(check_type, type_param_map)),
            extends_type: Box::from(replace_aliases_rec(extends_type, type_param_map)),
            true_type: Box::from(replace_aliases_rec(true_type, type_param_map)),
            false_type: Box::from(replace_aliases_rec(false_type, type_param_map)),
        }),
    };

    Type {
        kind,
        ..t.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compose_subs() {
        let mut s2 = Subst::new();
        let mut s1 = Subst::new();

        s2.insert(
            3,
            Type::from(TypeKind::Var(TVar {
                id: 5,
                constraint: None,
            })),
        );
        s2.insert(
            2,
            Type::from(TypeKind::Var(TVar {
                id: 4,
                constraint: None,
            })),
        );

        s1.insert(
            4,
            Type::from(TypeKind::Var(TVar {
                id: 2,
                constraint: None,
            })),
        );

        let s = compose_subs(&s2, &s1);

        eprintln!("s = {s:#?}");
    }
}
