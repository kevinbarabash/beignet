use defaultmap::*;
use derive_visitor::{DriveMut, VisitorMut};
use im::hashmap::HashMap;
use std::collections::BTreeSet;
use std::iter::Iterator;

use escalier_ast::types::*;

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
    let t = t.apply(s);

    let tvs = t.ftv();

    let t = if tvs.is_empty() {
        t
    } else {
        match &t.kind {
            // Generalize the lambda by giving it type parameters based on all
            // of its free type variables.
            TypeKind::Lam(TLam {
                params,
                ret,
                // TODO: handle situations where the lambda we're closing over
                // already has type parameters.
                type_params: _,
            }) => {
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

                let t = Type::from(TypeKind::Lam(TLam {
                    params: params.to_owned(),
                    ret: ret.to_owned(),
                    type_params: if type_params.is_empty() {
                        None
                    } else {
                        Some(type_params)
                    },
                }));

                t.apply(&sub)
            }
            _ => {
                eprintln!("t = {t}");
                panic!("We shouldn't have any free type variables when closing over non-lambdas")
            }
        }
    };

    normalize(&t, ctx)
}

#[derive(VisitorMut)]
#[visitor(Type(exit))]
struct NormalizeVisitor {
    mapping: HashMap<i32, Type>,
}

impl NormalizeVisitor {
    fn exit_type(&mut self, t: &mut Type) {
        if let TypeKind::Var(tv) = &t.kind {
            if let Some(rep_t) = self.mapping.get(&tv.id) {
                t.kind = rep_t.kind.to_owned();
                t.mutable = rep_t.mutable;
                t.provenance = rep_t.provenance.to_owned();
            }
        }
    }
}

pub fn normalize(t: &Type, _ctx: &Context) -> Type {
    let mapping = get_mapping(t);
    let mut t = t.clone();
    let mut visitor = NormalizeVisitor { mapping };
    t.drive_mut(&mut visitor);
    t
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
        out_types.push(Type::from(TypeKind::Object(TObject {
            elems,
            is_interface: false,
        })));
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
            let t = t.apply(s2);
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
            let t = t.apply(s1);
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

pub fn replace_aliases_in_lam(lam: &TLam, type_param_map: &HashMap<String, Type>) -> TLam {
    let TLam {
        params,
        ret,
        type_params,
    } = lam;

    let mut type_param_map = type_param_map.clone();
    if let Some(type_params) = type_params {
        for type_param in type_params {
            type_param_map.remove(&type_param.name);
        }
    }

    TLam {
        params: params
            .iter()
            .map(|param| TFnParam {
                t: replace_aliases_rec(&param.t, &type_param_map),
                ..param.to_owned()
            })
            .collect(),
        ret: Box::from(replace_aliases_rec(ret, &type_param_map)),
        type_params: type_params.to_owned(),
    }
}

#[derive(VisitorMut)]
#[visitor(Type(exit), TMethod)]
struct ReplaceAliasVisitor<'a> {
    type_param_map: &'a HashMap<String, Type>,
    // Methods can have their own type parameters which shadow type parameters
    // with the same names that are in scope.
    shadowed_type_param_names: Vec<String>,
}

impl ReplaceAliasVisitor<'_> {
    fn enter_t_method(&mut self, method: &mut TMethod) {
        if let Some(type_params) = &method.type_params {
            for type_param in type_params {
                self.shadowed_type_param_names
                    .push(type_param.name.to_owned());
            }
        }
    }
    fn exit_t_method(&mut self, method: &mut TMethod) {
        if let Some(type_params) = &method.type_params {
            for _ in type_params {
                self.shadowed_type_param_names.pop();
            }
        }
    }
    fn exit_type(&mut self, t: &mut Type) {
        if let TypeKind::Ref(alias) = &t.kind {
            if !self.shadowed_type_param_names.contains(&alias.name) {
                if let Some(rep_t) = self.type_param_map.get(&alias.name) {
                    t.kind = rep_t.kind.to_owned();
                    t.mutable = rep_t.mutable;
                    t.provenance = rep_t.provenance.to_owned();
                }
            }
        }
    }
}

pub fn replace_aliases_rec(t: &Type, type_param_map: &HashMap<String, Type>) -> Type {
    let mut t = t.clone();
    let mut visitor = ReplaceAliasVisitor {
        type_param_map,
        shadowed_type_param_names: vec![],
    };
    t.drive_mut(&mut visitor);
    t
}

pub fn immutable_obj_type(obj: &TObject) -> Option<TObject> {
    let mut changed = false;
    let elems: Vec<TObjElem> = obj
        .elems
        .iter()
        .filter_map(|elem| match elem {
            TObjElem::Call(_) => Some(elem.to_owned()),
            TObjElem::Constructor(_) => Some(elem.to_owned()),
            TObjElem::Method(method) => {
                if method.is_mutating {
                    changed = true;
                    None
                } else {
                    Some(elem.to_owned())
                }
                // TODO: Convert any `mut Self` to `Self`.  This is going to be
                // a little tricky b/c we need to know the name of the type and
                // in the case of arrays, that it's an array and what its type
                // argument is.
            }
            TObjElem::Getter(_) => Some(elem.to_owned()),
            TObjElem::Setter(_) => {
                changed = true;
                None
            }
            TObjElem::Index(index) => {
                if index.mutable {
                    changed = true;
                }
                Some(TObjElem::Index(TIndex {
                    mutable: false,
                    ..index.to_owned()
                }))
            }
            TObjElem::Prop(prop) => {
                if prop.mutable {
                    changed = true;
                }
                Some(TObjElem::Prop(TProp {
                    mutable: false,
                    ..prop.to_owned()
                }))
            }
        })
        .collect();

    if changed {
        Some(TObject {
            elems,
            is_interface: obj.is_interface,
        })
    } else {
        None
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

    #[test]
    fn test_close_over() {
        let ctx = Context::default();
        let tv = ctx.fresh_var();
        let lam = TLam {
            type_params: None,
            params: vec![
                TFnParam {
                    pat: TPat::Ident(BindingIdent {
                        name: "a".to_string(),
                        mutable: false,
                    }),
                    t: tv.clone(),
                    optional: false,
                },
                TFnParam {
                    pat: TPat::Ident(BindingIdent {
                        name: "b".to_string(),
                        mutable: false,
                    }),
                    t: ctx.fresh_var(),
                    optional: false,
                },
            ],
            ret: Box::from(tv),
        };
        let t = Type::from(TypeKind::Lam(lam));
        let s = Subst::default();
        let result = close_over(&s, &t, &ctx);

        assert_eq!(result.to_string(), "<A, B>(a: A, b: B) => A");
    }
}
