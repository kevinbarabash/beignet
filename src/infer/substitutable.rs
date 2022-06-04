use std::collections::HashMap;
use std::collections::HashSet;

use crate::types::*;

use super::constraint_solver::Constraint;
use super::context::Env;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    // TODO: use an ordered set
    fn ftv(&self) -> HashSet<i32>;
}

impl Substitutable for Constraint {
    fn apply(&self, sub: &Subst) -> Constraint {
        Constraint {
            types: (self.types.0.apply(sub), self.types.1.apply(sub)),
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        let mut result = HashSet::new();
        result.extend(self.types.0.ftv());
        result.extend(self.types.1.ftv());
        result
    }
}

impl Substitutable for Type {
    fn apply(&self, sub: &Subst) -> Type {
        match self {
            Type::Var(VarType { id, .. }) => sub.get(id).unwrap_or(self).clone(),
            // TODO: handle widening of lambdas
            Type::Lam(LamType {
                id,
                frozen,
                params,
                ret,
            }) => match sub.get(id) {
                Some(replacement) => replacement.to_owned(),
                None => Type::Lam(LamType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    params: params.iter().map(|param| param.apply(sub)).collect(),
                    ret: Box::from(ret.apply(sub)),
                }),
            },
            Type::Prim(PrimType { id, .. }) => sub.get(id).unwrap_or(self).clone(),
            Type::Lit(LitType { id, .. }) => sub.get(id).unwrap_or(self).clone(),
            Type::Union(UnionType { id, frozen, types }) => match sub.get(id) {
                Some(replacement) => replacement.to_owned(),
                None => norm_type(Type::Union(UnionType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    types: types.apply(sub),
                })),
            },
            Type::Intersection(IntersectionType { id, frozen, types }) => match sub.get(id) {
                Some(replacement) => replacement.to_owned(),
                None => norm_type(Type::Intersection(IntersectionType {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    types: types.apply(sub),
                })),
            },
            Type::Object(object) => match sub.get(&object.id) {
                Some(replacement) => replacement.to_owned(),
                None => Type::Object(ObjectType {
                    props: object
                        .props
                        .iter()
                        .map(|prop| TProp {
                            ty: prop.ty.apply(sub),
                            ..prop.to_owned()
                        })
                        .collect(),
                    ..object.to_owned()
                }),
            },
            Type::Alias(alias) => match sub.get(&alias.id) {
                Some(replacement) => replacement.to_owned(),
                None => Type::Alias(AliasType {
                    type_params: alias
                        .type_params
                        .clone()
                        .map(|params| params.iter().map(|ty| ty.apply(sub)).collect()),
                    ..alias.to_owned()
                }),
            },
            Type::Tuple(tuple) => match sub.get(&tuple.id) {
                Some(replacement) => replacement.to_owned(),
                None => Type::Tuple(TupleType {
                    types: tuple.types.iter().map(|ty| ty.apply(sub)).collect(),
                    ..tuple.to_owned()
                }),
            },
            Type::Rest(rest) => match sub.get(&rest.id) {
                Some(replacement) => replacement.to_owned(),
                None => Type::Rest(RestType {
                    ty: Box::from(rest.ty.apply(sub)),
                    ..rest.to_owned()
                }),
            }
            Type::Member(member) => match sub.get(&member.id) {
                Some(replacement) => replacement.to_owned(),
                None => Type::Member(MemberType {
                    obj: Box::from(member.obj.apply(sub)),
                    ..member.to_owned()
                }),
            },
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        match self {
            Type::Var(VarType { id, .. }) => HashSet::from([id.to_owned()]),
            Type::Lam(LamType { params, ret, .. }) => {
                let mut result: HashSet<_> = params.ftv();
                result.extend(ret.ftv());
                result
            }
            Type::Prim(_) => HashSet::new(),
            Type::Lit(_) => HashSet::new(),
            Type::Union(UnionType { types, .. }) => types.ftv(),
            Type::Intersection(IntersectionType { types, .. }) => types.ftv(),
            Type::Object(ObjectType { props, .. }) => {
                // TODO: implement Substitutable fro TProp
                props.iter().flat_map(|prop| prop.ty.ftv()).collect()
            }
            Type::Alias(AliasType { type_params, .. }) => {
                // TODO: implement Substitutable fro Option
                type_params.iter().flat_map(|ty| ty.ftv()).collect()
            }
            Type::Tuple(TupleType { types, .. }) => types.ftv(),
            Type::Rest(RestType { ty, .. }) => ty.ftv(),
            Type::Member(MemberType { obj, .. }) => obj.ftv(),
        }
    }
}

impl Substitutable for Scheme {
    fn apply(&self, sub: &Subst) -> Scheme {
        Scheme {
            qualifiers: self.qualifiers.clone(),
            ty: self.ty.apply(sub),
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        let qualifiers: HashSet<_> = self.qualifiers.iter().map(|id| id.to_owned()).collect();
        self.ty.ftv().difference(&qualifiers).cloned().collect()
    }
}

impl Substitutable for Env {
    fn apply(&self, sub: &Subst) -> Env {
        self.iter()
            .map(|(a, b)| (a.clone(), b.apply(sub)))
            .collect()
    }
    fn ftv(&self) -> HashSet<i32> {
        // we can't use iter_values() here because it's a consuming iterator
        self.iter().flat_map(|(_, b)| b.ftv()).collect()
    }
}

impl<I> Substitutable for Vec<I>
where
    I: Substitutable,
{
    fn apply(&self, sub: &Subst) -> Vec<I> {
        self.iter().map(|c| c.apply(sub)).collect()
    }
    fn ftv(&self) -> HashSet<i32> {
        self.iter().flat_map(|c| c.ftv()).collect()
    }
}

fn norm_type(ty: Type) -> Type {
    match ty {
        Type::Union(union) => {
            let types: HashSet<_> = union.types.into_iter().collect();
            let mut types: Vec<_> = types.into_iter().collect();
            types.sort_by_key(|k| k.id());

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type::Union(UnionType { types, ..union })
            }
        }
        Type::Intersection(intersection) => {
            let types: HashSet<_> = intersection.types.into_iter().collect();
            let mut types: Vec<_> = types.into_iter().collect();
            types.sort_by_key(|k| k.id());

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type::Intersection(IntersectionType {
                    types,
                    ..intersection
                })
            }
        }
        _ => ty,
    }
}
