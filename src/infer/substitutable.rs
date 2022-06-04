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
        let result = match sub.get(&self.id) {
            Some(replacement) => replacement.to_owned(),
            None => {
                let variant = match &self.variant {
                    Variant::Var => self.variant.to_owned(),
                    // TODO: handle widening of lambdas
                    Variant::Lam(LamType { params, ret }) => Variant::Lam(LamType {
                        params: params.iter().map(|param| param.apply(sub)).collect(),
                        ret: Box::from(ret.apply(sub)),
                    }),
                    Variant::Prim(_) => self.variant.to_owned(),
                    Variant::Lit(_) => self.variant.to_owned(),
                    Variant::Union(types) => Variant::Union(types.apply(sub)),
                    Variant::Intersection(types) => Variant::Intersection(types.apply(sub)),
                    Variant::Object(props) => Variant::Object(
                        props
                            .iter()
                            .map(|prop| TProp {
                                ty: prop.ty.apply(sub),
                                ..prop.to_owned()
                            })
                            .collect(),
                    ),
                    Variant::Alias(alias) => Variant::Alias(AliasType {
                        type_params: alias
                            .type_params
                            .clone()
                            .map(|params| params.iter().map(|ty| ty.apply(sub)).collect()),
                        ..alias.to_owned()
                    }),
                    Variant::Tuple(types) => {
                        Variant::Tuple(types.iter().map(|ty| ty.apply(sub)).collect())
                    }
                    Variant::Rest(arg) => Variant::Rest(Box::from(arg.apply(sub))),
                    Variant::Member(member) => Variant::Member(MemberType {
                        obj: Box::from(member.obj.apply(sub)),
                        ..member.to_owned()
                    }),
                };
                Type {
                    variant,
                    ..self.to_owned()
                }
            }
        };
        norm_type(result)
    }
    fn ftv(&self) -> HashSet<i32> {
        match &self.variant {
            Variant::Var => HashSet::from([self.id.to_owned()]),
            Variant::Lam(LamType { params, ret, .. }) => {
                let mut result: HashSet<_> = params.ftv();
                result.extend(ret.ftv());
                result
            }
            Variant::Prim(_) => HashSet::new(),
            Variant::Lit(_) => HashSet::new(),
            Variant::Union(types) => types.ftv(),
            Variant::Intersection(types) => types.ftv(),
            Variant::Object(props) => {
                // TODO: implement Substitutable fro TProp
                props.iter().flat_map(|prop| prop.ty.ftv()).collect()
            }
            Variant::Alias(AliasType { type_params, .. }) => {
                // TODO: implement Substitutable fro Option
                type_params.iter().flat_map(|ty| ty.ftv()).collect()
            }
            Variant::Tuple(types) => types.ftv(),
            Variant::Rest(arg) => arg.ftv(),
            Variant::Member(MemberType { obj, .. }) => obj.ftv(),
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
    match &ty.variant {
        Variant::Union(types) => {
            // Removes duplicates
            let types: HashSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let mut types: Vec<Type> = types.into_iter().collect();
            types.sort_by_key(|k| k.id);

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type {
                    variant: Variant::Union(types),
                    ..ty.to_owned()
                }
            }
        }
        Variant::Intersection(types) => {
            // Removes duplicates
            let types: HashSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let mut types: Vec<Type> = types.into_iter().collect();
            types.sort_by_key(|k| k.id);

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type {
                    variant: Variant::Intersection(types),
                    ..ty.to_owned()
                }
            }
        }
        _ => ty,
    }
}
