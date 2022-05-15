use std::collections::HashMap;
use std::collections::HashSet;

use crate::types::*;

use super::constraint_solver::{Constraint};
use super::context::{Env};

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
        let id = self.id;
        let frozen = self.frozen;
        match &self.kind {
            TypeKind::Var => sub.get(&id).unwrap_or(self).clone(),
            // TODO: handle widening of lambdas
            TypeKind::Lam(TLam { args, ret }) => match sub.get(&id) {
                Some(replacement) => replacement.to_owned(),
                None => Type {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    kind: TypeKind::Lam(TLam {
                        args: args.iter().map(|arg| arg.apply(sub)).collect(),
                        ret: Box::from(ret.apply(sub)),
                    }),
                },
            },
            TypeKind::Prim(_) => sub.get(&id).unwrap_or(self).clone(),
            TypeKind::Lit(_) => sub.get(&id).unwrap_or(self).clone(),
            TypeKind::Union(types) => match sub.get(&id) {
                Some(replacement) => replacement.to_owned(),
                None => Type {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    kind: TypeKind::Union(types.iter().map(|ty| ty.apply(sub)).collect()),
                },
            },
            TypeKind::Obj(props) => match sub.get(&id) {
                Some(replacement) => replacement.to_owned(),
                None => Type {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    kind: TypeKind::Obj(
                        props
                            .iter()
                            .map(|prop| TProp {
                                name: prop.name.clone(),
                                ty: prop.ty.apply(sub),
                            })
                            .collect(),
                    ),
                },
            },
            TypeKind::Alias { name, type_params } => match sub.get(&id) {
                Some(replacement) => replacement.to_owned(),
                None => Type {
                    id: id.to_owned(),
                    frozen: frozen.to_owned(),
                    kind: TypeKind::Alias {
                        name: name.to_owned(),
                        type_params: type_params.iter().map(|ty| ty.apply(sub)).collect(),
                    },
                },
            },
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        let id = self.id;
        match &self.kind {
            TypeKind::Var => HashSet::from([id.to_owned()]),
            TypeKind::Lam(TLam { args, ret }) => {
                let mut result: HashSet<_> = args.iter().flat_map(|a| a.ftv()).collect();
                result.extend(ret.ftv());
                result
            }
            TypeKind::Prim(_) => HashSet::new(),
            TypeKind::Lit(_) => HashSet::new(),
            TypeKind::Union(types) => types.iter().flat_map(|ty| ty.ftv()).collect(),
            TypeKind::Obj(props) => props.iter().flat_map(|prop| prop.ty.ftv()).collect(),
            TypeKind::Alias { type_params, .. } => {
                type_params.iter().flat_map(|ty| ty.ftv()).collect()
            }
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
