use std::collections::{HashMap, HashSet};

use super::context::Env;
use super::types::*;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    // TODO: use an ordered set
    fn ftv(&self) -> HashSet<i32>;
}

impl Substitutable for Type {
    fn apply(&self, sub: &Subst) -> Type {
        let result = match self {
            Type::Var(id) => match sub.get(id) {
                Some(replacement) => replacement.to_owned(),
                None => self.to_owned(),
            },
            Type::App(app) => Type::App(AppType {
                args: app.args.iter().map(|param| param.apply(sub)).collect(),
                ret: Box::from(app.ret.apply(sub)),
            }),
            // TODO: handle widening of lambdas
            Type::Lam(lam) => Type::Lam(LamType {
                params: lam.params.iter().map(|param| param.apply(sub)).collect(),
                ret: Box::from(lam.ret.apply(sub)),
            }),
            Type::Wildcard => self.to_owned(),
            Type::Prim(_) => self.to_owned(),
            Type::Lit(_) => self.to_owned(),
            Type::Union(types) => Type::Union(types.apply(sub)),
            Type::Intersection(types) => Type::Intersection(types.apply(sub)),
            Type::Object(props) => Type::Object(props.apply(sub)),
            Type::Alias(alias) => Type::Alias(AliasType {
                type_params: alias.type_params.apply(sub),
                ..alias.to_owned()
            }),
            Type::Tuple(types) => Type::Tuple(types.apply(sub)),
            Type::Array(t) => Type::Array(Box::from(t.apply(sub))),
            Type::Rest(arg) => Type::Rest(Box::from(arg.apply(sub))),
        };
        norm_type(result)
    }
    fn ftv(&self) -> HashSet<i32> {
        match &self {
            Type::Var(id) => HashSet::from([id.to_owned()]),
            Type::App(AppType { args, ret }) => {
                let mut result: HashSet<_> = args.ftv();
                result.extend(ret.ftv());
                result
            }
            Type::Lam(LamType { params, ret, .. }) => {
                let mut result: HashSet<_> = params.ftv();
                result.extend(ret.ftv());
                result
            }
            Type::Wildcard => HashSet::new(),
            Type::Prim(_) => HashSet::new(),
            Type::Lit(_) => HashSet::new(),
            Type::Union(types) => types.ftv(),
            Type::Intersection(types) => types.ftv(),
            Type::Object(props) => props.ftv(),
            Type::Alias(AliasType { type_params, .. }) => type_params.ftv(),
            Type::Tuple(types) => types.ftv(),
            Type::Array(t) => t.ftv(),
            Type::Rest(arg) => arg.ftv(),
        }
    }
}

impl Substitutable for TProp {
    fn apply(&self, sub: &Subst) -> TProp {
        TProp {
            ty: self.ty.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        self.ty.ftv()
    }
}

impl Substitutable for TFnParam {
    fn apply(&self, sub: &Subst) -> TFnParam {
        TFnParam {
            ty: self.ty.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        self.ty.ftv()
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

impl<I> Substitutable for Option<I>
where
    I: Substitutable,
{
    fn apply(&self, sub: &Subst) -> Option<I> {
        self.as_ref().map(|val| val.to_owned().apply(sub))
    }
    fn ftv(&self) -> HashSet<i32> {
        self.as_ref()
            .map(|val| val.to_owned().ftv())
            .unwrap_or_default()
    }
}

fn norm_type(ty: Type) -> Type {
    match &ty {
        Type::Union(types) => {
            // Removes duplicates
            let types: HashSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let types: Vec<Type> = types.into_iter().collect();

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type::Union(types)
            }
        }
        Type::Intersection(types) => {
            // Removes duplicates
            let types: HashSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let types: Vec<Type> = types.into_iter().collect();

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type::Intersection(types)
            }
        }
        _ => ty,
    }
}
