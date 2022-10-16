use array_tool::vec::*;
use std::collections::{HashMap, HashSet};

use crochet_types::*;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    // The vector return must not contain any `TVar`s with the same `id`.
    fn ftv(&self) -> Vec<TVar>;
}

impl Substitutable for Type {
    fn apply(&self, sub: &Subst) -> Type {
        let result = match self {
            Type::Generic(TGeneric { t, type_params }) => {
                // QUESTION: Do we really need to be filtering out type_params from
                // substitutions?
                let type_params: Vec<_> = type_params
                    .iter()
                    .filter(|tp| !sub.contains_key(&tp.id))
                    .cloned()
                    .collect();

                // If there are no type params then the returned type shouldn't be
                // a qualified type.
                if type_params.is_empty() {
                    t.apply(sub)
                } else {
                    Type::Generic(TGeneric {
                        t: Box::from(t.as_ref().apply(sub)),
                        type_params,
                    })
                }
            }

            Type::Var(tv) => match sub.get(&tv.id) {
                Some(replacement) => {
                    // TODO: apply the constraint and then check if the replacement
                    // is a subtype of it.
                    replacement.to_owned()
                }
                None => Type::Var(TVar {
                    id: tv.id.to_owned(),
                    constraint: tv
                        .constraint
                        .as_ref()
                        .map(|constraint| Box::from(constraint.apply(sub))),
                }),
            },
            Type::App(app) => Type::App(TApp {
                args: app.args.iter().map(|param| param.apply(sub)).collect(),
                ret: Box::from(app.ret.apply(sub)),
            }),
            // TODO: handle widening of lambdas
            Type::Lam(lam) => Type::Lam(lam.apply(sub)),
            Type::Lit(_) => self.to_owned(),
            Type::Keyword(_) => self.to_owned(),
            Type::Union(types) => Type::Union(types.apply(sub)),
            Type::Intersection(types) => Type::Intersection(types.apply(sub)),
            Type::Object(obj) => Type::Object(TObject {
                elems: obj.elems.apply(sub),
            }),
            Type::Ref(tr) => Type::Ref(tr.apply(sub)),
            Type::Tuple(types) => Type::Tuple(types.apply(sub)),
            Type::Array(t) => Type::Array(Box::from(t.apply(sub))),
            Type::Rest(arg) => Type::Rest(Box::from(arg.apply(sub))),
            Type::This => Type::This,
            Type::KeyOf(t) => Type::KeyOf(Box::from(t.apply(sub))),
            Type::Mutable(t) => Type::Mutable(Box::from(t.apply(sub))),
            Type::IndexAccess(TIndexAccess { object, index }) => Type::IndexAccess(TIndexAccess {
                object: Box::from(object.apply(sub)),
                index: Box::from(index.apply(sub)),
            }),
        };
        norm_type(result)
    }
    fn ftv(&self) -> Vec<TVar> {
        match self {
            Type::Generic(TGeneric { t, type_params }) => t
                .ftv()
                .uniq_via(type_params.to_owned(), |a, b| a.id == b.id),
            Type::Var(tv) => {
                let mut result = vec![tv.to_owned()];
                if let Some(constraint) = &tv.constraint {
                    result.extend(constraint.ftv());
                }
                result.unique_via(|a, b| a.id == b.id)
            }
            Type::App(TApp { args, ret }) => {
                let mut result = args.ftv();
                result.extend(ret.ftv());
                result.unique_via(|a, b| a.id == b.id)
            }
            Type::Lam(lam) => lam.ftv(),
            Type::Lit(_) => vec![],
            Type::Keyword(_) => vec![],
            Type::Union(types) => types.ftv(),
            Type::Intersection(types) => types.ftv(),
            Type::Object(obj) => obj.elems.ftv(),
            Type::Ref(tref) => tref.ftv(),
            Type::Tuple(types) => types.ftv(),
            Type::Array(t) => t.ftv(),
            Type::Rest(arg) => arg.ftv(),
            Type::This => vec![],
            Type::KeyOf(t) => t.ftv(),
            Type::Mutable(t) => t.ftv(),
            Type::IndexAccess(TIndexAccess { object, index }) => {
                let mut result = object.ftv();
                result.extend(index.ftv());
                result
            }
        }
    }
}

impl Substitutable for TObjElem {
    fn apply(&self, sub: &Subst) -> Self {
        match self {
            TObjElem::Call(qlam) => TObjElem::Call(qlam.apply(sub)),
            TObjElem::Constructor(qlam) => TObjElem::Constructor(qlam.apply(sub)),
            TObjElem::Index(index) => TObjElem::Index(index.apply(sub)),
            TObjElem::Prop(prop) => TObjElem::Prop(prop.apply(sub)),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        match self {
            TObjElem::Call(qlam) => qlam.ftv(),
            TObjElem::Constructor(qlam) => qlam.ftv(),
            TObjElem::Index(index) => index.ftv(),
            TObjElem::Prop(prop) => prop.t.ftv(),
        }
    }
}

impl Substitutable for TLam {
    fn apply(&self, sub: &Subst) -> Self {
        Self {
            params: self.params.iter().map(|param| param.apply(sub)).collect(),
            ret: Box::from(self.ret.apply(sub)),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.extend(self.ret.ftv());
        result.unique_via(|a, b| a.id == b.id)
    }
}

impl Substitutable for TRef {
    fn apply(&self, sub: &Subst) -> Self {
        TRef {
            type_args: self.type_args.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        match &self.type_args {
            Some(type_args) => type_args.ftv(),
            None => vec![],
        }
    }
}

impl Substitutable for TCallable {
    fn apply(&self, sub: &Subst) -> Self {
        // QUESTION: Do we really need to be filtering out type_params from
        // substitutions?
        let type_params = self
            .type_params
            .iter()
            .filter(|tp| !sub.contains_key(&tp.id))
            .cloned()
            .collect();

        Self {
            params: self.params.iter().map(|param| param.apply(sub)).collect(),
            ret: Box::from(self.ret.apply(sub)),
            type_params,
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.extend(self.ret.ftv());
        result.uniq(self.type_params.to_owned())
    }
}

impl Substitutable for TIndex {
    fn apply(&self, sub: &Subst) -> Self {
        TIndex {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

impl Substitutable for TProp {
    fn apply(&self, sub: &Subst) -> Self {
        TProp {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

impl Substitutable for TFnParam {
    fn apply(&self, sub: &Subst) -> Self {
        TFnParam {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

impl<I> Substitutable for HashMap<String, I>
where
    I: Substitutable,
{
    fn apply(&self, sub: &Subst) -> HashMap<String, I> {
        self.iter()
            .map(|(a, b)| (a.clone(), b.apply(sub)))
            .collect()
    }
    fn ftv(&self) -> Vec<TVar> {
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
    fn ftv(&self) -> Vec<TVar> {
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
    fn ftv(&self) -> Vec<TVar> {
        self.as_ref()
            .map(|val| val.to_owned().ftv())
            .unwrap_or_default()
    }
}

fn norm_type(t: Type) -> Type {
    match &t {
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
        _ => t,
    }
}
