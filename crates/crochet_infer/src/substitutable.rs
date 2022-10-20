use array_tool::vec::*;
use std::collections::{BTreeSet, HashMap};

use crochet_ast::types::*;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    // The vector return must not contain any `TVar`s with the same `id`.
    fn ftv(&self) -> Vec<TVar>;
}

impl Substitutable for Type {
    fn apply(&self, sub: &Subst) -> Type {
        let kind = match &self.kind {
            TypeKind::Generic(TGeneric { t, type_params }) => {
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
                    return norm_type(t.apply(sub));
                } else {
                    TypeKind::Generic(TGeneric {
                        t: Box::from(t.as_ref().apply(sub)),
                        type_params,
                    })
                }
            }

            TypeKind::Var(tv) => match sub.get(&tv.id) {
                Some(replacement) => {
                    // TODO: apply the constraint and then check if the replacement
                    // is a subtype of it.
                    return norm_type(replacement.to_owned());
                }
                None => TypeKind::Var(TVar {
                    id: tv.id.to_owned(),
                    constraint: tv
                        .constraint
                        .as_ref()
                        .map(|constraint| Box::from(constraint.apply(sub))),
                }),
            },
            TypeKind::App(app) => TypeKind::App(TApp {
                args: app.args.iter().map(|param| param.apply(sub)).collect(),
                ret: Box::from(app.ret.apply(sub)),
            }),
            // TODO: handle widening of lambdas
            TypeKind::Lam(lam) => TypeKind::Lam(lam.apply(sub)),
            TypeKind::Lit(_) => return norm_type(self.to_owned()),
            TypeKind::Keyword(_) => return norm_type(self.to_owned()),
            TypeKind::Union(types) => TypeKind::Union(types.apply(sub)),
            TypeKind::Intersection(types) => TypeKind::Intersection(types.apply(sub)),
            TypeKind::Object(obj) => TypeKind::Object(TObject {
                elems: obj.elems.apply(sub),
            }),
            TypeKind::Ref(tr) => TypeKind::Ref(tr.apply(sub)),
            TypeKind::Tuple(types) => TypeKind::Tuple(types.apply(sub)),
            TypeKind::Array(t) => TypeKind::Array(Box::from(t.apply(sub))),
            TypeKind::Rest(arg) => TypeKind::Rest(Box::from(arg.apply(sub))),
            TypeKind::This => TypeKind::This,
            TypeKind::KeyOf(t) => TypeKind::KeyOf(Box::from(t.apply(sub))),
            TypeKind::Mutable(t) => TypeKind::Mutable(Box::from(t.apply(sub))),
            TypeKind::IndexAccess(TIndexAccess { object, index }) => {
                TypeKind::IndexAccess(TIndexAccess {
                    object: Box::from(object.apply(sub)),
                    index: Box::from(index.apply(sub)),
                })
            }
        };
        norm_type(Type { kind })
    }
    fn ftv(&self) -> Vec<TVar> {
        match &self.kind {
            TypeKind::Generic(TGeneric { t, type_params }) => t
                .ftv()
                .uniq_via(type_params.to_owned(), |a, b| a.id == b.id),
            TypeKind::Var(tv) => {
                let mut result = vec![tv.to_owned()];
                if let Some(constraint) = &tv.constraint {
                    result.extend(constraint.ftv());
                }
                result.unique_via(|a, b| a.id == b.id)
            }
            TypeKind::App(TApp { args, ret }) => {
                let mut result = args.ftv();
                result.extend(ret.ftv());
                result.unique_via(|a, b| a.id == b.id)
            }
            TypeKind::Lam(lam) => lam.ftv(),
            TypeKind::Lit(_) => vec![],
            TypeKind::Keyword(_) => vec![],
            TypeKind::Union(types) => types.ftv(),
            TypeKind::Intersection(types) => types.ftv(),
            TypeKind::Object(obj) => obj.elems.ftv(),
            TypeKind::Ref(tref) => tref.ftv(),
            TypeKind::Tuple(types) => types.ftv(),
            TypeKind::Array(t) => t.ftv(),
            TypeKind::Rest(arg) => arg.ftv(),
            TypeKind::This => vec![],
            TypeKind::KeyOf(t) => t.ftv(),
            TypeKind::Mutable(t) => t.ftv(),
            TypeKind::IndexAccess(TIndexAccess { object, index }) => {
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
    match &t.kind {
        TypeKind::Union(types) => {
            // Removes duplicates
            let types: BTreeSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let types: Vec<Type> = types.into_iter().collect();

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type {
                    kind: TypeKind::Union(types),
                }
            }
        }
        TypeKind::Intersection(types) => {
            // Removes duplicates
            let types: BTreeSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let types: Vec<Type> = types.into_iter().collect();

            if types.len() == 1 {
                types.get(0).unwrap().to_owned()
            } else {
                Type {
                    kind: TypeKind::Intersection(types),
                }
            }
        }
        _ => t,
    }
}
