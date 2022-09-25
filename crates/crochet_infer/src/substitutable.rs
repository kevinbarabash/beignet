use std::collections::{HashMap, HashSet};

use crochet_types::*;

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
            Type::App(app) => Type::App(TApp {
                args: app.args.iter().map(|param| param.apply(sub)).collect(),
                ret: Box::from(app.ret.apply(sub)),
            }),
            // TODO: handle widening of lambdas
            Type::Lam(lam) => Type::Lam(lam.apply(sub)),
            Type::Prim(_) => self.to_owned(),
            Type::Lit(_) => self.to_owned(),
            Type::Keyword(_) => self.to_owned(),
            Type::Union(types) => Type::Union(types.apply(sub)),
            Type::Intersection(types) => Type::Intersection(types.apply(sub)),
            Type::Object(obj) => {
                // QUESTION: Do we really need to be filtering out type_params from
                // substitutions?
                let type_params = obj
                    .type_params
                    .iter()
                    .filter(|tp| !sub.contains_key(tp))
                    .cloned()
                    .collect();

                Type::Object(TObject {
                    elems: obj.elems.apply(sub),
                    type_params,
                })
            }
            Type::Alias(alias) => Type::Alias(TAlias {
                type_args: alias.type_args.apply(sub),
                ..alias.to_owned()
            }),
            Type::Tuple(types) => Type::Tuple(types.apply(sub)),
            Type::Array(t) => Type::Array(Box::from(t.apply(sub))),
            Type::Rest(arg) => Type::Rest(Box::from(arg.apply(sub))),
            Type::This => Type::This,
        };
        norm_type(result)
    }
    fn ftv(&self) -> HashSet<i32> {
        match &self {
            Type::Var(id) => HashSet::from([id.to_owned()]),
            Type::App(TApp { args, ret }) => {
                let mut result: HashSet<_> = args.ftv();
                result.extend(ret.ftv());
                result
            }
            Type::Lam(lam) => lam.ftv(),
            Type::Prim(_) => HashSet::new(),
            Type::Lit(_) => HashSet::new(),
            Type::Keyword(_) => HashSet::new(),
            Type::Union(types) => types.ftv(),
            Type::Intersection(types) => types.ftv(),
            Type::Object(obj) => {
                let qualifiers: HashSet<_> =
                    obj.type_params.iter().map(|id| id.to_owned()).collect();
                obj.elems.ftv().difference(&qualifiers).cloned().collect()
            }
            Type::Alias(TAlias {
                type_args: type_params,
                ..
            }) => type_params.ftv(),
            Type::Tuple(types) => types.ftv(),
            Type::Array(t) => t.ftv(),
            Type::Rest(arg) => arg.ftv(),
            Type::This => HashSet::new(),
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
    fn ftv(&self) -> HashSet<i32> {
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
        // QUESTION: Do we really need to be filtering out type_params from
        // substitutions?
        let type_params = self
            .type_params
            .iter()
            .filter(|tp| !sub.contains_key(tp))
            .cloned()
            .collect();

        TLam {
            params: self.params.iter().map(|param| param.apply(sub)).collect(),
            ret: Box::from(self.ret.apply(sub)),
            type_params,
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        let type_params = HashSet::from_iter(self.type_params.iter().cloned());

        let mut result: HashSet<_> = self.params.ftv();
        result.extend(self.ret.ftv());
        result.difference(&type_params).cloned().collect()
    }
}

impl Substitutable for TIndex {
    fn apply(&self, sub: &Subst) -> Self {
        TIndex {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        self.t.ftv()
    }
}

impl Substitutable for TProp {
    fn apply(&self, sub: &Subst) -> TProp {
        TProp {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> HashSet<i32> {
        self.t.ftv()
    }
}

impl Substitutable for TFnParam {
    fn apply(&self, sub: &Subst) -> TFnParam {
        TFnParam {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> HashSet<i32> {
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
