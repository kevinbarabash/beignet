use array_tool::vec::*;
use std::collections::{BTreeSet, HashMap};

use crochet_ast::types::*;

use crate::context::Binding;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&mut self, subs: &Subst);
    // The vector return must not contain any `TVar`s with the same `id`.
    fn ftv(&self) -> Vec<TVar>;
}

impl Substitutable for Type {
    fn apply(&mut self, sub: &Subst) {
        match &mut self.kind {
            TypeKind::Generic(TGeneric { t, type_params: _ }) => {
                // QUESTION: Do we really need to be filtering out type_params from
                // substitutions?
                // let type_params: Vec<_> = type_params
                //     .iter()
                //     .filter(|tp| !sub.contains_key(&tp.id))
                //     .cloned()
                //     .collect();

                t.apply(sub);
            }

            TypeKind::Var(tv) => {
                match sub.get(&tv.id) {
                    Some(replacement) => {
                        // If the replacement has no provenance, point to ourselves.
                        let provenance = match replacement.provenance {
                            Some(_) => replacement.provenance.to_owned(),
                            None => Some(Box::from(Provenance::from(self.clone()))),
                        };

                        // TODO: apply the constraint and then check if the replacement
                        // is a subtype of it.
                        self.kind = replacement.kind.to_owned();
                        self.mutable = replacement.mutable;
                        self.provenance = provenance;

                        // norm_type(self);
                        // return norm_type(Type {
                        //     kind: replacement.kind.to_owned(),
                        //     provenance,
                        //     mutable: replacement.mutable,
                        // });
                    }
                    None => {
                        if let Some(constraint) = &mut tv.constraint {
                            constraint.apply(sub);
                        }
                        // TypeKind::Var(TVar {
                        //     id: tv.id.to_owned(),
                        //     constraint: tv
                        //         .constraint
                        //         .as_ref()
                        //         .map(|constraint| Box::from(constraint.apply(sub))),
                        // })
                    }
                }
            }
            TypeKind::App(app) => {
                app.args.apply(sub);
                app.ret.apply(sub);
                // TypeKind::App(TApp {
                //     args: app.args.iter().map(|param| param.apply(sub)).collect(),
                //     ret: Box::from(app.ret.apply(sub)),
                // })
            }
            // TODO: handle widening of lambdas
            TypeKind::Lam(lam) => lam.apply(sub),
            TypeKind::Lit(_) => norm_type(self),
            TypeKind::Keyword(_) => norm_type(self),
            TypeKind::Union(types) => types.apply(sub),
            TypeKind::Intersection(types) => types.apply(sub),
            TypeKind::Object(obj) => obj.elems.apply(sub),
            TypeKind::Ref(tr) => tr.apply(sub),
            TypeKind::Tuple(types) => types.apply(sub),
            TypeKind::Array(t) => t.apply(sub),
            TypeKind::Rest(arg) => arg.apply(sub),
            TypeKind::This => (),
            TypeKind::KeyOf(t) => t.apply(sub),
            TypeKind::IndexAccess(TIndexAccess { object, index }) => {
                object.apply(sub);
                index.apply(sub);
            }
            TypeKind::MappedType(mapped) => {
                mapped.t.apply(sub);
                if let Some(constraint) = &mut mapped.type_param.constraint {
                    constraint.apply(sub);
                }
                if let Some(default) = &mut mapped.type_param.default {
                    default.apply(sub);
                }
            }
            TypeKind::ConditionalType(TConditionalType {
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => {
                check_type.apply(sub);
                extends_type.apply(sub);
                true_type.apply(sub);
                false_type.apply(sub);
            }
        };

        if let TypeKind::Generic(TGeneric { t, type_params }) = &self.kind {
            if type_params.is_empty() {
                self.kind = t.kind.clone();
            }
        }

        norm_type(self)
    }
    fn ftv(&self) -> Vec<TVar> {
        match &self.kind {
            TypeKind::Generic(TGeneric { t, type_params }) => t
                .ftv()
                .uniq_via(type_params.to_owned(), |a, b| a.id == b.id),
            TypeKind::Var(tv) => {
                let mut result = vec![tv.to_owned()];
                if let Some(constraint) = &tv.constraint {
                    result.append(&mut constraint.ftv());
                }
                result.unique_via(|a, b| a.id == b.id)
            }
            TypeKind::App(TApp { args, ret }) => {
                let mut result = args.ftv();
                result.append(&mut ret.ftv());
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
            TypeKind::IndexAccess(TIndexAccess { object, index }) => {
                let mut result = object.ftv();
                result.append(&mut index.ftv());
                result
            }
            TypeKind::MappedType(TMappedType { type_param, t, .. }) => {
                let mut result = t.ftv();
                if let Some(constraint) = &type_param.constraint {
                    result.append(&mut constraint.ftv());
                }
                if let Some(default) = &type_param.default {
                    result.append(&mut default.ftv());
                }

                result
            }
            TypeKind::ConditionalType(TConditionalType {
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => {
                let mut result = vec![];
                result.append(&mut check_type.ftv());
                result.append(&mut extends_type.ftv());
                result.append(&mut true_type.ftv());
                result.append(&mut false_type.ftv());
                result
            }
        }
    }
}

impl Substitutable for TObjElem {
    fn apply(&mut self, sub: &Subst) {
        match self {
            TObjElem::Call(qlam) => qlam.apply(sub),
            TObjElem::Constructor(qlam) => qlam.apply(sub),
            TObjElem::Index(index) => index.apply(sub),
            TObjElem::Prop(prop) => prop.apply(sub),
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
    fn apply(&mut self, sub: &Subst) {
        self.params.apply(sub);
        self.ret.apply(sub);
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.append(&mut self.ret.ftv());
        result.unique_via(|a, b| a.id == b.id)
    }
}

impl Substitutable for TRef {
    fn apply(&mut self, sub: &Subst) {
        self.type_args
            .iter_mut()
            .for_each(|type_arg| type_arg.apply(sub));
        // TRef {
        //     type_args: self.type_args.apply(sub),
        //     ..self.to_owned()
        // }
    }
    fn ftv(&self) -> Vec<TVar> {
        match &self.type_args {
            Some(type_args) => type_args.ftv(),
            None => vec![],
        }
    }
}

impl Substitutable for TCallable {
    fn apply(&mut self, sub: &Subst) {
        // QUESTION: Do we really need to be filtering out type_params from
        // substitutions?
        let type_params = self
            .type_params
            .iter()
            .filter(|tp| !sub.contains_key(&tp.id))
            .cloned()
            .collect();

        self.params.iter_mut().for_each(|param| param.apply(sub));
        self.ret.apply(sub);
        self.type_params = type_params;
        // Self {
        //     params: self.params.iter().map(|param| param.apply(sub)).collect(),
        //     ret: Box::from(self.ret.apply(sub)),
        //     type_params,
        // }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.append(&mut self.ret.ftv());
        result.uniq(self.type_params.to_owned())
    }
}

impl Substitutable for TIndex {
    fn apply(&mut self, sub: &Subst) {
        self.t.apply(sub);
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

impl Substitutable for TProp {
    fn apply(&mut self, sub: &Subst) {
        self.t.apply(sub);
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

impl Substitutable for TFnParam {
    fn apply(&mut self, sub: &Subst) {
        self.t.apply(sub)
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

impl<I> Substitutable for HashMap<String, I>
where
    I: Substitutable,
{
    fn apply(&mut self, sub: &Subst) {
        for val in self.values_mut() {
            val.apply(sub)
        }
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
    fn apply(&mut self, sub: &Subst) {
        self.iter_mut().for_each(|elem| elem.apply(sub))
    }
    fn ftv(&self) -> Vec<TVar> {
        self.iter().flat_map(|c| c.ftv()).collect()
    }
}

impl<I> Substitutable for Option<I>
where
    I: Substitutable,
{
    fn apply(&mut self, sub: &Subst) {
        if let Some(val) = self {
            val.apply(sub);
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        self.as_ref()
            .map(|val| val.to_owned().ftv())
            .unwrap_or_default()
    }
}

impl Substitutable for Binding {
    fn apply(&mut self, sub: &Subst) {
        self.t.apply(sub);
    }
    fn ftv(&self) -> Vec<TVar> {
        self.t.ftv()
    }
}

fn norm_type(t: &'_ mut Type) {
    match &t.kind {
        TypeKind::Union(types) => {
            // Removes duplicates
            let types: BTreeSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let types: Vec<Type> = types.into_iter().collect();

            if types.len() == 1 {
                t.kind = types.get(0).unwrap().kind.clone();
                // types.get(0).unwrap().to_owned()
            } else {
                t.kind = TypeKind::Union(types);
                // Type::from(TypeKind::Union(types))
            }
        }
        TypeKind::Intersection(types) => {
            // Removes duplicates
            let types: BTreeSet<Type> = types.clone().into_iter().collect();
            // Converts set back to an array
            let types: Vec<Type> = types.into_iter().collect();

            if types.len() == 1 {
                t.kind = types.get(0).unwrap().kind.clone();
                // types.get(0).unwrap().to_owned()
            } else {
                t.kind = TypeKind::Intersection(types);
                // Type::from(TypeKind::Intersection(types))
            }
        }
        _ => (),
    }
}
