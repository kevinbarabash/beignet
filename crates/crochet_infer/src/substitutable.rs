use array_tool::vec::*;
use std::collections::{BTreeSet, HashMap};
use std::iter::IntoIterator;

use crochet_ast::types::*;

use crate::context::Binding;
use crate::scheme::Scheme;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&mut self, subs: &Subst);
    // The vector return must not contain any `TVar`s with the same `id`.
    fn ftv(&self) -> Vec<TVar>;
}

impl Substitutable for Type {
    fn apply(&mut self, sub: &Subst) {
        match &mut self.kind {
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
                    }
                    None => {
                        if let Some(constraint) = &mut tv.constraint {
                            constraint.apply(sub);
                        }
                    }
                }
            }
            TypeKind::App(app) => {
                app.args.apply(sub);
                app.ret.apply(sub);
            }
            // TODO: handle widening of lambdas
            TypeKind::Lam(lam) => lam.apply(sub),
            TypeKind::GenLam(TGenLam { lam, type_params }) => {
                // TypeParams can have constraints and defaults which are types
                // so we apply the substitution to those here.
                type_params.apply(sub);
                lam.apply(sub)
            }
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

        norm_type(self)
    }
    fn ftv(&self) -> Vec<TVar> {
        match &self.kind {
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
            // QUESTION: Is it okay to not instantiate this type here?
            TypeKind::GenLam(TGenLam { lam, .. }) => lam.ftv(),
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

impl Substitutable for Scheme {
    fn apply(&mut self, s: &Subst) {
        self.t.apply(s);
        self.type_params.apply(s);
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.t.ftv();
        result.append(&mut self.type_params.ftv());
        result
    }
}

impl Substitutable for TypeParam {
    fn apply(&mut self, s: &Subst) {
        if let Some(constraint) = &mut self.constraint {
            constraint.apply(s);
        }
        if let Some(default) = &mut self.default {
            default.apply(s);
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = vec![];
        if let Some(constraint) = &self.constraint {
            result.append(&mut constraint.ftv());
        }
        if let Some(default) = &self.default {
            result.append(&mut default.ftv());
        }
        result
    }
}

impl Substitutable for TObjElem {
    fn apply(&mut self, sub: &Subst) {
        match self {
            TObjElem::Call(qlam) => qlam.apply(sub),
            TObjElem::Constructor(qlam) => qlam.apply(sub),
            TObjElem::Index(index) => index.apply(sub),
            TObjElem::Prop(prop) => prop.apply(sub),
            TObjElem::Method(_) => todo!(),
            TObjElem::Getter(_) => todo!(),
            TObjElem::Setter(_) => todo!(),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        match self {
            TObjElem::Call(qlam) => qlam.ftv(),
            TObjElem::Constructor(qlam) => qlam.ftv(),
            TObjElem::Index(index) => index.ftv(),
            TObjElem::Prop(prop) => prop.t.ftv(),
            TObjElem::Method(_) => todo!(),
            TObjElem::Getter(_) => todo!(),
            TObjElem::Setter(_) => todo!(),
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
        self.params.iter_mut().for_each(|param| param.apply(sub));
        self.ret.apply(sub);
        self.type_params.apply(sub);
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.append(&mut self.ret.ftv());
        result.append(&mut self.type_params.ftv());
        result
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

fn uniq(types: &[Type]) -> Vec<Type> {
    // Removes duplicates
    let types: BTreeSet<Type> = types.iter().cloned().collect();
    // Converts set back to an array
    types.into_iter().collect()
}

fn norm_type(t: &mut Type) {
    match &t.kind {
        TypeKind::Union(types) => {
            let types = uniq(types);

            t.kind = if types.len() == 1 {
                types.get(0).unwrap().kind.clone()
            } else {
                TypeKind::Union(types)
            };
        }
        TypeKind::Intersection(types) => {
            let types = uniq(types);

            t.kind = if types.len() == 1 {
                types.get(0).unwrap().kind.clone()
            } else {
                TypeKind::Intersection(types)
            };
        }
        _ => (),
    }
}
