use array_tool::vec::*;
use std::collections::{BTreeSet, HashMap};
use std::iter::IntoIterator;

use crochet_ast::types::*;

use crate::context::Binding;
use crate::infer_regex::parse_regex;
use crate::scheme::Scheme;

pub type Subst = HashMap<i32, Type>;

pub trait Substitutable {
    fn apply(&self, subs: &Subst) -> Self;
    // The vector return must not contain any `TVar`s with the same `id`.
    fn ftv(&self) -> Vec<TVar>;
}

impl Substitutable for Type {
    fn apply(&self, sub: &Subst) -> Type {
        let kind: TypeKind = match &self.kind {
            kind @ TypeKind::Var(tv) => {
                match sub.get(&tv.id) {
                    Some(replacement) => {
                        // If the replacement has no provenance, point to ourselves.
                        let provenance = match replacement.provenance {
                            Some(_) => replacement.provenance.to_owned(),
                            None => Some(Box::from(Provenance::from(self.clone()))),
                        };

                        // TODO: apply the constraint and then check if the replacement
                        // is a subtype of it.
                        return Type {
                            kind: replacement.kind.to_owned(),
                            mutable: replacement.mutable,
                            provenance,
                        };
                    }
                    None => {
                        if let Some(constraint) = &tv.constraint {
                            TypeKind::Var(TVar {
                                constraint: Some(Box::from(constraint.apply(sub))),
                                ..tv.to_owned()
                            })
                        } else {
                            kind.to_owned()
                        }
                    }
                }
            }
            TypeKind::App(app) => {
                TypeKind::App(TApp {
                    args: app.args.apply(sub),
                    ret: Box::from(app.ret.apply(sub)),
                    type_args: app.type_args.to_owned(), // TODO
                })
            }
            // TODO: handle widening of lambdas
            TypeKind::Lam(lam) => TypeKind::Lam(lam.apply(sub)),
            kind @ TypeKind::Lit(_) => kind.to_owned(),
            kind @ TypeKind::Keyword(_) => kind.to_owned(),
            TypeKind::Union(types) => TypeKind::Union(types.apply(sub)),
            TypeKind::Intersection(types) => TypeKind::Intersection(types.apply(sub)),
            TypeKind::Object(obj) => TypeKind::Object(TObject {
                elems: obj.elems.apply(sub),
                ..obj.to_owned()
            }),
            TypeKind::Ref(tr) => {
                let result = tr.apply(sub);

                // If we have enough information to infer a type from a regex
                // pattern we can replace `RegExpMatchArray` with a more accurate
                // type based on the regex itself.
                if tr.name == "RegExpMatchArray" {
                    if let Some(type_args) = &result.type_args {
                        let pattern = &type_args[0];
                        let flags = &type_args[1];
                        if let (
                            TypeKind::Lit(TLit::Str(pattern)),
                            TypeKind::Lit(TLit::Str(flags)),
                        ) = (&pattern.kind, &flags.kind)
                        {
                            if flags.contains('g') {
                                return Type::from(TypeKind::Array(Box::from(Type::from(
                                    TypeKind::Keyword(TKeyword::String),
                                ))));
                            } else {
                                return parse_regex(pattern);
                            }
                        }
                    }
                }

                TypeKind::Ref(result)
            }
            TypeKind::Tuple(types) => TypeKind::Tuple(types.apply(sub)),
            TypeKind::Array(t) => TypeKind::Array(Box::from(t.apply(sub))),
            TypeKind::Rest(arg) => TypeKind::Rest(Box::from(arg.apply(sub))),
            kind @ TypeKind::This => kind.to_owned(),
            TypeKind::KeyOf(t) => TypeKind::KeyOf(Box::from(t.apply(sub))),
            TypeKind::IndexAccess(TIndexAccess { object, index }) => {
                TypeKind::IndexAccess(TIndexAccess {
                    object: Box::from(object.apply(sub)),
                    index: Box::from(index.apply(sub)),
                })
            }
            TypeKind::MappedType(mapped) => TypeKind::MappedType(TMappedType {
                t: Box::from(mapped.t.apply(sub)),
                type_param: mapped.type_param.apply(sub),
                ..mapped.to_owned()
            }),
            TypeKind::ConditionalType(TConditionalType {
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => TypeKind::ConditionalType(TConditionalType {
                check_type: Box::from(check_type.apply(sub)),
                extends_type: Box::from(extends_type.apply(sub)),
                true_type: Box::from(true_type.apply(sub)),
                false_type: Box::from(false_type.apply(sub)),
            }),
            kind @ TypeKind::InferType(_) => kind.to_owned(),
        };

        let mut t = Type {
            kind,
            ..self.to_owned()
        };

        norm_type(&mut t);

        t
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
            TypeKind::App(TApp {
                args,
                ret,
                type_args,
            }) => {
                let mut result = args.ftv();
                result.append(&mut ret.ftv());
                result.append(&mut type_args.ftv());
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
            TypeKind::InferType(_) => vec![],
        }
    }
}

impl Substitutable for Scheme {
    fn apply(&self, s: &Subst) -> Self {
        Scheme {
            t: Box::from(self.t.apply(s)),
            type_params: self.type_params.apply(s),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.t.ftv();
        result.append(&mut self.type_params.ftv());
        result
    }
}

impl Substitutable for TypeParam {
    fn apply(&self, s: &Subst) -> Self {
        TypeParam {
            constraint: self.constraint.as_ref().map(|c| Box::from(c.apply(s))),
            default: self.default.as_ref().map(|d| Box::from(d.apply(s))),
            ..self.to_owned()
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

impl Substitutable for TMethod {
    fn apply(&self, sub: &Subst) -> Self {
        TMethod {
            params: self.params.iter().map(|param| param.apply(sub)).collect(),
            ret: Box::from(self.ret.apply(sub)),
            type_params: self.type_params.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.append(&mut self.ret.ftv());
        result.append(&mut self.type_params.ftv());
        result
    }
}

impl Substitutable for TGetter {
    fn apply(&self, sub: &Subst) -> Self {
        TGetter {
            ret: Box::from(self.ret.apply(sub)),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        self.ret.ftv()
    }
}

impl Substitutable for TSetter {
    fn apply(&self, sub: &Subst) -> Self {
        TSetter {
            param: self.param.apply(sub),
            ..self.to_owned()
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        self.param.ftv()
    }
}

impl Substitutable for TObjElem {
    fn apply(&self, sub: &Subst) -> Self {
        match self {
            TObjElem::Call(qlam) => TObjElem::Call(qlam.apply(sub)),
            TObjElem::Constructor(qlam) => TObjElem::Constructor(qlam.apply(sub)),
            TObjElem::Index(index) => TObjElem::Index(index.apply(sub)),
            TObjElem::Prop(prop) => TObjElem::Prop(prop.apply(sub)),
            TObjElem::Method(method) => TObjElem::Method(method.apply(sub)),
            TObjElem::Getter(getter) => TObjElem::Getter(getter.apply(sub)),
            TObjElem::Setter(setter) => TObjElem::Setter(setter.apply(sub)),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        match self {
            TObjElem::Call(qlam) => qlam.ftv(),
            TObjElem::Constructor(qlam) => qlam.ftv(),
            TObjElem::Index(index) => index.ftv(),
            TObjElem::Prop(prop) => prop.t.ftv(),
            TObjElem::Method(method) => method.ftv(),
            TObjElem::Getter(getter) => getter.ftv(),
            TObjElem::Setter(setter) => setter.ftv(),
        }
    }
}

impl Substitutable for TLam {
    fn apply(&self, sub: &Subst) -> Self {
        TLam {
            type_params: self.type_params.apply(sub),
            params: self.params.apply(sub),
            ret: Box::from(self.ret.apply(sub)),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.append(&mut self.ret.ftv());
        result.unique_via(|a, b| a.id == b.id)
    }
}

impl Substitutable for TRef {
    fn apply(&self, sub: &Subst) -> Self {
        TRef {
            type_args: self
                .type_args
                .as_ref()
                .map(|args| args.iter().map(|arg| arg.apply(sub)).collect()),
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
        TCallable {
            params: self.params.iter().map(|param| param.apply(sub)).collect(),
            ret: Box::from(self.ret.apply(sub)),
            type_params: self.type_params.apply(sub),
        }
    }
    fn ftv(&self) -> Vec<TVar> {
        let mut result = self.params.ftv();
        result.append(&mut self.ret.ftv());
        result.append(&mut self.type_params.ftv());
        result
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
    fn apply(&self, sub: &Subst) -> Self {
        self.iter()
            .map(|(k, v)| (k.to_owned(), v.apply(sub)))
            .collect()
    }
    fn ftv(&self) -> Vec<TVar> {
        // we can't use iter_values() here because it's a consuming iterator
        self.iter().flat_map(|(_, b)| b.ftv()).collect()
    }
}

// TODO: update apply() to not mutate so that we can use `im::hashmap` _et al_.
// TODO: figure out if doing so will affect our ability to update exprs with
// inferred types (it shouldn't be we need to be diligent).
// impl<I> ImSubstitutable for hashmap::HashMap<String, I>
// where
//     I: ImSubstitutable,
// {
//     fn apply(&self, sub: &Subst) -> Subst {
//         self.into_iter()
//             .map(|(key, val)| (key.to_owned(), val.apply(sub).to_owned()))
//             .collect()
//         // for val in self.values() {
//         //     val.apply(sub)
//         // }
//     }
//     fn ftv(&self) -> Vec<TVar> {
//         // we can't use iter_values() here because it's a consuming iterator
//         self.iter().flat_map(|(_, b)| b.ftv()).collect()
//     }
// }

impl<I> Substitutable for Vec<I>
where
    I: Substitutable,
{
    fn apply(&self, sub: &Subst) -> Self {
        self.iter().map(|elem| elem.apply(sub)).collect()
    }
    fn ftv(&self) -> Vec<TVar> {
        self.iter().flat_map(|c| c.ftv()).collect()
    }
}

impl<I> Substitutable for Option<I>
where
    I: Substitutable,
{
    fn apply(&self, sub: &Subst) -> Self {
        self.as_ref().map(|val| val.apply(sub))
    }
    fn ftv(&self) -> Vec<TVar> {
        self.as_ref()
            .map(|val| val.to_owned().ftv())
            .unwrap_or_default()
    }
}

impl Substitutable for Binding {
    fn apply(&self, sub: &Subst) -> Self {
        Binding {
            t: self.t.apply(sub),
            ..self.to_owned()
        }
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
