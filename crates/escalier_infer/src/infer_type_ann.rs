use im::hashmap::HashMap;
use std::iter::Iterator;

use escalier_ast::types::{
    self as types, Provenance, TConditionalType, TFnParam, TIndex, TIndexAccess, TIndexKey,
    TInferType, TMappedType, TObjElem, TObject, TProp, TPropKey, Type, TypeKind,
};
use escalier_ast::values::*;

use crate::infer_fn_param::pattern_to_tpat;
use crate::substitutable::{Subst, Substitutable};
use crate::type_error::TypeError;
use crate::util::{compose_many_subs, compose_subs};

use crate::checker::Checker;

impl Checker {
    pub fn infer_type_ann(
        &mut self,
        type_ann: &mut TypeAnn,
        type_params: &mut Option<Vec<TypeParam>>,
    ) -> Result<(Subst, Type), Vec<TypeError>> {
        let mut type_params = if type_params.is_none() {
            match &mut type_ann.kind {
                TypeAnnKind::Lam(lam) => lam.type_params.to_owned(),
                _ => None,
            }
        } else {
            type_params.to_owned()
        };

        // NOTE: There's a scoping issue when using this mapping hash map.
        // <T>(arg: T, cb: <T>(T) => T) => T
        // The <T> type param list for `cb` shadows the outer `T`
        let type_param_map: HashMap<String, Type> = match &mut type_params {
            Some(params) => params
                .iter_mut()
                .map(|param| {
                    let tv = match &mut param.constraint {
                        Some(type_ann) => {
                            // TODO: push `s` on to `ss`
                            let (_s, t) = self.infer_type_ann(type_ann, &mut None)?;
                            self.fresh_var(Some(Box::from(t)))
                        }
                        None => self.fresh_var(None),
                    };

                    Ok((param.name.name.to_owned(), tv))
                })
                .collect::<Result<HashMap<String, Type>, Vec<TypeError>>>()?,
            None => HashMap::default(),
        };

        self.infer_type_ann_with_params(type_ann, &type_param_map)
    }

    pub fn infer_type_ann_with_params(
        &mut self,
        type_ann: &mut TypeAnn,
        type_param_map: &HashMap<String, Type>,
    ) -> Result<(Subst, Type), Vec<TypeError>> {
        self.infer_type_ann_rec(type_ann, type_param_map)
    }

    fn infer_type_ann_rec(
        &mut self,
        type_ann: &mut TypeAnn,
        type_param_map: &HashMap<String, Type>,
    ) -> Result<(Subst, Type), Vec<TypeError>> {
        let (s, mut t) = match &mut type_ann.kind {
            TypeAnnKind::Lam(lam) => {
                let mut ss: Vec<Subst> = vec![];
                let mut params: Vec<types::TFnParam> = vec![];

                for param in &mut lam.params {
                    let (param_s, param_t) =
                        self.infer_type_ann_rec(&mut param.type_ann, type_param_map)?;
                    ss.push(param_s);
                    params.push(TFnParam {
                        pat: pattern_to_tpat(&param.pat),
                        t: param_t,
                        optional: param.optional,
                    });
                }

                let (ret_s, ret_t) = self.infer_type_ann_rec(&mut lam.ret, type_param_map)?;
                let ret = Box::from(ret_t);
                ss.push(ret_s);

                let s = compose_many_subs(&ss);
                let t = Type::from(TypeKind::Lam(types::TLam {
                    type_params: None,
                    params,
                    ret,
                }));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Lit(lit) => {
                let s = Subst::new();
                let t = Type::from(lit.to_owned());
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Keyword(KeywordType { keyword, .. }) => {
                let s = Subst::new();
                let t = Type::from(keyword.to_owned());
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Object(obj) => {
                let mut ss: Vec<Subst> = vec![];
                let mut elems: Vec<types::TObjElem> = vec![];

                for elem in &mut obj.elems {
                    match elem {
                        escalier_ast::values::TObjElem::Index(index) => {
                            let (index_s, index_t) =
                                self.infer_type_ann_rec(&mut index.type_ann, type_param_map)?;

                            let (key_s, key_t) =
                                self.infer_type_ann_rec(&mut index.key.type_ann, type_param_map)?;

                            ss.push(index_s);
                            ss.push(key_s);

                            if let PatternKind::Ident(BindingIdent { name, .. }) =
                                &index.key.pat.kind
                            {
                                elems.push(TObjElem::Index(TIndex {
                                    key: TIndexKey {
                                        name: name.to_owned(),
                                        t: Box::from(key_t),
                                    },
                                    mutable: index.mutable,
                                    t: index_t,
                                }))
                            } else {
                                return Err(vec![TypeError::Unspecified]);
                            }
                        }
                        escalier_ast::values::TObjElem::Prop(prop) => {
                            let (prop_s, prop_t) =
                                self.infer_type_ann_rec(&mut prop.type_ann, type_param_map)?;

                            ss.push(prop_s);
                            elems.push(TObjElem::Prop(TProp {
                                name: TPropKey::StringKey(prop.name.to_owned()),
                                optional: prop.optional,
                                mutable: prop.mutable,
                                t: prop_t,
                            }))
                        }
                    }
                }

                let s = compose_many_subs(&ss);
                let t = Type::from(TypeKind::Object(TObject {
                    elems,
                    is_interface: false,
                }));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::TypeRef(TypeRef {
                name,
                type_args: type_params,
                ..
            }) => match type_param_map.get(name) {
                Some(tv) => {
                    let s = Subst::new();
                    let t = tv.to_owned();
                    type_ann.inferred_type = Some(t.clone());
                    Ok((s, t))
                }
                None => {
                    let mut type_args: Vec<Type> = vec![];
                    let mut ss: Vec<Subst> = vec![];

                    if let Some(type_params) = type_params {
                        for param in type_params {
                            let (type_arg_s, type_arg_t) =
                                self.infer_type_ann_rec(param, type_param_map)?;
                            ss.push(type_arg_s);
                            type_args.push(type_arg_t);
                        }
                    }

                    let s = compose_many_subs(&ss);
                    let t = Type::from(TypeKind::Ref(types::TRef {
                        name: name.to_owned(),
                        type_args: if type_args.is_empty() {
                            None
                        } else {
                            Some(type_args)
                        },
                    }));
                    type_ann.inferred_type = Some(t.clone());
                    Ok((s, t))
                }
            },
            TypeAnnKind::Union(union) => {
                let mut ts: Vec<Type> = vec![];
                let mut ss: Vec<Subst> = vec![];

                for t in &mut union.types {
                    let (s, t) = self.infer_type_ann_rec(t, type_param_map)?;

                    ss.push(s);
                    ts.push(t);
                }

                let s = compose_many_subs(&ss);
                let t = Type::from(TypeKind::Union(ts));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Intersection(intersection) => {
                let mut ts: Vec<Type> = vec![];
                let mut ss: Vec<Subst> = vec![];

                for t in &mut intersection.types {
                    let (s, t) = self.infer_type_ann_rec(t, type_param_map)?;

                    ss.push(s);
                    ts.push(t);
                }

                let s = compose_many_subs(&ss);
                let t = Type::from(TypeKind::Intersection(ts));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Tuple(tuple) => {
                let mut ts: Vec<Type> = vec![];
                let mut ss: Vec<Subst> = vec![];

                for t in &mut tuple.types {
                    let (s, t) = self.infer_type_ann_rec(t, type_param_map)?;

                    ss.push(s);
                    ts.push(t);
                }

                let s = compose_many_subs(&ss);
                let t = Type::from(TypeKind::Tuple(ts));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Array(ArrayType { elem_type, .. }) => {
                let (elem_s, elem_t) = self.infer_type_ann_rec(elem_type, type_param_map)?;
                let s = elem_s;
                let t = Type::from(TypeKind::Array(Box::from(elem_t)));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::KeyOf(KeyOfType { type_ann, .. }) => {
                let (arg_s, arg_t) = self.infer_type_ann_rec(type_ann, type_param_map)?;
                let s = arg_s;
                let t = Type::from(TypeKind::KeyOf(Box::from(arg_t)));
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Query(QueryType { expr, .. }) => self.infer_expr(expr, false),
            TypeAnnKind::Mutable(MutableType { type_ann, .. }) => {
                let (s, mut t) = self.infer_type_ann_rec(type_ann, type_param_map)?;

                match &t.kind {
                    TypeKind::Keyword(_) => {
                        return Err(vec![TypeError::PrimitivesCantBeMutable(Box::from(
                            t.to_owned(),
                        ))])
                    }
                    TypeKind::Tuple(_) => {
                        return Err(vec![TypeError::TuplesCantBeMutable(Box::from(
                            t.to_owned(),
                        ))])
                    }
                    _ => (),
                };

                t.mutable = true;
                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Infer(InferType { name }) => {
                let t = Type::from(TypeKind::InferType(TInferType {
                    name: name.to_owned(),
                }));
                let s = Subst::new();

                Ok((s, t))
            }
            TypeAnnKind::IndexedAccess(IndexedAccessType {
                obj_type,
                index_type,
                ..
            }) => {
                let (obj_s, obj_t) = self.infer_type_ann_rec(obj_type, type_param_map)?;
                let (index_s, index_t) = self.infer_type_ann_rec(index_type, type_param_map)?;

                let s = compose_many_subs(&[obj_s, index_s]);
                let t = Type::from(TypeKind::IndexAccess(TIndexAccess {
                    object: Box::from(obj_t),
                    index: Box::from(index_t),
                }));

                type_ann.inferred_type = Some(t.clone());
                Ok((s, t))
            }
            TypeAnnKind::Mapped(MappedType {
                type_param,
                type_ann,
                optional,
                mutable,
                ..
            }) => {
                if let Some(constraint) = &mut type_param.constraint {
                    let (constraint_s, constraint_t) =
                        self.infer_type_ann_rec(constraint.as_mut(), type_param_map)?;

                    constraint.inferred_type = Some(constraint_t.clone());

                    let (type_ann_s, type_ann_t) =
                        self.infer_type_ann_rec(type_ann, type_param_map)?;

                    // QUESTION: Do we need to apply `constraint_s` to `type_ann_t`?
                    type_ann.inferred_type = Some(type_ann_t.clone());

                    let t = Type::from(TypeKind::MappedType(TMappedType {
                        type_param: types::TypeParam {
                            name: type_param.name.name.to_owned(),
                            constraint: Some(Box::from(constraint_t)),
                            default: None,
                        },
                        optional: optional.as_ref().map(|value| match &value.change {
                            TMappedTypeChangeProp::Plus => types::TMappedTypeChangeProp::Plus,
                            TMappedTypeChangeProp::Minus => types::TMappedTypeChangeProp::Minus,
                        }),
                        mutable: mutable.as_ref().map(|value| match &value.change {
                            TMappedTypeChangeProp::Plus => types::TMappedTypeChangeProp::Plus,
                            TMappedTypeChangeProp::Minus => types::TMappedTypeChangeProp::Minus,
                        }),
                        t: Box::from(type_ann_t),
                    }));

                    let s = compose_subs(&type_ann_s, &constraint_s);
                    let t = t.apply(&s); // I think we can skip this

                    Ok((s, t))
                } else {
                    // TODO: this should never happen since the parser only creates
                    // a mapped type when there's a constraint in the index signature.
                    Err(vec![TypeError::Unspecified])
                }
            }
            TypeAnnKind::Conditional(ConditionalType {
                check_type: left,
                extends_type: right,
                true_type: consequent,
                false_type: alternate,
                ..
            }) => {
                let (check_s, check_t) = self.infer_type_ann_rec(left, type_param_map)?;
                let (extends_s, extends_t) = self.infer_type_ann_rec(right, type_param_map)?;
                let (true_s, true_t) = self.infer_type_ann_rec(consequent, type_param_map)?;
                let (false_s, false_t) = self.infer_type_ann_rec(alternate, type_param_map)?;

                let t = Type::from(TypeKind::ConditionalType(TConditionalType {
                    check_type: Box::from(check_t),
                    extends_type: Box::from(extends_t),
                    true_type: Box::from(true_t),
                    false_type: Box::from(false_t),
                }));

                let s = compose_many_subs(&[check_s, extends_s, true_s, false_s]);

                Ok((s, t))
            }
        }?;

        t.provenance = Some(Box::from(Provenance::TypeAnn(Box::from(
            type_ann.to_owned(),
        ))));

        Ok((s, t))
    }
}
