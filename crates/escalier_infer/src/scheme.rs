use array_tool::vec::*;
use im::hashmap::HashMap;
use itertools::join;
use std::fmt;
use std::iter::Iterator;

use escalier_ast::types::*;

use crate::context::Env;
use crate::substitutable::{Subst, Substitutable};
use crate::util::{replace_aliases_in_lam, replace_aliases_rec};

use crate::checker::Checker;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Scheme {
    pub t: Box<Type>,
    pub type_params: Option<Vec<TypeParam>>,
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { t, type_params } = self;
        if let Some(type_params) = type_params {
            write!(f, "<{}>", join(type_params, ", "))?;
        }
        write!(f, "{t}")
    }
}

pub fn get_sub_and_type_params(tvars: &[TVar]) -> (Subst, Option<Vec<TypeParam>>) {
    if tvars.is_empty() {
        return (Subst::new(), None);
    }

    let mut sub = Subst::new();
    let mut type_params: Vec<TypeParam> = vec![];

    for (i, tv) in tvars.iter().enumerate() {
        let c = char::from_u32((i + 65) as u32).unwrap();
        let name = c.to_string();

        type_params.push(TypeParam {
            name: name.to_owned(),
            constraint: tv.constraint.clone(),
            default: None,
        });

        sub.insert(
            tv.id,
            Type {
                kind: TypeKind::Ref(TRef {
                    name: name.to_owned(),
                    type_args: None,
                }),
                provenance: None,
                mutable: false,
            },
        );
    }

    (sub, Some(type_params))
}

pub fn generalize(env: &Env, t: &Type) -> Scheme {
    let tvars: Vec<TVar> = t.ftv().uniq_via(env.ftv(), |a, b| a.id == b.id);

    let (sub, type_params) = get_sub_and_type_params(&tvars);

    Scheme {
        t: Box::from(t.apply(&sub)),
        type_params,
    }
}

impl Checker {
    pub fn get_type_param_map(&mut self, type_params: &[TypeParam]) -> HashMap<String, Type> {
        let mut type_param_map = HashMap::new();

        for type_param in type_params {
            let TypeParam {
                name,
                constraint,
                default: _, // TODO: figure out what to do for defaults
            } = type_param;

            let t = self.fresh_var(constraint.to_owned());
            type_param_map.insert(name.to_owned(), t);
        }

        type_param_map
    }

    pub fn instantiate(&mut self, sc: &Scheme) -> Type {
        let type_param_map = match &sc.type_params {
            Some(type_params) => self.get_type_param_map(type_params),
            None => HashMap::new(),
        };

        // TODO: name functions so it's more obvious when a function is mutating
        // it's args.
        replace_aliases_rec(&sc.t, &type_param_map)
    }

    pub fn instantiate_gen_lam(&mut self, lam: &TLam, type_args: Option<&Vec<Type>>) -> TLam {
        if lam.type_params.is_none() {
            return lam.to_owned();
        }

        // TODO: check if `type_args` conform the the constraints in `type_params`
        let type_param_map = match &lam.type_params {
            Some(type_params) => match type_args {
                Some(type_args) => {
                    let mut map: HashMap<String, Type> = HashMap::new();
                    for (type_param, type_arg) in type_params.iter().zip(type_args) {
                        map.insert(type_param.name.to_owned(), type_arg.to_owned());
                    }
                    map
                }
                None => self.get_type_param_map(type_params),
            },
            None => HashMap::new(),
        };

        let lam = TLam {
            type_params: None,
            ..lam.to_owned()
        };

        replace_aliases_in_lam(&lam, &type_param_map)
    }

    // TODO: Update to acception optional type args
    pub fn instantiate_callable(&mut self, callable: &TCallable) -> Type {
        let TCallable {
            type_params,
            params,
            ret,
        } = callable;

        let type_param_map = match type_params {
            Some(type_params) => self.get_type_param_map(type_params),
            None => HashMap::new(),
        };
        let t = Type::from(TypeKind::Lam(TLam {
            params: params.to_owned(),
            ret: ret.to_owned(),
            type_params: None,
        }));

        replace_aliases_rec(&t, &type_param_map)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generalize_with_unique_type_vars() {
        let mut checker = Checker::default();

        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: checker.fresh_var(None),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: checker.fresh_var(None),
                }),
            ],
            is_interface: false,
        }));

        let env = Env::new();
        let sc = generalize(&env, &t);

        assert_eq!(sc.to_string(), "<A, B>{a: A, b: B}");
    }

    #[test]
    fn test_generalize_with_reused_type_vars() {
        let mut checker = Checker::default();

        let tv = checker.fresh_var(None);
        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: tv.to_owned(),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: tv,
                }),
            ],
            is_interface: false,
        }));

        let env = Env::new();
        let sc = generalize(&env, &t);

        assert_eq!(sc.to_string(), "<A>{a: A, b: A}");
    }

    #[test]
    fn test_generalize_with_no_type_vars() {
        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Keyword(TKeyword::Number)),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Keyword(TKeyword::String)),
                }),
            ],
            is_interface: false,
        }));

        let env = Env::new();
        let sc = generalize(&env, &t);

        assert_eq!(sc.to_string(), "{a: number, b: string}");
    }

    #[test]
    fn test_generalize_lam_to_scheme() {
        let mut checker = Checker::default();

        let tv = checker.fresh_var(None);

        let lam = TLam {
            type_params: None,
            params: vec![
                TFnParam {
                    pat: TPat::Ident(BindingIdent {
                        name: "a".to_string(),
                        mutable: false,
                    }),
                    t: tv.clone(),
                    optional: false,
                },
                TFnParam {
                    pat: TPat::Ident(BindingIdent {
                        name: "b".to_string(),
                        mutable: false,
                    }),
                    t: checker.fresh_var(None),
                    optional: false,
                },
            ],
            ret: Box::from(tv),
        };
        let t = Type::from(TypeKind::Lam(lam));

        let env = Env::new();
        let gen_lam = generalize(&env, &t);

        assert_eq!(gen_lam.to_string(), "<A, B>(a: A, b: B) => A");
    }

    #[test]
    fn test_instantiate_scheme_without_no_type_params() {
        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Keyword(TKeyword::Number)),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Keyword(TKeyword::String)),
                }),
            ],
            is_interface: false,
        }));

        let sc = Scheme {
            t: Box::from(t),
            type_params: None,
        };

        let mut checker = Checker::default();
        let t = checker.instantiate(&sc);

        assert_eq!(t.to_string(), "{a: number, b: string}");
    }

    #[test]
    fn test_instantiate_scheme_with_multiple_type_params() {
        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "A".to_string(),
                        type_args: None,
                    })),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "B".to_string(),
                        type_args: None,
                    })),
                }),
            ],
            is_interface: false,
        }));

        let sc = Scheme {
            t: Box::from(t),
            type_params: Some(vec![
                TypeParam {
                    name: "A".to_string(),
                    constraint: None,
                    default: None,
                },
                TypeParam {
                    name: "B".to_string(),
                    constraint: None,
                    default: None,
                },
            ]),
        };

        let mut checker = Checker::default();
        let t = checker.instantiate(&sc);

        assert_eq!(t.to_string(), "{a: t1, b: t2}");
    }

    #[test]
    fn test_instantiate_scheme_with_repeated_type_param() {
        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "A".to_string(),
                        type_args: None,
                    })),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "A".to_string(),
                        type_args: None,
                    })),
                }),
            ],
            is_interface: false,
        }));

        let sc = Scheme {
            t: Box::from(t),
            type_params: Some(vec![TypeParam {
                name: "A".to_string(),
                constraint: None,
                default: None,
            }]),
        };

        let mut checker = Checker::default();
        let t = checker.instantiate(&sc);

        assert_eq!(t.to_string(), "{a: t1, b: t1}");
    }

    #[test]
    fn test_instantiate_scheme_with_constrained_type_param() {
        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "A".to_string(),
                        type_args: None,
                    })),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "B".to_string(),
                        type_args: None,
                    })),
                }),
            ],
            is_interface: false,
        }));

        let sc = Scheme {
            t: Box::from(t),
            type_params: Some(vec![
                TypeParam {
                    name: "A".to_string(),
                    constraint: None,
                    default: None,
                },
                TypeParam {
                    name: "B".to_string(),
                    constraint: Some(Box::from(Type::from(TypeKind::Keyword(TKeyword::String)))),
                    default: None,
                },
            ]),
        };

        let mut checker = Checker::default();
        let t = checker.instantiate(&sc);

        insta::assert_debug_snapshot!(t);
    }

    #[test]
    fn test_instantiate_gen_lam() {
        let mut checker = Checker::default();

        let gen_lam = TLam {
            type_params: Some(vec![
                TypeParam {
                    name: "A".to_string(),
                    constraint: None,
                    default: None,
                },
                TypeParam {
                    name: "B".to_string(),
                    constraint: Some(Box::from(Type::from(TypeKind::Keyword(TKeyword::String)))),
                    default: None,
                },
            ]),
            params: vec![
                TFnParam {
                    pat: TPat::Ident(BindingIdent {
                        name: "a".to_string(),
                        mutable: false,
                    }),
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "A".to_string(),
                        type_args: None,
                    })),
                    optional: false,
                },
                TFnParam {
                    pat: TPat::Ident(BindingIdent {
                        name: "b".to_string(),
                        mutable: false,
                    }),
                    t: Type::from(TypeKind::Ref(TRef {
                        name: "B".to_string(),
                        type_args: None,
                    })),
                    optional: false,
                },
            ],
            ret: Box::from(Type::from(TypeKind::Ref(TRef {
                name: "A".to_string(),
                type_args: None,
            }))),
        };

        let lam = checker.instantiate_gen_lam(&gen_lam, None);

        assert_eq!(lam.to_string(), "(a: t1, b: t2) => t1");
    }
}
