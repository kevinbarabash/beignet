use array_tool::vec::*;
use itertools::join;
use std::collections::HashMap;
use std::fmt;
use std::iter::Iterator;

use crochet_ast::types::*;

use crate::context::{Context, Env};
use crate::replace_aliases_rec;
use crate::substitutable::{Subst, Substitutable};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Scheme {
    pub t: Box<Type>,
    pub type_params: Vec<TypeParam>,
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self { t, type_params } = self;
        if !type_params.is_empty() {
            write!(f, "<{}>", join(type_params, ", "))?;
        }
        write!(f, "{t}")
    }
}

pub fn get_sub_and_type_params(tvars: &[TVar]) -> (Subst, Vec<TypeParam>) {
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

    (sub, type_params)
}

pub fn generalize(env: &Env, t: &Type) -> Scheme {
    let tvars: Vec<TVar> = t.ftv().uniq_via(env.ftv(), |a, b| a.id == b.id);

    let (sub, type_params) = get_sub_and_type_params(&tvars);

    // TODO: Consider not cloning here once we have some tests in place
    let mut t = t.clone();
    t.apply(&sub);

    Scheme {
        t: Box::from(t),
        type_params,
    }
}

pub fn generalize_gen_lam(env: &Env, lam: &TLam) -> TGenLam {
    let tvars: Vec<TVar> = lam.ftv().uniq_via(env.ftv(), |a, b| a.id == b.id);

    let (sub, type_params) = get_sub_and_type_params(&tvars);

    // TODO: Consider not cloning here once we have some tests in place
    let mut lam = lam.clone();
    lam.apply(&sub);

    TGenLam {
        lam: Box::from(lam),
        type_params,
    }
}

fn get_type_param_map(ctx: &Context, type_params: &[TypeParam]) -> HashMap<String, Type> {
    let mut type_param_map = HashMap::new();

    for type_param in type_params {
        let TypeParam {
            name,
            constraint,
            default: _, // TODO: figure out what to do for defaults
        } = type_param;

        // TODO: make a helper for this
        let t = Type::from(TypeKind::Var(TVar {
            id: ctx.fresh_id(),
            constraint: constraint.to_owned(),
        }));

        type_param_map.insert(name.to_owned(), t);
    }

    type_param_map
}

pub fn instantiate(ctx: &Context, sc: &Scheme) -> Type {
    let type_param_map = get_type_param_map(ctx, &sc.type_params);

    // TODO: name functions so it's more obvious when a function is mutating
    // it's args.
    replace_aliases_rec(&sc.t, &type_param_map)
}

pub fn instantiate_gen_lam(ctx: &Context, gen_lam: &TGenLam) -> Type {
    let TGenLam { type_params, lam } = gen_lam;

    let type_param_map = get_type_param_map(ctx, type_params);
    let t = Type::from(TypeKind::Lam(lam.as_ref().to_owned()));

    replace_aliases_rec(&t, &type_param_map)
}

pub fn instantiate_callable(ctx: &Context, callable: &TCallable) -> Type {
    let TCallable {
        type_params,
        params,
        ret,
    } = callable;

    let type_param_map = get_type_param_map(ctx, type_params);
    let t = Type::from(TypeKind::Lam(TLam {
        params: params.to_owned(),
        ret: ret.to_owned(),
    }));

    replace_aliases_rec(&t, &type_param_map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;

    #[test]
    fn test_generalize_with_unique_type_vars() {
        let ctx = Context::default();

        let t = Type::from(TypeKind::Object(TObject {
            elems: vec![
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("a")),
                    optional: false,
                    mutable: false,
                    t: ctx.fresh_var(),
                }),
                TObjElem::Prop(TProp {
                    name: TPropKey::StringKey(String::from("b")),
                    optional: false,
                    mutable: false,
                    t: ctx.fresh_var(),
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
        let ctx = Context::default();

        let tv = ctx.fresh_var();
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
    fn test_generalize_lam_to_gen_lam() {
        let ctx = Context::default();

        let tv = ctx.fresh_var();

        let lam = TLam {
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
                    t: ctx.fresh_var(),
                    optional: false,
                },
            ],
            ret: Box::from(tv),
        };

        let env = Env::new();
        let gen_lam = generalize_gen_lam(&env, &lam);

        assert_eq!(gen_lam.to_string(), "<A, B>(a: A, b: B) => A");
    }

    #[test]
    fn test_generalize_lam_to_scheme() {
        let ctx = Context::default();

        let tv = ctx.fresh_var();

        let lam = TLam {
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
                    t: ctx.fresh_var(),
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
            type_params: vec![],
        };

        let ctx = Context::default();
        let t = instantiate(&ctx, &sc);

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
            type_params: vec![
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
            ],
        };

        let ctx = Context::default();
        let t = instantiate(&ctx, &sc);

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
            type_params: vec![TypeParam {
                name: "A".to_string(),
                constraint: None,
                default: None,
            }],
        };

        let ctx = Context::default();
        let t = instantiate(&ctx, &sc);

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
            type_params: vec![
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
            ],
        };

        let ctx = Context::default();
        let t = instantiate(&ctx, &sc);

        insta::assert_debug_snapshot!(t);
    }

    #[test]
    fn test_instantiate_gen_lam() {
        let ctx = Context::default();

        let gen_lam = TGenLam {
            type_params: vec![
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
            ],
            lam: Box::from(TLam {
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
            }),
        };

        let lam = instantiate_gen_lam(&ctx, &gen_lam);

        assert_eq!(lam.to_string(), "(a: t1, b: t2) => t1");
    }
}
