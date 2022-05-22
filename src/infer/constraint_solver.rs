use std::collections::HashSet;

use crate::types::*;

use super::context::Context;
use super::substitutable::*;

#[derive(Clone, Debug)]
pub struct Constraint {
    pub types: (Type, Type),
}

type Unifier = (Subst, Vec<Constraint>);

pub fn run_solve(cs: &[Constraint], ctx: &Context) -> Subst {
    let empty_subst = Subst::new();

    solver((empty_subst, cs.to_vec()), ctx)
}

fn solver(u: Unifier, ctx: &Context) -> Subst {
    let (su, cs) = u;

    if cs.is_empty() {
        return su;
    }

    let rest = &cs[1..]; // const [_ , ...rest] = cs;

    // println!("-----------");
    // println!("constraints:");
    // for Constraint {types: (left, right)} in cs.iter() {
    //     println!("{left} = {right}")
    // }

    match cs.get(0) {
        Some(c) => {
            // su1 represents new substitutions from unifying the first constraint in cs.
            let su1 = unifies(c, ctx);
            // This is applied to the remaining constraints (rest).  compose_subs is used
            // to apply su1 to the HashMap of subsitutions, su.
            let unifier: Unifier = (compose_subs(&su1, &su), Vec::from(rest).apply(&su1));
            solver(unifier, ctx)
        }
        None => su,
    }
}

fn compose_subs(s1: &Subst, s2: &Subst) -> Subst {
    let mut result: Subst = s2.iter().map(|(id, tv)| (*id, tv.apply(s1))).collect();
    result.extend(s1.to_owned());
    result
}

fn unifies(c: &Constraint, ctx: &Context) -> Subst {
    let (t1, t2) = c.types.clone();

    match (&t1, &t2) {
        (Type::Var(v), _) => bind(&v.id, &t2, ctx),
        (_, Type::Var(v)) => bind(&v.id, &t1, ctx),
        (Type::Prim(PrimType { prim: p1, .. }), Type::Prim(PrimType { prim: p2, .. }))
            if p1 == p2 =>
        {
            Subst::new()
        }
        (Type::Lit(LitType { lit: l1, .. }), Type::Lit(LitType { lit: l2, .. })) if l1 == l2 => {
            Subst::new()
        }
        (Type::Lam(lam1), Type::Lam(lam2)) => unify_lams(&lam1, &lam2, ctx),
        // TODO: copy tests case from `compiler` project for this
        (
            Type::Alias(AliasType {
                name: name1,
                type_params: type_params1,
                ..
            }),
            Type::Alias(AliasType {
                name: name2,
                type_params: type_params2,
                ..
            }),
        ) if name1 == name2 => {
            // TODO: throw if vars1 and vars2 have different lengths
            let cs: Vec<_> = match (type_params1, type_params2) {
                (Some(params1), Some(params2)) => {
                    params1
                        .iter()
                        .zip(params2)
                        .map(|(a, b)| Constraint {
                            types: (a.clone(), b.clone()),
                        })
                        .collect()
                },
                (None, None) => vec![],
                _ => panic!("mismatch in type params"),
            };
            unify_many(&cs, ctx)
        }

        _ => {
            if is_subtype(&t1, &t2) {
                return Subst::new();
            }

            if !t1.frozen() && !t2.frozen() {
                return widen_types(&t1, &t2, ctx);
            }

            panic!("unification failed")
        }
    }
}

fn unify_lams(t1: &LamType, t2: &LamType, ctx: &Context) -> Subst {
    // TODO:
    // - varargs
    // - subtyping

    // Partial application
    if t1.args.len() < t2.args.len() {
        let partial_args = t2.args[..t1.args.len()].to_vec();
        let partial_ret = ctx.lam(t2.args[t1.args.len()..].to_vec(), t2.ret.clone());
        let mut cs: Vec<_> = t1
            .args
            .iter()
            .zip(&partial_args)
            .map(|(a, b)| Constraint {
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {
            types: (*t1.ret.clone(), partial_ret),
        });
        unify_many(&cs, ctx)
    } else if t1.args.len() != t2.args.len() {
        panic!("Unification mismatch: arity mismatch");
    } else {
        let mut cs: Vec<_> = t1
            .args
            .iter()
            .zip(&t2.args)
            .map(|(a, b)| Constraint {
                types: (a.clone(), b.clone()),
            })
            .collect();
        cs.push(Constraint {
            types: (*t1.ret.clone(), *t2.ret.clone()),
        });
        unify_many(&cs, ctx)
    }
}

fn unify_many(cs: &[Constraint], ctx: &Context) -> Subst {
    match cs {
        [head, tail @ ..] => {
            let su_1 = unifies(head, ctx);
            let su_2 = unify_many(&tail.to_vec().apply(&su_1), ctx);
            compose_subs(&su_2, &su_1)
        }
        _ => Subst::new(),
    }
}

pub fn is_subtype(t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Lit(LitType { lit, .. }), Type::Prim(PrimType { prim, .. })) => match (lit, prim) {
            (Lit::Num(_), Primitive::Num) => true,
            (Lit::Str(_), Primitive::Str) => true,
            (Lit::Bool(_), Primitive::Bool) => true,
            _ => false,
        },
        _ => false,
    }
}

fn bind(tv_id: &i32, ty: &Type, _: &Context) -> Subst {
    // TODO: Handle Type::Mem once it's added
    // NOTE: This will require the use of the &Context

    match ty {
        Type::Var(VarType { id, .. }) if id == tv_id => Subst::new(),
        ty if ty.ftv().contains(tv_id) => panic!("type var appears in type"),
        ty => {
            let mut subst = Subst::new();
            subst.insert(tv_id.to_owned(), ty.clone());
            subst
        }
    }
}

fn flatten_types(ty: &Type) -> Vec<Type> {
    match ty {
        Type::Union(UnionType { types, .. }) => types.iter().flat_map(flatten_types).collect(),
        _ => vec![ty.to_owned()],
    }
}

fn union_types(t1: &Type, t2: &Type, ctx: &Context) -> Type {
    let mut types: Vec<Type> = vec![];
    types.extend(flatten_types(t1));
    types.extend(flatten_types(t2));

    let types_set: HashSet<_> = types.iter().cloned().collect();

    let prim_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| match ty {
            Type::Prim(_) => true,
            _ => false,
        })
        .collect();
    let lit_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| match ty {
            // Primitive types subsume corresponding literal types
            Type::Lit(LitType { lit, .. }) => match lit {
                Lit::Num(_) => !prim_types.contains(&ctx.prim(Primitive::Num)),
                Lit::Bool(_) => !prim_types.contains(&ctx.prim(Primitive::Bool)),
                Lit::Str(_) => !prim_types.contains(&ctx.prim(Primitive::Str)),
                Lit::Null => !prim_types.contains(&ctx.prim(Primitive::Null)),
                Lit::Undefined => !prim_types.contains(&ctx.prim(Primitive::Undefined)),
            },
            _ => false,
        })
        .collect();
    let rest_types: HashSet<_> = types_set
        .iter()
        .cloned()
        .filter(|ty| match ty {
            Type::Prim(_) => false,
            Type::Lit(_) => false,
            _ => true,
        })
        .collect();

    let mut types: Vec<_> = prim_types
        .iter()
        .chain(lit_types.iter())
        .chain(rest_types.iter())
        .cloned()
        .collect();
    types.sort_by_key(|k| k.id());

    if types.len() > 1 {
        ctx.union(types)
    } else {
        types[0].clone()
    }
}

fn widen_types(t1: &Type, t2: &Type, ctx: &Context) -> Subst {
    let mut result = Subst::new();

    let new_type = union_types(t1, t2, ctx);

    result.insert(t1.id(), new_type.clone());
    result.insert(t2.id(), new_type.clone());

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn union_of_number_primitives() {
        let ctx = Context::new();
        let t1 = ctx.prim(Primitive::Num);
        let t2 = ctx.prim(Primitive::Num);
        
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Num));
    }

    #[test]
    fn union_of_number_literals() {
        let ctx = Context::new();
        let t1 = ctx.lit_type(Lit::Num(String::from("5")));
        let t2 = ctx.lit_type(Lit::Num(String::from("10")));
        
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.union(vec![t1, t2]));
    }

    #[test]
    fn union_of_number_prim_and_lit() {
        let ctx = Context::new();
        let t1 = ctx.lit_type(Lit::Num(String::from("5")));
        let t2 = ctx.prim(Primitive::Num);
        
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Num));
    }

    #[test]
    fn union_of_string_prim_and_lit() {
        let ctx = Context::new();
        let t1 = ctx.lit_type(Lit::Str(String::from("hello")));
        let t2 = ctx.prim(Primitive::Str);
        
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Str));
    }

    #[test]
    fn union_of_boolean_prim_and_lit() {
        let ctx = Context::new();
        let t1 = ctx.lit_type(Lit::Bool(true));
        let t2 = ctx.prim(Primitive::Bool);
        
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.prim(Primitive::Bool));
    }

    #[test]
    fn union_of_unions() {
        let ctx = Context::new();
        let t1 = ctx.union(vec![
            ctx.prim(Primitive::Num),
            ctx.prim(Primitive::Bool),
        ]);
        let t2 = ctx.union(vec![
            ctx.prim(Primitive::Bool),
            ctx.prim(Primitive::Str),
        ]);
   
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.union(vec![
            ctx.prim(Primitive::Num),
            ctx.prim(Primitive::Bool),
            ctx.prim(Primitive::Str),
        ]));
    }

    #[test]
    fn union_of_type_vars() {
        let ctx = Context::new();
        let t1 = ctx.fresh_var();
        let t2 = ctx.fresh_var();
        
        let result = union_types(&t1, &t2, &ctx);
        assert_eq!(result, ctx.union(vec![t1, t2]));
    }
}
