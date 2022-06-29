use std::collections::HashSet;

use super::assump::*;
use super::context::*;
use super::id::*;
use super::pat::*;
use super::pred::*;
use super::prim::*;
use super::r#type::*;
use super::scheme::*;
use super::subst::*;

// data Expr = Var   Id
//           | Lit   Literal
//           -- named constant, i.e. the type of which is pre-determined
//           | Const Assump
//           | Ap    Expr Expr
//           | Let   BindGroup Expr

//           | Lam   Alt
//           | If    Expr Expr Expr
//           | Case  Expr [(Pat,Expr)]

pub enum Expr {
    Var(ID),
    Const(Assump),
    App(Box<Expr>, Vec<Expr>),
    Let(BindGroup, Box<Expr>),
    Lam(Alt),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Case(Box<Expr>, Vec<(Pat, Box<Expr>)>),
}

// tiExpr                       :: Infer Expr Type
// tiExpr ce as (Var i)          = do sc         <- find i as
//                                    (ps :=> t) <- freshInst sc
//                                    return (ps, t)
pub fn ti_expr(ctx: &mut Context, r#as: &[Assump], e: &Expr) -> (Vec<Pred>, Type) {
    match e {
        Expr::Var(id) => {
            let a = r#as.iter().find(|a| &a.0 == id).unwrap();
            let sc = &a.1;
            let (ps, t) = ctx.fresh_inst(sc);
            (ps, t)
        }
        Expr::Const((_id, sc)) => {
            let (ps, t) = ctx.fresh_inst(sc);
            (ps, t)
        }
        Expr::App(r#fn, args) => {
            let mut all_preds: Vec<Pred> = vec![];

            let (mut fn_pred, fn_type) = ti_expr(ctx, r#as, r#fn);

            let mut arg_types: Vec<Type> = vec![];
            for arg in args {
                let (mut arg_preds, arg_type) = ti_expr(ctx, r#as, arg);
                all_preds.append(&mut arg_preds);
                arg_types.push(arg_type);
            }
            let ret_type = ctx.new_tvar();

            let call_type = Type {
                usage: Some(Usage::FnCall),
                variant: Variant::Lam(arg_types, Box::from(ret_type.clone())),
            };

            unify(ctx, &call_type, &fn_type);

            all_preds.append(&mut fn_pred);

            (all_preds, ret_type)
        }
        Expr::Let(bg, e) => {
            let (ps, as1) = ti_bind_group(ctx, r#as, bg);
            let (qs, t) = ti_expr(ctx, &plus_plus(r#as, &as1), e);
            (plus_plus(&ps, &qs), t)
        }
        Expr::Lam(alt) => ti_alt(ctx, r#as, alt),
        Expr::If(cond, e1, e2) => {
            let (ps, t) = ti_expr(ctx, r#as, cond);
            let t_bool = Type {
                usage: None,
                variant: Variant::Prim(Prim::Bool),
            };
            unify(ctx, &t, &t_bool);
            let (ps1, t1) = ti_expr(ctx, r#as, e1);
            let (ps2, t2) = ti_expr(ctx, r#as, e2);
            unify(ctx, &t1, &t2);
            (plus_plus(&plus_plus(&ps, &ps1), &ps2), t1)
        }
        Expr::Case(_, _) => todo!(),
    }
}

// -----------------------------------------------------------------------------

// type Alt = ([Pat], Expr)
type Alt = (Vec<Pat>, Box<Expr>);

// type Expl = (Id, Scheme, [Alt])
// NOTE: each implicitly typed binding has a single alternative in crochet
type Expl = (ID, Scheme, Vec<Alt>);

pub fn plus_plus<T: Clone>(v1: &[T], v2: &[T]) -> Vec<T> {
    v1.iter().chain(v2).cloned().collect()
}

// -----------------------------------------------------------------------------

// type Impl   = (Id, [Alt])
// NOTE: each implicitly typed binding has a single alternative in crochet
type Impl = (ID, Vec<Alt>);

// restricted   :: [Impl] -> Bool
// restricted bs = any simple bs
//  where simple (i,alts) = any (null . fst) alts
pub fn restricted(bs: &[Impl]) -> bool {
    bs.iter()
        .any(|(_i, alts)| alts.iter().any(|(fst, _snd)| fst.is_empty()))
}

// tiImpls         :: Infer [Impl] [Assump]
pub fn ti_impls(ctx: &mut Context, r#as: &[Assump], bs: &[Impl]) -> (Vec<Pred>, Vec<Assump>) {
    let ts: Vec<_> = bs.iter().map(|_| ctx.new_tvar()).collect();
    let r#is: Vec<String> = bs.iter().map(|(fst, _)| fst).cloned().collect();
    let scs: Vec<_> = ts.iter().map(to_scheme).collect();
    let as1 = plus_plus(&(r#is.iter().cloned().zip(scs).collect::<Vec<_>>()), r#as);
    let altss: Vec<_> = bs.iter().map(|(_, snd)| snd).collect();
    let pss: Vec<_> = altss
        .iter()
        .zip(&ts)
        .flat_map(|(alts, t)| ti_alts(ctx, &as1, alts, t))
        .collect();
    let s = ctx.get_subst();
    let ps1 = pss.apply(&s);
    let ts1 = ts.apply(&s);
    let fs = r#as.to_owned().apply(&s).tv();
    let vss: Vec<_> = ts1.iter().map(|t| t.tv()).collect();
    // gs      = foldr1 union vss \\ fs
    let gs = vss // TODO: make this vss \\ fs
        .iter()
        .cloned()
        .reduce(|accum, x| accum.union(&x).cloned().collect())
        .unwrap();

    let shared_vs = vss
        .iter()
        .cloned()
        .reduce(|accum, x| accum.intersection(&x).cloned().collect())
        .unwrap();
    let shared_vs: Vec<_> = shared_vs.iter().cloned().collect();

    let (ds, rs) = split(&fs, &shared_vs, &ps1);

    //     if restricted bs then
    if restricted(bs) {
        //     let gs'  = gs \\ tv rs
        let gs1: Vec<_> = gs.difference(&rs.tv()).cloned().collect();
        //         scs' = map (quantify gs' . ([]:=>)) ts'
        let scs1 = ts1.iter().map(|t| quantify(&gs1, &(vec![], t.to_owned())));
        //     in return (ds++rs, zipWith (:>:) is scs')
        (
            plus_plus(&ds, &rs),
            r#is.iter().cloned().zip(scs1).collect(),
        )
    } else {
        //     let scs' = map (quantify gs . (rs:=>)) ts'
        let scs1 = ts1.iter().map(|t| {
            quantify(
                &gs.iter().cloned().collect::<Vec<_>>(),
                &(rs.clone(), t.to_owned()),
            )
        });
        //     in return (ds, zipWith (:>:) is scs')
        (ds, r#is.iter().cloned().zip(scs1).collect())
    }
}

// tiAlt                :: Infer Alt Type
// tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
//                            (qs,t)  <- tiExpr ce (as'++as) e
//                            return (ps++qs, foldr fn t ts)
pub fn ti_alt(ctx: &mut Context, r#as: &[Assump], (pats, e): &Alt) -> (Vec<Pred>, Type) {
    let (ps, as_1, ts) = ti_pats(ctx, pats);
    let (qs, t) = ti_expr(ctx, &plus_plus(&as_1, r#as), e);
    (
        // ps++qs
        plus_plus(&ps, &qs),
        // foldr fn t ts
        Type {
            usage: Some(Usage::Alt),
            variant: Variant::Lam(ts, Box::from(t)),
        },
    )
}

// tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
// tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
//                          mapM (unify t) (map snd psts)
//                          return (concat (map fst psts))
pub fn ti_alts(ctx: &mut Context, r#as: &[Assump], alts: &[Alt], t: &Type) -> Vec<Pred> {
    let psts: Vec<_> = alts.iter().map(|alt| ti_alt(ctx, r#as, alt)).collect();
    for (_fst, snd) in psts.clone() {
        unify(ctx, t, &snd);
    }
    psts.iter().flat_map(|(fst, _snd)| fst).cloned().collect()
}

// tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
// tiExpl ce as (i, sc, alts)
pub fn ti_expl(ctx: &mut Context, r#as: &[Assump], (_i, sc, alts): &Expl) -> Vec<Pred> {
    let (qs, t) = ctx.fresh_inst(sc);
    let ps = ti_alts(ctx, r#as, alts, &t);
    let s = ctx.get_subst();
    let qs_1 = qs.apply(&s);
    let t_1 = t.apply(&s);
    // gs = tv t' \\ fs
    let fs = r#as.to_vec().apply(&s).tv();
    let gs: Vec<_> = t_1.apply(&s).tv().difference(&fs).cloned().collect();
    let sc_1 = quantify(&gs, &(qs_1, t_1));

    // ps' = filter (not . entail ce qs') (apply s ps)
    // NOTE: since `entail` always returns `false`, we can drop the `filter` call.
    let ps_1: Vec<_> = ps.apply(&s);
    let (ds, rs) = split(&fs, &gs, &ps_1);
    if sc != &sc_1 {
        panic!("signature too general");
    } else if !rs.is_empty() {
        panic!("context too weak");
    } else {
        ds
    }
}

// split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
//                       -> m ([Pred], [Pred])
pub fn split(fs: &HashSet<ID>, _gs: &[ID], ps: &[Pred]) -> (Vec<Pred>, Vec<Pred>) {
    // split ce fs gs ps = do ps' <- reduce ce ps
    //                        let (ds, rs) = partition (all (`elem` fs) . tv) ps'
    //                        rs' <- defaultedPreds ce (fs++gs) rs
    //                        return (ds, rs \\ rs')
    // "deferred"
    let mut ds: Vec<Pred> = vec![];
    // "retained"
    let mut rs: Vec<Pred> = vec![];
    for p in ps {
        if p.tv().iter().all(|p_tv| fs.contains(p_tv)) {
            ds.push(p.to_owned());
        } else {
            rs.push(p.to_owned());
        }
    }
    // TODO: defaultedPreds
    // rs' <- defaultedPreds ce (fs++gs) rs
    // return (ds, rs \\ rs')
    (ds, rs)
}

// type Ambiguity       = (Tyvar, [Pred])
type Ambiguity = (ID, Vec<Pred>);

// ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
pub fn ambiguities(vs: &[ID], ps: &[Pred]) -> Vec<Ambiguity> {
    let vs: HashSet<_> = vs.iter().cloned().collect();
    ps.to_owned()
        .tv()
        .difference(&vs)
        .map(|v| {
            // (v, filter (elem v . tv) ps)
            (
                v.to_owned(),
                ps.iter().filter(|p| p.tv().contains(v)).cloned().collect(),
            )
        })
        .collect()
}

// -----------------------------------------------------------------------------

// type BindGroup  = ([Expl], [[Impl]])
type BindGroup = (Vec<Expl>, Vec<Vec<Impl>>);

pub fn ti_bind_group(
    ctx: &mut Context,
    r#as: &[Assump],
    (es, iss): &BindGroup,
) -> (Vec<Pred>, Vec<Assump>) {
    // do let as' = [ v:>:sc | (v,sc,alts) <- es ]
    let as1: Vec<Assump> = es
        .iter()
        .map(|(v, sc, _alts)| (v.to_owned(), sc.to_owned()))
        .collect();

    // (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
    let (ps, as2) = ti_seq(ctx, &plus_plus(&as1, r#as), iss);

    // qss        <- mapM (tiExpl ce (as''++as'++as)) es
    let qss: Vec<_> = es
        .iter()
        .flat_map(|e| ti_expl(ctx, &plus_plus(&plus_plus(&as2, &as1), r#as), e))
        .collect();

    // return (ps++concat qss, as''++as')
    (plus_plus(&ps, &qss), plus_plus(&as2, &as1))
}

// tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
pub fn ti_seq(ctx: &mut Context, r#as: &[Assump], bgs: &[Vec<Impl>]) -> (Vec<Pred>, Vec<Assump>) {
    let (all_preds, all_assumps): (Vec<Pred>, Vec<Assump>) = match bgs {
        [bs, bss @ ..] => {
            let (ps, as_1) = ti_impls(ctx, r#as, bs);
            let (qs, as_2) = ti_seq(ctx, &plus_plus(r#as, &as_1), bss);
            (plus_plus(&ps, &qs), plus_plus(&as_2, &as_1))
        }
        [] => (vec![], vec![]),
    };
    (all_preds, all_assumps)
}
