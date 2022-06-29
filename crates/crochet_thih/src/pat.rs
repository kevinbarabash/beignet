use super::assump::*;
use super::context::*;
use super::id::*;
use super::lit::*;
use super::pred::*;
use super::scheme::*;
use super::r#type::*;

// data Pat        = PVar Id
//                 | PWildcard
//                 | PAs  Id Pat
//                 | PLit Literal
//                 | PNpk Id Integer
//                 | PCon Assump [Pat]

//                 | PLazy Pat

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Pat {
    Var(ID), // matches any value and binds the result to a variable with the given id
    Wildcard, // matches any value but doesn't bind the result
    As(ID, Box<Pat>), // binds the variable with the given id to any value matched by the associated pattern
    Lit(Lit),
    Con(Assump, Vec<Pat>),
}

pub fn ti_pat(ctx: &mut Context, pat: &Pat) -> (Vec<Pred>, Vec<Assump>, Type) {
    match pat {
        Pat::Var(id) => {
            let v = ctx.new_tvar();
            (vec![], vec![(id.to_owned(), to_scheme(&v))], v)
        },
        Pat::Wildcard => {
            let v = ctx.new_tvar();
            (vec![], vec![], v)
        },
        Pat::As(id, pat) => {
            let (ps, r#as, t) = ti_pat(ctx, pat);
            let mut r#as = r#as;
            r#as.insert(0, (id.to_owned(), to_scheme(&t)));
            (ps, r#as, t)
        },
        Pat::Lit(lit) => {
            let (ps, t) = ti_lit(lit);
            (ps, vec![], t)
        },
        // TODO: replace with Pat::Tuple() and Pat::Object()
        Pat::Con(_, _) => todo!(),
        // TODO: add Pat::Is()
    }
}

// tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
// tiPats pats = do psasts <- mapM tiPat pats
//                  let ps = concat [ ps' | (ps',_,_) <- psasts ]
//                      as = concat [ as' | (_,as',_) <- psasts ]
//                      ts = [ t | (_,_,t) <- psasts ]
//                  return (ps, as, ts)
pub fn ti_pats(ctx: &mut Context, pats: &[Pat]) -> (Vec<Pred>, Vec<Assump>, Vec<Type>) {
    let psasts: Vec<_> = pats.iter().map(|pat| ti_pat(ctx, pat)).collect();
    let ps: Vec<_> = psasts.iter().flat_map(|(ps_1, _, _)| ps_1).cloned().collect();
    let r#as: Vec<_> = psasts.iter().flat_map(|(_,as_1, _)| as_1).cloned().collect();
    let ts: Vec<_> = psasts.iter().map(|(_, _, t)| t).cloned().collect();
    (ps, r#as, ts)
}
