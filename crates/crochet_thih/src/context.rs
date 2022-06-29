use super::pred::*;
use super::scheme::*;
use super::subst::*;
use super::r#type::*;
use super::unify::*;

pub struct Context {
    pub next_id: u32,
    pub subst: Subst,
}

impl Context {
    pub fn new_tvar(&mut self) -> Type {
        let id = format!("v{}", self.next_id);
        self.next_id += 1;
        Type { usage: None, variant: Variant::Var(id) }
    }

    // freshInst               :: Scheme -> TI (Qual Type)
    pub fn fresh_inst(&mut self, sc: &Scheme) -> Qual<Type> {
        let Scheme::Forall(ks, qt) = sc;
        let ts: Vec<_> = ks.iter().map(|_| self.new_tvar()).collect();
        qt.to_owned().inst(&ts)
        // freshInst (Forall ks qt) = do ts <- mapM newTVar ks
        //                       return (inst ts qt)
    }

    // getSubst   :: TI Subst
    // getSubst    = TI (\s n -> (s,n,s))
    pub fn get_subst(&mut self) -> Subst {
        self.subst.to_owned()
    }

    // extSubst   :: Subst -> TI ()
    // extSubst s' = TI (\s n -> (s'@@s, n, ()))
    pub fn ext_subst(&mut self, s1: &Subst) {
        self.subst = at_at(s1, &self.subst);
    }
}

// class Instantiate t where
//   inst  :: [Type] -> t -> t
pub trait Instantiate {
    fn inst(&self, ts: &[Type]) -> Self;
}

// instance Instantiate Type where
//   inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
//   inst ts (TGen n)  = ts !! n
//   inst ts t         = t
impl Instantiate for Type {
    fn inst(&self, ts: &[Type]) -> Self {
        match &self.variant {
            Variant::Lam(args, ret) => {
                Type {
                    usage: self.usage.to_owned(),
                    variant: Variant::Lam(args.inst(ts), ret.inst(ts))
                }
            },
            _ => self.to_owned(),
        }
    }
}

// instance Instantiate a => Instantiate [a] where
//   inst ts = map (inst ts)
impl<I> Instantiate for Vec<I>
where
    I: Instantiate,
{
    fn inst(&self, ts: &[Type]) -> Self {
        self.iter().map(|t| t.inst(ts)).collect()
    }
}

impl<I> Instantiate for Box<I>
where
    I: Instantiate,
{
    fn inst(&self, ts: &[Type]) -> Self {
        Box::from(self.as_ref().inst(ts))
    }
}

// instance Instantiate t => Instantiate (Qual t) where
//   inst ts (ps :=> t) = inst ts ps :=> inst ts t
impl Instantiate for Qual<Type> {
    fn inst(&self, ts: &[Type]) -> Self {
        let (ps, t) = self;
        (ps.inst(ts), t.inst(ts))
    }
}

// instance Instantiate Pred where
//   inst ts (IsIn c t) = IsIn c (inst ts t)
impl Instantiate for Pred {
    fn inst(&self, ts: &[Type]) -> Self {
        let Pred::IsIn(c, t) = self;
        Pred::IsIn(c.to_owned(), t.inst(ts))
    }
}

// TODO: move `unify` to be a method on Context
// unify      :: Type -> Type -> TI ()
// unify t1 t2 = do s <- getSubst
//                  u <- mgu (apply s t1) (apply s t2)
//                  extSubst u
pub fn unify(ctx: &mut Context, t1: &Type, t2: &Type) {
    let s = ctx.get_subst();
    let u = mgu(&t1.apply(&s), &t2.apply(&s), &None);
    ctx.ext_subst(&u);
}
