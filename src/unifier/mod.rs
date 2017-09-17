
pub struct LVar(pub usize);

pub enum LError {
    Contradiction
}

pub trait Unifiable<E=LError> {
    fn unify(&self,&Self) -> Result<Self,E>;
}

pub trait LMap {
    type Target;
    fn bump(&mut self,usize);
    fn fresh(&mut self) -> LVar;
    fn unify(&mut self,(LVar,Self::Target));
}



