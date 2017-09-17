use std::collections::{HashMap,HashSet};

#[derive(PartialEq,Eq,Clone,Hash,Debug)]
pub enum VarType {
    Unknown,
    Float,
    Int,
    Number,
    String,
    Bool,
    Unify(Vec<String>)
}

pub type TypeMap = HashMap<String,VarType>;

#[derive(Debug,PartialEq,Eq)]
pub enum TypeOrd{ Sub,Super,Same,Unrelated,Cross}

impl VarType {
    pub fn type_cmp(&self,other:&Self) -> TypeOrd {
        use self::VarType::*;
        use self::TypeOrd::*;
        match (self,other) {
            (&Unknown,_) => Super,
            (_,&Unknown) => Sub,
            (&Float,&Number) | (&Int,&Number) | (&Float,&Int) => Sub,
            (&Number,&Float) | (&Number,&Int) | (&Int,&Float) => Super,
            (&Float,&Float) | (&Int,&Int) | (&Bool,&Bool) | (&String,&String) | (&Number,&Number) => Same,
            (&ref x,&ref y) if x==y => Same,
            (&Unify(_),&Unify(_)) => Cross,
            (_,&Unify(_)) => Sub,
            (&Unify(_),_) => Super,
            (&String,& Float) | (&Bool,& Float) | (&Int,& Bool)
                | (&Float,&String) | (&Float,&Bool) | (&Int,&String)
                | (&Number,&String) | (&Number,&Bool) | (&String,&Int)
                | (&String,&Number) | (&String,&Bool) | (&Bool,&Int)
                | (&Bool,&Number) | (&Bool,&String) => Unrelated
        }
    }

    pub fn precise_type(&self, other:&Self) -> Result<Self,TIError> {
        match self.type_cmp(other) {
            TypeOrd::Sub => Ok(self.clone()),
            TypeOrd::Super => Ok(other.clone()),
            TypeOrd::Same => Ok(self.clone()),
            TypeOrd::Unrelated => Err(TIError::Contradiction),
            TypeOrd::Cross => match (self,other) {(&VarType::Unify(ref x),&VarType::Unify(ref y)) => {let mut nv = x.clone();nv.extend(y.clone()); Ok(VarType::Unify(nv))}, _=> panic!("Impossible Type Comparison")}
        }
    }

    pub fn unify_var(&mut self, other:String) {
        if let &mut VarType::Unify(ref mut v) = self {
            v.push(other);
        }
    }
}




// Error types for Type Inference
#[derive(Debug,PartialEq,Eq)]
pub enum TIError {
    Contradiction
}

impl TIError {
    pub fn contra<T>() -> Result<T,TIError> {Err(TIError::Contradiction)}
}

pub mod implication {
    use super::*;
    use ::parser::{UnaryOperator,BinaryOperator,Variable,Expr,Literal,Term};
    use ::parser::BinaryOperator::*;

    pub trait ImpliesType {
        fn implies(&self) -> VarType { VarType::Unknown }
    }

    impl ImpliesType for Literal {
        fn implies(&self) -> VarType {
            match *self {
                Literal::Int(_) => VarType::Int,
                Literal::Float(_) => VarType::Float,
                Literal::Bool(_) => VarType::Bool,
                Literal::String(_) => VarType::String,
            }
        }
    }
    impl ImpliesType for Variable {
        fn implies(&self) -> VarType {
            match *self {
                Variable::Hole => {VarType::Unknown},
                Variable::Name(ref n) => {VarType::Unify(vec![n.0.clone()])}
            }
        }
    }

    impl ImpliesType for Term {
        fn implies(&self) -> VarType {
            match *self {
                Term::Literal(ref l) => {l.implies()},
                Term::Variable(ref v) => {v.implies()}
            }
        }
    }

    impl ImpliesType for BinaryOperator {
        fn implies(&self) -> VarType {
            match *self {
                Add | Subtract | Multiply | Divide => VarType::Number,
                Modulus => VarType::Int,
                And | Or | Xor => VarType::Int,
                LessThan | LessThanEq | Eq | NotEq | GreaterThanEq | GreaterThan => VarType::Number
            }
        }
    }

    impl ImpliesType for UnaryOperator {
        fn implies(&self) -> VarType {
            match *self {
                UnaryOperator::BoolNegate => VarType::Bool,
                UnaryOperator::ArithNegate => VarType::Number,
            }
        }
    }

    impl ImpliesType for Expr {
        fn implies(&self) -> VarType {
            use ::parser::Expr::*;
            match self {
                &Value(ref x) => {x.implies()},
                &Variable(ref v) => {v.implies()},
                &Paren(ref e) => {e.implies()},
                &BinaryResult((_,ref o, _)) => {o.implies()},
                &UnaryResult((ref o,_)) => {o.implies()}
            }
        }
    }
}

pub trait TypeInferable {
    fn get_vars(&self,VarType) -> Result<TypeMap,TIError> { Ok(HashMap::new())}
    fn empty_type_map() -> Result<TypeMap,TIError> { Ok(HashMap::new())}
}

mod type_inference {
    use super::*;
    use ::parser::*;
    use compiler::implication::*;


    pub fn merge_maps(a:Result<TypeMap,TIError>,b:Result<TypeMap,TIError>) -> Result<TypeMap,TIError> {
        match (a,b) {
            (Err(e),_) => Err(e),
            (_,Err(e)) => Err(e),
            (Ok(x),Ok(y)) => {
                let (smaller,mut larger) = if x.len() < y.len() {(x,y)} else {(y,x)};
                for (sv,st) in smaller.into_iter() {
                    match larger.get(&sv).and_then(|t| Some(t.type_cmp(&st))) {
                        None => {larger.insert(sv,st);},
                        Some(TypeOrd::Sub) => {},
                        _ => {}
                    }
                }
                Ok(larger)
            }
        }
    }

    use std::collections::HashSet;

    fn unify_map(a:HashMap<String,VarType>) -> HashMap<String,VarType> {
        let mut need = HashMap::new();
        let mut have :HashMap<String,VarType> = HashMap::new();

        for (v,t) in a.into_iter() {
            match t {
                VarType::Unify(rvs) => {for rv in rvs{need.entry(rv).or_insert(vec![]).push(v.clone())}},
                nt => {have.insert(v,nt);}
            }
        }

        let mut inferable : Vec<String> = have.keys().filter(|k| have.get(*k) != Some(&VarType::Unknown)).collect::<HashSet<&String>>()
            .intersection(&need.keys().collect::<HashSet<&String>>())
            .map(|x| ((*x).clone())).collect();


        while let Some(curr_var) = inferable.pop() {
            let curr_type = if let Some(t_type) = have.get(&curr_var) {
                t_type.clone()
            } else {panic!("Take that Borrow Checker!")};

            for resolvable_var in need.remove(&curr_var).into_iter().flat_map(|x| x.into_iter()) {
                // The following line will need to change to check for Type Conflicts
                use std::collections::hash_map::Entry;
                match have.entry(resolvable_var.clone()) {
                    Entry::Vacant(x) => {x.insert(curr_type.clone());},
                    Entry::Occupied(x) => {
                        let has_type : &VarType = x.get();
                        let union_type = has_type.precise_type(&curr_type);
                    }
                }

                have.insert(resolvable_var.clone(),curr_type.clone());

                inferable.push(resolvable_var);
            }
        }

        for (rv,mut vs) in need.drain() {
            for v in vs.drain(..) {
                have.entry(v).or_insert(VarType::Unify(vec![])).unify_var(rv.clone());
            }
        }
        have
    }

    impl TypeInferable for Identifier {}
    impl TypeInferable for Variable {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            match self {
                &Variable::Hole => {Ok(HashMap::new())},
                &Variable::Name(Identifier(ref name)) => {
                    let mut ret = HashMap::new();
                    ret.insert(name.clone(),t);
                    Ok(ret)
                }
            }
        }
    }
    impl TypeInferable for Number {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            use ::compiler::TypeOrd::*;
            match t.type_cmp(&VarType::Number) {
                Sub | Super | Same => Self::empty_type_map(),
                _ => TIError::contra()
            }
        }
    }
    impl TypeInferable for BinaryOperator {}
    impl TypeInferable for UnaryOperator {}
    impl TypeInferable for Expr {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            match self {
                &Expr::Value(_) => {Ok(HashMap::new())},
                &Expr::Variable(ref x) => {x.get_vars(t)},
                &Expr::Paren(ref x) => {x.get_vars(t)},
                &Expr::BinaryResult((ref l,ref o,ref r)) => {
                    let nt = o.implies();
                    if nt != t && t != VarType::Unknown {
                        return Err(TIError::Contradiction)
                    }
                    merge_maps(l.get_vars(nt.clone()),r.get_vars(nt))
                }
                &Expr::UnaryResult((ref o,ref r)) => {
                    let nt = o.implies();
                    if nt != t && t != VarType::Unknown {return Err(TIError::Contradiction)}
                    r.get_vars(nt)
                }
            }
        }
    }
    impl TypeInferable for Equation {
        fn get_vars(&self,_:VarType) -> Result<TypeMap,TIError> {
            let right_map = self.expr.get_vars(VarType::Unknown);
            let right_type = self.expr.implies();
            if let Variable::Name(ref v) = self.value {
                let mut left_map = HashMap::new();
                left_map.insert(v.0.clone(),right_type);
                merge_maps(Ok(left_map),right_map)
            } else {
                right_map
            }
        }
    }
    impl TypeInferable for SemiRange {
        fn get_vars(&self,_:VarType) -> Result<TypeMap,TIError> {
            use std::collections::Bound::*;
            let lower_type = match self.lower {
                Included(ref l) => l.implies(),
                Excluded(ref l) => l.implies(),
                Unbounded => VarType::Unknown
            };
            let upper_type = match self.upper {
                Included(ref u) => u.implies(),
                Excluded(ref u) => u.implies(),
                Unbounded => VarType::Unknown
            };

            if let Ok(union_type) = lower_type.precise_type(&upper_type) {
                let lower_map = match &self.lower {
                    &Included(ref l) => l.get_vars(union_type.clone()),
                    &Excluded(ref l) => l.get_vars(union_type.clone()),
                    &Unbounded => Ok(HashMap::new())
                };
                let upper_map = match &self.upper {
                    &Included(ref u) => u.get_vars(union_type.clone()),
                    &Excluded(ref u) => u.get_vars(union_type.clone()),
                    &Unbounded => Ok(HashMap::new())
                };


                let val_map = if let Variable::Name(ref id) = self.val {
                    let mut vmap = HashMap::new();
                    vmap.insert(id.0.clone(),union_type.clone());
                    vmap
                } else {HashMap::new()};
                merge_maps(Ok(val_map),merge_maps(lower_map,upper_map))
            } else {
                TIError::contra()
            }

        }
    }
    impl TypeInferable for Literal {}
    impl TypeInferable for Term {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            match self {
                &Term::Literal(ref l) => l.get_vars(t),
                &Term::Variable(ref v) => v.get_vars(t)
            }
        }
    }
    impl TypeInferable for RowFact {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            self.terms.iter().map(|x| x.get_vars(t.clone())).fold(Ok(HashMap::new()),merge_maps)
        }
    }
    impl TypeInferable for TreeTerm {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            match self {
                &TreeTerm::Term(ref term) => term.get_vars(t),
                &TreeTerm::Tree(ref tree) => tree.get_vars(t)
            }
        }
    }
    impl TypeInferable for TreeFact {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            merge_maps(
                self.avs.iter().map(|&(ref r,ref rr)| merge_maps(r.get_vars(t.clone()),rr.get_vars(t.clone()))).fold(self.entity.get_vars(t.clone()),merge_maps),
                self.t.get_vars(t))
        }
    }
    impl TypeInferable for Fact {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            self.0.get_vars(t)
        }
    }
    impl TypeInferable for Pred {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            match self {
                &Pred::RowFact(ref x) => x.get_vars(t),
                &Pred::TreeFact(ref x) => x.get_vars(t),
                &Pred::Equation(ref x) => x.get_vars(t),
                &Pred::SemiRange(ref x) => x.get_vars(t)
            }
        }
    }
    impl TypeInferable for Relation {
        fn get_vars(&self,t:VarType) -> Result<TypeMap,TIError> {
            merge_maps(
                self.vars.iter().map(|x| x.get_vars(t.clone())).fold(Ok(HashMap::new()), merge_maps),
                self.preps.iter().map(|x| x.get_vars(t.clone())).fold(Ok(HashMap::new()), merge_maps),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use ::parser::*;
    use ::parser::parse_fns::*;
    use nom::*;
    use super::*;
    use quickcheck::*;
    use ::compiler::type_inference::*;
    use rand::thread_rng;

    #[test]
    fn sample_equations() {
        let testo = ["a = 1+2","b=1+false","a = b > (c | d)"];
        for t in testo.iter() {
            let res = equation(&t.as_bytes());
            println!("{:?}",res);
            if let IResult::Done(_,t) = res {
                println!("{:?}",t.get_vars(VarType::Unknown));
            } else {
                assert!(false)
            }
        }
    }

    #[test]
    fn commutative_merge() {
        fn prop(r:Relation,s:Relation) -> bool {
            println!("Attempting to gather Vars");
            let lr = merge_maps(r.get_vars(VarType::Unknown),s.get_vars(VarType::Unknown));
            let rl = merge_maps(s.get_vars(VarType::Unknown),r.get_vars(VarType::Unknown));
            println!("Gathered Vars");
            lr == rl
        }
        println!("------Starting commutativity check------");
        QuickCheck::new()
            .gen(StdGen::new(thread_rng(),10))
            .tests(1000)
            .max_tests(5000)
            .quickcheck(prop as fn(Relation,Relation)-> bool);
    }

    #[test]
    fn print_sample_type_maps() {
        let mut g = StdGen::new(thread_rng(),10);
        println!("\n-----------------------------------");
        for _ in 0..5 {
            let r :Relation = Arbitrary::arbitrary(&mut g);

            println!("String: {}",r);
            println!("{:?}",r.get_vars(VarType::Unknown));
            println!("---");

        }
    }


}
