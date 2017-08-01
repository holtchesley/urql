// use chrono::*;
// use nom::*;

mod parse_fns;

/// This contains the various structs used to represent the AST

#[derive(Clone,Debug,Hash,Eq,PartialEq,Ord,PartialOrd)]
pub struct Identifier {
    id:String
}

impl FromStr for Identifier {
    type Err = ();
    fn from_str(s:&str) -> Result<Identifier,Self::Err> {
        Ok(Identifier{id:String::from(s)})
    }
}

impl From<String> for Identifier {
    fn from(s:String) -> Identifier{
        Identifier{id:s}
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        write!(format,"{}",self.id)
    }
}

#[derive(Clone,Debug,Hash,Eq,PartialEq,Ord,PartialOrd)]
pub enum Variable {
    Hole,
    Name(Identifier)
}

impl Variable {
    fn from_str(input:& str) -> Result<Variable,()> {
        if input == "true" || input == "false" || input == "output" {
            return Err(())
        }
        if input == "_" {
            Ok(Variable::Hole)
        } else {
            Ok(Variable::Name(Identifier::from_str(input).unwrap()))
        }
    }
}

use std::fmt;

impl fmt::Display for Variable {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Variable::Name(ref name) => write!(format, "{}", name),
            &Variable::Hole => write!(format,"_")
        }
    }
}



/// Expressions -
/// Expressions need to
/// 1) be able to represent an evaluable function
/// 2) Propagate the optimal type up to the top for evaluation
/// 3) Propogate a list of Vars up to the top.

#[derive(PartialEq,Eq,Clone,Debug)]
pub enum ParseErrors {
    NumericParseFailed,
    UTF8ConversionError
}



#[derive(PartialEq,Clone,Debug)]
pub enum Number {
    Int(i64),
    Float(f64)
}

impl fmt::Display for Number {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Number::Int(x) => write!(format, "{}", x),
            &Number::Float(x) => write!(format, "{}", x),
        }
    }
}

#[derive(PartialEq,Clone,Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    And,
    Or,
    Xor,
    LessThan,
    LessThanEq,
    Eq,
    NotEq,
    GreaterThanEq,
    GreaterThan
}


impl fmt::Display for BinaryOperator {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &BinaryOperator::Add            => write!(format, "+"),
            &BinaryOperator::Subtract       => write!(format, "-"),
            &BinaryOperator::Multiply       => write!(format, "*"),
            &BinaryOperator::Divide         => write!(format, "/"),
            &BinaryOperator::Modulus        => write!(format, "%"),
            &BinaryOperator::And            => write!(format, "&"),
            &BinaryOperator::Or             => write!(format, "|"),
            &BinaryOperator::Xor            => write!(format, "^"),
            &BinaryOperator::LessThan       => write!(format, "<"),
            &BinaryOperator::LessThanEq     => write!(format, "<="),
            &BinaryOperator::Eq             => write!(format, "=="),
            &BinaryOperator::NotEq          => write!(format, "!="),
            &BinaryOperator::GreaterThanEq  => write!(format, ">="),
            &BinaryOperator::GreaterThan    => write!(format, ">"),
        }
    }
}

use std::str::FromStr;

impl FromStr for BinaryOperator {
    type Err = ();
    fn from_str(s:&str) -> Result<BinaryOperator,Self::Err> {
        match s {
            "+"  => Ok(BinaryOperator::Add),
            "-"  => Ok(BinaryOperator::Subtract),
            "*"  => Ok(BinaryOperator::Multiply),
            "/"  => Ok(BinaryOperator::Divide),
            "%"  => Ok(BinaryOperator::Modulus),
            "&"  => Ok(BinaryOperator::And),
            "|"  => Ok(BinaryOperator::Or),
            "^"  => Ok(BinaryOperator::Xor),
            "<"  => Ok(BinaryOperator::LessThan),
            "<=" => Ok(BinaryOperator::LessThanEq),
            "==" => Ok(BinaryOperator::Eq),
            "!=" => Ok(BinaryOperator::NotEq),
            ">=" => Ok(BinaryOperator::GreaterThanEq),
            ">"  => Ok(BinaryOperator::GreaterThan),
            _ => Err(())
        }
    }
}




#[derive(PartialEq,Clone,Debug)]
pub enum UnaryOperator {
    ArithNegate,
    BoolNegate
}

impl FromStr for UnaryOperator {
    type Err = ();
    fn from_str(s:&str) -> Result<UnaryOperator,Self::Err> {
        match s.chars().next() {
            Some('-') => Ok(UnaryOperator::ArithNegate),
            Some('!') => Ok(UnaryOperator::BoolNegate),
            _ => Err(())
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &UnaryOperator::ArithNegate => write!(format, "-"),
            &UnaryOperator::BoolNegate  => write!(format, "!"),
        }
    }
}



#[derive(PartialEq,Clone,Debug)]
pub enum Expr {
    Value(Literal),
    Variable(Variable),
    Paren(Box<Expr>),
    BinaryResult((Box<Expr>,BinaryOperator,Box<Expr>)),
    UnaryResult((UnaryOperator,Box<Expr>))
}

impl Expr {
    pub fn to_unary(op:Option<UnaryOperator>,exp:Expr) -> Expr {
        match (op,exp) {
            (Some(UnaryOperator::ArithNegate),Expr::Value(Literal::Float(x))) => Expr::Value(Literal::Float(-x)),
            (Some(UnaryOperator::ArithNegate),Expr::Value(Literal::Int(x))) => Expr::Value(Literal::Int(-x)),
            (Some(o),Expr::BinaryResult(x)) => Expr::UnaryResult((o,Box::new(Expr::Paren(Box::new(Expr::BinaryResult(x)))))),
            (Some(_),Expr::UnaryResult((_,x))) => *x,
            (Some(o),e) => Expr::UnaryResult((o,Box::new(e))),
            (None,e)    => e
        }
    }
    pub fn to_paren(exp:Expr) -> Expr {
        match exp {
            Expr::Paren(_) => exp,
            _ => Expr::Paren(Box::new(exp))
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expr::Value(ref n)                      => write!(format, "{}",n),
            &Expr::Variable(ref v)                   => write!(format, "{}",v),
            &Expr::Paren(ref a)                      => write!(format, "({})",a),
            &Expr::BinaryResult((ref l,ref o,ref r)) => write!(format, "{} {} {}",l ,o, r),
            &Expr::UnaryResult((ref o,ref a))        => write!(format, "{}{}",o,a)
        }
    }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Equation {
    value: Variable,
    expr: Expr
}

impl fmt::Display for Equation {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        write!(format, "{} = {}",self.value,self.expr)
    }
}


use std::collections::Bound;

#[derive(PartialEq,Clone,Debug)]
pub struct SemiRange {
    val: Variable,
    lower: Bound<Term>,
    upper: Bound<Term>
}

impl SemiRange {
    pub fn new(var:Variable,lbt:&[u8],lb:Option<Term>,ubt:&[u8],ub:Option<Term>) -> SemiRange {
        let lower_bound = if let Some(l) = lb {
            if lbt == (b"[") {
                Bound::Included(l)
            } else {
                Bound::Excluded(l)
            }
        } else { Bound::Unbounded};
        let upper_bound = if let Some(u) = ub {
            if ubt == (b"]") {
                Bound::Included(u)
            } else {
                Bound::Excluded(u)
            }
        } else { Bound::Unbounded};

        SemiRange{val:var,lower:lower_bound,upper:upper_bound}
    }
}

impl fmt::Display for SemiRange {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(format,"{} in ", self.val);


        let _ = match self.lower {
            Bound::Unbounded    => write!(format, "(,"),
            Bound::Included(ref x)  => write!(format, "[{},",x),
            Bound::Excluded(ref x)  => write!(format, "({},",x)
        };
        let _ = match self.upper {
            Bound::Unbounded    => write!(format, ")"),
            Bound::Included(ref x)  => write!(format, "{}]",x),
            Bound::Excluded(ref x)  => write!(format, "{})",x)
        };

        write!(format, "")
    }
}



#[derive(PartialEq,Clone,Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool)
}

impl fmt::Display for Literal {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Literal::Int(ref x)    => {write!(format,"{}",x)},
            &Literal::Float(ref x)  => {write!(format,"{:.64}",x)},
            &Literal::String(ref x) => {write!(format,"\"{}\"",x
                                               .replace("\\","\\\\")
                                               .replace("\"","\\\""))},
            &Literal::Bool(ref x)   => {write!(format,"{}",x)}
        }
    }
}


#[derive(PartialEq,Clone,Debug)]
pub enum Term {
    Literal(Literal),
    Variable(Variable)
}

impl fmt::Display for Term {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Term::Literal(ref x) => {write!(format,"{}",x)},
            &Term::Variable(ref x) => {write!(format,"{}",x)}
        }
    }
}


#[derive(PartialEq,Clone,Debug)]
pub struct RowFact {
    head: Identifier,
    terms: Vec<Term>
}

impl fmt::Display for RowFact {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(format,"{}(", self.head);
        let mut first = true;
        for t in self.terms.iter() {
            if !first {
                let _ = write!(format, ",{}",t);
            } else {
                let _ = write!(format, "{}",t);
            }
            first = false;
        }
        write!(format,")")
    }
}


// Compound [e {a1 v1 a2 [e2 {a3 v3 a4 [e3 a4 v4 t3]} t2]} t1]

// Leaf: [e a v t] -> [e {a v} t]

#[derive(PartialEq,Clone,Debug)]
pub enum TreeTerm {
    Term(Term),
    Tree(Box<TreeFact>)
}

impl fmt::Display for TreeTerm {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &TreeTerm::Term(ref x) => {write!(format,"{}",x)},
            &TreeTerm::Tree(ref x) => {write!(format,"{}",x)}
        }
    }
}


#[derive(PartialEq,Clone,Debug)]
pub struct TreeFact {
    entity: Term,
    avs: Vec<(Term,TreeTerm)>,
    t: Term
}

impl fmt::Display for TreeFact {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(format,"[{}",self.entity);
        let mut first = true;
        if self.avs.len() > 1 {
            let _ = write!(format," {{");
        }
        for &(ref at,ref val) in self.avs.iter() {
            if !first {
                let _ = write!(format,",");
            }
            first = true;
            let _ = write!(format," {} {}",at,val);
        }
        if self.avs.len() > 1 {
            let _ = write!(format,"}}");
        }
        write!(format," {}]",self.t)
    }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Fact(Pred);

impl fmt::Display for Fact {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        write!(format,"{};",self.0)
    }
}


#[derive(PartialEq,Clone,Debug)]
pub enum Pred {
    RowFact(RowFact),
    TreeFact(TreeFact),
    Equation(Equation),
    SemiRange(SemiRange)
}


impl fmt::Display for Pred {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Pred::RowFact(ref x) => {write!(format,"{}",x)},
            &Pred::TreeFact(ref x) => {write!(format,"{}",x)},
            &Pred::Equation(ref x) => {write!(format,"{}",x)},
            &Pred::SemiRange(ref x) => {write!(format,"{}",x)},
        }
    }
}


#[derive(PartialEq,Clone,Debug)]
pub struct Relation {
    head:Identifier,
    vars: Vec<Variable>,
    preps: Vec<Pred>
}

impl fmt::Display for Relation {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(format,"{}(",self.head);
        let mut first = true;
        for v in self.vars.iter() {
            if !first {
                let _ = write!(format,",");
            }
            first = false;
            let _ = write!(format,"{}",v);
        }
        let _ = write!(format,") :- ");

        first = true;
        for p in self.preps.iter() {
            if !first {
                let _ = write!(format,", ");
            }
            first = false;
            let _ = write!(format, "{}",p);
        }
        write!(format,";")
    }
}


#[cfg(test)]
mod test_support {
    use quickcheck::{Arbitrary,Gen,StdGen,empty_shrinker,single_shrinker};
    use rand::{ThreadRng,thread_rng};
    use super::*;

    fn smaller<G:Gen>(g:&G) -> StdGen<ThreadRng> {
        StdGen::new(thread_rng(),g.size()-1)
    }

    fn between<T:Ord>(low:T,high:T,x:T) -> T {
        if x < low {
            low
        } else if x > high {
            high
        } else {x}
    }

    impl Arbitrary for Identifier {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("ID>");
            let s = g.size();
            Identifier{id:g.gen_ascii_chars().skip_while(|c| c >= &'0' && c <= &'9').take(s).collect()}
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            if self.id.len() <= 1 {
                empty_shrinker()
            } else {
                Box::new(self.id.shrink().filter(|x| {
                    if x.len() > 0 {
                        if let Some(c) = x.chars().next() {
                            (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                        } else {false}
                    } else {false}
                })
                         .map(|x| Identifier{id:x}))
            }
        }
    }
    impl Arbitrary for Variable {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("VR>");
            let hole:bool = g.gen();
            if hole {
                Variable::Hole
            } else {
                Variable::Name(Arbitrary::arbitrary(g))
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match self {
                &Variable::Hole => empty_shrinker(),
                &Variable::Name(ref n) => {
                    let chain = single_shrinker(Variable::Hole).chain(n.shrink().map(Variable::Name));
                    Box::new(chain)
                }
            }
        }
    }
    impl Arbitrary for Number {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("NR>");
            if g.gen() {
                Number::Int(Arbitrary::arbitrary(g))
            } else {
                Number::Float(Arbitrary::arbitrary(g))
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match self {
                &Number::Int(ref x) => {
                    Box::new(x.shrink().map(Number::Int))
                },
                &Number::Float(ref x) => {
                    Box::new(x.shrink().map(Number::Float))
                }
            }
        }
    }
    impl Arbitrary for BinaryOperator {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("BO>");
            let choices = vec![BinaryOperator::Add,
                               BinaryOperator::Subtract,
                               BinaryOperator::Multiply,
                               BinaryOperator::Divide,
                               BinaryOperator::Modulus,
                               BinaryOperator::And,
                               BinaryOperator::Or,
                               BinaryOperator::Xor,
                               BinaryOperator::LessThan,
                               BinaryOperator::LessThanEq,
                               BinaryOperator::Eq,
                               BinaryOperator::NotEq,
                               BinaryOperator::GreaterThanEq,
                               BinaryOperator::GreaterThan
            ];
            g.choose(&choices).unwrap().clone()
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            empty_shrinker()
        }
    }
    impl Arbitrary for UnaryOperator {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("UAO>");
            let choices = vec![UnaryOperator::BoolNegate,UnaryOperator::ArithNegate];
            g.choose(&choices).unwrap().clone()
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            empty_shrinker()
        }
    }
    impl Arbitrary for Expr {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("AE>");
            let mut ng = smaller(g);
            let size = g.size();
            match g.gen_range(0,between(0,5,size)) {
                0 => {Expr::Value(Arbitrary::arbitrary(g))},
                1 => {Expr::Variable(Arbitrary::arbitrary(g))},
                2 => {Expr::to_paren(Arbitrary::arbitrary(&mut ng))},
                3 => {Expr::BinaryResult((Box::new(Arbitrary::arbitrary(&mut ng)),Arbitrary::arbitrary(&mut ng),Box::new(Arbitrary::arbitrary(&mut ng))))},
                4 => {Expr::to_unary(Arbitrary::arbitrary(g),Arbitrary::arbitrary(&mut ng))},
                _ => panic!("Outside of specified range")
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match *self {
                Expr::Value(ref x)        => {Box::new(x.shrink().map(Expr::Value))},
                Expr::Variable(ref x)     => {Box::new(x.shrink().map(Expr::Variable))},
                Expr::Paren(ref x)        => {Box::new(x.shrink().map(Box::new).map(Expr::Paren))},
                Expr::BinaryResult((ref l,ref o, ref r)) => {
                    Box::new((l.as_ref().clone(),o.clone(),r.as_ref().clone()).shrink()
                             .map(|(a,b,c)| (Box::new(a),b,Box::new(c)))
                             .map(Expr::BinaryResult))
                },
                Expr::UnaryResult((ref o,ref r))  => {
                    Box::new((o.clone(),r.as_ref().clone()).shrink()
                             .map(|(o,r)| (o,Box::new(r)))
                             .map(Expr::UnaryResult))},
            }
        }
    }
    impl Arbitrary for Equation {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("EQ>");
            Equation{value:Arbitrary::arbitrary(g),expr:Arbitrary::arbitrary(g)}
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            Box::new(
                (self.value.clone(),self.expr.clone()).shrink().map(|(v,e)| {Equation{value:v,expr:e}})
            )
        }
    }
    impl Arbitrary for SemiRange {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("SR>");
            let lower = match g.gen_range(0,3) {
                0 => {Bound::Included(Arbitrary::arbitrary(g))},
                1 => {Bound::Excluded(Arbitrary::arbitrary(g))},
                2 => {Bound::Unbounded},
                _ => panic!()
            };
            let upper = match g.gen_range(0,3) {
                0 => {Bound::Included(Arbitrary::arbitrary(g))},
                1 => {Bound::Excluded(Arbitrary::arbitrary(g))},
                2 => {Bound::Unbounded},
                _ => panic!()
            };

            SemiRange{val:Arbitrary::arbitrary(g),lower:lower,upper:upper}
        }

        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match (self.lower.clone(),self.upper.clone()) {
                (Bound::Unbounded,Bound::Unbounded) => {
                    Box::new(self.val.shrink()
                             .map(|v|SemiRange{val:v,lower:Bound::Unbounded,upper:Bound::Unbounded}))
                },
                (Bound::Unbounded,Bound::Included(ref u))  => {
                    Box::new((self.val.clone(),u.clone()).shrink()
                             .map(|(v,u)|
                                  SemiRange{val:v,
                                            lower:Bound::Unbounded,
                                            upper:Bound::Included(u)}))
                },
                (Bound::Unbounded,Bound::Excluded(ref u))  => {
                    Box::new((self.val.clone(),u.clone()).shrink()
                             .map(|(v,u)|
                                  SemiRange{val:v,
                                            lower:Bound::Unbounded,
                                            upper:Bound::Excluded(u)}))
                },
                (Bound::Included(ref l),Bound::Unbounded) => {
                    Box::new((self.val.clone(),l.clone()).shrink()
                             .map(|(v,l)|
                                  SemiRange{val:v,
                                            lower:Bound::Included(l),
                                            upper:Bound::Unbounded}))
                },
                (Bound::Included(ref l),Bound::Included(ref u))  => {
                    Box::new((self.val.clone(),l.clone(),u.clone()).shrink()
                             .map(|(v,l,u)|
                                  SemiRange{val:v,
                                            lower:Bound::Included(l),
                                            upper:Bound::Included(u)}))
                },
                (Bound::Included(ref l),Bound::Excluded(ref u))  => {
                    Box::new((self.val.clone(),l.clone(),u.clone()).shrink()
                             .map(|(v,l,u)|
                                  SemiRange{val:v,
                                            lower:Bound::Included(l),
                                            upper:Bound::Excluded(u)}))
                },
                (Bound::Excluded(ref l),Bound::Unbounded) => {
                    Box::new((self.val.clone(),l.clone()).shrink()
                             .map(|(v,l)|
                                  SemiRange{val:v,
                                            lower:Bound::Excluded(l),
                                            upper:Bound::Unbounded}))
                },
                (Bound::Excluded(ref l),Bound::Included(ref u))  => {
                    Box::new((self.val.clone(),l.clone(),u.clone()).shrink()
                             .map(|(v,l,u)|
                                  SemiRange{val:v,
                                            lower:Bound::Excluded(l),
                                            upper:Bound::Included(u)}))
                },
                (Bound::Excluded(ref l),Bound::Excluded(ref u))  => {
                    Box::new((self.val.clone(),l.clone(),u.clone()).shrink()
                             .map(|(v,l,u)|
                                  SemiRange{val:v,
                                            lower:Bound::Excluded(l),
                                            upper:Bound::Excluded(u)}))
                },
            }
        }
    }
    impl Arbitrary for Literal {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("LR>");
            match g.gen_range(0,4) {
                0 => {Literal::Int(Arbitrary::arbitrary(g))},
                1 => {Literal::Float(Arbitrary::arbitrary(g))},
                2 => {Literal::String(Arbitrary::arbitrary(g))},
                3 => {Literal::Bool(Arbitrary::arbitrary(g))},
                _ => panic!()
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match *self {
                Literal::Int(ref x) => {Box::new(x.shrink().map(Literal::Int))},
                Literal::Float(ref x) => {Box::new(x.shrink().map(Literal::Float))},
                Literal::String(ref x) => {Box::new(x.shrink().map(Literal::String))},
                Literal::Bool(ref x) => {Box::new(x.shrink().map(Literal::Bool))},
            }
        }
    }
    impl Arbitrary for Term {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("T>");
            if g.gen() {
                Term::Literal(Arbitrary::arbitrary(g))
            } else {
                Term::Variable(Arbitrary::arbitrary(g))
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match *self {
                Term::Literal(ref x) => {Box::new(x.shrink().map(Term::Literal))},
                Term::Variable(ref x) => {Box::new(x.shrink().map(Term::Variable))},
            }
        }
    }
    impl Arbitrary for RowFact {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("RF>");
            let mut ret = RowFact{head:Arbitrary::arbitrary(g),terms:Arbitrary::arbitrary(g)};
            
            if ret.terms.len() == 0 {
                ret.terms = vec![Term::Variable(Variable::Hole)];
            }
            ret
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            Box::new((self.head.clone(),self.terms.clone()).shrink()
                     .filter(|&(_,ref t)| t.len() > 0)
                     .map(|(h,t)| RowFact{head:h,terms:t}))
        }
    }
    impl Arbitrary for TreeTerm {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("TT>");
            if g.gen() && g.size() > 1 {
                let mut ng = smaller(g);
                TreeTerm::Tree(Box::new(Arbitrary::arbitrary(&mut ng)))
            } else {
                TreeTerm::Term(Arbitrary::arbitrary(g))
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match *self {
                TreeTerm::Term(ref x) => {Box::new(x.shrink().map(TreeTerm::Term))},
                TreeTerm::Tree(ref x) => {Box::new(x.as_ref()
                                                   .shrink()
                                                   .map(|b| TreeTerm::Tree(Box::new(b))))},
            }
        }
    }
    impl Arbitrary for TreeFact {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("TF>");
            let mut ret = TreeFact{entity:Arbitrary::arbitrary(g),
                                   avs:Arbitrary::arbitrary(g),
                                   t: Arbitrary::arbitrary(g)};
            if ret.avs.len() == 0 {
                ret.avs = vec!((Term::Variable(Variable::Hole),TreeTerm::Term(Term::Variable(Variable::Hole))))
            }
            ret
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            Box::new((self.entity.clone(),self.avs.clone(),self.t.clone())
                     .shrink()
                     .filter(|&(_,ref a,_)| a.len() > 0)
                     .map(|(e,av,t)| TreeFact{entity:e,avs:av,t:t}))
        }
    }
    impl Arbitrary for Pred {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("PD>");
            match g.gen_range(0,4) {
                0 => {Pred::RowFact(Arbitrary::arbitrary(g))},
                1 => {Pred::TreeFact(Arbitrary::arbitrary(g))},
                2 => {Pred::Equation(Arbitrary::arbitrary(g))},
                3 => {Pred::SemiRange(Arbitrary::arbitrary(g))},
                _ => panic!()
            }
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            match *self {
                Pred::RowFact(ref x) => {Box::new(x.shrink().map(Pred::RowFact))},
                Pred::TreeFact(ref x) => {Box::new(x.shrink().map(Pred::TreeFact))},
                Pred::Equation(ref x) => {Box::new(x.shrink().map(Pred::Equation))},
                Pred::SemiRange(ref x) => {Box::new(x.shrink().map(Pred::SemiRange))}
            }
        }
    }
    impl Arbitrary for Fact {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("F>");
            Fact(Arbitrary::arbitrary(g))
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            Box::new(self.0.shrink().map(Fact))
        }
    }
    impl Arbitrary for Relation {
        fn arbitrary<G:Gen>(g: &mut G) -> Self {
            //print!("RL>");
            let mut t = Relation{head:Arbitrary::arbitrary(g),
                     vars:Arbitrary::arbitrary(g),
                                 preps:Arbitrary::arbitrary(g)};
            
            if t.vars.len() == 0 {
                t.vars = vec![Variable::Hole];
            }
            if t.preps.len() == 0 {
                t.preps = vec![Arbitrary::arbitrary(g)];
            }
            t
        }
        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            Box::new((self.head.clone(),self.vars.clone(),self.preps.clone()).shrink()
                     .filter(|&(_,ref v,ref p)| {v.len() > 0 && p.len() > 0})
                     .map(|(h,v,p)| Relation{head:h,vars:v,preps:p}))
        }
    }

}


