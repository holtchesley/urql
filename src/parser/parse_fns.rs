use nom::*;
use super::*;
use std::str;
use std::str::{from_utf8,FromStr};

////////// Tokens


// Ignored
named!(comment_inline<()>,
       do_parse!(
           tag!("/*") >>
               take_until_and_consume!("*/") >>
               ()
       )
);
named!(comment_line<()>,
       do_parse!(
           tag!("//") >>
               take_until_and_consume!("\n") >>
               ()
       )
);
named!(toss<()>,
       map!(
           many0!(
               alt_complete!( map!(eof!(),|_|{()})
                              | map!(multispace,|_|{()})
                              | comment_inline
                              | comment_line
               )
           ),
           |_|{()}
       )
);

// Delimiters

named!(comma<()>, delimited!(toss,map!(tag!(","),|x| {()}),toss));

named!(implicates<()>, delimited!(toss,map!(tag!(":-"),|x| {()}),toss));

named!(open_paren<()>,delimited!(toss,map!(tag!("("),|x| {()}),toss));
named!(close_paren<()>,delimited!(toss,map!(tag!(")"),|x| {()}),toss));

named!(open_curly<()>,delimited!(toss,map!(tag!("{"),|x| {()}),toss));
named!(close_curly<()>,delimited!(toss,map!(tag!("}"),|x| {()}),toss));

named!(open_square<()>,delimited!(toss,map!(tag!("["),|x| {()}),toss));
named!(close_square<()>,delimited!(toss,map!(tag!("]"),|x| {()}),toss));

// Numbers
named!(integer<i64>,
       do_parse!(
           neg: opt!(tag!("-")) >>
           i:map_res!(
               delimited!(toss,digit,toss),
               |x| {
                   if let Ok(y) = from_utf8(x) {
                       if let Ok(n) = FromStr::from_str(y) {
                           Ok(n)
                       } else {
                           Err(ParseErrors::NumericParseFailed)
                       }
                   } else {
                       Err(ParseErrors::UTF8ConversionError)
                   }
               }
           ) >>
               (match neg {Some(_) => -1*i, _ => i})
       )
);
named!(float_nan,tag!("NaN"));
named!(float_inf,do_parse!(opt!(tag!("-")) >> a:tag!("inf") >> (a)));
named!(float_e,do_parse!(is_a!("eE") >> opt!(tag!("-")) >> a:digit >> (a)));
named!(float_happy,do_parse!(
    opt!(tag!("-")) >>
    opt!(digit) >>
        a:tag!(".") >>
        alt!(recognize!(eof!()) | recognize!(opt!(digit))) >>
        alt!(recognize!(eof!()) | recognize!(opt!(float_e))) >>
        (a)));
named!(float_alts<f64>,
       delimited!(toss,
                  add_return_error!(ErrorKind::Custom(1),
                                    map_res!(
                                        map_res!(
                                            recognize!(
                                                alt_complete!(float_nan | float_inf | float_happy)
                                            ),
                                            str::from_utf8
                                        ),
                                        FromStr::from_str
                                    )
                  ),
                  toss)
);


// Booleans
named!(boolean<bool>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          alt_complete!(tag!("true") | tag!("false")),
                          str::from_utf8
                      ),
                      FromStr::from_str
                  ),
                  toss
       )
);


// Identifier
named!(identifier<&'a str>,
       map_res!(
           recognize!(
               do_parse!(
                   alt_complete!(alpha | is_a!("_")) >>
                       alt!(recognize!(eof!())
                            | recognize!(opt!(alphanumeric))) >>
                       (())
               )
           ),
           str::from_utf8)
);

// Vars
named!(variable<Variable >,delimited!(toss,map_res!(identifier, Variable::from_str),toss));



// String Literals
fn to_s(i:Vec<u8>) -> String {
    String::from_utf8_lossy(&i).into_owned()
}

named!(string_contents<String >,
       map!(
           escaped_transform!(is_not!("\\\""), '\\',
                              alt_complete!(
                                  tag!("\\")       => { |_| &b"\\"[..] }
                                  | tag!("\"")       => { |_| &b"\""[..] }
                                  | tag!("n")        => { |_| &b"\n"[..] }
                              )
           ), to_s
       )
);

named!(string_lit<String>,
       do_parse!(
           toss >>
           tag!("\"") >>
               cont:opt!(string_contents) >>
               tag!("\"") >>
               toss >>
               (cont.unwrap_or_default())
       )
);


// Expressions

named!(number<Number>,alt_complete!(map!(float_alts, Number::Float) | map!(integer,Number::Int)));

named!(literal<Literal>,alt_complete!(map!(string_lit,Literal::String)
                                      |map!(float_alts,Literal::Float)
                                      |map!(integer,Literal::Int)
                                      |map!(boolean,Literal::Bool)));

// Arithmetic Trees
named!(binary_operator_1<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          tag!("%"),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(binary_operator_2<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          alt!(tag!("*") | tag!("/")),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(binary_operator_3<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          alt!(tag!("+") | tag!("-")),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
// Comparison Trees
named!(binary_operator_4<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          alt_complete!( tag!("<=") | tag!("<")
                                         | tag!("==") | tag!("!=")
                                         | tag!(">=") | tag!(">")),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(binary_operator_5<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          tag!("^"),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(binary_operator_6<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          tag!("|"),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(binary_operator_7<BinaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          tag!("&"),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);

named!(expr_parens<Box<Expr> >,delimited!(open_paren,map!(expr,Box::new),close_paren) );

fn left_fold(initial: Expr,remainder: Vec<(BinaryOperator,Expr)>) -> Expr {
    remainder.into_iter().fold(initial,|left,pair| {
        let (op,right) = pair;
        Expr::BinaryResult((Box::new(left),op,Box::new(right)))
    })
}
named!(expr_bottom< Expr >,
       alt_complete!(
           map!(literal,|n| {Expr::Value(n)})
               | map!(expr_parens,Expr::Paren)
               | map!(variable,Expr::Variable)
       )
);

named!(unary_operator<UnaryOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          alt!(tag!("!") | tag!("-")),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);

named!(expr_0< Expr >,
       do_parse!(
           op: opt!(unary_operator) >>
           exp : expr_bottom >>
               (Expr::to_unary(op,exp))
       )
);
named!(expr_1<Expr >,
       do_parse!(
           left: expr_0 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_1 >>
                             right:expr_0 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )
);
named!(expr_2<Expr >,
       do_parse!(
           left: expr_1 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_2 >>
                             right:expr_1 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )
);
named!(expr_3<Expr >,
       do_parse!(
           left: expr_2 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_3 >>
                             right:expr_2 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )
);

named!(expr_4<Expr >,
       do_parse!(
           left: expr_3 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_4 >>
                             right:expr_3 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )

       // do_parse!(
       //     l: expr_3 >>
       //         m: opt!(tuple!(binary_operator_4,expr_3)) >>
       //         (if let Some((op,r)) = m {Expr::BinaryResult((Box::new(l),op,Box::new(r)))} else {l})
       // )
);

named!(expr_5<Expr >,
       do_parse!(
           left: expr_4 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_5 >>
                             right:expr_4 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )
);

named!(expr_6<Expr >,
       do_parse!(
           left: expr_5 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_6 >>
                             right:expr_5 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )
);

named!(expr<Expr >,
       do_parse!(
           left: expr_6 >>
               remainder: many0!(
                   do_parse!(op: binary_operator_7 >>
                             right:expr_6 >>
                             (op,right)
                   )) >>
               (left_fold(left,remainder))
       )
);




// Master Expr
// named!(expr<Expr>, call!(expr_7));




// Term
named!(term<Term >,
       alt_complete!(map!(float_alts, |x| {Term::Literal(Literal::Float(x))})
           | map!(integer, |x| {Term::Literal(Literal::Int(x))})
            | map!(string_lit, |x| {Term::Literal(Literal::String(x))})
            | map!(boolean, |x| {Term::Literal(Literal::Bool(x))})
            | map!(variable, Term::Variable)
       )
);

/////// Facts

// Equation

named!(equation<Equation >,
       do_parse!(
           var:variable >>
               delimited!(toss,tag!("="),toss) >>
               exp: expr >>
               (Equation{value:var,expr:exp})
       )
);


// Within clause

named!(within<SemiRange >,
       do_parse!(
          var: variable >>
               toss >>
               tag!("in") >>
               toss >>
          lbt: alt!(tag!("(") | tag!("[")) >>
               toss >>
               lower: opt!(term) >>
               comma >>
               upper: opt!(term) >>
          ubt: alt!(tag!(")") | tag!("]")) >>
               (SemiRange::new(var,lbt,lower,ubt,upper))
       )
);

// Row
named!(row_fact<RowFact >,
       do_parse!(
       id: identifier >>
           open_paren >>
    terms: separated_nonempty_list!(tag!(","),term) >>
           close_paren >>
           (RowFact{head:Identifier::from(String::from(id)),terms:terms})
       )
);


// Tree

named!(av_pair<(Term,TreeTerm)>,
       do_parse!(
           at: term >>
               val: opt!(alt_complete!(subtree | map!(term,TreeTerm::Term))) >>
               (at, val.unwrap_or(TreeTerm::Term(Term::Variable(Variable::Hole))))
       )
);

named!(unnamed_subtree<Vec<(Term,TreeTerm)> >,
       do_parse!(
           open_curly >>
               pairs: many1!(av_pair) >>
               close_curly >>
           (pairs)
       )
);


named!(subtree<TreeTerm >,
       alt_complete!(
           map!(unnamed_subtree,
                |avs|
                TreeTerm::Tree(Box::new(TreeFact{entity: Term::Variable(Variable::Hole),
                                                 avs: avs,
                                                 t: Term::Variable(Variable::Hole)
                }))
           ) |
           map!(named_subtree, |x| {TreeTerm::Tree(Box::new(x))})
       )
);


named!(named_subtree<TreeFact >,
       do_parse!(
           open_square >>
               entity: opt!(term) >>
               av: alt_complete!(unnamed_subtree | map!( av_pair, |x| vec![x] ) )>>
               t: opt!(term) >>
               close_square >>
               (TreeFact{entity: entity.unwrap_or(Term::Variable(Variable::Hole)),
                         avs:av,
                         t:t.unwrap_or(Term::Variable(Variable::Hole))})
       )
);

named!(tree_fact<TreeFact >,
       alt_complete!(named_subtree
                     | map!(unnamed_subtree,
                            |avs| TreeFact{entity:Term::Variable(Variable::Hole),
                                           avs:avs, t: Term::Variable(Variable::Hole)}
            )
       )
);


named!(prep<Pred>,
       alt_complete!(map!(row_fact, Pred::RowFact)
            | map!(tree_fact,       Pred::TreeFact)
            | map!(equation,        Pred::Equation)
            | map!(within,          Pred::SemiRange)
       )
);

named!(fact<Fact>,
       do_parse!(
           f: prep  >>
              tag!(";") >>
              (Fact(f))
       )
);


// Relation
named!(relation<Relation>,
       do_parse!(
         head: identifier >>
               open_paren >>
               vars: separated_nonempty_list!(comma,variable) >>
               close_paren >>
               implicates >>
               preps: separated_nonempty_list!(comma,prep) >>
               tag!(";") >>
               (Relation{head:Identifier::from(String::from(head)),vars:vars,preps:preps})
       )
);




#[cfg(test)]
mod tests {
    use std::str;
    use std::string::*;
    use super::*;
    use quickcheck::*;
    use rand::thread_rng;

    #[test]
    fn floats() {
        fn prop(s:String) -> bool {
            if let IResult::Done(rem,out) = float_alts(&s[..].as_bytes()) {
                let newstr = &s[0..(s.len()-rem.len())];
                if let Ok(Ok(y)) = str::from_utf8(newstr.as_bytes()).map(|x| x.trim()).map(FromStr::from_str) {
                    out == y
                } else {
                    println!("1: {:?}, ", float_alts(&s[..].as_bytes()));
                    false
                }
            } else {
                if let Ok(y) = FromStr::from_str(&s) {
                    let _ :f64 = y;
                    if let Ok(z) = FromStr::from_str(&s) {
                        let _ : i64 = z;
                        // The nom parser should fail to recognize an integer, because we want to have it correctly typed
                        // Unlike Rust's FromStr, which is happy to cast your integer to a float
                        // ...
                        // It's just trying to be nice
                        true
                    } else {
                        println!("2: s:\"{}\", {:?}",s,float_alts(&s[..].as_bytes()));
                        false
                    }
                } else {
                    true
                }
            }
        }
        assert!(prop(String::from("0. ")));
        QuickCheck::new()
            .tests(100000)
            .max_tests(500000)
            .quickcheck(prop as fn(String)-> bool);
    }

    #[test]
    fn int_tower() {
        let testa : Vec<String> = ["1","10"," 1","1 "," 1 "].into_iter().map(|x| String::from(*x)).collect();
        for s in testa {
            if let IResult::Done(_,_) = integer(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at integer.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = number(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at number.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_bottom(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at bottom.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_0(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 0.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_1(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 1.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_2(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 2.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_3(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 3.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_4(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 4.",s);
                println!("{:?}",expr_4(&s[..].as_bytes()));
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_5(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 5.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr_6(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at 6.",s);
                assert!(false);
            }
            if let IResult::Done(_,_) = expr(&s[..].as_bytes()) {
                assert!(true);
            } else {
                println!("{} failed at master.",s);
                assert!(false);
            }
        }
    }

    #[test]
    fn display() {
        let testo = String::from("  1 + 2 * 3 - 4 % 5 +  /* Happy Days */  (     foo * bar )     ");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
        let testo = String::from("  -1 + 2 * 3 - -4 % 5 +  /* Happy Days */  (     -foo * bar )     ");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
        let testo = String::from("-1");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
        println!("Subtest:\n---");
        if let IResult::Done(_,r) = expr_0(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
        let testo = String::from("1");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
    }

    #[test]
    fn bool_display() {
        let testo = String::from("  a & b | c ^ /* Bad Days */ (true & false)  /* Happy Days */    ");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
        let testo = String::from(" ! true ^ true");
        println!("----------------\ntesto:{}",testo);
        match  expr(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
        let testo = String::from(" !true ");
        println!("----------------\ntesto:{}",testo);
        match  expr(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
        println!("Subtest:\n---");
        match  expr_0(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
        let testo = String::from(" true ");
        println!("----------------\ntesto:{}",testo);
        match  expr(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
    }

    #[test]
    fn complex_expr() {
        let tests = ["1 / 2",
                     "1 + x",
                     "x | y",
                     "y | x",
                     "_ / _",
                     "_ + _ - 10 / 2",
                     "1+x",
                     "(1 + x)",
                     "1+x < 10",
                     "(1+x) < 10",
                     "(1+x < 10)",
                     "((1 + x) < 10)",
                     "true|false",
                     "x|y",
                     "(x & true | y)",
                     "((1 + x) < 10) | y",
                     "((1 + x) < 10) ^ false"];
        for s in tests.iter() {
            let res = expr(&s[..].as_bytes());
            println!("Parsing: {:?}",s);
            println!("Result: {:?}",res);
            if let IResult::Done(x,_) = res {
                assert!(x.len() == 0);
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn equations() {
        let tests = ["_ = _ / _"];
        for s in tests.iter() {
            let res = equation(&s[..].as_bytes());
            println!("Parsing: {:?}",s);
            println!("Result: {:?}",res);
            if let IResult::Done(_,_) = res {
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn row_facts() {
        let tests = ["foo(1,2,3)","bar(\"red\",\"blue\",\"green\")","bam(x,1.5)"];
        for s in tests.iter() {
            let res = row_fact(&s[..].as_bytes());
            println!("Parsing: {:?}",s);
            println!("Result: {:?}",res);
            if let IResult::Done(_,_) = res {
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn tree_facts() {
        let tests = ["[a b c d]","{\"foo\" x}",
                     "[x {1 2}]",
                     "{ \"\" 1}",
                     "{ \"\" [0 0 _ _]}",
                     "{ \"\" 1 2 3}",
                     "{ \"\" 1 2 [0 0 _ _]}",
                     "{ \"\" [0 0 _ _] 2 [0 0 _ _]}",
                     "{ \"\" [0 0 _ _] -1 [_ 0 _ false]}"];
        for s in tests.iter() {
            let res = tree_fact(&s[..].as_bytes());
            println!("Parsing: {:?}",s);
            println!("Result: {:?}",res);
            if let IResult::Done(_,_) = res {
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn literals() {
        let tests = ["1","1.0","\"foo\"","\"\"","true","-3.9574767075378148"];
        for s in tests.iter() {
            let res = term(&s[..].as_bytes());
            println!("Parsing: {:?}",s);
            println!("Result: {:?}",res);
            if let IResult::Done(_,_) = res {
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn facts() {
        let tests = ["x=y;","foo(x,y);","[{1 x}];","x in [1,10];","x in (,10);","x in (,\"\");","_ = _ / _;",
                     "[0 0 _ _];",
                     "[_ 0 _ false];",
                     "{ \"\" [0 0 _ _] -1 [_ 0 _ false]};",
                     "[0.0000000000000000000000000000000000000000000000000000000000000000 { \"\" [0 0 _ _] -1 [_ 0 _ false]} false];",
                     "[_ _ [0.0000000000000000000000000000000000000000000000000000000000000000 { \"\" [0 0 _ _] -1 [_ 0 _ false]} false] 0];",
                     "[false 0 [_ _ [0.0000000000000000000000000000000000000000000000000000000000000000 { \"\" [0 0 _ _] -1 [_ 0 _ false]} false] 0] _];",
        ];
        for s in tests.iter() {
            let res = fact(&s[..].as_bytes());
            println!("Parsing: {:?}",s);
            println!("Result: {:?}",res);
            if let IResult::Done(_,_) = res {
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn relations() {
        let tests = ["same( x , y ) :- x=y  ,  foo( x ) , bar( y   );","foo(x,y):-foo(y,x);","foo(x) :- [{1 x}];","q(_) :- _ = _;","e(_) :- _ in (0,\"\");","X(_) :- _ = 0;","E(_) :- _ = ((0));","w(_) :- [_ _ _ _];","Q(_) :- _ = _ + _ ;","Q(_) :- _ = _ - _ ;","Q(_) :- _ = _ % _ ;","Q(_) :- _ = _ * _ ;","Q(_) :- _ = _ / _ ;","b(_) :- [false 0 [_ _ [0.0000000000000000000000000000000000000000000000000000000000000000 { \"\" [0 0 _ _] -1 [_ 0 _ false]} false] 0] _];"];
        for s in tests.iter() {
            let res = relation(&s[..].as_bytes());
            if let IResult::Done(_,_) = res {
            } else {
                println!("{} - {:?}",s,res);
                assert!(false);
            }
        }
    }

    #[test]
    fn roundtrip_relation() {
        fn prop(r:Relation) -> bool {
            println!("{}",r);
            println!("-----------------");
            let s = format!("{}",r);
            let res = relation(&s.as_bytes());
            if let IResult::Done(_,t) = res {
                let ss = format!("{}",t);
                let ret = s == ss;
                if !ret {
                    println!("----------Alt---------");
                    println!("Orig: {}|{:?}",r,r);
                    println!("New: {}|{:?}",t,t);
                }
                ret
            } else {
                println!("----------------------");
                println!("Orig: {}",s);
                println!("New: {:?}",res);
                false
            }
        }
        match QuickCheck::new()
            .gen(StdGen::new(thread_rng(),5))
            .tests(10000)
            .max_tests(50000)
            .quicktest(prop as fn(Relation)-> bool) {
                Ok(_) => {},
                Err(fail) => {
                    println!("Failed.");
                    // println!("Str: {}",fail.);
                    println!("Dbg: {:?}",fail);
                    assert!(false);
                }
        };
    }


    #[test]
    fn gen_expr() {
        fn prop(e:Expr) -> bool {
            let s = format!("{}",e);
            let res = expr(&s.as_bytes());
            if let IResult::Done(_,t) = res {
                let ss = format!("{}",t);
                let ret = s == ss;
                if !ret {
                    println!("----------------");
                    println!("Orig: {}",s);
                    println!("Dbg: {:?}",e);
                    println!("Round: {}",t);
                    println!("Dbg: {:?}",t);
                }
                ret
            } else {
                println!("Failed {} - {:?}",s,res);
                false
            }
        }
        QuickCheck::new()
            .gen(StdGen::new(thread_rng(),10))
            .tests(10000)
            .max_tests(50000)
            .quickcheck(prop as fn(Expr)-> bool);
    }

    #[test]
    fn roundtrip_literal() {
        fn prop(r:Literal) -> bool {
            let s = format!("{}",r);
            let res = term(&s.as_bytes());
            if let IResult::Done(_,t) = res {
                t == Term::Literal(r)
            } else {
                false
            }
        }
        QuickCheck::new()
            .gen(StdGen::new(thread_rng(),10))
            .tests(10000)
            .max_tests(50000)
            .quickcheck(prop as fn(Literal)-> bool);
    }
}
