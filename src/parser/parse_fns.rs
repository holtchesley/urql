use nom::*;
use super::*;




    use std::str::{from_utf8,FromStr};

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

    named!(integer<i64>,
           map_res!(
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
           )
    );


    named!(float_nan,tag!("NaN"));
    named!(float_inf,do_parse!(opt!(tag!("-")) >> a:tag!("inf") >> (a)));
    named!(float_e,do_parse!(is_a!("eE") >> opt!(tag!("-")) >> a:digit >> (a)));
    named!(float_happy,do_parse!(
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
                                                    alt!(float_nan | float_inf | float_happy)
                                                ),
                                                str::from_utf8
                                            ),
                                            FromStr::from_str
                                        )
                      ),
                      toss)
    );


    named!(boolean<bool>,
           delimited!(toss,
                      map_res!(
                          map_res!(
                              alt!(tag!("true") | tag!("false")),
                              str::from_utf8
                          ),
                          FromStr::from_str
                      ),
                      toss
           )
    );

    named!(variable<Variable<'a> >,
           delimited!(toss,
                      map_res!(
                          map_res!(
                              recognize!(
                                  do_parse!(
                                      alt!(alpha | is_a!("_")) >>
                                          alt!(recognize!(eof!()) | recognize!(opt!(alt!(alphanumeric)))) >>
                                          (())
                                  )
                              ),
                              str::from_utf8),
                          Variable::from_str
                      )
                      ,toss
           )
    );



use std::str;


named!(number<Number>,alt_complete!(map!(float_alts, Number::Float) | map!(integer,Number::Int)));


named!(binary_arith_operator_1<BinaryOperator>,
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
named!(binary_arith_operator_2<BinaryOperator>,
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
named!(binary_arith_operator_3<BinaryOperator>,
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
named!(arith_parens<Box<ArithExpr<'a> > >,
       delimited!(toss,
                  map!(delimited!(tag!("("),arith_expr,tag!(")")),Box::new),
                  toss
       )
);

fn arith_left_fold<'a>(initial: ArithExpr<'a>,remainder: Vec<(BinaryOperator,ArithExpr<'a>)>) -> ArithExpr<'a> {
    remainder.into_iter().fold(initial,|left,pair| {
        let (op,right) = pair;
        ArithExpr::BinaryResult((Box::new(left),op,Box::new(right)))
    })
}

named!(arith_expr_bottom< ArithExpr<'a> >,
       alt!(
           map!(number,|n| {ArithExpr::Value(n)})
               | map!(arith_parens,ArithExpr::Paren)
               | map!(variable,ArithExpr::Variable)
       )
);
named!(unary_arith_operator<UnaryArithOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          tag!("-"),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(arith_expr_0< ArithExpr<'a> >,
       do_parse!(
           op: opt!(unary_arith_operator) >>
           exp : arith_expr_bottom >>
               (if let Some(o) = op {ArithExpr::UnaryResult((o,Box::new(exp)))} else {exp})
       )
);
named!(arith_expr_1<ArithExpr<'a> >,
       do_parse!(
           left: arith_expr_0 >>
               remainder: many0!(
                   do_parse!(op: binary_arith_operator_1 >>
                             right:arith_expr_0 >>
                             (op,right)
                   )) >>
               (arith_left_fold(left,remainder))
       )
);
named!(arith_expr_2<ArithExpr<'a> >,
       do_parse!(
           left: arith_expr_1 >>
               remainder: many0!(
                   do_parse!(op: binary_arith_operator_2 >>
                             right:arith_expr_1 >>
                             (op,right)
                   )) >>
               (arith_left_fold(left,remainder))
       )
);
named!(arith_expr<ArithExpr<'a> >,
       do_parse!(
           left: arith_expr_2 >>
               remainder: many0!(
                   do_parse!(op: binary_arith_operator_3 >>
                             right:arith_expr_2 >>
                             (op,right)
                   )) >>
               (arith_left_fold(left,remainder))
       )
);



named!(binary_bool_operator_1<BinaryBoolOperator>,
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
named!(binary_bool_operator_2<BinaryBoolOperator>,
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
named!(binary_bool_operator_3<BinaryBoolOperator>,
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

named!(bool_parens<Box<BoolExpr<'a> > >,
       delimited!(toss,
                  map!(delimited!(tag!("("),bool_expr,tag!(")")),Box::new),
                  toss
       )
);


fn bool_left_fold<'a>(initial: BoolExpr<'a>,remainder: Vec<(BinaryBoolOperator,BoolExpr<'a>)>) -> BoolExpr<'a> {
    remainder.into_iter().fold(initial,|left,pair| {
        let (op,right) = pair;
        BoolExpr::BinaryResult((Box::new(left),op,Box::new(right)))
    })
}


named!(bool_expr_bottom< BoolExpr<'a> >,
       alt!(
           map!(boolean,|n| {BoolExpr::Value(n)})
               | map!(bool_parens,BoolExpr::Paren)
               | map!(variable,BoolExpr::Variable)
               | map!(num_to_bool_expr,BoolExpr::Comparison)
       )
);
named!(unary_bool_operator<UnaryBoolOperator>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          tag!("!"),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);
named!(bool_expr_0< BoolExpr<'a> >,
       do_parse!(
           op: opt!(unary_bool_operator) >>
           exp : bool_expr_bottom >>
               (if let Some(o) = op {BoolExpr::UnaryResult((o,Box::new(exp)))} else {exp})
       )
);
named!(bool_expr_1<BoolExpr<'a> >,
       do_parse!(
           left: bool_expr_0 >>
               remainder: many0!(
                   do_parse!(op: binary_bool_operator_1 >>
                             right:bool_expr_0 >>
                             (op,right)
                   )) >>
               (bool_left_fold(left,remainder))
       )
);
named!(bool_expr_2<BoolExpr<'a> >,
       do_parse!(
           left: bool_expr_1 >>
               remainder: many0!(
                   do_parse!(op: binary_bool_operator_2 >>
                             right:bool_expr_1 >>
                             (op,right)
                   )) >>
               (bool_left_fold(left,remainder))
       )
);
named!(bool_expr<BoolExpr<'a> >,
       do_parse!(
           left: bool_expr_2 >>
               remainder: many0!(
                   do_parse!(op: binary_bool_operator_3 >>
                             right:bool_expr_2 >>
                             (op,right)
                   )) >>
               (bool_left_fold(left,remainder))
       )
);


named!(num_to_bool_operator<NumToBoolMorph>,
       delimited!(toss,
                  map_res!(
                      map_res!(
                          alt!( tag!("<") | tag!("<=")
                                | tag!("=") | tag!("!=")
                                | tag!(">") | tag!(">=")),
                          str::from_utf8),
                      FromStr::from_str
                  ),
                  toss
       )
);

named!(num_to_bool_expr<NumToBoolExpr<'a> >,
       do_parse!(
           l: arith_expr >>
               op: num_to_bool_operator >>
               r: arith_expr >>
               (NumToBoolExpr::BinaryResult((Box::new(l),op,Box::new(r))))
       )
);





#[cfg(test)]
mod tests {
    use std::str;
    use std::string::*;
    use super::*;
    use quickcheck::*;


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
                assert!(false);
            }
            if let IResult::Done(_,_) = number(&s[..].as_bytes()) {
                assert!(true);
            } else {
                assert!(false);
            }
            if let IResult::Done(_,_) = arith_expr_bottom(&s[..].as_bytes()) {
                assert!(true);
            } else {
                assert!(false);
            }
            if let IResult::Done(_,_) = arith_expr_0(&s[..].as_bytes()) {
                assert!(true);
            } else {
                assert!(false);
            }
            if let IResult::Done(_,_) = arith_expr_1(&s[..].as_bytes()) {
                assert!(true);
            } else {
                assert!(false);
            }
            if let IResult::Done(_,_) = arith_expr_2(&s[..].as_bytes()) {
                assert!(true);
            } else {
                assert!(false);
            }
            if let IResult::Done(_,_) = arith_expr(&s[..].as_bytes()) {
                assert!(true);
            } else {
                assert!(false);
            }

        }


    }

    #[test]
    fn arith_display() {
        let testo = String::from("  1 + 2 * 3 - 4 % 5 +  /* Happy Days */  (     foo * bar )     ");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = arith_expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }

        let testo = String::from("  -1 + 2 * 3 - -4 % 5 +  /* Happy Days */  (     -foo * bar )     ");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = arith_expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }

        let testo = String::from("-1");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = arith_expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
        println!("Subtest:\n---");
        if let IResult::Done(_,r) = arith_expr_0(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }

        let testo = String::from("1");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = arith_expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }
    }

    #[test]
    fn bool_display() {

        let testo = String::from("  a & b | c ^ /* Bad Days */ (true & false)  /* Happy Days */    ");
        println!("----------------\ntesto:{}",testo);
        if let IResult::Done(_,r) = bool_expr(&testo[..].as_bytes()) {
            println!("{}",r);
        } else {
            println!("Failed to parse.");
        }

        let testo = String::from(" ! true ^ true");
        println!("----------------\ntesto:{}",testo);
        match  bool_expr(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }

        let testo = String::from(" !true ");
        println!("----------------\ntesto:{}",testo);
        match  bool_expr(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
        println!("Subtest:\n---");
        match  bool_expr_0(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
        


        let testo = String::from(" true ");
        println!("----------------\ntesto:{}",testo);
        match  bool_expr(&testo[..].as_bytes()) {
            IResult::Done(_,r) => println!("{}",r),
            o => println!("Failed with: {:?}",o)
        }
    }

    #[test]
    fn complex_expr() {
        let testo = b"1 + x < 10 ^ y";
        if let IResult::Done(_,r) = bool_expr(&testo[..]) {
            println!("{}",r);
            assert!(true);
        } else {
            assert!(false);
        }

        let testo = b"1 + x < 10 ^ false";
        if let IResult::Done(_,r) = bool_expr(&testo[..]) {
            println!("{:?}",r);
            assert!(true);
        } else {
            assert!(false);
        }

    }
}
