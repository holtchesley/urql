// use chrono::*;
// use nom::*;

mod parse_fns;

/// This contains the various structs used to represent the AST


#[derive(Debug,Hash,Eq,PartialEq,Ord,PartialOrd)]
pub struct Variable<'a> {
    name:&'a str
}

impl<'a> Variable<'a> {
    fn from_str<'b:'a>(input:&'b str) -> Result<Variable<'a>,()> {
        if input == "true" || input == "false" || input == "find" {
            return Err(())
        }
        Ok(Variable{name:input})
    }
}

use std::fmt;

impl<'a> fmt::Display for Variable<'a> {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        write!(format, "{}", self.name)
    }
}



/// Expressions -
/// Expressions need to
/// 1) be able to represent an evaluable function
/// 2) Propagate the optimal type up to the top for evaluation
/// 3) Propogate a list of Vars up to the top.

#[derive(Debug)]
pub enum ParseErrors {
    NumericParseFailed,
    UTF8ConversionError
}



#[derive(Debug)]
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

#[derive(Debug)]
pub enum BinaryOperator {
    // metic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
}

#[derive(Debug)]
pub enum BinaryBoolOperator {
    And,
    Or,
    Xor
}


#[derive(Debug)]
pub enum NumToBoolMorph {
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
            &BinaryOperator::Add      => write!(format, "+"),
            &BinaryOperator::Subtract => write!(format, "-"),
            &BinaryOperator::Multiply => write!(format, "*"),
            &BinaryOperator::Divide   => write!(format, "/"),
            &BinaryOperator::Modulus  => write!(format, "%"),
        }
    }
}


impl fmt::Display for BinaryBoolOperator {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &BinaryBoolOperator::And      => write!(format, "&"),
            &BinaryBoolOperator::Or       => write!(format, "|"),
            &BinaryBoolOperator::Xor      => write!(format, "^"),
        }
    }
}


impl fmt::Display for NumToBoolMorph {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &NumToBoolMorph::LessThan       => write!(format, "<"),
            &NumToBoolMorph::LessThanEq     => write!(format, "<="),
            &NumToBoolMorph::Eq             => write!(format, "=="),
            &NumToBoolMorph::NotEq          => write!(format, "!="),
            &NumToBoolMorph::GreaterThanEq  => write!(format, ">="),
            &NumToBoolMorph::GreaterThan    => write!(format, ">"),
        }
    }
}




use std::str::FromStr;

impl FromStr for BinaryOperator {
    type Err = ();
    fn from_str(s:&str) -> Result<BinaryOperator,Self::Err> {
        match s {
            "+" => Ok(BinaryOperator::Add),
            "-" => Ok(BinaryOperator::Subtract),
            "*" => Ok(BinaryOperator::Multiply),
            "/" => Ok(BinaryOperator::Divide),
            "%" => Ok(BinaryOperator::Modulus),
            _ => Err(())
        }
    }
}

impl FromStr for BinaryBoolOperator {
    type Err = ();
    fn from_str(s:&str) -> Result<BinaryBoolOperator,Self::Err> {
        match s {
            "&" => Ok(BinaryBoolOperator::And),
            "|" => Ok(BinaryBoolOperator::Or),
            "^" => Ok(BinaryBoolOperator::Xor),
            _ => Err(())
        }
    }
}

impl FromStr for NumToBoolMorph {
    type Err = ();
    fn from_str(s:&str) -> Result<NumToBoolMorph,Self::Err> {
        match s {
            "<"  => Ok(NumToBoolMorph::LessThan),
            "<=" => Ok(NumToBoolMorph::LessThanEq),
            "==" => Ok(NumToBoolMorph::Eq),
            "!=" => Ok(NumToBoolMorph::NotEq),
            ">=" => Ok(NumToBoolMorph::GreaterThanEq),
            ">"  => Ok(NumToBoolMorph::GreaterThan),

            _ => Err(())
        }
    }
}



#[derive(Debug)]
pub enum UnaryArithOperator {
    Negate
}

impl FromStr for UnaryArithOperator {
    type Err = ();
    fn from_str(s:&str) -> Result<UnaryArithOperator,Self::Err> {
        match s.chars().next() {
            Some('-') => Ok(UnaryArithOperator::Negate),
            _ => Err(())
        }
    }
}

impl fmt::Display for UnaryArithOperator {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &UnaryArithOperator::Negate    => write!(format, "-")
        }
    }
}


#[derive(Debug)]
pub enum UnaryBoolOperator {
    Negate
}

impl FromStr for UnaryBoolOperator {
    type Err = ();
    fn from_str(s:&str) -> Result<UnaryBoolOperator,Self::Err> {
        match s.chars().next() {
            Some('!') => Ok(UnaryBoolOperator::Negate),
            _ => Err(())
        }
    }
}

impl fmt::Display for UnaryBoolOperator {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &UnaryBoolOperator::Negate    => write!(format, "!")
        }
    }
}



#[derive(Debug)]
pub enum ArithExpr<'a> {
    Value(Number),
    Variable(Variable<'a>),
    Paren(Box<ArithExpr<'a>>),
    BinaryResult((Box<ArithExpr<'a>>,BinaryOperator,Box<ArithExpr<'a>>)),
    UnaryResult((UnaryArithOperator,Box<ArithExpr<'a>>))
}

impl<'a> fmt::Display for ArithExpr<'a> {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ArithExpr::Value(ref n)                      => write!(format, "{}",n),
            &ArithExpr::Variable(ref v)                   => write!(format, "{}",v),
            &ArithExpr::Paren(ref a)                      => write!(format, "{}",a),
            &ArithExpr::BinaryResult((ref l,ref o,ref r)) => write!(format, "({} {} {})",l ,o, r),
            &ArithExpr::UnaryResult((ref o,ref a))        => write!(format, "{}{}",o,a)
        }
    }
}





#[derive(Debug)]
pub enum BoolExpr<'a> {
    Value(bool),
    Variable(Variable<'a>),
    Paren(Box<BoolExpr<'a>>),
    BinaryResult((Box<BoolExpr<'a>>,BinaryBoolOperator,Box<BoolExpr<'a>>)),
    UnaryResult((UnaryBoolOperator,Box<BoolExpr<'a>>)),
    Comparison(NumToBoolExpr<'a>)
}

impl<'a> fmt::Display for BoolExpr<'a> {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &BoolExpr::Value(ref n)                      => write!(format, "{}",n),
            &BoolExpr::Variable(ref v)                   => write!(format, "{}",v),
            &BoolExpr::Paren(ref a)                      => write!(format, "{}",a),
            &BoolExpr::BinaryResult((ref l,ref o,ref r)) => write!(format, "({} {} {})",l ,o, r),
            &BoolExpr::UnaryResult((ref o,ref a))        => write!(format, "{}{}",o,a),
            &BoolExpr::Comparison(ref c)                 => write!(format, "{}",c),
        }
    }
}


#[derive(Debug)]
pub enum NumToBoolExpr<'a> {
    BinaryResult((Box<ArithExpr<'a>>,NumToBoolMorph,Box<ArithExpr<'a>>)),
}

impl<'a> fmt::Display for NumToBoolExpr<'a> {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &NumToBoolExpr::BinaryResult((ref l,ref o,ref r)) => write!(format, "({} {} {})",l ,o, r),
        }
    }
}



















