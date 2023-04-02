use std::fmt;

use crate::literal::Literal;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub body: Box<Syntax>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Number {
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Str {
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Apply {
    pub func: Box<Syntax>,
    pub args: Vec<Syntax>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub v: String,
    pub defn: Box<Syntax>,
    pub body: Box<Syntax>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Letrec {
    pub v: String,
    pub defn: Box<Syntax>,
    pub body: Box<Syntax>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElse {
    pub cond: Box<Syntax>,
    pub consequent: Box<Syntax>,
    pub alternate: Box<Syntax>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Syntax {
    Lambda(Lambda),
    Identifier(Identifier),
    Literal(Literal),
    Apply(Apply),
    Let(Let),
    Letrec(Letrec),
    IfElse(IfElse),
}

impl fmt::Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Syntax::Lambda(Lambda { params, body }) => {
                let params = params
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<_>>();
                write!(f, "(fn ({}) => {body})", params.join(", "))
            }
            Syntax::Identifier(Identifier { name }) => {
                write!(f, "{}", name)
            }
            Syntax::Literal(literal) => {
                write!(f, "{literal}")
            }
            Syntax::Apply(Apply { func, args }) => {
                let args = args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>();
                write!(f, "{func}({})", args.join(", "))
            }
            Syntax::Let(Let { v, defn, body }) => {
                write!(f, "(let {v} = {defn} in {body})",)
            }
            Syntax::Letrec(Letrec { v, defn, body }) => {
                write!(f, "(letrec {v} = {defn} in {body})",)
            }
            Syntax::IfElse(IfElse {
                cond,
                consequent,
                alternate,
            }) => {
                write!(f, "(if {cond} then {consequent} else {alternate})",)
            }
        }
    }
}
