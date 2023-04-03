use std::fmt;

use crate::literal::Literal;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub body: Vec<Statement>,
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
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub var: String,
    pub defn: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Letrec {
    pub decls: Vec<(String, Box<Expression>)>, // (var, defn)
    pub body: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElse {
    pub cond: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Lambda(Lambda),
    Identifier(Identifier),
    Literal(Literal),
    Apply(Apply),
    Let(Let),
    Letrec(Letrec),
    IfElse(IfElse),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Lambda(Lambda { params, body }) => {
                let params = params
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<_>>();
                let body = body.iter().map(|stmt| stmt.to_string()).collect::<Vec<_>>();
                write!(f, "(fn ({}) => {})", params.join(", "), body.join(" "))
            }
            Expression::Identifier(Identifier { name }) => {
                write!(f, "{}", name)
            }
            Expression::Literal(literal) => {
                write!(f, "{literal}")
            }
            Expression::Apply(Apply { func, args }) => {
                let args = args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>();
                write!(f, "{func}({})", args.join(", "))
            }
            Expression::Let(Let { var, defn, body }) => {
                write!(f, "(let {var} = {defn} in {body})")
            }
            Expression::Letrec(Letrec { decls, body }) => {
                write!(f, "(letrec ")?;
                let decls = decls
                    .iter()
                    .map(|(var, defn)| format!("{} = {}", var, defn))
                    .collect::<Vec<_>>();
                write!(f, "{}", decls.join(" and "))?;
                write!(f, " in {body})")
            }
            Expression::IfElse(IfElse {
                cond,
                consequent,
                alternate,
            }) => {
                write!(f, "(if {cond} then {consequent} else {alternate})",)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Declaration {
    pub var: String,
    pub defn: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Declaration(Declaration),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Declaration(Declaration { var, defn }) => {
                write!(f, "let {var} = {defn}")
            }
            Statement::Expression(expr) => write!(f, "{expr}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
