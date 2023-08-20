use std::fmt;
use swc_common;
use swc_ecma_ast::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Literal {
    Number(String),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl Literal {
    pub fn get_scheme_name(&self) -> Option<&'static str> {
        match self {
            Literal::Number(_) => Some("Number"),
            Literal::String(_) => Some("String"),
            Literal::Boolean(_) => Some("Boolean"),
            Literal::Null => None,
            Literal::Undefined => None,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Null => write!(f, "null"),
            Literal::Undefined => write!(f, "undefined"),
        }
    }
}

impl From<&Literal> for swc_ecma_ast::Expr {
    fn from(literal: &Literal) -> Self {
        // TODO: use the span from the literal
        let span = swc_common::DUMMY_SP;

        let lit = match literal {
            Literal::Number(value) => Lit::Num(Number {
                span,
                value: value.parse().unwrap(),
                raw: None,
            }),
            Literal::String(value) => Lit::Str(Str {
                span,
                value: swc_atoms::JsWord::from(value.as_str()),
                raw: None,
            }),
            Literal::Boolean(value) => Lit::Bool(Bool {
                span,
                value: *value,
            }),
            Literal::Null => todo!(),
            Literal::Undefined => todo!(),
        };

        Expr::Lit(lit)
    }
}
