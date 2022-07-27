use swc_atoms::JsWord;
use swc_common::source_map::DUMMY_SP;
use swc_ecma_ast;

use std::fmt;

use crate::span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Num {
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bool {
    pub span: Span,
    pub value: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Null {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Undefined {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(Num),
    Bool(Bool),
    Str(Str),
    Null(Null),
    Undefined(Undefined),
}

impl Lit {
    pub fn bool(value: bool, span: Span) -> Self {
        Lit::Bool(Bool { value, span })
    }

    pub fn str(value: String, span: Span) -> Self {
        Lit::Str(Str { value, span })
    }

    pub fn num(value: String, span: Span) -> Self {
        Lit::Num(Num { value, span })
    }

    pub fn span(&self) -> Span {
        match &self {
            Lit::Num(n) => n.span.to_owned(),
            Lit::Bool(b) => b.span.to_owned(),
            Lit::Str(s) => s.span.to_owned(),
            Lit::Null(n) => n.span.to_owned(),
            Lit::Undefined(u) => u.span.to_owned(),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Num(n) => write!(f, "{}", n.value),
            Lit::Bool(b) => write!(f, "{}", b.value),
            Lit::Str(s) => write!(f, "\"{}\"", s.value),
            Lit::Null(_) => write!(f, "null"),
            Lit::Undefined(_) => write!(f, "undefined"),
        }
    }
}

impl From<&Lit> for swc_ecma_ast::Expr {
    fn from(lit: &Lit) -> Self {
        match lit {
            Lit::Num(n) => {
                let lit = swc_ecma_ast::Lit::Num(swc_ecma_ast::Number {
                    span: DUMMY_SP,
                    // TODO: include the parsed value in the source AST node
                    value: n.value.parse().unwrap(),
                    // Use `None` value only for transformations to avoid recalculate
                    // characters in number literal
                    raw: Some(JsWord::from(n.value.clone())),
                });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Lit::Bool(b) => {
                let lit = swc_ecma_ast::Lit::Bool(swc_ecma_ast::Bool {
                    span: DUMMY_SP,
                    value: b.value,
                });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Lit::Str(s) => {
                let lit = swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                    span: DUMMY_SP,
                    value: JsWord::from(s.value.clone()),
                    // Use `None` value only for transformations to avoid recalculate escaped
                    // characters in strings
                    raw: None,
                });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Lit::Null(_) => {
                let lit = swc_ecma_ast::Lit::Null(swc_ecma_ast::Null { span: DUMMY_SP });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Lit::Undefined(_) => swc_ecma_ast::Expr::from(swc_ecma_ast::Ident {
                span: DUMMY_SP,
                sym: JsWord::from("undefined"),
                optional: false,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boolean() {
        let t = Lit::bool(true, 0..4);
        let f = Lit::bool(false, 0..5);
        assert_eq!(format!("{}", t), "true");
        assert_eq!(format!("{}", f), "false");
        assert_eq!(t.span(), 0..4);
    }

    #[test]
    fn number() {
        let num = Lit::num(String::from("1.23"), 0..4);
        assert_eq!(format!("{}", num), "1.23");
        assert_eq!(num.span(), 0..4);
    }

    #[test]
    fn string() {
        let s = Lit::str(String::from("hello"), 0..5);
        assert_eq!(format!("{}", s), "\"hello\"");
        assert_eq!(s.span(), 0..5);
    }

    #[test]
    fn null() {
        let null = Lit::Null(Null { span: 0..4});
        assert_eq!(format!("{}", null), "null");
        assert_eq!(null.span(), 0..4);
    }

    #[test]
    fn undefined() {
        let undefined = Lit::Undefined(Undefined { span: 0..9});
        assert_eq!(format!("{}", undefined), "undefined");
        assert_eq!(undefined.span(), 0..9);
    }
}
