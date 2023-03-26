use derive_visitor::{Drive, DriveMut};
use std::fmt;
use swc_atoms::{Atom, JsWord};
use swc_common::{self, BytePos, SyntaxContext};
use swc_ecma_ast;

use crate::types::{TLit, Type, TypeKind};
use crate::values::common::{SourceLocation, Span};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Num {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub value: String,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Bool {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub value: bool,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Str {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub value: String,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum Lit {
    // We store all of the values as strings since f64 doesn't
    // support the Eq trait because NaN and 0.1 + 0.2 != 0.3.
    Num(Num),
    Bool(Bool),
    Str(Str),
}

impl Lit {
    pub fn bool(value: bool, span: Span, loc: SourceLocation) -> Self {
        Lit::Bool(Bool { value, span, loc })
    }

    pub fn str(value: String, span: Span, loc: SourceLocation) -> Self {
        Lit::Str(Str { value, span, loc })
    }

    pub fn num(value: String, span: Span, loc: SourceLocation) -> Self {
        Lit::Num(Num { value, span, loc })
    }

    pub fn span(&self) -> Span {
        match &self {
            Lit::Num(n) => n.span.to_owned(),
            Lit::Bool(b) => b.span.to_owned(),
            Lit::Str(s) => s.span.to_owned(),
        }
    }

    pub fn loc(&self) -> SourceLocation {
        match &self {
            Lit::Num(n) => n.loc.to_owned(),
            Lit::Bool(b) => b.loc.to_owned(),
            Lit::Str(s) => s.loc.to_owned(),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Num(n) => write!(f, "{}", n.value),
            Lit::Bool(b) => write!(f, "{}", b.value),
            Lit::Str(s) => write!(f, "\"{}\"", s.value),
        }
    }
}

impl From<&Lit> for swc_ecma_ast::Expr {
    fn from(lit: &Lit) -> Self {
        match lit {
            Lit::Num(n) => {
                let span = swc_common::Span {
                    lo: BytePos(n.span.start as u32 + 1),
                    hi: BytePos(n.span.end as u32 + 1),
                    ctxt: SyntaxContext::empty(),
                };

                let lit = swc_ecma_ast::Lit::Num(swc_ecma_ast::Number {
                    span,
                    // TODO: include the parsed value in the source AST node
                    value: n.value.parse().unwrap(),
                    // Use `None` value only for transformations to avoid recalculate
                    // characters in number literal
                    raw: Some(Atom::new(n.value.clone())),
                });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Lit::Bool(b) => {
                let span = swc_common::Span {
                    lo: BytePos(b.span.start as u32 + 1),
                    hi: BytePos(b.span.end as u32 + 1),
                    ctxt: SyntaxContext::empty(),
                };

                let lit = swc_ecma_ast::Lit::Bool(swc_ecma_ast::Bool {
                    span,
                    value: b.value,
                });
                swc_ecma_ast::Expr::Lit(lit)
            }
            Lit::Str(s) => {
                let span = swc_common::Span {
                    lo: BytePos(s.span.start as u32 + 1),
                    hi: BytePos(s.span.end as u32 + 1),
                    ctxt: SyntaxContext::empty(),
                };

                let lit = swc_ecma_ast::Lit::Str(swc_ecma_ast::Str {
                    span,
                    value: JsWord::from(s.value.clone()),
                    // Use `None` value only for transformations to avoid recalculate escaped
                    // characters in strings
                    raw: None,
                });
                swc_ecma_ast::Expr::Lit(lit)
            }
        }
    }
}

// impl From<Lit> for Type {
//     fn from(lit: Lit) -> Self {
//         Type {
//             kind: TypeKind::Lit(match lit {
//                 Lit::Num(n) => TLit::Num(n.value),
//                 Lit::Bool(b) => TLit::Bool(b.value),
//                 Lit::Str(s) => TLit::Str(s.value),
//             }),
//             provenance: None,
//             mutable: false,
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::values::common::DUMMY_LOC;

    #[test]
    fn boolean() {
        let t = Lit::bool(true, 0..4, DUMMY_LOC);
        let f = Lit::bool(false, 0..5, DUMMY_LOC);
        assert_eq!(format!("{}", t), "true");
        assert_eq!(format!("{}", f), "false");
        assert_eq!(t.span(), 0..4);
    }

    #[test]
    fn number() {
        let num = Lit::num(String::from("1.23"), 0..4, DUMMY_LOC);
        assert_eq!(format!("{}", num), "1.23");
        assert_eq!(num.span(), 0..4);
    }

    #[test]
    fn string() {
        let s = Lit::str(String::from("hello"), 0..5, DUMMY_LOC);
        assert_eq!(format!("{}", s), "\"hello\"");
        assert_eq!(s.span(), 0..5);
    }
}
