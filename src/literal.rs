use std::fmt;

pub type Span = std::ops::Range<usize>;

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
