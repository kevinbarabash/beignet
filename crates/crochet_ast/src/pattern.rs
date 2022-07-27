use crate::expr::Expr;
use crate::ident::Ident;
use crate::span::Span;
use crate::types::TypeAnn;
use crate::Lit;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Ident(BindingIdent),
    Rest(RestPat),
    Object(ObjectPat),
    Array(ArrayPat),
    Lit(LitPat),
    Is(IsPat),
    // This can't be used at the top level similar to rest
    // Assign(AssignPat),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Ident(ident) => ident.span.to_owned(),
            Pattern::Rest(rest) => rest.span.to_owned(),
            Pattern::Object(obj) => obj.span.to_owned(),
            Pattern::Array(array) => array.span.to_owned(),
            Pattern::Lit(lit) => lit.span.to_owned(),
            Pattern::Is(is) => is.span.to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingIdent {
    pub span: Span,
    pub id: Ident,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LitPat {
    pub span: Span,
    pub lit: Lit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IsPat {
    pub span: Span,
    pub id: Ident,
    pub is_id: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestPat {
    pub span: Span,
    pub arg: Box<Pattern>,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayPat {
    pub span: Span,
    pub elems: Vec<Option<Pattern>>,
    pub optional: bool,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectPat {
    pub span: Span,
    pub props: Vec<ObjectPatProp>,
    pub optional: bool,
    pub type_ann: Option<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectPatProp {
    KeyValue(KeyValuePatProp),
    Rest(RestPat),
    Assign(AssignPatProp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValuePatProp {
    pub key: Ident,
    pub value: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignPatProp {
    pub span: Span,
    pub key: Ident,
    pub value: Option<Box<Expr>>,
}

pub fn is_refutable(pat: &Pattern) -> bool {
    match pat {
        Pattern::Ident(_) => false,
        Pattern::Rest(_) => false,
        Pattern::Object(ObjectPat { props, .. }) => props.iter().any(|prop| match prop {
            ObjectPatProp::KeyValue(KeyValuePatProp { value, .. }) => is_refutable(value),
            ObjectPatProp::Rest(RestPat { arg, .. }) => is_refutable(arg),
            ObjectPatProp::Assign(_) => false, // corresponds to {x = 5}
        }),
        Pattern::Array(ArrayPat { elems, .. }) => {
            elems.iter().any(|elem| {
                match elem {
                    Some(elem) => is_refutable(elem),
                    // FixMe: this should probably be true since it's equivalent
                    // to having an element with the value `undefined`
                    None => false,
                }
            })
        }
        Pattern::Lit(_) => true,
        Pattern::Is(_) => true,
    }
}

pub fn is_irrefutable(pat: &Pattern) -> bool {
    !is_refutable(pat)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ident(name: &str) -> Ident {
        Ident {
            span: 0..0,
            name: name.to_owned(),
        }
    }

    fn ident_pattern(name: &str) -> Pattern {
        Pattern::Ident(BindingIdent {
            span: 0..0,
            id: ident(name),
            type_ann: None,
        })
    }

    fn num_lit_pat(value: &str) -> Pattern {
        Pattern::Lit(LitPat {
            span: 0..0,
            lit: Lit::num(String::from(value), 0..0),
        })
    }

    #[test]
    fn ident_is_irrefutable() {
        let ident = ident_pattern("foo");
        assert!(is_irrefutable(&ident));
    }

    #[test]
    fn rest_is_irrefutable() {
        let ident = ident_pattern("foo");
        let rest = Pattern::Rest(RestPat {
            span: 0..0,
            arg: Box::from(ident),
            type_ann: None,
        });
        assert!(is_irrefutable(&rest));
    }

    #[test]
    fn obj_with_all_irrefutable_props_is_irrefutable() {
        let obj = Pattern::Object(ObjectPat {
            props: vec![ObjectPatProp::KeyValue(KeyValuePatProp {
                key: ident("foo"),
                value: Box::from(ident_pattern("foo")),
            })],
            optional: false,
            type_ann: None,
            span: 0..0,
        });
        assert!(is_irrefutable(&obj));
    }

    #[test]
    fn obj_with_one_refutable_prop_is_refutable() {
        let obj = Pattern::Object(ObjectPat {
            props: vec![
                ObjectPatProp::KeyValue(KeyValuePatProp {
                    key: ident("foo"),
                    value: Box::from(ident_pattern("foo")),
                }),
                ObjectPatProp::KeyValue(KeyValuePatProp {
                    key: ident("bar"),
                    value: Box::from(num_lit_pat("5")),
                }),
            ],
            optional: false,
            type_ann: None,
            span: 0..0,
        });
        assert!(is_refutable(&obj));
    }

    #[test]
    fn array_with_all_irrefutable_elements_is_irrefutable() {
        let array = Pattern::Array(ArrayPat {
            elems: vec![Some(ident_pattern("foo"))],
            optional: false,
            type_ann: None,
            span: 0..0,
        });
        assert!(is_irrefutable(&array));
    }

    #[test]
    fn array_with_one_refutable_prop_is_refutable() {
        let array = Pattern::Array(ArrayPat {
            elems: vec![Some(ident_pattern("foo")), Some(num_lit_pat("5"))],
            optional: false,
            type_ann: None,
            span: 0..0,
        });
        assert!(is_refutable(&array));
    }

    #[test]
    fn literal_pattern_is_refutable() {
        assert!(is_refutable(&num_lit_pat("5")));
    }

    #[test]
    fn is_is_refutable() {
        let is_pat = Pattern::Is(IsPat {
            span: 0..0,
            id: ident("foo"),
            is_id: ident("string"),
        });
        assert!(is_refutable(&is_pat));
    }
}
