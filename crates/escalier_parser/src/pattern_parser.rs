use crate::identifier::{BindingIdent, Ident};
use crate::literal::Literal;
use crate::parser::Parser;
use crate::pattern::*;
use crate::source_location::*;
use crate::token::*;

impl<'a> Parser<'a> {
    pub fn parse_pattern(&mut self) -> Pattern {
        let mut span = self.peek().unwrap_or(&EOF).span;
        let kind = match self.next().unwrap_or(EOF.clone()).kind {
            TokenKind::Identifier(name) => PatternKind::Ident(BindingIdent {
                name,
                span,
                mutable: false,
            }),
            TokenKind::StrLit(value) => PatternKind::Lit(LitPat {
                lit: Literal::String(value),
            }),
            TokenKind::NumLit(value) => PatternKind::Lit(LitPat {
                lit: Literal::Number(value),
            }),
            TokenKind::BoolLit(value) => PatternKind::Lit(LitPat {
                lit: Literal::Boolean(value),
            }),
            TokenKind::Null => PatternKind::Lit(LitPat { lit: Literal::Null }),
            TokenKind::Undefined => PatternKind::Lit(LitPat {
                lit: Literal::Undefined,
            }),
            TokenKind::LeftBracket => {
                let mut elems: Vec<Option<TuplePatElem>> = vec![];
                let mut has_rest = false;
                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBracket {
                    match &self.peek().unwrap_or(&EOF).kind {
                        TokenKind::DotDotDot => {
                            if has_rest {
                                panic!("only one rest pattern is allowed per object pattern");
                            }
                            elems.push(Some(TuplePatElem {
                                pattern: self.parse_pattern(),
                                init: None,
                            }));
                            has_rest = true;
                        }
                        _ => {
                            elems.push(Some(TuplePatElem {
                                pattern: self.parse_pattern(),
                                init: None,
                            }));
                        }
                    }

                    // TODO: don't allow commas after rest pattern
                    if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }

                span = merge_spans(&span, &self.peek().unwrap_or(&EOF).span);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBracket
                );

                PatternKind::Tuple(TuplePat {
                    elems,
                    optional: false,
                })
            }
            TokenKind::LeftBrace => {
                let mut props: Vec<ObjectPatProp> = vec![];
                let mut has_rest = false;

                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                    let first = self.peek().unwrap_or(&EOF);
                    let first_span = first.span;
                    match &self.next().unwrap_or(EOF.clone()).kind {
                        TokenKind::Identifier(name) => {
                            if self.peek().unwrap_or(&EOF).kind == TokenKind::Colon {
                                self.next();

                                let pattern = self.parse_pattern();

                                // TODO: handle `var` and `mut` modifiers
                                props.push(ObjectPatProp::KeyValue(KeyValuePatProp {
                                    span: merge_spans(&first_span, &pattern.span),
                                    key: Ident {
                                        name: name.clone(),
                                        span: first_span,
                                    },
                                    value: Box::new(pattern),
                                    init: None,
                                }));
                            } else {
                                // TODO: handle `var` and `mut` modifiers
                                props.push(ObjectPatProp::Shorthand(ShorthandPatProp {
                                    span: first_span,
                                    ident: BindingIdent {
                                        name: name.clone(),
                                        span: first_span,
                                        mutable: false,
                                    },
                                    init: None,
                                }))
                            }

                            if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                                self.next();
                            }
                        }
                        TokenKind::DotDotDot => {
                            if has_rest {
                                panic!("only one rest pattern is allowed per object pattern");
                            }
                            props.push(ObjectPatProp::Rest(RestPat {
                                arg: Box::new(self.parse_pattern()),
                            }));
                            has_rest = true;
                        }
                        _ => panic!("expected identifier or rest pattern"),
                    }
                }

                span = merge_spans(&span, &self.peek().unwrap_or(&EOF).span);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBrace
                );

                PatternKind::Object(ObjectPat {
                    props,
                    optional: false,
                })
            }
            // This code can be called when parsing rest patterns in function params.
            TokenKind::DotDotDot => PatternKind::Rest(RestPat {
                arg: Box::new(self.parse_pattern()),
            }),
            TokenKind::Underscore => PatternKind::Wildcard,
            token => {
                panic!("expected token to start type annotation, found {:?}", token)
            }
        };

        Pattern { span, kind }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    pub fn parse(input: &str) -> Pattern {
        let mut parser = Parser::new(input);
        parser.parse_pattern()
    }

    #[test]
    fn parse_literal_patterns() {
        insta::assert_debug_snapshot!(parse("123"));
        insta::assert_debug_snapshot!(parse("true"));
        insta::assert_debug_snapshot!(parse("false"));
        insta::assert_debug_snapshot!(parse("null"));
        insta::assert_debug_snapshot!(parse("undefined"));
        insta::assert_debug_snapshot!(parse(r#""hello""#));
    }

    #[test]
    fn parse_tuple_patterns() {
        insta::assert_debug_snapshot!(parse("[a, b, c]"));
        insta::assert_debug_snapshot!(parse("[a, b, ...c]"));
    }

    #[test]
    #[should_panic]
    fn parse_tuple_patterns_multiple_rest() {
        insta::assert_debug_snapshot!(parse("[...a, ...b, ...c]"));
    }

    #[test]
    fn parse_object_patterns() {
        insta::assert_debug_snapshot!(parse("{x, y, z}"));
        insta::assert_debug_snapshot!(parse("{x, y, ...z}"));
        insta::assert_debug_snapshot!(parse("{x: a, y: b, z: c}"));
        insta::assert_debug_snapshot!(parse("{x: {y: {z}}}"));
    }

    #[test]
    #[should_panic]
    fn parse_object_patterns_multiple_rest() {
        insta::assert_debug_snapshot!(parse("{...x, ...y, ...z}"));
    }

    #[test]
    fn parse_wildcard() {
        insta::assert_debug_snapshot!(parse("_"));
    }

    #[test]
    fn parse_rest() {
        insta::assert_debug_snapshot!(parse("...rest"));
    }

    #[test]
    fn parse_mixed_patterns() {
        insta::assert_debug_snapshot!(parse(r#"{type: "foo", bar: _, values: [head, ...tail]}"#));
    }
}
