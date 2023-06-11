use crate::identifier::{BindingIdent, Ident};
use crate::literal::Literal;
use crate::parser::Parser;
use crate::pattern::*;
use crate::source_location::merge_locations;
use crate::token::TokenKind;

pub fn parse_pattern(parser: &mut Parser) -> Pattern {
    let mut loc = parser.peek(0).loc;
    let kind = match parser.next().kind {
        TokenKind::Identifier(name) => PatternKind::Ident(BindingIdent {
            name,
            loc: loc.clone(),
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
            while parser.peek(0).kind != TokenKind::RightBracket {
                match &parser.peek(0).kind {
                    TokenKind::DotDotDot => {
                        if has_rest {
                            panic!("only one rest pattern is allowed per object pattern");
                        }
                        parser.next();
                        elems.push(Some(TuplePatElem {
                            pattern: parse_pattern(parser),
                            init: None,
                        }));
                        has_rest = true;
                    }
                    _ => {
                        elems.push(Some(TuplePatElem {
                            pattern: parse_pattern(parser),
                            init: None,
                        }));
                    }
                }

                // TODO: don't allow commas after rest pattern
                if parser.peek(0).kind == TokenKind::Comma {
                    parser.next();
                } else {
                    break;
                }
            }

            loc = merge_locations(&loc, &parser.peek(0).loc);
            assert_eq!(parser.next().kind, TokenKind::RightBracket);

            PatternKind::Tuple(TuplePat {
                elems,
                optional: false,
            })
        }
        TokenKind::LeftBrace => {
            let mut props: Vec<ObjectPatProp> = vec![];
            let mut has_rest = false;

            while parser.peek(0).kind != TokenKind::RightBrace {
                let first = parser.peek(0);
                match &parser.next().kind {
                    TokenKind::Identifier(name) => {
                        if parser.peek(0).kind == TokenKind::Colon {
                            parser.next();

                            let pattern = parse_pattern(parser);

                            // TODO: handle `var` and `mut` modifiers
                            props.push(ObjectPatProp::KeyValue(KeyValuePatProp {
                                loc: merge_locations(&first.loc, &pattern.loc),
                                key: Ident {
                                    name: name.clone(),
                                    loc: first.loc,
                                },
                                value: Box::new(pattern),
                                init: None,
                            }));
                        } else {
                            // TODO: handle `var` and `mut` modifiers
                            props.push(ObjectPatProp::Shorthand(ShorthandPatProp {
                                loc: first.loc.clone(),
                                ident: BindingIdent {
                                    name: name.clone(),
                                    loc: first.loc,
                                    mutable: false,
                                },
                                init: None,
                            }))
                        }

                        if parser.peek(0).kind == TokenKind::Comma {
                            parser.next();
                        }
                    }
                    TokenKind::DotDotDot => {
                        if has_rest {
                            panic!("only one rest pattern is allowed per object pattern");
                        }
                        props.push(ObjectPatProp::Rest(RestPat {
                            arg: Box::new(parse_pattern(parser)),
                        }));
                        has_rest = true;
                    }
                    _ => panic!("expected identifier or rest pattern"),
                }
            }

            PatternKind::Object(ObjectPat {
                props,
                optional: false,
            })
        }
        TokenKind::DotDotDot => PatternKind::Rest(RestPat {
            arg: Box::new(parse_pattern(parser)),
        }),
        token => {
            panic!("expected token to start type annotation, found {:?}", token)
        }
    };

    Pattern { loc, kind }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    pub fn parse(input: &str) -> Pattern {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        parse_pattern(&mut parser)
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
}
