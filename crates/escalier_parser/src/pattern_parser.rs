use std::iter::Peekable;

use crate::identifier::{BindingIdent, Ident};
use crate::lexer::Lexer;
use crate::literal::Literal;
use crate::pattern::*;
use crate::source_location::{merge_locations, Position, SourceLocation};
use crate::token::Token;
use crate::token::TokenKind;

const EOF: Token = Token {
    kind: TokenKind::Eof,
    loc: SourceLocation {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    },
};

pub fn parse_pattern(lexer: &mut Peekable<Lexer>) -> Pattern {
    let mut loc = lexer.peek().unwrap_or(&EOF).loc.clone();
    let kind = match lexer.next().unwrap_or(EOF.clone()).kind {
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
            while lexer.peek().unwrap_or(&EOF).kind != TokenKind::RightBracket {
                match &lexer.peek().unwrap_or(&EOF).kind {
                    TokenKind::DotDotDot => {
                        if has_rest {
                            panic!("only one rest pattern is allowed per object pattern");
                        }
                        elems.push(Some(TuplePatElem {
                            pattern: parse_pattern(lexer),
                            init: None,
                        }));
                        has_rest = true;
                    }
                    _ => {
                        elems.push(Some(TuplePatElem {
                            pattern: parse_pattern(lexer),
                            init: None,
                        }));
                    }
                }

                // TODO: don't allow commas after rest pattern
                if lexer.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                    lexer.next();
                } else {
                    break;
                }
            }

            loc = merge_locations(&loc, &lexer.peek().unwrap_or(&EOF).loc);
            assert_eq!(
                lexer.next().unwrap_or(EOF.clone()).kind,
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

            while lexer.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                let first = lexer.peek().unwrap_or(&EOF);
                let first_loc = first.loc.clone();
                match &lexer.next().unwrap_or(EOF.clone()).kind {
                    TokenKind::Identifier(name) => {
                        if lexer.peek().unwrap_or(&EOF).kind == TokenKind::Colon {
                            lexer.next();

                            let pattern = parse_pattern(lexer);

                            // TODO: handle `var` and `mut` modifiers
                            props.push(ObjectPatProp::KeyValue(KeyValuePatProp {
                                loc: merge_locations(&first_loc, &pattern.loc),
                                key: Ident {
                                    name: name.clone(),
                                    loc: first_loc,
                                },
                                value: Box::new(pattern),
                                init: None,
                            }));
                        } else {
                            // TODO: handle `var` and `mut` modifiers
                            props.push(ObjectPatProp::Shorthand(ShorthandPatProp {
                                loc: first_loc.clone(),
                                ident: BindingIdent {
                                    name: name.clone(),
                                    loc: first_loc,
                                    mutable: false,
                                },
                                init: None,
                            }))
                        }

                        if lexer.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                            lexer.next();
                        }
                    }
                    TokenKind::DotDotDot => {
                        if has_rest {
                            panic!("only one rest pattern is allowed per object pattern");
                        }
                        props.push(ObjectPatProp::Rest(RestPat {
                            arg: Box::new(parse_pattern(lexer)),
                        }));
                        has_rest = true;
                    }
                    _ => panic!("expected identifier or rest pattern"),
                }
            }

            loc = merge_locations(&loc, &lexer.peek().unwrap_or(&EOF).loc);
            assert_eq!(
                lexer.next().unwrap_or(EOF.clone()).kind,
                TokenKind::RightBrace
            );

            PatternKind::Object(ObjectPat {
                props,
                optional: false,
            })
        }
        // This code can be called when parsing rest patterns in function params.
        TokenKind::DotDotDot => PatternKind::Rest(RestPat {
            arg: Box::new(parse_pattern(lexer)),
        }),
        TokenKind::Underscore => PatternKind::Wildcard,
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
        let lexer = Lexer::new(input);
        parse_pattern(&mut lexer.peekable())
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
