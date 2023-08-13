use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::precedence::{Associativity, Operator, PRECEDENCE_TABLE};
use crate::token::*;

fn get_infix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        // multiplicative
        TokenKind::Times => PRECEDENCE_TABLE.get(&Operator::Multiplication).cloned(),
        TokenKind::Divide => PRECEDENCE_TABLE.get(&Operator::Division).cloned(),
        TokenKind::Modulo => PRECEDENCE_TABLE.get(&Operator::Remainder).cloned(),

        // additive
        TokenKind::Plus => PRECEDENCE_TABLE.get(&Operator::Addition).cloned(),
        TokenKind::Minus => PRECEDENCE_TABLE.get(&Operator::Subtraction).cloned(),

        TokenKind::Ampersand => Some((4, Associativity::Left)), // same as LogicalAnd
        TokenKind::Pipe => Some((3, Associativity::Left)),      // same as LogicalOr

        _ => None,
    }
}

fn get_postfix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        TokenKind::LeftBracket => Some((12, Associativity::NotApplicable)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    fn parse_type_ann_atom(&mut self) -> Result<TypeAnn, ParseError> {
        let mut span = self.peek().unwrap_or(&EOF).span;
        let kind = match self.peek().unwrap_or(&EOF).kind.clone() {
            TokenKind::BoolLit(value) => {
                self.next();
                TypeAnnKind::BoolLit(value)
            }
            TokenKind::Boolean => {
                self.next();
                TypeAnnKind::Boolean
            }
            TokenKind::NumLit(value) => {
                self.next();
                TypeAnnKind::NumLit(value)
            }
            TokenKind::Number => {
                self.next();
                TypeAnnKind::Number
            }
            TokenKind::StrLit(value) => {
                self.next();
                TypeAnnKind::StrLit(value)
            }
            TokenKind::String => {
                self.next();
                TypeAnnKind::String
            }
            TokenKind::Symbol => {
                self.next();
                TypeAnnKind::Symbol
            }
            TokenKind::Null => {
                self.next();
                TypeAnnKind::Null
            }
            TokenKind::Undefined => {
                self.next();
                TypeAnnKind::Undefined
            }
            TokenKind::Unknown => {
                self.next();
                TypeAnnKind::Unknown
            }
            TokenKind::Never => {
                self.next();
                TypeAnnKind::Never
            }
            TokenKind::Underscore => {
                self.next(); // consume '_'

                if self.peek().unwrap_or(&EOF).kind == TokenKind::Colon {
                    self.next(); // consume ':'
                    let constraint = self.parse_type_ann()?;
                    TypeAnnKind::Wildcard(Some(Box::new(constraint)))
                } else {
                    TypeAnnKind::Wildcard(None)
                }
            }
            TokenKind::LeftBrace => {
                self.next(); // consumes '{'
                let mut props: Vec<ObjectProp> = vec![];

                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                    match self.next().unwrap_or(EOF.clone()).kind {
                        TokenKind::Identifier(name) => {
                            let optional =
                                if self.peek().unwrap_or(&EOF).kind == TokenKind::Question {
                                    self.next().unwrap_or(EOF.clone());
                                    true
                                } else {
                                    false
                                };
                            assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);

                            let prop = match self.peek().unwrap_or(&EOF).kind {
                                TokenKind::Get => {
                                    self.next(); // consume `get`

                                    // TODO - `params` should only be `self`
                                    let params = self.parse_params()?;
                                    assert_eq!(
                                        self.next().unwrap_or(EOF.clone()).kind,
                                        TokenKind::SingleArrow
                                    );
                                    let ret = self.parse_type_ann()?;

                                    let type_ann = TypeAnn {
                                        kind: TypeAnnKind::Function(FunctionType {
                                            type_params: None,
                                            params,
                                            ret: Box::new(ret),
                                            throws: None,
                                        }),
                                        span: Span { start: 0, end: 0 }, // TODO
                                        inferred_type: None,
                                    };

                                    ObjectProp::Prop(type_ann::Prop {
                                        name,
                                        modifier: Some(PropModifier::Getter),
                                        optional,
                                        mutable: false, // TODO
                                        type_ann: Box::new(type_ann),
                                        // TODO(#642): compute correct spans for type annotations
                                        span: Span { start: 0, end: 0 },
                                    })
                                }
                                TokenKind::Set => {
                                    self.next(); // consume `set`

                                    // TODO - `params` should only be `mut self, value`
                                    let params = self.parse_params()?;
                                    assert_eq!(
                                        self.next().unwrap_or(EOF.clone()).kind,
                                        TokenKind::SingleArrow
                                    );
                                    let ret = self.parse_type_ann()?;

                                    let type_ann = TypeAnn {
                                        kind: TypeAnnKind::Function(FunctionType {
                                            type_params: None,
                                            params,
                                            ret: Box::new(ret),
                                            throws: None,
                                        }),
                                        span: Span { start: 0, end: 0 }, // TODO
                                        inferred_type: None,
                                    };

                                    ObjectProp::Prop(type_ann::Prop {
                                        name,
                                        modifier: Some(PropModifier::Setter),
                                        optional,
                                        mutable: false, // TODO
                                        type_ann: Box::new(type_ann),
                                        // TODO(#642): compute correct spans for type annotations
                                        span: Span { start: 0, end: 0 },
                                    })
                                }
                                _ => {
                                    // This means we can get rid of the difference
                                    // between methods and properties that are functions.
                                    let type_ann = self.parse_type_ann()?;
                                    ObjectProp::Prop(type_ann::Prop {
                                        name,
                                        modifier: None,
                                        optional,
                                        mutable: false, // TODO
                                        type_ann: Box::new(type_ann),
                                        // TODO(#642): compute correct spans for type annotations
                                        span: Span { start: 0, end: 0 },
                                    })
                                }
                            };

                            props.push(prop);
                        }
                        TokenKind::LeftBracket => {
                            let name = match self.next().unwrap_or(EOF.clone()).kind {
                                TokenKind::Identifier(name) => name,
                                _ => {
                                    return Err(ParseError {
                                        message: "expected identifier".to_string(),
                                    })
                                }
                            };
                            assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);
                            let type_ann = self.parse_type_ann()?;
                            assert_eq!(
                                self.next().unwrap_or(EOF.clone()).kind,
                                TokenKind::RightBracket
                            );

                            let key = IndexerKey {
                                name,
                                type_ann: Box::new(type_ann),
                            };

                            assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);
                            let type_ann = self.parse_type_ann()?;

                            props.push(ObjectProp::Indexer(Indexer {
                                key,
                                mutable: false, // TODO
                                type_ann: Box::new(type_ann),
                                // TODO(#642): compute correct spans for type annotations
                                span: Span { start: 0, end: 0 },
                            }));
                        }
                        TokenKind::Fn => {
                            match self.peek().unwrap_or(&EOF).kind.clone() {
                                TokenKind::LeftParen => {
                                    let type_params = self.maybe_parse_type_params()?;
                                    let params = self.parse_params()?;
                                    assert_eq!(
                                        self.next().unwrap_or(EOF.clone()).kind,
                                        TokenKind::SingleArrow
                                    );
                                    let ret = self.parse_type_ann()?;

                                    props.push(ObjectProp::Call(ObjCallable {
                                        type_params,
                                        params,
                                        ret: Box::new(ret),
                                        // TODO(#642): compute correct spans for type annotations
                                        span: Span { start: 0, end: 0 },
                                    }));
                                }
                                _ => {
                                    return Err(ParseError {
                                        message: "expected identifier or left paren".to_string(),
                                    })
                                }
                            }
                        }
                        _ => {
                            return Err(ParseError {
                                message: "expected identifier or indexer".to_string(),
                            })
                        }
                    }

                    match self.peek().unwrap_or(&EOF).kind {
                        TokenKind::Comma => {
                            self.next();
                        }
                        TokenKind::RightBrace => {
                            break;
                        }
                        _ => {
                            return Err(ParseError {
                                message: "expected ',' or '}'".to_string(),
                            })
                        }
                    }
                }

                span = merge_spans(&span, &self.peek().unwrap_or(&EOF).span);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBrace
                );

                TypeAnnKind::Object(props)
            }
            TokenKind::LeftBracket => {
                self.next(); // consumes '['
                let mut elems: Vec<TypeAnn> = vec![];

                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBracket {
                    if self.peek().unwrap_or(&EOF).kind == TokenKind::DotDotDot {
                        let token = self.next().ok_or(ParseError {
                            message: "expected '...'".to_string(),
                        })?;
                        let type_ann = self.parse_type_ann()?;
                        let span = merge_spans(&token.span, &type_ann.span);

                        elems.push(TypeAnn {
                            kind: TypeAnnKind::Rest(Box::new(type_ann)),
                            span,
                            inferred_type: None,
                        });
                    } else {
                        elems.push(self.parse_type_ann()?);
                    }

                    if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                        self.next(); // consume the ','
                    } else {
                        break;
                    }
                }

                span = merge_spans(&span, &self.peek().unwrap_or(&EOF).span);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBracket
                );

                TypeAnnKind::Tuple(elems)
            }
            TokenKind::LeftParen => {
                let atom = self.parse_inside_parens(|p| p.parse_type_ann())?;
                return Ok(atom);
            }
            TokenKind::Identifier(ident) => {
                self.next(); // consumes identifier

                if self.peek().unwrap_or(&EOF).kind == TokenKind::LessThan {
                    self.next().unwrap_or(EOF.clone());
                    let mut params: Vec<TypeAnn> = vec![];

                    while self.peek().unwrap_or(&EOF).kind != TokenKind::GreaterThan {
                        params.push(self.parse_type_ann()?);

                        if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                            self.next().unwrap_or(EOF.clone());
                        } else {
                            break;
                        }
                    }

                    span = merge_spans(&span, &self.peek().unwrap_or(&EOF).span);
                    assert_eq!(
                        self.next().unwrap_or(EOF.clone()).kind,
                        TokenKind::GreaterThan
                    );

                    TypeAnnKind::TypeRef(ident, Some(params))
                } else {
                    TypeAnnKind::TypeRef(ident, None)
                }
            }
            TokenKind::Fn => {
                self.next(); // consumes 'fn'

                let type_params = self.maybe_parse_type_params()?;
                let params = self.parse_params()?;
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::SingleArrow
                );
                let return_type = self.parse_type_ann()?;

                let throws = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::Throws => {
                        self.next(); // consume `throws`
                        let type_ann = self.parse_type_ann()?;
                        Some(Box::new(type_ann))
                    }
                    _ => None,
                };

                TypeAnnKind::Function(FunctionType {
                    type_params,
                    params,
                    ret: Box::new(return_type),
                    throws,
                })
            }
            TokenKind::KeyOf => {
                self.next(); // consumes 'keyof'

                let type_ann = self.parse_type_ann()?;

                TypeAnnKind::KeyOf(Box::new(type_ann))
            }
            TokenKind::TypeOf => {
                self.next(); // consumes 'typeof'

                let expr = self.parse_expr()?;

                TypeAnnKind::TypeOf(Box::new(expr))
            }
            TokenKind::Infer => {
                self.next(); // consumes 'infer'

                let name = match self.next().unwrap_or(EOF.clone()).kind {
                    TokenKind::Identifier(name) => name,
                    _ => {
                        return Err(ParseError {
                            message: "expected identifier".to_string(),
                        })
                    }
                };

                TypeAnnKind::Infer(name)
            }
            TokenKind::If => return self.parse_conditional_type(),
            TokenKind::Match => {
                self.next(); // consumes 'match'

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftParen
                );
                let matchable = self.parse_type_ann()?;
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightParen
                );

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftBrace
                );

                let mut cases: Vec<MatchTypeCase> = vec![];
                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                    let extends = self.parse_type_ann()?;
                    assert_eq!(
                        self.next().unwrap_or(EOF.clone()).kind,
                        TokenKind::DoubleArrow
                    );
                    let true_type = self.parse_type_ann()?;

                    cases.push(MatchTypeCase {
                        extends: Box::new(extends),
                        true_type: Box::new(true_type),
                    });

                    if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }

                self.next(); // consumes '}'

                TypeAnnKind::Match(MatchType {
                    matchable: Box::new(matchable),
                    cases,
                })
            }
            token => {
                panic!("expected token to start type annotation, found {:?}", token)
            }
        };

        let atom = TypeAnn {
            kind,
            span,
            inferred_type: None,
        };

        Ok(atom)
    }

    fn parse_type_ann_postfix(
        &mut self,
        lhs: TypeAnn,
        next_precedence: (u8, Associativity),
    ) -> Result<TypeAnn, ParseError> {
        let _precedence = if next_precedence.1 == Associativity::Left {
            next_precedence.0
        } else {
            next_precedence.0 - 1
        };

        let token = self.peek().unwrap_or(&EOF).clone();

        let type_ann = match &token.kind {
            // TODO: handle parsing index access type
            TokenKind::LeftBracket => {
                self.next();
                match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::RightBracket => {
                        let next = self.next().unwrap_or(EOF.clone());
                        let span = merge_spans(&lhs.span, &next.span);
                        TypeAnn {
                            kind: TypeAnnKind::Array(Box::new(lhs)),
                            span,
                            inferred_type: None,
                        }
                    }
                    _ => {
                        let index_type = self.parse_type_ann()?;
                        let merged_span = merge_spans(&lhs.span, &index_type.span);
                        assert_eq!(
                            self.next().unwrap_or(EOF.clone()).kind,
                            TokenKind::RightBracket
                        );
                        TypeAnn {
                            kind: TypeAnnKind::IndexedAccess(Box::new(lhs), Box::new(index_type)),
                            span: merged_span,
                            inferred_type: None,
                        }
                    }
                }
                // let next = self.peek().unwrap_or(&EOF);
                // let merged_span = merge_spans(&lhs.span, &next.span);
                // assert_eq!(
                //     self.next().unwrap_or(EOF.clone()).kind,
                //     TokenKind::RightBracket
                // );
                // TypeAnn {
                //     kind: TypeAnnKind::Array(Box::new(lhs)),
                //     span: merged_span,
                //     inferred_type: None,
                // }
            }
            _ => panic!("unexpected token: {:?}", token),
        };

        Ok(type_ann)
    }

    fn parse_type_ann_with_precedence(&mut self, precedence: u8) -> Result<TypeAnn, ParseError> {
        let mut lhs = self.parse_type_ann_atom()?;

        loop {
            let next = self.peek().unwrap_or(&EOF).clone();
            if let TokenKind::Eof = next.kind {
                return Ok(lhs);
            }

            if let TokenKind::Semicolon = next.kind {
                return Ok(lhs);
            }

            if let Some(next_precedence) = get_postfix_precedence(&next) {
                if precedence >= next_precedence.0 {
                    return Ok(lhs);
                }

                lhs = self.parse_type_ann_postfix(lhs.clone(), next_precedence)?;

                continue;
            }

            if let Some(next_precedence) = get_infix_precedence(&next) {
                if precedence >= next_precedence.0 {
                    return Ok(lhs);
                }

                self.next();

                let precedence = if next_precedence.1 == Associativity::Left {
                    next_precedence.0
                } else {
                    next_precedence.0 - 1
                };

                lhs = match &next.kind {
                    TokenKind::Ampersand => {
                        let start = lhs.span.start;
                        let rhs = self.parse_type_ann_with_precedence(precedence)?;
                        let mut end = rhs.span.end;
                        let mut types = vec![lhs, rhs];
                        while TokenKind::Ampersand == self.peek().unwrap_or(&EOF).kind {
                            self.next();
                            let rhs = self.parse_type_ann_with_precedence(precedence)?;
                            end = rhs.span.end;
                            types.push(rhs);
                        }
                        let span = Span { start, end };

                        TypeAnn {
                            kind: TypeAnnKind::Intersection(types),
                            span,
                            inferred_type: None,
                        }
                    }
                    TokenKind::Pipe => {
                        let start = lhs.span.start;
                        let rhs = self.parse_type_ann_with_precedence(precedence)?;
                        let mut end = rhs.span.end;
                        let mut types = vec![lhs, rhs];
                        while TokenKind::Pipe == self.peek().unwrap_or(&EOF).kind {
                            self.next();
                            let rhs = self.parse_type_ann_with_precedence(precedence)?;
                            end = rhs.span.end;
                            types.push(rhs);
                        }
                        let span = Span { start, end };

                        TypeAnn {
                            kind: TypeAnnKind::Union(types),
                            span,
                            inferred_type: None,
                        }
                    }
                    _ => {
                        let op: BinaryOp = match &next.kind {
                            TokenKind::Plus => BinaryOp::Plus,
                            TokenKind::Minus => BinaryOp::Minus,
                            TokenKind::Times => BinaryOp::Times,
                            TokenKind::Divide => BinaryOp::Divide,
                            TokenKind::Modulo => BinaryOp::Modulo,
                            _ => panic!("unexpected token: {:?}", next),
                        };

                        let precedence = if next_precedence.1 == Associativity::Left {
                            next_precedence.0
                        } else {
                            next_precedence.0 - 1
                        };

                        let rhs = self.parse_type_ann_with_precedence(precedence)?;
                        // let span = merge_spans(&lhs.get_span(), &rhs.get_span());

                        lhs = TypeAnn {
                            kind: TypeAnnKind::Binary(BinaryTypeAnn {
                                op,
                                left: Box::new(lhs),
                                right: Box::new(rhs),
                            }),
                            span: Span { start: 0, end: 0 },
                            inferred_type: None,
                        };

                        continue;
                    }
                };

                continue;
            }

            return Ok(lhs);
        }
    }

    fn parse_conditional_type(&mut self) -> Result<TypeAnn, ParseError> {
        // TODO(#642): compute correct spans for type annotations
        let span = self.peek().unwrap_or(&EOF).span;
        self.next(); // consumes 'if'

        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::LeftParen
        );
        let check = self.parse_type_ann()?;
        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);
        let extends = self.parse_type_ann()?;
        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::RightParen
        );

        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::LeftBrace
        );
        let true_type = self.parse_type_ann()?;
        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::RightBrace
        );
        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Else);

        let false_type = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::If => self.parse_conditional_type()?,
            _ => {
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftBrace
                );
                let false_type = self.parse_type_ann()?;
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBrace
                );
                false_type
            }
        };

        // TODO: allow chaining of if/else

        let kind = TypeAnnKind::Condition(ConditionType {
            check: Box::new(check),
            extends: Box::new(extends),
            true_type: Box::new(true_type),
            false_type: Box::new(false_type),
        });

        let atom = TypeAnn {
            kind,
            span,
            inferred_type: None,
        };

        Ok(atom)
    }

    pub fn parse_type_ann(&mut self) -> Result<TypeAnn, ParseError> {
        self.parse_type_ann_with_precedence(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    pub fn parse(input: &str) -> TypeAnn {
        let mut parser = Parser::new(input);
        parser.parse_type_ann().unwrap()
    }

    #[test]
    fn parse_literal_types() {
        insta::assert_debug_snapshot!(parse("123"));
        insta::assert_debug_snapshot!(parse("true"));
        insta::assert_debug_snapshot!(parse("false"));
        insta::assert_debug_snapshot!(parse("null"));
        insta::assert_debug_snapshot!(parse("undefined"));
        insta::assert_debug_snapshot!(parse(r#""hello""#));
    }

    #[test]
    fn parse_primitive_types() {
        insta::assert_debug_snapshot!(parse("number"));
        insta::assert_debug_snapshot!(parse("string"));
        insta::assert_debug_snapshot!(parse("boolean"));
        insta::assert_debug_snapshot!(parse("symbol"));
    }

    #[test]
    fn parse_object_types() {
        insta::assert_debug_snapshot!(parse("{a: number, b?: string, c: boolean}"));
        insta::assert_debug_snapshot!(parse("{a: {b: {c: boolean}}}"));
        insta::assert_debug_snapshot!(parse("{\n  a: number,\n  b?: string,\n  c: boolean,\n}"));
    }

    #[test]
    fn parse_object_type_all_sig_types() -> Result<(), ParseError> {
        let input = r#"
            {
                fn (a: number) -> string,
                foo: fn (a: number) -> string,
                bar: fn (self, a: number) -> string,
                baz: get (self) -> string,
                baz: set (mut self, value: string) -> undefined,
                [key: string]: number,
                qux: string,
            }
        "#;
        let mut parser = Parser::new(input);
        let result = parser.parse_type_ann()?;
        insta::assert_debug_snapshot!(result);

        Ok(())
    }

    #[test]
    #[should_panic]
    fn parse_object_type_missing_comma() {
        insta::assert_debug_snapshot!(parse("{a: number b: string}"));
    }

    #[test]
    #[should_panic]
    fn parse_object_type_missing_right_brace() {
        insta::assert_debug_snapshot!(parse("{a: number, b: string"));
    }

    #[test]
    fn parse_tuple_types() {
        insta::assert_debug_snapshot!(parse("[number, string, boolean]"));
        insta::assert_debug_snapshot!(parse("[\n  number,\n  string,\n  boolean,\n]"));
        insta::assert_debug_snapshot!(parse("[number, ...number[]]"));
    }

    #[test]
    #[should_panic]
    fn parse_tuple_type_missing_comma() {
        insta::assert_debug_snapshot!(parse("[number string]"));
    }

    #[test]
    #[should_panic]
    fn parse_tuple_type_missing_right_bracket() {
        insta::assert_debug_snapshot!(parse("[number, string"));
    }

    #[test]
    fn parse_array_types() {
        insta::assert_debug_snapshot!(parse("number[]"));
        insta::assert_debug_snapshot!(parse("{x: number, y: number}[]"));
        insta::assert_debug_snapshot!(parse("T[][]"));
    }

    #[test]
    fn parse_type_refs() {
        insta::assert_debug_snapshot!(parse("Array<T>"));
        insta::assert_debug_snapshot!(parse("Map<K, V>"));
        insta::assert_debug_snapshot!(parse("Array<Array<T>>"));
        insta::assert_debug_snapshot!(parse("T"));
    }

    #[test]
    fn parse_fn_type_ann() {
        insta::assert_debug_snapshot!(parse("fn (a: number, b: number) -> number"));
        insta::assert_debug_snapshot!(parse("fn (a: number, b: number) -> number throws string"));
    }

    #[test]
    fn parse_union_types() {
        insta::assert_debug_snapshot!(parse("number | string"));
        insta::assert_debug_snapshot!(parse("number | string | boolean"));
    }

    #[test]
    fn parse_intersection_types() {
        insta::assert_debug_snapshot!(parse("number & string"));
        insta::assert_debug_snapshot!(parse("number & string & boolean"));
    }

    #[test]
    fn parse_union_and_intersection_combo() {
        insta::assert_debug_snapshot!(parse("number | string & boolean"));
        insta::assert_debug_snapshot!(parse("number & string | boolean"));
    }

    #[test]
    fn parse_parens_for_grouping() {
        insta::assert_debug_snapshot!(parse("number & (string | boolean)"));
    }

    #[test]
    fn parse_indexed_access() {
        insta::assert_debug_snapshot!(parse("T[K]"));
        insta::assert_debug_snapshot!(parse(r#"T["foo"]"#));
    }

    #[test]
    fn parse_indexer_type() {
        insta::assert_debug_snapshot!(parse("{[key: string]: number}"));
        insta::assert_debug_snapshot!(parse("{[key: string]: number, [key: number]: string}"));
    }

    #[test]
    fn parse_conditional_type() {
        insta::assert_debug_snapshot!(parse("if (T: U) { never } else { T }"));
        insta::assert_debug_snapshot!(parse(
            r#"if (T: string) { "string" } else if (T: number) { "number" } else { "other" }"#
        ));
    }

    #[test]
    fn parse_wildcard_type() {
        insta::assert_debug_snapshot!(parse("Array<_>"));
    }

    #[test]
    fn parse_func_with_rest_param() {
        insta::assert_debug_snapshot!(parse("fn (...args: Array<number>) -> number"));
        insta::assert_debug_snapshot!(parse("fn (...args: Array<_>) -> _"));
        insta::assert_debug_snapshot!(parse("fn (...args: _) -> _"));
    }

    #[test]
    fn parse_infer_type() {
        insta::assert_debug_snapshot!(parse("infer T"));
    }

    #[test]
    fn parse_pattern_mathing_type() {
        insta::assert_debug_snapshot!(parse(
            r#"
            match (T) {
                number => "number",
                string => "string",
                _ => "other",
            }
        "#
        ));
    }

    #[test]
    fn parse_arithmetic() {
        insta::assert_debug_snapshot!(parse(r#"A + B"#));
        insta::assert_debug_snapshot!(parse(r#"A - B"#));
        insta::assert_debug_snapshot!(parse(r#"A * B"#));
        insta::assert_debug_snapshot!(parse(r#"A / B"#));
        insta::assert_debug_snapshot!(parse(r#"A % B"#));
        insta::assert_debug_snapshot!(parse(r#"A * B + C"#));
        insta::assert_debug_snapshot!(parse(r#"A * (B + C)"#));
    }
}
