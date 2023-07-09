use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::precedence::Associativity;
use crate::token::*;

fn get_infix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        TokenKind::Ampersand => Some((11, Associativity::Left)),
        TokenKind::Pipe => Some((10, Associativity::Left)),
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
                            let type_ann = self.parse_type_ann()?;

                            props.push(ObjectProp::Prop(type_ann::Prop {
                                name,
                                optional,
                                mutable: false, // TODO
                                type_ann: Box::new(type_ann),
                                span: Span { start: 0, end: 0 }, // TODO
                            }));
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
                                span: Span { start: 0, end: 0 }, // TODO
                            }));
                        }
                        TokenKind::Fn => {
                            match self.peek().unwrap_or(&EOF).kind.clone() {
                                TokenKind::Identifier(name) => {
                                    self.next(); // consume identifier

                                    let type_params = self.maybe_parse_type_params()?;
                                    let params = self.parse_params()?;
                                    assert_eq!(
                                        self.next().unwrap_or(EOF.clone()).kind,
                                        TokenKind::Colon
                                    );
                                    let ret = self.parse_type_ann()?;

                                    props.push(ObjectProp::Method(ObjMethod {
                                        name,
                                        type_params,
                                        params,
                                        ret: Box::new(ret),
                                        span: Span { start: 0, end: 0 }, // TODO
                                    }));
                                }
                                TokenKind::LeftParen => {
                                    let type_params = self.maybe_parse_type_params()?;
                                    let params = self.parse_params()?;
                                    assert_eq!(
                                        self.next().unwrap_or(EOF.clone()).kind,
                                        TokenKind::Colon
                                    );
                                    let ret = self.parse_type_ann()?;

                                    props.push(ObjectProp::Call(ObjCallable {
                                        type_params,
                                        params,
                                        ret: Box::new(ret),
                                        span: Span { start: 0, end: 0 }, // TODO
                                    }));
                                }
                                _ => {
                                    return Err(ParseError {
                                        message: "expected identifier or left paren".to_string(),
                                    })
                                }
                            }
                        }
                        TokenKind::Get => {
                            let name = match self.next().unwrap_or(EOF.clone()).kind {
                                TokenKind::Identifier(name) => name,
                                _ => {
                                    return Err(ParseError {
                                        message: "expected identifier".to_string(),
                                    })
                                }
                            };

                            let params = self.parse_params()?;
                            assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);
                            let ret = self.parse_type_ann()?;

                            props.push(ObjectProp::Getter(ObjGetter {
                                name,
                                ret: Box::new(ret),
                                params,
                                span: Span { start: 0, end: 0 }, // TODO
                            }));
                        }
                        TokenKind::Set => {
                            let name = match self.next().unwrap_or(EOF.clone()).kind {
                                TokenKind::Identifier(name) => name,
                                _ => {
                                    return Err(ParseError {
                                        message: "expected identifier".to_string(),
                                    })
                                }
                            };

                            let params = self.parse_params()?;
                            assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);
                            let _ret = self.parse_type_ann()?;

                            // TODO: check that _ret is undefined

                            props.push(ObjectProp::Setter(ObjSetter {
                                name,
                                params,
                                span: Span { start: 0, end: 0 }, // TODO
                            }));
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
                    elems.push(self.parse_type_ann()?);

                    if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                        self.next().unwrap_or(EOF.clone());
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
                assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);
                let return_type = self.parse_type_ann()?;

                // TODO: handle generics
                TypeAnnKind::Function(FunctionType {
                    type_params,
                    params,
                    ret: Box::new(return_type),
                })
            }
            TokenKind::KeyOf => {
                self.next(); // consumes 'keyof'

                let type_ann = self.parse_type_ann()?;

                TypeAnnKind::KeyOf(Box::new(type_ann))
            }
            TokenKind::Mut => {
                self.next(); // consumes 'mut'

                let type_ann = self.parse_type_ann()?;

                TypeAnnKind::Mutable(Box::new(type_ann))
            }
            TokenKind::TypeOf => {
                self.next(); // consumes 'typeof'

                let expr = self.parse_expr()?;

                TypeAnnKind::TypeOf(Box::new(expr))
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
                    _ => panic!("unexpected token {:?}", next.kind),
                };

                continue;
            }

            return Ok(lhs);
        }
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
                fn (a: number): string,
                fn foo(a: number): string,
                fn bar(self, a: number): string,
                get baz(): string,
                set baz(value: string): undefined,
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
        insta::assert_debug_snapshot!(parse("fn (a: number, b: number) => number"));
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
}
