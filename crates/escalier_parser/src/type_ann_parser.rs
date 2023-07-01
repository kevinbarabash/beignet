use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::token::*;

impl<'a> Parser<'a> {
    pub fn parse_type_ann(&mut self) -> Result<TypeAnn, ParseError> {
        let mut span = self.peek().unwrap_or(&EOF).span;
        let mut kind = match self.next().unwrap_or(EOF.clone()).kind {
            TokenKind::BoolLit(value) => TypeAnnKind::BoolLit(value),
            TokenKind::Boolean => TypeAnnKind::Boolean,
            TokenKind::NumLit(value) => TypeAnnKind::NumLit(value),
            TokenKind::Number => TypeAnnKind::Number,
            TokenKind::StrLit(value) => TypeAnnKind::StrLit(value),
            TokenKind::String => TypeAnnKind::String,
            TokenKind::Symbol => TypeAnnKind::Symbol,
            TokenKind::Null => TypeAnnKind::Null,
            TokenKind::Undefined => TypeAnnKind::Undefined,
            TokenKind::LeftBrace => {
                let mut props: Vec<ObjectProp> = vec![];

                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                    if let TokenKind::Identifier(name) = self.next().unwrap_or(EOF.clone()).kind {
                        let optional = if self.peek().unwrap_or(&EOF).kind == TokenKind::Question {
                            self.next().unwrap_or(EOF.clone());
                            true
                        } else {
                            false
                        };
                        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);
                        let type_ann = self.parse_type_ann()?;

                        props.push(ObjectProp {
                            name,
                            optional,
                            mutable: false, // TODO
                            type_ann,
                        });

                        if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma {
                            self.next().unwrap_or(EOF.clone());
                        } else {
                            break;
                        }
                    } else {
                        panic!("expected identifier")
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
            TokenKind::Identifier(ident) => {
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
                let params = self.parse_params()?;
                assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);
                let return_type = self.parse_type_ann()?;

                TypeAnnKind::Function(params, Box::new(return_type))
            }
            token => {
                panic!("expected token to start type annotation, found {:?}", token)
            }
        };

        while self.peek().unwrap_or(&EOF).kind == TokenKind::LeftBracket {
            self.next().unwrap_or(EOF.clone());
            let right_span = self.peek().unwrap_or(&EOF).span;
            let merged_span = merge_spans(&span, &right_span);
            assert_eq!(
                self.next().unwrap_or(EOF.clone()).kind,
                TokenKind::RightBracket
            );
            kind = TypeAnnKind::Array(Box::new(TypeAnn { kind, span }));
            span = merged_span;
        }

        Ok(TypeAnn { kind, span })
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
}
