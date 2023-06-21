use crate::func_param::parse_params;
use crate::parser::Parser;
use crate::source_location::merge_locations;
use crate::token::TokenKind;
use crate::type_ann::{ObjectProp, TypeAnn, TypeAnnKind};

pub fn parse_type_ann(parser: &mut Parser) -> TypeAnn {
    let mut loc = parser.peek().loc;
    let mut kind = match parser.next().kind {
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

            while parser.peek().kind != TokenKind::RightBrace {
                if let TokenKind::Identifier(name) = parser.next().kind {
                    let optional = if parser.peek().kind == TokenKind::Question {
                        parser.next();
                        true
                    } else {
                        false
                    };
                    assert_eq!(parser.next().kind, TokenKind::Colon);
                    let type_ann = parse_type_ann(parser);

                    props.push(ObjectProp {
                        name,
                        optional,
                        mutable: false, // TODO
                        type_ann,
                    });

                    if parser.peek().kind == TokenKind::Comma {
                        parser.next();
                    } else {
                        break;
                    }
                } else {
                    panic!("expected identifier")
                }
            }

            loc = merge_locations(&loc, &parser.peek().loc);
            assert_eq!(parser.next().kind, TokenKind::RightBrace);

            TypeAnnKind::Object(props)
        }
        TokenKind::LeftBracket => {
            let mut elems: Vec<TypeAnn> = vec![];

            while parser.peek().kind != TokenKind::RightBracket {
                elems.push(parse_type_ann(parser));

                if parser.peek().kind == TokenKind::Comma {
                    parser.next();
                } else {
                    break;
                }
            }

            loc = merge_locations(&loc, &parser.peek().loc);
            assert_eq!(parser.next().kind, TokenKind::RightBracket);

            TypeAnnKind::Tuple(elems)
        }
        TokenKind::Identifier(ident) => {
            if parser.peek().kind == TokenKind::LessThan {
                parser.next();
                let mut params: Vec<TypeAnn> = vec![];

                while parser.peek().kind != TokenKind::GreaterThan {
                    params.push(parse_type_ann(parser));

                    if parser.peek().kind == TokenKind::Comma {
                        parser.next();
                    } else {
                        break;
                    }
                }

                loc = merge_locations(&loc, &parser.peek().loc);
                assert_eq!(parser.next().kind, TokenKind::GreaterThan);

                TypeAnnKind::TypeRef(ident, Some(params))
            } else {
                TypeAnnKind::TypeRef(ident, None)
            }
        }
        TokenKind::Fn => {
            let params = parse_params(parser);
            assert_eq!(parser.next().kind, TokenKind::Arrow);
            let return_type = Box::new(parse_type_ann(parser));

            TypeAnnKind::Function(params, return_type)
        }
        token => {
            panic!("expected token to start type annotation, found {:?}", token)
        }
    };

    while parser.peek().kind == TokenKind::LeftBracket {
        parser.next();
        let right = parser.peek().loc;
        let merged_loc = merge_locations(&loc, &right);
        assert_eq!(parser.next().kind, TokenKind::RightBracket);
        kind = TypeAnnKind::Array(Box::new(TypeAnn { kind, loc }));
        loc = merged_loc;
    }

    TypeAnn { kind, loc }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    pub fn parse(input: &str) -> TypeAnn {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(Box::new(tokens.into_iter()));
        parse_type_ann(&mut parser)
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
