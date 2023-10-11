use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::token::*;

impl<'a> Parser<'a> {
    pub fn parse_class(&mut self) -> Result<Expr, ParseError> {
        let token = self.next().unwrap_or(EOF.clone());
        assert_eq!(token.kind, TokenKind::Class);

        let type_params = self.maybe_parse_type_params()?;

        let super_class = if self.peek().unwrap_or(&EOF).kind == TokenKind::Extends {
            self.next(); // consumes 'extends'
            let token = self.next().unwrap_or(EOF.clone());
            if let TokenKind::Identifier(name) = token.kind {
                Some(Ident {
                    span: token.span,
                    name,
                })
            } else {
                panic!("expected identifier");
            }
        } else {
            None
        };

        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::LeftBrace
        );

        let mut body = vec![];

        while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
            let member = self.parse_class_member()?;
            body.push(member);
        }

        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::RightBrace
        );

        let end = self.scanner.cursor();
        let span = Span {
            start: token.span.start,
            end,
        };
        let kind = ExprKind::Class(Class {
            span,
            type_params,
            super_class,
            super_type_args: None, // TODO
            body,
        });

        Ok(Expr {
            kind,
            span,
            inferred_type: None,
        })
    }

    fn parse_class_member(&mut self) -> Result<ClassMember, ParseError> {
        let is_public = if self.peek().unwrap_or(&EOF).kind == TokenKind::Pub {
            self.next(); // consumes 'pub'
            true
        } else {
            false
        };

        let is_static = if self.peek().unwrap_or(&EOF).kind == TokenKind::Static {
            self.next(); // consumes 'static'
            true
        } else {
            false
        };

        let token = self.peek().unwrap_or(&EOF);
        match token.kind {
            TokenKind::Identifier(_) => self.parse_field(is_public, is_static),
            TokenKind::Fn => self.parse_method(is_public, is_static),
            TokenKind::Gen => self.parse_method(is_public, is_static),
            TokenKind::Async => self.parse_method(is_public, is_static),
            TokenKind::Get => match is_static {
                true => Err(ParseError {
                    message: "static getters are not allowed".to_string(),
                }),
                false => self.parse_getter(is_public),
            },
            TokenKind::Set => match is_static {
                true => Err(ParseError {
                    message: "static setters are not allowed".to_string(),
                }),
                false => self.parse_setter(is_public),
            },
            _ => Err(ParseError {
                message: format!("unexpected token {:?}", token),
            }),
        }
    }

    fn parse_field(&mut self, is_public: bool, is_static: bool) -> Result<ClassMember, ParseError> {
        // TODO: how do we include `pub` and `static` in the span?
        let token = self.next().unwrap_or(EOF.clone());
        let start = token.span.start;

        let name = if let TokenKind::Identifier(name) = &token.kind {
            Ident {
                span: token.span,
                name: name.to_owned(),
            }
        } else {
            panic!("expected identifier");
        };

        let field = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::Colon => {
                self.next(); // consumes ':'
                let type_ann = self.parse_type_ann()?;
                let end = self.scanner.cursor();

                let span = Span { start, end };

                ClassMember::Field(Field {
                    span,
                    name,
                    is_public,
                    is_static,
                    init: None,
                    type_ann: Some(type_ann),
                })
            }
            TokenKind::Assign => {
                self.next(); // consumes '='
                let init = self.parse_expr()?;
                let end = self.scanner.cursor();

                let span = Span { start, end };

                ClassMember::Field(Field {
                    span,
                    name,
                    is_public,
                    is_static,
                    init: Some(Box::new(init)),
                    type_ann: None,
                })
            }
            _ => panic!("expected ':' or '='"),
        };

        Ok(field)
    }

    fn parse_getter(&mut self, is_public: bool) -> Result<ClassMember, ParseError> {
        let token = self.next().unwrap_or(EOF.clone());
        assert_eq!(token.kind, TokenKind::Get);
        let start = token.span.start;

        let name = self.parse_name()?;
        let params = self.parse_params()?;
        let body = self.parse_block()?;
        let span = Span {
            start,
            end: self.scanner.cursor(),
        };

        let getter = ClassMember::Getter(Getter {
            span,
            name,
            is_public,
            type_ann: None,
            params,
            body,
        });

        Ok(getter)
    }

    fn parse_setter(&mut self, is_public: bool) -> Result<ClassMember, ParseError> {
        let token = self.next().unwrap_or(EOF.clone());
        assert_eq!(token.kind, TokenKind::Set);
        let start = token.span.start;

        let name = self.parse_name()?;
        let params = self.parse_params()?;
        let body = self.parse_block()?;
        let span = Span {
            start,
            end: self.scanner.cursor(),
        };

        let setter = ClassMember::Setter(Setter {
            span,
            name,
            is_public,
            type_ann: None,
            params,
            body,
        });

        Ok(setter)
    }

    fn parse_method(
        &mut self,
        is_public: bool,
        is_static: bool,
    ) -> Result<ClassMember, ParseError> {
        // TODO: how do we include `pub` and `static` in the span?
        let start = self.peek().unwrap_or(&EOF).span.start;

        let is_async = if self.peek().unwrap_or(&EOF).kind == TokenKind::Async {
            self.next(); // consumes 'async'
            true
        } else {
            false
        };

        let is_gen = if self.peek().unwrap_or(&EOF).kind == TokenKind::Gen {
            self.next(); // consumes 'gen'
            true
        } else {
            false
        };

        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Fn);

        let name = self.parse_name()?;
        let type_params = self.maybe_parse_type_params()?;
        let (params, is_mutating) = match is_static {
            true => (self.parse_params()?, false),
            false => self.parse_method_params()?,
        };
        let type_ann = if self.peek().unwrap_or(&EOF).kind == TokenKind::SingleArrow {
            self.next(); // consumes '->'
            Some(self.parse_type_ann()?)
        } else {
            None
        };

        let throws = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::Throws => {
                self.next();
                Some(self.parse_type_ann()?)
            }
            _ => None,
        };

        let body = self.parse_block()?;
        let end = self.scanner.cursor();
        let span = Span { start, end };

        let method = ClassMember::Method(Method {
            span,
            name,
            is_public,
            is_async,
            is_gen,
            is_mutating,
            is_static,
            params,
            body,
            type_params,
            type_ann,
            throws,
        });

        Ok(method)
    }

    fn parse_name(&mut self) -> Result<PropName, ParseError> {
        let next = self.next().unwrap_or(EOF.clone());
        let name = match &next.kind {
            TokenKind::Identifier(ident) => PropName::Ident(Ident {
                span: next.span,
                name: ident.to_owned(),
            }),
            // TokenKind::NumLit(num) => PropName::Num(Num {
            //     span: next.span,
            //     value: num.to_owned(),
            // }),
            // TokenKind::StrLit(str) => PropName::Str(Str {
            //     span: next.span,
            //     value: str.to_owned(),
            // }),
            TokenKind::LeftBracket => {
                let expr = self.parse_expr()?;
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBracket
                );
                PropName::Computed(expr)
            }
            _ => panic!("expected identifier or computed property name"),
        };

        Ok(name)
    }
}
