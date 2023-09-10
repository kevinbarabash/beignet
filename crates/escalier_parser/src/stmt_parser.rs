use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::token::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let mut token = self.peek().unwrap_or(&EOF).clone();
        let start = token.span.start;

        // TODO: only allow `declare` in front of `let`
        let is_declare = match &token.kind {
            TokenKind::Declare => {
                self.next(); // consumes 'declare'
                token = self.peek().unwrap_or(&EOF).clone();
                true
            }
            _ => false,
        };

        let stmt = match &token.kind {
            TokenKind::Let | TokenKind::Var => {
                let token = self.next().unwrap_or(EOF.clone()); // consumes 'let' or 'var'

                let is_var = token.kind == TokenKind::Var;

                let pattern = self.parse_pattern()?;

                let type_ann = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::Colon => {
                        self.next().unwrap_or(EOF.clone());
                        Some(self.parse_type_ann()?)
                    }
                    _ => None,
                };

                let expr = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::Assign => {
                        self.next().unwrap_or(EOF.clone());
                        Some(self.parse_expr()?)
                    }
                    _ => None,
                };

                let span = Span {
                    start,
                    end: if let Some(expr) = &expr {
                        expr.get_span().end
                    } else if let Some(type_ann) = &type_ann {
                        type_ann.span.end
                    } else {
                        pattern.span.end
                    },
                };

                // TODO: check invariants in semantic analysis pass
                Stmt {
                    kind: StmtKind::VarDecl(VarDecl {
                        is_declare,
                        is_var,
                        pattern,
                        expr,
                        type_ann,
                    }),
                    span,
                    inferred_type: None,
                }
            }
            TokenKind::For => {
                self.next(); // consumes 'for'

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftParen
                );
                let left = self.parse_pattern()?;
                assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::In);
                let right = self.parse_expr()?;
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightParen
                );
                assert_eq!(self.peek().unwrap_or(&EOF).kind, TokenKind::LeftBrace);
                let body = self.parse_block()?;

                let span = merge_spans(&left.span, &body.span);

                Stmt {
                    kind: StmtKind::For(ForStmt {
                        left: Box::new(left),
                        right: Box::new(right),
                        body,
                    }),
                    span,
                    inferred_type: None,
                }
            }
            TokenKind::Return => {
                self.next(); // consumes 'return'
                let next = self.peek().unwrap_or(&EOF).clone();
                match next.kind {
                    TokenKind::Eof => Stmt {
                        kind: StmtKind::Return(ReturnStmt { arg: None }),
                        span: token.span,
                        inferred_type: None,
                    },
                    _ => {
                        let arg = self.parse_expr()?;

                        let span = merge_spans(&next.span, &arg.get_span());
                        Stmt {
                            kind: StmtKind::Return(ReturnStmt { arg: Some(arg) }),
                            span,
                            inferred_type: None,
                        }
                    }
                }
            }
            TokenKind::Type => {
                self.next(); // consumes 'type'

                let name = match self.next().unwrap_or(EOF.clone()).kind {
                    TokenKind::Identifier(name) => name,
                    _ => {
                        return Err(ParseError {
                            message: "expected identifier".to_string(),
                        })
                    }
                };

                let type_params = self.maybe_parse_type_params()?;

                assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Assign);
                let type_ann = self.parse_type_ann()?;
                let span = merge_spans(&token.span, &type_ann.span);

                Stmt {
                    kind: StmtKind::TypeDecl(TypeDecl {
                        name,
                        type_ann,
                        type_params,
                    }),
                    span,
                    inferred_type: None,
                }
            }
            _ => {
                let expr = self.parse_expr()?;
                let span = expr.get_span();
                Stmt {
                    kind: StmtKind::Expr(ExprStmt { expr }),
                    span,
                    inferred_type: None,
                }
            }
        };

        Ok(stmt)
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != TokenKind::Eof {
            // TODO: attach comments to AST nodes
            if let TokenKind::Comment(_) = &self.peek().unwrap_or(&EOF).kind {
                self.next(); // consumes the comment
                continue;
            }
            stmts.push(self.parse_stmt()?);
        }
        Ok(Program { stmts })
    }
}

pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(input);
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Vec<Stmt> {
        let mut parser = Parser::new(input);
        parser.parse_program().unwrap().stmts
    }

    #[test]
    fn single_statement() {
        let input = "let x = 5";
        let stmts = parse(input);
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn single_variable_expression() {
        let input = "x";
        let stmts = parse(input);
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn multiple_statements() {
        let input = r#"
        let x = 5
        let y = 10
        x + y
        return
        "#;

        let stmts = parse(input);

        assert_eq!(stmts.len(), 4);
    }

    #[test]
    fn parse_let() {
        insta::assert_debug_snapshot!(parse(r#"let y = m*x + b"#));
    }

    #[test]
    fn parse_let_with_type_annotation() {
        insta::assert_debug_snapshot!(parse(r#"let y: number = m*x + b"#));
    }

    #[test]
    fn parse_declare_let() {
        insta::assert_debug_snapshot!(parse(r#"declare let foo: number"#));
        insta::assert_debug_snapshot!(parse(r#"declare let bar: fn () -> number"#));
    }

    #[test]
    fn parse_let_with_destructuring() {
        insta::assert_debug_snapshot!(parse(r#"let {x, y} = point"#));
    }

    #[test]
    fn parse_let_with_destructuring_and_type_annotation() {
        insta::assert_debug_snapshot!(parse(r#"let {x, y}: Point = point"#));
    }

    #[test]
    fn parse_assignment() {
        insta::assert_debug_snapshot!(parse(r#"y = m*x + b"#));
        insta::assert_debug_snapshot!(parse(r#"p.x = 5"#));
        insta::assert_debug_snapshot!(parse(r#"p["y"] = 10"#));
    }

    #[test]
    fn parse_conditionals() {
        insta::assert_debug_snapshot!(parse("let max = if (x > y) { x } else { y }"));
        insta::assert_debug_snapshot!(parse("if (foo) { console.log(foo) }"));
    }

    #[test]
    fn parse_lambda() {
        insta::assert_debug_snapshot!(parse("let add = fn (x, y) => x + y"));
        insta::assert_debug_snapshot!(parse("let add = fn (x) => fn (y) => x + y"));
    }

    #[test]
    fn parse_let_destructuring() {
        insta::assert_debug_snapshot!(parse("let {x, y} = point"));
        insta::assert_debug_snapshot!(parse("let {x: x1, y: y1} = p1"));
        insta::assert_debug_snapshot!(parse("let [p1, p2] = line"));
        insta::assert_debug_snapshot!(parse("let [head, ...tail] = polygon"));
    }

    #[test]
    fn parse_let_fn_with_fn_type() {
        insta::assert_debug_snapshot!(parse(
            r#"let add: fn (a: number, b: number) -> number = fn (a, b) => a + b"#
        ));
    }

    #[test]
    fn parse_one_liners() {
        insta::assert_debug_snapshot!(parse(r#"foo() bar()"#));
        insta::assert_debug_snapshot!(parse(r#"let add = fn(a, b) => a + b add(5, 10)"#));
    }

    #[test]
    fn parse_new_line_inference() {
        insta::assert_debug_snapshot!(parse("1 + \n2"));
        insta::assert_debug_snapshot!(parse("1 \n+ 2"));
        insta::assert_debug_snapshot!(parse("foo\n.bar()"));
        insta::assert_debug_snapshot!(parse("return\nfoo()"));
    }

    #[test]
    fn parse_type_alias() {
        insta::assert_debug_snapshot!(parse(r#"type Foo = Bar"#));
        insta::assert_debug_snapshot!(parse(r#"type Point<T> = {x: T, y: T}"#));
        insta::assert_debug_snapshot!(parse(
            r#"
            type ReturnType<T: fn (...args: Array<_>) -> _> = if (
                T: fn (...args: Array<_>) -> infer R
            ) { 
                R
            } else {
                never 
            }"#
        ));
        insta::assert_debug_snapshot!(parse(
            r#"type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string}"#
        ));
    }

    #[test]
    fn parse_var_decls() {
        insta::assert_debug_snapshot!(parse(r#"let mut p = {x: 5, y: 10}"#));
        insta::assert_debug_snapshot!(parse(r#"var i = 0"#));
        insta::assert_debug_snapshot!(parse(r#"var mut p = {x: 5, y: 10}"#));
        insta::assert_debug_snapshot!(parse(
            r#"declare let scale: fn (mut p: Point, scale: number) -> void"#
        ));
    }

    #[test]
    fn parse_for_loop() {
        insta::assert_debug_snapshot!(parse(
            r#"
            for ({x, y} in points) {
                console.log(`(${x}, ${y})`)
            }"#
        ));
    }

    #[test]
    fn parse_comments() {
        insta::assert_debug_snapshot!(parse(
            r#"
            let x = 5  // x-coord
            let y = 10 // y-coord
            "#
        ));

        insta::assert_debug_snapshot!(parse(
            r#"
            let make_point = fn (x: number, y: number) {
                // returns a point
                return {x, y}
            }
            "#
        ));
    }

    #[test]
    fn parse_typeof() {
        insta::assert_debug_snapshot!(parse("type RetType = GetReturnType<typeof foo>"));
    }

    #[test]
    fn parse_keyof() {
        insta::assert_debug_snapshot!(parse("type Pick<T, K : keyof T> = {[P]: T[P] for P in K}"));
    }

    #[test]
    fn parse_jsx() {
        insta::assert_debug_snapshot!(parse(
            r#"let button = <Button count={5} foo="bar"></Button>"#
        ));
    }
}
