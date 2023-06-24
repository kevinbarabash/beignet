use crate::parser::*;
use crate::span::merge_spans;
use crate::stmt::{Stmt, StmtKind};
use crate::token::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Stmt {
        let token = self.peek().unwrap_or(&EOF).clone();

        match &token.kind {
            TokenKind::Let => {
                self.next().unwrap_or(EOF.clone());
                let pattern = self.parse_pattern();

                let type_ann = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::Colon => {
                        self.next().unwrap_or(EOF.clone());
                        Some(self.parse_type_ann())
                    }
                    _ => None,
                };

                assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Assign);
                let expr = self.parse_expr();
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::Semicolon
                );

                let span = merge_spans(&token.span, &expr.get_span());
                Stmt {
                    kind: StmtKind::Let {
                        pattern,
                        expr,
                        type_ann,
                    },
                    span,
                }
            }
            TokenKind::Return => {
                self.next().unwrap_or(EOF.clone());
                let next = self.peek().unwrap_or(&EOF).clone();
                match next.kind {
                    TokenKind::Semicolon => {
                        self.next().unwrap_or(EOF.clone());
                        Stmt {
                            kind: StmtKind::Return { arg: None },
                            span: merge_spans(&token.span, &next.span),
                        }
                    }
                    _ => {
                        let arg = self.parse_expr();
                        assert_eq!(
                            self.next().unwrap_or(EOF.clone()).kind,
                            TokenKind::Semicolon
                        );

                        let span = merge_spans(&next.span, &arg.get_span());
                        Stmt {
                            kind: StmtKind::Return { arg: Some(arg) },
                            span,
                        }
                    }
                }
            }
            _ => {
                eprintln!("--- parse_stmt (expr) ---");
                let expr = self.parse_expr();
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::Semicolon
                );
                eprintln!("--- parse_stmt (assert semicolon) ---");

                let span = expr.get_span();
                Stmt {
                    kind: StmtKind::Expr { expr },
                    span,
                }
            }
        }
    }

    pub fn parse_program(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != TokenKind::Eof {
            stmts.push(self.parse_stmt());
        }
        stmts
    }
}

pub fn parse(input: &str) -> Vec<Stmt> {
    let mut parser = Parser::new(input);
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_statement() {
        let input = "let x = 5;";
        let stmts = parse(input);
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn single_variable_expression() {
        let input = "x;";
        let stmts = parse(input);
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn multiple_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        x + y;
        return; 
        "#;

        let stmts = parse(input);

        assert_eq!(stmts.len(), 4);
    }

    #[test]
    fn parse_let() {
        insta::assert_debug_snapshot!(parse(r#"let y = m*x + b;"#));
    }

    #[test]
    fn parse_let_with_type_annotation() {
        insta::assert_debug_snapshot!(parse(r#"let y: number = m*x + b;"#));
    }

    #[test]
    fn parse_let_with_destructuring() {
        insta::assert_debug_snapshot!(parse(r#"let {x, y} = point;"#));
    }

    #[test]
    fn parse_let_with_destructuring_and_type_annotation() {
        insta::assert_debug_snapshot!(parse(r#"let {x, y}: Point = point;"#));
    }

    #[test]
    fn parse_assignment() {
        insta::assert_debug_snapshot!(parse(r#"y = m*x + b;"#));
        insta::assert_debug_snapshot!(parse(r#"p.x = 5;"#));
        insta::assert_debug_snapshot!(parse(r#"p["y"] = 10;"#));
    }

    #[test]
    fn parse_conditionals() {
        insta::assert_debug_snapshot!(parse("let max = if (x > y) { x; } else { y; };"));
        insta::assert_debug_snapshot!(parse("if (foo) { console.log(foo); };"));
    }

    #[test]
    fn parse_lambda() {
        insta::assert_debug_snapshot!(parse("let add = fn (x, y) => x + y;"));
        insta::assert_debug_snapshot!(parse("let add = fn (x) => fn (y) => x + y;"));
    }

    #[test]
    fn parse_let_destructuring() {
        insta::assert_debug_snapshot!(parse("let {x, y} = point;"));
        insta::assert_debug_snapshot!(parse("let {x: x1, y: y1} = p1;"));
        insta::assert_debug_snapshot!(parse("let [p1, p2] = line;"));
        insta::assert_debug_snapshot!(parse("let [head, ...tail] = polygon;"));
    }

    #[test]
    fn parse_let_fn_with_fn_type() {
        insta::assert_debug_snapshot!(parse(
            r#"let add: fn (a: number, b: number) => number = fn (a, b) => a + b;"#
        ));
    }
}
