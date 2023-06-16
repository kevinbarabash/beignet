use crate::expr_parser::parse_expr;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::pattern_parser::parse_pattern;
use crate::source_location::merge_locations;
use crate::stmt::{Stmt, StmtKind};
use crate::token::TokenKind;
use crate::type_ann_parser::parse_type_ann;

pub fn parse_stmt(parser: &mut Parser) -> Stmt {
    let token = parser.peek(0);
    // let next = parser.peek(1);

    match &token.kind {
        TokenKind::Let => {
            parser.next();
            let pattern = parse_pattern(parser);

            let type_ann = match parser.peek(0).kind {
                TokenKind::Colon => {
                    parser.next();
                    Some(parse_type_ann(parser))
                }
                _ => None,
            };

            assert_eq!(parser.next().kind, TokenKind::Assign);
            let expr = parse_expr(parser);
            assert_eq!(parser.next().kind, TokenKind::Semicolon);

            let loc = merge_locations(&token.loc, &expr.loc);
            Stmt {
                kind: StmtKind::Let {
                    pattern,
                    expr,
                    type_ann,
                },
                loc,
            }
        }
        TokenKind::Return => {
            parser.next();
            let next = parser.peek(0);
            match next.kind {
                TokenKind::Semicolon => {
                    parser.next();
                    Stmt {
                        kind: StmtKind::Return { arg: None },
                        loc: merge_locations(&token.loc, &next.loc),
                    }
                }
                _ => {
                    let arg = parse_expr(parser);
                    assert_eq!(parser.next().kind, TokenKind::Semicolon);

                    let loc = merge_locations(&next.loc, &arg.loc);
                    Stmt {
                        kind: StmtKind::Return { arg: Some(arg) },
                        loc,
                    }
                }
            }
        }
        _ => {
            let expr = parse_expr(parser);
            assert_eq!(parser.next().kind, TokenKind::Semicolon);

            let loc = expr.loc.clone();
            Stmt {
                kind: StmtKind::Expr { expr },
                loc,
            }
        }
    }
}

pub fn parse_program(parser: &mut Parser) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    while parser.peek(0).kind != TokenKind::Eof {
        stmts.push(parse_stmt(parser));
    }
    stmts
}

pub fn parse(input: &str) -> Vec<Stmt> {
    let tokens = Lexer::new(input).lex();
    let mut parser = Parser::new(tokens);
    parse_program(&mut parser)
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

    // TODO: support assignment separate from let decls
    #[test]
    #[ignore]
    fn parse_assignment() {
        insta::assert_debug_snapshot!(parse(r#"y = m*x + b;"#));
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
