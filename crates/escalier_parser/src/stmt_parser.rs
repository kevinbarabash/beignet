use crate::expr_parser::parse_expr;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::source_location::merge_locations;
use crate::stmt::{Stmt, StmtKind};
use crate::token::TokenKind;

pub fn parse_stmt(parser: &mut Parser) -> Stmt {
    let token = parser.next();
    let next = parser.peek();

    match (&token.kind, &next.kind) {
        (TokenKind::Let, TokenKind::Identifier(id)) => {
            let name = id.to_owned();
            parser.next();
            assert_eq!(parser.next().kind, TokenKind::Assign);
            let expr = parse_expr(parser);
            assert_eq!(parser.next().kind, TokenKind::Semicolon);

            let loc = merge_locations(&token.loc, &expr.loc);
            Stmt {
                kind: StmtKind::Let { name, expr },
                loc,
            }
        }
        (TokenKind::Return, TokenKind::Semicolon) => {
            parser.next();
            Stmt {
                kind: StmtKind::Return { arg: None },
                loc: merge_locations(&token.loc, &next.loc),
            }
        }
        (TokenKind::Return, _) => {
            let arg = parse_expr(parser);
            assert_eq!(parser.next().kind, TokenKind::Semicolon);

            let loc = merge_locations(&next.loc, &arg.loc);
            Stmt {
                kind: StmtKind::Return { arg: Some(arg) },
                loc,
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
    while parser.peek().kind != TokenKind::Eof {
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
}
