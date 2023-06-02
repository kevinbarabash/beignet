use crate::expr::{BinaryOp, Expr, ExprKind, UnaryOp};
use crate::parser::Parser;
use crate::source_location::*;
use crate::stmt::Stmt;
use crate::stmt_parser::parse_stmt;
use crate::token::{Token, TokenKind};

fn get_prefix_precedence(op: &Token) -> Option<u8> {
    match &op.kind {
        TokenKind::Plus => Some(9),
        TokenKind::Minus => Some(9),
        _ => None,
    }
}

fn get_infix_precedence(op: &Token) -> Option<u8> {
    match &op.kind {
        // multiplicative
        TokenKind::Times => Some(7),
        TokenKind::Divide => Some(7),

        // additive
        TokenKind::Plus => Some(6),
        TokenKind::Minus => Some(6),

        // equality
        TokenKind::Equals => Some(3),
        TokenKind::LessThan => Some(3),
        TokenKind::LessThanOrEqual => Some(3),
        TokenKind::GreaterThan => Some(3),
        TokenKind::GreaterThanOrEqual => Some(3),

        // logic
        TokenKind::And => Some(2),
        TokenKind::Or => Some(1),
        _ => None,
    }
}

fn get_postfix_precedence(op: &Token) -> Option<u8> {
    // TODO: handle things like:
    // - dot member access
    // - square bracket member access
    // - parens for function calls
    match &op.kind {
        TokenKind::LeftBracket => Some(11),
        TokenKind::LeftParen => Some(11), // function call
        _ => None,
    }
}

fn parse_params(parser: &mut Parser) -> Vec<String> {
    let mut params = Vec::new();
    while parser.peek().kind != TokenKind::RightParen {
        let param = parser.next();
        if let TokenKind::Identifier(name) = param.kind {
            params.push(name.to_owned());
        } else {
            panic!("Expected identifier, got {:?}", param);
        }

        match parser.peek().kind {
            TokenKind::RightParen => break,
            TokenKind::Comma => {
                parser.next();
            }
            _ => panic!("Expected comma or right paren, got {:?}", parser.peek()),
        }
    }
    params
}

fn parse_block(parser: &mut Parser) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    while parser.peek().kind != TokenKind::RightBrace {
        stmts.push(parse_stmt(parser));
    }
    stmts
}

fn parse_expr_with_precedence(parser: &mut Parser, precedence: u8) -> Expr {
    let next = parser.next();

    let mut lhs = match &next.kind {
        TokenKind::Number(n) => Expr {
            kind: ExprKind::Number(n.to_owned()),
            loc: next.loc.clone(),
        },
        TokenKind::Identifier(id) => Expr {
            kind: ExprKind::Identifier(id.to_owned()),
            loc: next.loc.clone(),
        },
        TokenKind::LeftParen => {
            let lhs = parse_expr_with_precedence(parser, 0);
            assert_eq!(parser.next().kind, TokenKind::RightParen);
            lhs
        }
        TokenKind::Fn => {
            let name = parser.next();
            assert!(matches!(name.kind, TokenKind::Identifier(_)));
            assert_eq!(parser.next().kind, TokenKind::LeftParen);
            let params = parse_params(parser);
            assert_eq!(parser.next().kind, TokenKind::RightParen);
            assert_eq!(parser.next().kind, TokenKind::LeftBrace);
            let body = parse_block(parser);
            let close_brace = parser.next();
            assert_eq!(close_brace.kind, TokenKind::RightBrace);

            let loc = merge_locations(&next.loc, &close_brace.loc);
            Expr {
                kind: ExprKind::Function { params, body },
                loc,
            }
        }
        t => match get_prefix_precedence(&next) {
            Some(precendence) => {
                let op = match t {
                    TokenKind::Plus => UnaryOp::Plus,
                    TokenKind::Minus => UnaryOp::Minus,
                    _ => panic!("unexpected token: {:?}", t),
                };

                let rhs = parse_expr_with_precedence(parser, precendence);
                let loc = merge_locations(&next.loc, &rhs.loc);
                Expr {
                    kind: ExprKind::Unary {
                        op,
                        right: Box::new(rhs),
                    },
                    loc,
                }
            }
            None => panic!("unexpected token: {:?}", t),
        },
    };

    loop {
        let next = parser.peek();
        if let TokenKind::Eof = next.kind {
            break;
        }

        if let Some(next_precedence) = get_postfix_precedence(&next) {
            if next_precedence < precedence {
                break;
            }

            parser.next();

            lhs = match &next.kind {
                TokenKind::LeftBracket => {
                    let rhs = parse_expr_with_precedence(parser, 0);
                    let loc = merge_locations(&lhs.loc, &rhs.loc);
                    assert_eq!(parser.next().kind, TokenKind::RightBracket);
                    Expr {
                        kind: ExprKind::Index {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        },
                        loc,
                    }
                }
                TokenKind::LeftParen => {
                    let mut args = Vec::new();
                    while parser.peek().kind != TokenKind::RightParen {
                        args.push(parse_expr_with_precedence(parser, 0));

                        match parser.peek().kind {
                            TokenKind::RightParen => break,
                            TokenKind::Comma => {
                                parser.next();
                            }
                            _ => panic!("Expected comma or right paren, got {:?}", parser.peek()),
                        }
                    }
                    let loc = merge_locations(&lhs.loc, &parser.peek().loc);
                    assert_eq!(parser.next().kind, TokenKind::RightParen);
                    Expr {
                        kind: ExprKind::Call {
                            callee: Box::new(lhs),
                            args,
                        },
                        loc,
                    }
                }
                _ => panic!("unexpected token: {:?}", next),
            };

            continue;
        }

        if let Some(next_precedence) = get_infix_precedence(&next) {
            // '<' produces right associativity
            // '<=' produces left associativity
            if next_precedence <= precedence {
                break;
            }

            parser.next();

            let op: BinaryOp = match &next.kind {
                TokenKind::Plus => BinaryOp::Plus,
                TokenKind::Minus => BinaryOp::Minus,
                TokenKind::Times => BinaryOp::Times,
                TokenKind::Divide => BinaryOp::Divide,
                TokenKind::Equals => BinaryOp::Equals,
                TokenKind::NotEquals => BinaryOp::NotEquals,
                TokenKind::LessThan => BinaryOp::LessThan,
                TokenKind::LessThanOrEqual => BinaryOp::LessThanOrEqual,
                TokenKind::GreaterThan => BinaryOp::GreaterThan,
                TokenKind::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
                TokenKind::And => BinaryOp::And,
                TokenKind::Or => BinaryOp::Or,
                _ => panic!("unexpected token: {:?}", next),
            };

            let rhs = parse_expr_with_precedence(parser, next_precedence);
            let loc = merge_locations(&lhs.loc, &rhs.loc);

            lhs = Expr {
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                },
                loc,
            };

            continue;
        }

        break;
    }

    lhs
}

pub fn parse_expr(parser: &mut Parser) -> Expr {
    parse_expr_with_precedence(parser, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    pub fn parse(input: &str) -> Expr {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        parse_expr(&mut parser)
    }

    #[test]
    fn parse_simple_addition() {
        insta::assert_debug_snapshot!(parse("1 + 2 + 3"));
    }

    #[test]
    fn parse_addition_and_subtraction() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3 + 4"));
    }

    #[test]
    fn parse_additive_and_multiplicative() {
        insta::assert_debug_snapshot!(parse("1 * 2 + 3"));
    }

    #[test]
    fn parse_parens() {
        insta::assert_debug_snapshot!(parse("5 * (x + 1)"));
    }

    #[test]
    fn parse_comparisons_and_logic() {
        insta::assert_debug_snapshot!(parse("a > b && c >= d || e < f && g <= h"));
    }

    #[test]
    fn parse_unary_operators() {
        insta::assert_debug_snapshot!(parse("--a - +b"));
    }

    #[test]
    fn parse_indexing() {
        insta::assert_debug_snapshot!(parse("a[1][c]"));
    }

    #[test]
    fn parse_function() {
        let src = r#"fn add() { let x = 5; let y = 10; return x + y; }"#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn parse_function_with_params() {
        let src = r#"fn add(x, y) { return x + y; }"#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn parse_function_call() {
        let src = r#"add(5 * i, 10 * j)"#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn parse_call_expr() {
        let src = r#"foo[bar](5, 10)"#;
        insta::assert_debug_snapshot!(parse(src));
    }
}
