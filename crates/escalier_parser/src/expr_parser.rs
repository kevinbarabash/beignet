use crate::expr::{BinaryOp, Expr, ExprKind, UnaryOp};
use crate::lexer::Lexer;
use crate::source_location::*;
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn next(&mut self) -> Token {
        let result = self.peek();
        self.cursor += 1;
        result
    }
    fn peek(&mut self) -> Token {
        self.tokens.get(self.cursor).cloned().unwrap_or(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        })
    }
}

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
        _ => None,
    }
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

        eprintln!(
            "get_postfix_precedence(&next) = {:#?}",
            get_postfix_precedence(&next)
        );
        eprintln!("next = {:#?}", next);

        if let Some(next_precedence) = get_postfix_precedence(&next) {
            eprintln!("next_precedence = {}", next_precedence);
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

pub fn parse_expr(tokens: Vec<Token>) -> Expr {
    let mut parser = Parser::new(tokens);
    parse_expr_with_precedence(&mut parser, 0)
}

pub fn parse(input: &str) -> Expr {
    let mut lexer = Lexer::new(input);
    let ast = lexer.lex();
    eprintln!("{:#?}", ast);
    parse_expr(ast)
}
