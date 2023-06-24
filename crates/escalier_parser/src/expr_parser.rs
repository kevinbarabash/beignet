// use std::iter::Peekable;

use crate::expr::*;
use crate::literal::Literal;
use crate::parser::*;
use crate::precedence::{Associativity, Operator, PRECEDENCE_TABLE};
use crate::source_location::*;
use crate::token::*;

fn get_prefix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        TokenKind::Plus => PRECEDENCE_TABLE.get(&Operator::UnaryPlus).cloned(),
        TokenKind::Minus => PRECEDENCE_TABLE.get(&Operator::UnaryMinus).cloned(),
        _ => None,
    }
}

fn get_infix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        // multiplicative
        TokenKind::Times => PRECEDENCE_TABLE.get(&Operator::Multiplication).cloned(),
        TokenKind::Divide => PRECEDENCE_TABLE.get(&Operator::Division).cloned(),
        TokenKind::Modulo => PRECEDENCE_TABLE.get(&Operator::Remainder).cloned(),

        // additive
        TokenKind::Plus => PRECEDENCE_TABLE.get(&Operator::Addition).cloned(),
        TokenKind::Minus => PRECEDENCE_TABLE.get(&Operator::Subtraction).cloned(),

        // equality
        TokenKind::Equals => PRECEDENCE_TABLE.get(&Operator::Equals).cloned(),
        TokenKind::NotEquals => PRECEDENCE_TABLE.get(&Operator::NotEquals).cloned(),
        TokenKind::LessThan => PRECEDENCE_TABLE.get(&Operator::LessThan).cloned(),
        TokenKind::LessThanOrEqual => PRECEDENCE_TABLE.get(&Operator::LessThanOrEqual).cloned(),
        TokenKind::GreaterThan => PRECEDENCE_TABLE.get(&Operator::GreaterThan).cloned(),
        TokenKind::GreaterThanOrEqual => {
            PRECEDENCE_TABLE.get(&Operator::GreaterThanOrEqual).cloned()
        }

        // logic
        TokenKind::And => PRECEDENCE_TABLE.get(&Operator::LogicalAnd).cloned(),
        TokenKind::Or => PRECEDENCE_TABLE.get(&Operator::LogicalOr).cloned(),

        // assignment
        TokenKind::Assign => PRECEDENCE_TABLE.get(&Operator::Assignment).cloned(),
        TokenKind::PlusAssign => PRECEDENCE_TABLE.get(&Operator::Assignment).cloned(),
        TokenKind::MinusAssign => PRECEDENCE_TABLE.get(&Operator::Assignment).cloned(),
        TokenKind::TimesAssign => PRECEDENCE_TABLE.get(&Operator::Assignment).cloned(),
        TokenKind::DivideAssign => PRECEDENCE_TABLE.get(&Operator::Assignment).cloned(),
        TokenKind::ModuloAssign => PRECEDENCE_TABLE.get(&Operator::Assignment).cloned(),
        _ => None,
    }
}

fn get_postfix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        TokenKind::LeftBracket => PRECEDENCE_TABLE
            .get(&Operator::ComputedMemberAccess)
            .cloned(),
        TokenKind::LeftParen => PRECEDENCE_TABLE.get(&Operator::FunctionCall).cloned(),
        TokenKind::Dot => PRECEDENCE_TABLE.get(&Operator::MemberAccess).cloned(),
        TokenKind::QuestionDot => PRECEDENCE_TABLE.get(&Operator::OptionalChaining).cloned(),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    // consumes leading '{' and trailing '}' tokens
    fn parse_block(&mut self) -> Block {
        let open = self.next().unwrap_or(EOF.clone());
        assert_eq!(open.kind, TokenKind::LeftBrace);
        let mut stmts = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
            stmts.push(self.parse_stmt());
        }
        let close = self.next().unwrap_or(EOF.clone());
        assert_eq!(close.kind, TokenKind::RightBrace);
        let loc = merge_locations(&open.loc, &close.loc);

        Block { loc, stmts }
    }

    fn parse_atom(&mut self) -> Expr {
        let first = self.peek().unwrap_or(&EOF).clone();

        let lhs = match &first.kind {
            TokenKind::NumLit(n) => {
                self.next(); // consume number
                Expr {
                    kind: ExprKind::Literal(Literal::Number(n.to_owned())),
                    loc: first.loc.clone(),
                }
            }
            TokenKind::Identifier(id) => {
                self.next(); // consume identifier
                Expr {
                    kind: ExprKind::Identifier(id.to_owned()),
                    loc: first.loc.clone(),
                }
            }
            TokenKind::BoolLit(b) => {
                self.next(); // consume boolean
                Expr {
                    kind: ExprKind::Literal(Literal::Boolean(*b)),
                    loc: first.loc.clone(),
                }
            }
            TokenKind::StrLit(s) => {
                self.next(); // consume string
                Expr {
                    kind: ExprKind::Literal(Literal::String(s.to_owned())),
                    loc: first.loc.clone(),
                }
            }
            TokenKind::StrTemplateLit { parts, exprs } => {
                self.next(); // consume string template
                Expr {
                    kind: ExprKind::TemplateLiteral {
                        parts: parts
                            .iter()
                            .map(|s| match &s.kind {
                                TokenKind::StrLit(s) => Literal::String(s.to_owned()),
                                _ => panic!("Expected string literal, got {:?}", s),
                            })
                            .collect(),
                        exprs: exprs.to_owned(),
                    },
                    loc: first.loc.clone(),
                }
            }
            TokenKind::Null => {
                self.next(); // consume 'null'
                Expr {
                    kind: ExprKind::Literal(Literal::Null),
                    loc: first.loc.clone(),
                }
            }
            TokenKind::Undefined => {
                self.next(); // consume 'undefined'
                Expr {
                    kind: ExprKind::Literal(Literal::Undefined),
                    loc: first.loc.clone(),
                }
            }
            TokenKind::LeftParen => {
                self.next(); // consume '('
                let lhs = self.parse_expr_with_precedence(0);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightParen
                );
                lhs
            }
            TokenKind::LeftBracket => {
                self.next(); // consumes '['
                let start = first;
                let mut elements: Vec<ExprOrSpread> = Vec::new();
                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBracket {
                    let elem = match self.peek().unwrap_or(&EOF).kind {
                        TokenKind::DotDotDot => {
                            self.next().unwrap_or(EOF.clone()); // consumes `...`
                            let expr = self.parse_expr_with_precedence(0);
                            ExprOrSpread::Spread(expr)
                        }
                        _ => {
                            let expr = self.parse_expr_with_precedence(0);
                            ExprOrSpread::Expr(expr)
                        }
                    };

                    elements.push(elem);

                    match self.peek().unwrap_or(&EOF).kind {
                        TokenKind::RightBracket => break,
                        TokenKind::Comma => {
                            self.next().unwrap_or(EOF.clone());
                        }
                        _ => panic!(
                            "Expected comma or right bracket, got {:?}",
                            self.peek().unwrap_or(&EOF)
                        ),
                    }
                }

                assert_eq!(self.peek().unwrap_or(&EOF).kind, TokenKind::RightBracket);

                let end = self.next().unwrap_or(EOF.clone());

                Expr {
                    kind: ExprKind::Tuple { elements },
                    loc: merge_locations(&start.loc, &end.loc),
                }
            }
            TokenKind::LeftBrace => {
                self.next(); // consumes '{'
                let start = first;
                let mut properties: Vec<PropOrSpread> = Vec::new();
                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                    let next = self.next().unwrap_or(EOF.clone());

                    let prop = match &next.kind {
                        TokenKind::DotDotDot => {
                            let expr = self.parse_expr_with_precedence(0);
                            PropOrSpread::Spread(expr)
                        }
                        TokenKind::Identifier(id)
                            if self.peek().unwrap_or(&EOF).kind == TokenKind::Comma
                                || self.peek().unwrap_or(&EOF).kind == TokenKind::RightBrace =>
                        {
                            PropOrSpread::Prop(Prop::Shorthand { key: id.to_owned() })
                        }
                        _ => {
                            let key = match &next.kind {
                                TokenKind::Identifier(id) => ObjectKey::Identifier(id.to_owned()),
                                TokenKind::StrLit(s) => ObjectKey::String(s.to_owned()),
                                TokenKind::NumLit(n) => ObjectKey::Number(n.to_owned()),
                                TokenKind::LeftBracket => {
                                    let expr = self.parse_expr_with_precedence(0);
                                    assert_eq!(
                                        self.next().unwrap_or(EOF.clone()).kind,
                                        TokenKind::RightBracket
                                    );
                                    ObjectKey::Computed(Box::new(expr))
                                }
                                _ => {
                                    panic!("Expected identifier or string literal, got {:?}", next)
                                }
                            };

                            assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);

                            let value = self.parse_expr();

                            PropOrSpread::Prop(Prop::Property { key, value })
                        }
                    };

                    properties.push(prop);

                    match self.peek().unwrap_or(&EOF).kind {
                        TokenKind::RightBrace => break,
                        TokenKind::Comma => {
                            self.next().unwrap_or(EOF.clone());
                        }
                        _ => panic!(
                            "Expected comma or right brace, got {:?}",
                            self.peek().unwrap_or(&EOF)
                        ),
                    }
                }

                let end = self.next().unwrap_or(EOF.clone());

                Expr {
                    kind: ExprKind::Object { properties },
                    loc: merge_locations(&start.loc, &end.loc),
                }
            }
            TokenKind::Fn => {
                self.next(); // consumes 'fn'
                let params = self.parse_params();

                let type_ann = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::Colon => {
                        self.next().unwrap_or(EOF.clone());
                        Some(self.parse_type_ann())
                    }
                    _ => None,
                };

                assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);

                match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::LeftBrace => {
                        let block = self.parse_block();
                        let loc = merge_locations(&first.loc, &block.loc);
                        let body = BlockOrExpr::Block(block);

                        Expr {
                            kind: ExprKind::Function {
                                params,
                                body,
                                type_ann,
                            },
                            loc,
                        }
                    }
                    _ => {
                        let expr = self.parse_expr();
                        let loc = merge_locations(&first.loc, &expr.loc);
                        let body = BlockOrExpr::Expr(Box::new(expr));

                        Expr {
                            kind: ExprKind::Function {
                                params,
                                body,
                                type_ann,
                            },
                            loc,
                        }
                    }
                }
            }
            TokenKind::If => {
                self.next(); // consumes 'if'
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftParen
                );
                let cond = self.parse_expr();
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightParen
                );
                let consequent = self.parse_block();

                if self.peek().unwrap_or(&EOF).kind == TokenKind::Else {
                    self.next().unwrap_or(EOF.clone());
                    let alternate = self.parse_block();
                    let loc = merge_locations(&first.loc, &alternate.loc);
                    Expr {
                        kind: ExprKind::IfElse {
                            cond: Box::new(cond),
                            consequent,
                            alternate: Some(alternate),
                        },
                        loc,
                    }
                } else {
                    let loc = merge_locations(&first.loc, &consequent.loc);
                    Expr {
                        kind: ExprKind::IfElse {
                            cond: Box::new(cond),
                            consequent,
                            alternate: None,
                        },
                        loc,
                    }
                }
            }
            TokenKind::Match => {
                // TODO: make 'match' use parens to align with 'if'
                self.next(); // consumes 'match'
                let expr = self.parse_expr();
                let mut loc = expr.loc.clone();
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftBrace
                );
                let mut arms: Vec<MatchArm> = Vec::new();
                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
                    let pattern = self.parse_pattern();
                    assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);

                    let (body, end) = match self.peek().unwrap_or(&EOF).kind {
                        TokenKind::LeftBrace => {
                            let block = self.parse_block();
                            let loc = block.loc.clone();
                            (BlockOrExpr::Block(block), loc)
                        }
                        _ => {
                            let expr = self.parse_expr();
                            let loc = expr.loc.clone();
                            (BlockOrExpr::Expr(Box::new(expr)), loc)
                        }
                    };

                    assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Comma);

                    arms.push(MatchArm {
                        loc: merge_locations(&pattern.loc, &end),
                        pattern,
                        guard: None,
                        body,
                    });
                    loc = merge_locations(&loc, &end);
                }

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBrace
                );

                Expr {
                    kind: ExprKind::Match {
                        expr: Box::new(expr),
                        arms,
                    },
                    loc,
                }
            }
            TokenKind::Try => {
                self.next(); // consumes 'try'
                let try_body = self.parse_block();

                match self.next().unwrap_or(EOF.clone()).kind {
                    TokenKind::Catch => {
                        assert_eq!(
                            self.next().unwrap_or(EOF.clone()).kind,
                            TokenKind::LeftParen
                        );
                        let error = self.parse_pattern();
                        assert_eq!(
                            self.next().unwrap_or(EOF.clone()).kind,
                            TokenKind::RightParen
                        );

                        let catch_body = self.parse_block();

                        match self.peek().unwrap_or(&EOF).kind {
                            TokenKind::Finally => {
                                self.next().unwrap_or(EOF.clone());
                                let finally_body = self.parse_block();
                                let loc = merge_locations(&first.loc, &finally_body.loc);

                                Expr {
                                    kind: ExprKind::Try {
                                        body: try_body,
                                        catch: Some(CatchClause {
                                            param: Some(error),
                                            body: catch_body,
                                        }),
                                        finally: Some(finally_body),
                                    },
                                    loc,
                                }
                            }
                            _ => {
                                let loc = merge_locations(&first.loc, &catch_body.loc);

                                Expr {
                                    kind: ExprKind::Try {
                                        body: try_body,
                                        catch: Some(CatchClause {
                                            param: Some(error),
                                            body: catch_body,
                                        }),
                                        finally: None,
                                    },
                                    loc,
                                }
                            }
                        }
                    }
                    TokenKind::Finally => {
                        let finally_body = self.parse_block();
                        let loc = merge_locations(&first.loc, &finally_body.loc);

                        Expr {
                            kind: ExprKind::Try {
                                body: try_body,
                                catch: None,
                                finally: Some(finally_body),
                            },
                            loc,
                        }
                    }
                    _ => {
                        panic!("expected catch or finally");
                    }
                }

                // assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Catch);
                // assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::LeftParen);
                // let error = parse_pattern(parser);
                // assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::RightParen);
                // let catch_body = parse_block(parser); // TODO: create a BlockStmt and include .loc in it
            }
            TokenKind::Do => {
                self.next(); // consumes 'do'
                let body = self.parse_block();
                let loc = merge_locations(&first.loc, &body.loc);

                Expr {
                    kind: ExprKind::Do { body },
                    loc,
                }
            }
            TokenKind::LessThan => {
                // TODO: this is wrong
                let loc = first.loc.clone();

                // HACK: We use self.scanner.peek() to lookahead further than
                // self.peek() will allow.  The reason why this is scanner.peek(0)
                // and not scanner.peek(1) is because the call to self.peek() at
                // the top of the method has already advanced the scanner's position.
                match self.scanner.peek(0) {
                    Some('>') => Expr {
                        kind: ExprKind::JSXFragment(self.parse_jsx_fragment()),
                        loc,
                    },
                    _ => Expr {
                        kind: ExprKind::JSXElement(self.parse_jsx_element()),
                        loc,
                    },
                }
            }
            t => {
                self.next(); // consume the token
                match get_prefix_precedence(&first) {
                    Some(precendence) => {
                        let op = match t {
                            TokenKind::Plus => UnaryOp::Plus,
                            TokenKind::Minus => UnaryOp::Minus,
                            _ => panic!("unexpected token: {:?}", t),
                        };
                        let rhs = self.parse_expr_with_precedence(precendence.0);
                        let loc = merge_locations(&first.loc, &rhs.loc);
                        Expr {
                            kind: ExprKind::Unary {
                                op,
                                right: Box::new(rhs),
                            },
                            loc,
                        }
                    }
                    None => panic!("unexpected token: {:?}", first),
                }
            }
        };

        lhs
    }

    fn parse_expr_with_precedence(&mut self, precedence: u8) -> Expr {
        let mut lhs = self.parse_atom();

        loop {
            let next = self.peek().unwrap_or(&EOF).clone();
            if let TokenKind::Eof = next.kind {
                return lhs;
            }

            if let TokenKind::Semicolon = next.kind {
                return lhs;
            }

            if let Some(next_precedence) = get_postfix_precedence(&next) {
                if precedence >= next_precedence.0 {
                    return lhs;
                }

                lhs = self.parse_postfix(lhs, next_precedence);

                continue;
            }

            if let Some(next_precedence) = get_infix_precedence(&next) {
                if precedence >= next_precedence.0 {
                    return lhs;
                }

                self.next().unwrap_or(EOF.clone());

                let op: Option<AssignOp> = match &next.kind {
                    TokenKind::Assign => Some(AssignOp::Assign),
                    TokenKind::PlusAssign => Some(AssignOp::AddAssign),
                    TokenKind::MinusAssign => Some(AssignOp::SubAssign),
                    TokenKind::TimesAssign => Some(AssignOp::MulAssign),
                    TokenKind::DivideAssign => Some(AssignOp::DivAssign),
                    TokenKind::ModuloAssign => Some(AssignOp::ModAssign),
                    _ => None,
                };

                if let Some(op) = op {
                    if !is_lvalue(&lhs) {
                        panic!("expected lvalue");
                    }

                    let precedence = if next_precedence.1 == Associativity::Left {
                        next_precedence.0
                    } else {
                        next_precedence.0 - 1
                    };

                    let rhs = self.parse_expr_with_precedence(precedence);
                    let loc = merge_locations(&lhs.loc, &rhs.loc);

                    lhs = Expr {
                        kind: ExprKind::Assign {
                            op,
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        },
                        loc,
                    };

                    continue;
                }

                let op: BinaryOp = match &next.kind {
                    TokenKind::Plus => BinaryOp::Plus,
                    TokenKind::Minus => BinaryOp::Minus,
                    TokenKind::Times => BinaryOp::Times,
                    TokenKind::Divide => BinaryOp::Divide,
                    TokenKind::Modulo => BinaryOp::Modulo,
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

                let precedence = if next_precedence.1 == Associativity::Left {
                    next_precedence.0
                } else {
                    next_precedence.0 - 1
                };

                let rhs = self.parse_expr_with_precedence(precedence);
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

            return lhs;
        }

        lhs
    }

    fn parse_postfix(&mut self, lhs: Expr, next_precedence: (u8, Associativity)) -> Expr {
        let precedence = if next_precedence.1 == Associativity::Left {
            next_precedence.0
        } else {
            next_precedence.0 - 1
        };

        let next = self.next().unwrap_or(EOF.clone());

        match &next.kind {
            TokenKind::LeftBracket => {
                let rhs = self.parse_expr();
                let loc = merge_locations(&lhs.loc, &rhs.loc);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBracket
                );
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
                while self.peek().unwrap_or(&EOF).kind != TokenKind::RightParen {
                    args.push(self.parse_expr());

                    match self.peek().unwrap_or(&EOF).kind {
                        TokenKind::RightParen => break,
                        TokenKind::Comma => {
                            self.next().unwrap_or(EOF.clone());
                        }
                        _ => panic!(
                            "Expected comma or right paren, got {:?}",
                            self.peek().unwrap_or(&EOF)
                        ),
                    }
                }
                let loc = merge_locations(&lhs.loc, &self.peek().unwrap_or(&EOF).loc);
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightParen
                );
                Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(lhs),
                        args,
                    },
                    loc,
                }
            }
            TokenKind::Dot => {
                let rhs = self.parse_expr_with_precedence(precedence);
                let loc = merge_locations(&lhs.loc, &rhs.loc);
                Expr {
                    kind: ExprKind::Member {
                        object: Box::new(lhs),
                        property: Box::new(rhs),
                    },
                    loc,
                }
            }
            TokenKind::QuestionDot => {
                let lhs_loc = lhs.loc.clone();

                let base = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::LeftParen | TokenKind::LeftBracket => {
                        self.parse_postfix(lhs, next_precedence)
                    }
                    _ => {
                        let rhs = self.parse_expr_with_precedence(precedence);
                        let loc = merge_locations(&lhs.loc, &rhs.loc);
                        Expr {
                            kind: ExprKind::Member {
                                object: Box::new(lhs),
                                property: Box::new(rhs),
                            },
                            loc,
                        }
                    }
                };

                let loc = merge_locations(&lhs_loc, &base.loc);

                Expr {
                    kind: ExprKind::OptionalChain {
                        base: Box::new(base),
                    },
                    loc,
                }
            }
            _ => panic!("unexpected token: {:?}", next),
        }
    }

    pub fn parse_expr(&mut self) -> Expr {
        self.parse_expr_with_precedence(0)
    }
}

fn is_lvalue(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Identifier(_) => true,
        ExprKind::Member { object, .. } => is_lvalue(object),
        ExprKind::Index { left, .. } => is_lvalue(left),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    pub fn parse(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.parse_expr()
    }

    #[test]
    fn parse_literals() {
        insta::assert_debug_snapshot!(parse("123"));
        insta::assert_debug_snapshot!(parse("true"));
        insta::assert_debug_snapshot!(parse("false"));
        insta::assert_debug_snapshot!(parse("null"));
        insta::assert_debug_snapshot!(parse("undefined"));
        insta::assert_debug_snapshot!(parse(r#""hello""#));
    }

    #[test]
    fn parse_tuple_literals() {
        insta::assert_debug_snapshot!(parse("[]"));
        insta::assert_debug_snapshot!(parse("[1]"));
        insta::assert_debug_snapshot!(parse("[1, 2]"));
        insta::assert_debug_snapshot!(parse("[1, 2,]"));
        insta::assert_debug_snapshot!(parse(r#"[1, "two", [3]]"#));
        insta::assert_debug_snapshot!(parse("[a, b, ...c]"));
        insta::assert_debug_snapshot!(parse("[...a, ...b, ...c]"));
    }

    #[test]
    #[should_panic]
    fn parse_tuple_literals_missing_comma() {
        parse("[1 2]");
    }

    #[test]
    #[should_panic]
    fn parse_tuple_literals_missing_right_brace() {
        parse("[1, 2");
    }

    #[test]
    fn parse_object_literals() {
        insta::assert_debug_snapshot!(parse("{}"));
        insta::assert_debug_snapshot!(parse("{ a: 1 }"));
        insta::assert_debug_snapshot!(parse("{ a: 1, b: 2 }"));
        insta::assert_debug_snapshot!(parse("{ a: 1, b: 2, }"));
        insta::assert_debug_snapshot!(parse(r#"{ "a": 1, [b]: 2, 0: "zero" }"#));
        insta::assert_debug_snapshot!(parse("{ a: 1, b: 2, ...c }"));
        insta::assert_debug_snapshot!(parse("{ ...a, ...b, ...c }"));
        insta::assert_debug_snapshot!(parse("{ a, b }"));
    }

    #[test]
    #[should_panic]
    fn parse_object_literals_missing_colon() {
        parse("{ a 1 }");
    }

    #[test]
    #[should_panic]
    fn parse_object_literals_missing_comma() {
        parse("{ a: 1 b: 2 }");
    }

    #[test]
    #[should_panic]
    fn parse_object_literals_missing_right_brace() {
        parse("{ a: 1, b: 2");
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
    fn parse_multiplicative_operators() {
        insta::assert_debug_snapshot!(parse("a * b / c % d"));
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
        insta::assert_debug_snapshot!(parse("x != y && z == w"));
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
        insta::assert_debug_snapshot!(parse("fn () => { let x = 5; let y = 10; return x + y; }"));
    }

    #[test]
    fn parse_function_with_params() {
        let src = r#"fn (x, y) => { return x + y; }"#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn parse_function_with_type_annotations() {
        insta::assert_debug_snapshot!(parse(
            r#"fn (x: number, y: number): number => { return x + y; }"#
        ));
    }

    #[test]
    fn parse_function_with_optional_params() {
        insta::assert_debug_snapshot!(parse(
            r#"fn (x: number, y: number, z?: number): number => { return x + y; }"#
        ));
    }

    #[test]
    fn parse_function_with_destructuring() {
        insta::assert_debug_snapshot!(parse(r#"fn ({x, y}) => { return x + y; }"#));
    }

    #[test]
    fn parse_function_with_destructuring_and_type_annotation() {
        insta::assert_debug_snapshot!(parse(r#"fn ({x, y}: Point): number => { return x + y; }"#));
    }

    #[test]
    fn parse_lambdas() {
        insta::assert_debug_snapshot!(parse("fn (x, y) => x + y;"));
        insta::assert_debug_snapshot!(parse("fn (x) => fn (y) => x + y;"));
        insta::assert_debug_snapshot!(parse(r#"fn (x: number, y: number): number => x + y;"#));
    }

    #[test]
    #[should_panic]
    fn parse_function_expected_comma_or_left_brace() {
        let src = r#"fn (x, y { return x + y; }"#;
        parse(src);
    }

    #[test]
    #[should_panic]
    fn parse_function_expected_identifier() {
        let src = r#"fn (, y) { return x + y; }"#;
        parse(src);
    }

    #[test]
    fn parse_function_call() {
        insta::assert_debug_snapshot!(parse("add(5, 10)"));
        insta::assert_debug_snapshot!(parse("add(5)(10)"));
        insta::assert_debug_snapshot!(parse("add(obj.x, obj.y)"));
    }

    #[test]
    fn parse_call_expr() {
        let src = r#"foo[bar](5, 10)"#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn parse_member_access() {
        insta::assert_debug_snapshot!(parse("a.b.c"));
        insta::assert_debug_snapshot!(parse("a.b+c.d"));
        insta::assert_debug_snapshot!(parse("a[b][c]"));
    }

    #[test]
    fn parse_optional_chaining() {
        insta::assert_debug_snapshot!(parse("a?.b?.c"));
        insta::assert_debug_snapshot!(parse("a?.[b]"));
        insta::assert_debug_snapshot!(parse("foo?.()"));
    }

    #[test]
    #[should_panic]
    fn parse_invalid_optional_chaining() {
        insta::assert_debug_snapshot!(parse("x?.?.y"));
    }

    #[test]
    fn parse_callback() {
        insta::assert_debug_snapshot!(parse(r#"ids.map(fn (id) => id).join(", ")"#));
    }

    #[test]
    fn parse_conditionals() {
        insta::assert_debug_snapshot!(parse(r#"if (cond) { x; }"#));
        insta::assert_debug_snapshot!(parse(r#"if (cond) { x; } else { y; }"#));
        insta::assert_debug_snapshot!(parse(
            r#"
            if (cond) {
                {x: 5, y: 10};
            } else {
                {a: 1, b: 2};
            }
            "#
        ));
    }

    #[test]
    fn parse_param_destructuring() {
        insta::assert_debug_snapshot!(parse("fn ({x, y}) => { return x + y; }"));
        insta::assert_debug_snapshot!(parse("fn ([head, ...tail]) => head"));
    }

    #[test]
    fn parse_pattern_matching() {
        insta::assert_debug_snapshot!(parse(
            r#"
            match (obj.type) {
                "foo" => obj.foo,
                "bar" => {
                    obj.bar;
                },
                _ => "default",
            };
            "#
        ));
    }

    #[test]
    fn parse_try_catch() {
        insta::assert_debug_snapshot!(parse(
            r#"
            try {
                canThrow();
            } catch (e) {
                console.log("Error: " + e);
            }
            "#
        ));
    }

    #[test]
    fn parse_try_finally() {
        insta::assert_debug_snapshot!(parse(
            r#"
            try {
                canThrow();
            } finally {
                cleanup();
            }
            "#
        ));
    }

    #[test]
    fn parse_try_catch_finally() {
        insta::assert_debug_snapshot!(parse(
            r#"
            try {
                canThrow();
            } catch (e) {
                console.log("Error: " + e);
            } finally {
                cleanup();
            }
            "#
        ));
    }

    #[test]
    fn parse_do_expr() {
        insta::assert_debug_snapshot!(parse(
            r#"
            do {
                let x = 5;
                let y = 10;
                x + y;
            }
            "#
        ))
    }

    #[test]
    fn parse_assignment() {
        insta::assert_debug_snapshot!(parse("x = y"));
        insta::assert_debug_snapshot!(parse("x = y = z"));
        insta::assert_debug_snapshot!(parse("x.a = y.b"));
        insta::assert_debug_snapshot!(parse("x += 1"));
        insta::assert_debug_snapshot!(parse("x -= 1"));
        insta::assert_debug_snapshot!(parse("x *= 2"));
        insta::assert_debug_snapshot!(parse("x /= 2"));
        insta::assert_debug_snapshot!(parse("x %= 2"));
    }

    #[test]
    fn parse_valid_lvalues() {
        insta::assert_debug_snapshot!(parse("a.b.c = x"));
        insta::assert_debug_snapshot!(parse(r#"a["b"][c] = x""#));
    }

    #[test]
    #[should_panic]
    fn parse_invalid_lvalues_fail() {
        insta::assert_debug_snapshot!(parse("a + b = x"));
    }

    #[test]
    fn parse_exprs_with_template_strings() {
        insta::assert_debug_snapshot!(parse("a + `b ${c} d`"));
    }

    #[test]
    fn parse_functional_component() {
        insta::assert_debug_snapshot!(parse("fn (props) => <div>{props.children}</div>"));
    }

    #[test]
    fn parse_functional_component_with_fragment() {
        insta::assert_debug_snapshot!(parse("fn (props) => <>{props.children}</>"));
    }

    #[test]
    #[ignore]
    fn parse_invalid_fn_should_error() {
        insta::assert_debug_snapshot!(parse("(x) => x"));
    }
}
