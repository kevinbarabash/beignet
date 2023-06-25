// use std::iter::Peekable;

use crate::expr::*;
use crate::identifier::Ident;
use crate::parser::*;
use crate::precedence::{Associativity, Operator, PRECEDENCE_TABLE};
use crate::span::*;
use crate::token::*;

fn get_prefix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        TokenKind::Plus => PRECEDENCE_TABLE.get(&Operator::UnaryPlus).cloned(),
        TokenKind::Minus => PRECEDENCE_TABLE.get(&Operator::UnaryMinus).cloned(),
        TokenKind::Await => PRECEDENCE_TABLE.get(&Operator::Await).cloned(),
        TokenKind::Yield => PRECEDENCE_TABLE.get(&Operator::Yield).cloned(),
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
        // TODO: re-enable once we're using Result for error handling
        // TokenKind::LessThan => PRECEDENCE_TABLE.get(&Operator::LessThan).cloned(),
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

            // The last statement in a block is allowed to omit the trailing
            // semicolon.
            if self.peek().unwrap_or(&EOF).kind == TokenKind::RightBrace {
                break;
            }
        }
        let close = self.next().unwrap_or(EOF.clone());
        assert_eq!(close.kind, TokenKind::RightBrace);
        let span = merge_spans(&open.span, &close.span);

        Block { span, stmts }
    }

    fn parse_atom(&mut self) -> Expr {
        let token = self.peek().unwrap_or(&EOF).clone();

        let lhs = match &token.kind {
            TokenKind::NumLit(n) => {
                self.next(); // consume number
                Expr::Num(Num {
                    span: token.span,
                    value: n.to_owned(),
                })
            }
            TokenKind::Identifier(id) => {
                self.next(); // consume identifier
                Expr::Ident(Ident {
                    name: id.to_owned(),
                    span: token.span,
                })
            }
            TokenKind::BoolLit(b) => {
                self.next(); // consume boolean
                Expr::Bool(Bool {
                    span: token.span,
                    value: *b,
                })
            }
            TokenKind::StrLit(s) => {
                self.next(); // consume string
                Expr::Str(Str {
                    span: token.span,
                    value: s.to_owned(),
                })
            }
            TokenKind::StrTemplateLit { parts, exprs } => {
                self.next(); // consume string template
                Expr::TemplateLiteral(TemplateLiteral {
                    span: token.span,
                    parts: parts
                        .iter()
                        .map(|token| match &token.kind {
                            TokenKind::StrLit(value) => Str {
                                span: token.span,
                                value: value.to_owned(),
                            },
                            _ => panic!("Expected string literal, got {:?}", token),
                        })
                        .collect(),
                    exprs: exprs.to_owned(),
                })
            }
            TokenKind::Null => {
                self.next(); // consume 'null'
                Expr::Null(Null { span: token.span })
            }
            TokenKind::Undefined => {
                self.next(); // consume 'undefined'
                Expr::Undefined(Undefined { span: token.span })
            }
            TokenKind::LeftParen => self.parse_inside_parens(|p| p.parse_expr()),
            TokenKind::LeftBracket => {
                self.next(); // consumes '['
                let start = token;
                let elements = self.parse_many(
                    |p| {
                        match p.peek().unwrap_or(&EOF).kind {
                            TokenKind::DotDotDot => {
                                p.next().unwrap_or(EOF.clone()); // consumes `...`
                                let expr = p.parse_expr_with_precedence(0);
                                ExprOrSpread::Spread(expr)
                            }
                            _ => {
                                let expr = p.parse_expr_with_precedence(0);
                                ExprOrSpread::Expr(expr)
                            }
                        }
                    },
                    TokenKind::Comma,
                    TokenKind::RightBracket,
                );

                assert_eq!(self.peek().unwrap_or(&EOF).kind, TokenKind::RightBracket);

                let end = self.next().unwrap_or(EOF.clone());

                Expr::Tuple(Tuple {
                    span: merge_spans(&start.span, &end.span),
                    elements,
                })
            }
            TokenKind::LeftBrace => {
                self.next(); // consumes '{'
                let start = token;

                let properties = self.parse_many(
                    |p| {
                        let next = p.next().unwrap_or(EOF.clone());

                        match &next.kind {
                            TokenKind::DotDotDot => {
                                let expr = p.parse_expr_with_precedence(0);
                                PropOrSpread::Spread(expr)
                            }
                            TokenKind::Identifier(id)
                                if p.peek().unwrap_or(&EOF).kind == TokenKind::Comma
                                    || p.peek().unwrap_or(&EOF).kind == TokenKind::RightBrace =>
                            {
                                PropOrSpread::Prop(Prop::Shorthand { key: id.to_owned() })
                            }
                            _ => {
                                let key = match &next.kind {
                                    TokenKind::Identifier(id) => ObjectKey::Ident(Ident {
                                        span: next.span,
                                        name: id.to_owned(),
                                    }),
                                    TokenKind::StrLit(s) => ObjectKey::String(s.to_owned()),
                                    TokenKind::NumLit(n) => ObjectKey::Number(n.to_owned()),
                                    TokenKind::LeftBracket => {
                                        let expr = p.parse_expr_with_precedence(0);
                                        assert_eq!(
                                            p.next().unwrap_or(EOF.clone()).kind,
                                            TokenKind::RightBracket
                                        );
                                        ObjectKey::Computed(Box::new(expr))
                                    }
                                    _ => {
                                        panic!(
                                            "Expected identifier or string literal, got {:?}",
                                            next
                                        )
                                    }
                                };

                                assert_eq!(p.next().unwrap_or(EOF.clone()).kind, TokenKind::Colon);

                                let value = p.parse_expr();

                                PropOrSpread::Prop(Prop::Property { key, value })
                            }
                        }
                    },
                    TokenKind::Comma,
                    TokenKind::RightBrace,
                );

                let end = self.next().unwrap_or(EOF.clone());

                Expr::Object(Object {
                    span: merge_spans(&start.span, &end.span),
                    properties,
                })
            }
            TokenKind::Async => {
                self.next(); // consumes 'async'
                self.parse_function(&token, true, false)
            }
            TokenKind::Gen => {
                self.next(); // consumes 'gen'
                self.parse_function(&token, false, true)
            }
            TokenKind::Fn => self.parse_function(&token, false, false),
            TokenKind::If => {
                self.next(); // consumes 'if'
                let cond = self.parse_inside_parens(|p| p.parse_expr());
                let consequent = self.parse_block();

                if self.peek().unwrap_or(&EOF).kind == TokenKind::Else {
                    self.next().unwrap_or(EOF.clone());
                    let alternate = self.parse_block();
                    let span = merge_spans(&token.span, &alternate.span);
                    Expr::IfElse(IfElse {
                        span,
                        cond: Box::new(cond),
                        consequent,
                        alternate: Some(alternate),
                    })
                } else {
                    let span = merge_spans(&token.span, &consequent.span);
                    Expr::IfElse(IfElse {
                        span,
                        cond: Box::new(cond),
                        consequent,
                        alternate: None,
                    })
                }
            }
            TokenKind::Match => {
                let start = token;
                self.next(); // consumes 'match'
                let expr = self.parse_inside_parens(|p| p.parse_expr());

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftBrace
                );

                let arms = self.parse_many(
                    |p| {
                        let pattern = p.parse_pattern();
                        assert_eq!(p.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);

                        let (body, end) = match p.peek().unwrap_or(&EOF).kind {
                            TokenKind::LeftBrace => {
                                let block = p.parse_block();
                                let span = block.span;
                                (BlockOrExpr::Block(block), span)
                            }
                            _ => {
                                let expr = p.parse_expr();
                                let span = expr.get_span();
                                (BlockOrExpr::Expr(Box::new(expr)), span)
                            }
                        };

                        MatchArm {
                            span: merge_spans(&pattern.span, &end),
                            pattern,
                            guard: None,
                            body,
                        }
                    },
                    TokenKind::Comma,
                    TokenKind::RightBrace,
                );

                let end = self.next().unwrap_or(EOF.clone());
                assert_eq!(end.kind, TokenKind::RightBrace);

                Expr::Match(Match {
                    span: merge_spans(&start.span, &end.span),
                    expr: Box::new(expr),
                    arms,
                })
            }
            TokenKind::Try => {
                let start = token;
                self.next(); // consumes 'try'
                let try_body = self.parse_block();

                match self.next().unwrap_or(EOF.clone()).kind {
                    TokenKind::Catch => {
                        let error = self.parse_inside_parens(|p| p.parse_pattern());
                        let catch_body = self.parse_block();

                        match self.peek().unwrap_or(&EOF).kind {
                            TokenKind::Finally => {
                                self.next().unwrap_or(EOF.clone());
                                let finally_body = self.parse_block();
                                let span = merge_spans(&start.span, &finally_body.span);

                                Expr::Try(Try {
                                    span,
                                    body: try_body,
                                    catch: Some(CatchClause {
                                        param: Some(error),
                                        body: catch_body,
                                    }),
                                    finally: Some(finally_body),
                                })
                            }
                            _ => {
                                let span = merge_spans(&start.span, &catch_body.span);

                                Expr::Try(Try {
                                    span,
                                    body: try_body,
                                    catch: Some(CatchClause {
                                        param: Some(error),
                                        body: catch_body,
                                    }),
                                    finally: None,
                                })
                            }
                        }
                    }
                    TokenKind::Finally => {
                        let finally_body = self.parse_block();
                        let span = merge_spans(&start.span, &finally_body.span);

                        Expr::Try(Try {
                            span,
                            body: try_body,
                            catch: None,
                            finally: Some(finally_body),
                        })
                    }
                    _ => {
                        panic!("expected catch or finally");
                    }
                }
            }
            TokenKind::Do => {
                self.next(); // consumes 'do'
                let body = self.parse_block();
                let span = merge_spans(&token.span, &body.span);

                Expr::Do(Do { span, body })
            }
            TokenKind::LessThan => {
                // HACK: We use self.scanner.peek() to lookahead further than
                // self.peek() will allow.  The reason why this is scanner.peek(0)
                // and not scanner.peek(1) is because the call to self.peek() at
                // the top of the method has already advanced the scanner's position.
                match self.scanner.peek(0) {
                    Some('>') => Expr::JSXFragment(self.parse_jsx_fragment()),
                    _ => Expr::JSXElement(self.parse_jsx_element()),
                }
            }
            t => {
                self.next(); // consume the token
                match get_prefix_precedence(&token) {
                    Some(precendence) => {
                        let rhs = self.parse_expr_with_precedence(precendence.0);
                        let span = merge_spans(&token.span, &rhs.get_span());

                        match t {
                            TokenKind::Plus => Expr::Unary(Unary {
                                span,
                                op: UnaryOp::Plus,
                                right: Box::new(rhs),
                            }),
                            TokenKind::Minus => Expr::Unary(Unary {
                                span,
                                op: UnaryOp::Minus,
                                right: Box::new(rhs),
                            }),
                            TokenKind::Await => Expr::Await(Await {
                                span,
                                arg: Box::new(rhs),
                            }),
                            TokenKind::Yield => Expr::Yield(Yield {
                                span,
                                arg: Box::new(rhs),
                            }),
                            _ => panic!("unexpected token: {:?}", t),
                        }
                    }
                    None => panic!("unexpected token: {:?}", token),
                }
            }
        };

        lhs
    }

    fn parse_function(&mut self, start: &Token, is_async: bool, is_gen: bool) -> Expr {
        self.next(); // consumes 'fn'

        let type_params = if self.peek().unwrap_or(&EOF).kind == TokenKind::LessThan {
            self.next(); // consumes '<'
            let type_params = self.parse_many(
                |p| p.parse_type_param(),
                TokenKind::Comma,
                TokenKind::GreaterThan,
            );
            assert_eq!(
                self.next().unwrap_or(EOF.clone()).kind,
                TokenKind::GreaterThan
            );
            Some(type_params)
        } else {
            None
        };

        let params = self.parse_params();

        let type_ann = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::Colon => {
                self.next().unwrap_or(EOF.clone());
                Some(self.parse_type_ann())
            }
            _ => None,
        };

        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);

        let (body, span) = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::LeftBrace => {
                let block = self.parse_block();
                let span = merge_spans(&start.span, &block.span);
                (BlockOrExpr::Block(block), span)
            }
            _ => {
                let expr = self.parse_expr();
                let span = merge_spans(&start.span, &expr.get_span());
                (BlockOrExpr::Expr(Box::new(expr)), span)
            }
        };

        Expr::Function(Function {
            span,
            type_params,
            params,
            body,
            type_ann,
            is_async,
            is_gen,
        })
    }

    fn parse_type_param(&mut self) -> TypeParam {
        let start = self.scanner.cursor();
        let t = self.parse_type_ann();
        let bound = if self.peek().unwrap_or(&EOF).kind == TokenKind::Colon {
            self.next().unwrap_or(EOF.clone());
            Some(self.parse_type_ann())
        } else {
            None
        };
        let end = self.scanner.cursor();

        TypeParam {
            span: Span { start, end },
            t,
            bound,
            default: None,
        }
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
                    let span = merge_spans(&lhs.get_span(), &rhs.get_span());

                    lhs = Expr::Assign(Assign {
                        span,
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    });

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
                let span = merge_spans(&lhs.get_span(), &rhs.get_span());

                lhs = Expr::Binary(Binary {
                    span,
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                });

                continue;
            }

            return lhs;
        }
    }

    fn parse_postfix(&mut self, lhs: Expr, next_precedence: (u8, Associativity)) -> Expr {
        let precedence = if next_precedence.1 == Associativity::Left {
            next_precedence.0
        } else {
            next_precedence.0 - 1
        };

        let token = self.peek().unwrap_or(&EOF).clone();

        match &token.kind {
            TokenKind::LeftBracket => {
                self.next(); // consumes '{'
                let rhs = self.parse_expr();
                let span = merge_spans(&lhs.get_span(), &rhs.get_span());
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBracket
                );
                Expr::Index(Index {
                    span,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                })
            }
            TokenKind::LeftParen => {
                let args = self.parse_inside_parens(|p| {
                    p.parse_many(|p| p.parse_expr(), TokenKind::Comma, TokenKind::RightParen)
                });

                let end = self.scanner.cursor();

                Expr::Call(Call {
                    span: Span {
                        start: lhs.get_span().start,
                        end,
                    },
                    callee: Box::new(lhs),
                    args,
                })
            }
            // TODO: re-enable once we're using Result<> for error handling
            // TokenKind::LessThan => {
            //     self.next(); // consumes '<'
            //     let args = self.parse_many(
            //         |p| p.parse_type_ann(),
            //         TokenKind::Comma,
            //         TokenKind::GreaterThan,
            //     );
            //     assert_eq!(
            //         self.next().unwrap_or(EOF.clone()).kind,
            //         TokenKind::GreaterThan
            //     );

            //     let end = self.scanner.cursor();

            //     let args = self.parse_inside_parens(|p| {
            //         p.parse_many(|p| p.parse_expr(), TokenKind::Comma, TokenKind::RightParen)
            //     });

            //     let end = self.scanner.cursor();

            //     Expr::Call(Call {
            //         span: Span {
            //             start: lhs.get_span().start,
            //             end,
            //         },
            //         callee: Box::new(lhs),
            //         args,
            //     })
            // }
            TokenKind::Dot => {
                self.next(); // consumes '.'
                let rhs = self.parse_expr_with_precedence(precedence);
                let span = merge_spans(&lhs.get_span(), &rhs.get_span());
                Expr::Member(Member {
                    span,
                    object: Box::new(lhs),
                    property: Box::new(rhs),
                })
            }
            TokenKind::QuestionDot => {
                self.next(); // consumes '?.'
                let lhs_span = lhs.get_span();

                let base = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::LeftParen | TokenKind::LeftBracket => {
                        self.parse_postfix(lhs, next_precedence)
                    }
                    _ => {
                        let rhs = self.parse_expr_with_precedence(precedence);
                        let span = merge_spans(&lhs.get_span(), &rhs.get_span());
                        Expr::Member(Member {
                            span,
                            object: Box::new(lhs),
                            property: Box::new(rhs),
                        })
                    }
                };

                let span = merge_spans(&lhs_span, &base.get_span());

                Expr::OptionalChain(OptionalChain {
                    span,
                    base: Box::new(base),
                })
            }
            _ => panic!("unexpected token: {:?}", token),
        }
    }

    fn parse_inside_parens<T>(&mut self, callback: impl FnOnce(&mut Self) -> T) -> T {
        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::LeftParen
        );
        let result = callback(self);
        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::RightParen
        );
        result
    }

    fn parse_many<T>(
        &mut self,
        mut callback: impl FnMut(&mut Self) -> T,
        separator: TokenKind,
        terminator: TokenKind,
    ) -> Vec<T> {
        let mut result = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != terminator {
            result.push(callback(self));

            let next = self.peek().unwrap_or(&EOF);

            if next.kind == terminator {
                break;
            } else if next.kind == separator {
                self.next().unwrap_or(EOF.clone());
            } else {
                panic!(
                    "Expected {:?} or {:?}, got {:?}",
                    separator, terminator, next
                );
            }
        }
        result
    }

    pub fn parse_expr(&mut self) -> Expr {
        self.parse_expr_with_precedence(0)
    }
}

fn is_lvalue(expr: &Expr) -> bool {
    match expr {
        Expr::Ident(_) => true,
        Expr::Member(Member { object, .. }) => is_lvalue(object),
        Expr::Index(Index { left, .. }) => is_lvalue(left),
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
        insta::assert_debug_snapshot!(parse("fn () => { let x = 5 let y = 10 return x + y }"));
    }

    #[test]
    fn parse_function_with_params() {
        let src = r#"fn (x, y) => { return x + y }"#;
        insta::assert_debug_snapshot!(parse(src));
    }

    #[test]
    fn parse_function_with_type_annotations() {
        insta::assert_debug_snapshot!(parse(
            r#"fn (x: number, y: number): number => { return x + y }"#
        ));
    }

    #[test]
    fn parse_function_with_optional_params() {
        insta::assert_debug_snapshot!(parse(
            r#"fn (x: number, y: number, z?: number): number => { return x + y }"#
        ));
    }

    #[test]
    fn parse_function_with_destructuring() {
        insta::assert_debug_snapshot!(parse(r#"fn ({x, y}) => { return x + y }"#));
    }

    #[test]
    fn parse_function_with_destructuring_and_type_annotation() {
        insta::assert_debug_snapshot!(parse(r#"fn ({x, y}: Point): number => { return x + y }"#));
    }

    #[test]
    fn parse_lambdas() {
        insta::assert_debug_snapshot!(parse("fn (x, y) => x + y"));
        insta::assert_debug_snapshot!(parse("fn (x) => fn (y) => x + y"));
        insta::assert_debug_snapshot!(parse(r#"fn (x: number, y: number): number => x + y"#));
    }

    #[test]
    #[should_panic]
    fn parse_function_expected_comma_or_left_brace() {
        let src = r#"fn (x, y { return x + y }"#;
        parse(src);
    }

    #[test]
    #[should_panic]
    fn parse_function_expected_identifier() {
        let src = r#"fn (, y) { return x + y }"#;
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
        insta::assert_debug_snapshot!(parse(r#"if (cond) { x }"#));
        insta::assert_debug_snapshot!(parse(r#"if (cond) { x } else { y }"#));
        insta::assert_debug_snapshot!(parse(
            r#"
            if (cond) {
                {x: 5, y: 10}
            } else {
                {a: 1, b: 2}
            }
            "#
        ));
    }

    #[test]
    fn parse_param_destructuring() {
        insta::assert_debug_snapshot!(parse("fn ({x, y}) => { return x + y }"));
        insta::assert_debug_snapshot!(parse("fn ([head, ...tail]) => head"));
    }

    #[test]
    fn parse_pattern_matching() {
        insta::assert_debug_snapshot!(parse(
            r#"
            match (obj.type) {
                "foo" => obj.foo,
                "bar" => {
                    obj.bar
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
                canThrow()
            } catch (e) {
                console.log("Error: " + e)
            }
            "#
        ));
    }

    #[test]
    fn parse_try_finally() {
        insta::assert_debug_snapshot!(parse(
            r#"
            try {
                canThrow()
            } finally {
                cleanup()
            }
            "#
        ));
    }

    #[test]
    fn parse_try_catch_finally() {
        insta::assert_debug_snapshot!(parse(
            r#"
            try {
                canThrow()
            } catch (e) {
                console.log("Error: " + e)
            } finally {
                cleanup()
            }
            "#
        ));
    }

    #[test]
    fn parse_do_expr() {
        insta::assert_debug_snapshot!(parse(
            r#"
            do {
                let x = 5
                let y = 10
                x + y
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

    #[test]
    fn parse_multiple_application() {
        insta::assert_debug_snapshot!(parse("foo()\n(3+4) * 5"));
    }

    #[test]
    fn parse_async_await() {
        insta::assert_debug_snapshot!(parse(
            r#"
            async fn () => { 
                let x = await foo()
                return x
            }
        "#
        ));
    }

    #[test]
    fn parse_generator_function() {
        insta::assert_debug_snapshot!(parse(
            r#"
            gen fn () => { 
                yield 1
                yield 2
                yield 3
            }
        "#
        ));
    }

    #[test]
    fn parse_func_with_type_params() {
        insta::assert_debug_snapshot!(parse("fn <T> (x: T) => x"));
        insta::assert_debug_snapshot!(parse("fn <A, B> (a: A, b: B): A => a"));
        insta::assert_debug_snapshot!(parse("fn <A: number, B: number> (a: A, b: B): A => a"));
    }

    // TODO: reenable once we're using Result<> for error handling
    #[test]
    #[ignore]
    fn parse_func_calls_with_type_args() {
        insta::assert_debug_snapshot!(parse("id<number>(5)"));
        insta::assert_debug_snapshot!(parse(r#"fst<number, string>(5, "hello")"#));
    }
}
