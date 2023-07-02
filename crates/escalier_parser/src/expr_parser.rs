// use std::iter::Peekable;
use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::precedence::{Associativity, Operator, PRECEDENCE_TABLE};
use crate::token::*;

fn get_prefix_precedence(op: &Token) -> Option<(u8, Associativity)> {
    match &op.kind {
        TokenKind::Plus => PRECEDENCE_TABLE.get(&Operator::UnaryPlus).cloned(),
        TokenKind::Minus => PRECEDENCE_TABLE.get(&Operator::UnaryMinus).cloned(),
        TokenKind::Await => PRECEDENCE_TABLE.get(&Operator::Await).cloned(),
        TokenKind::Yield => PRECEDENCE_TABLE.get(&Operator::Yield).cloned(),
        TokenKind::Not => PRECEDENCE_TABLE.get(&Operator::LogicalNot).cloned(),
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
        TokenKind::LessThan => PRECEDENCE_TABLE.get(&Operator::LessThan).cloned(),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    // consumes leading '{' and trailing '}' tokens
    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        let open = self.next().unwrap_or(EOF.clone());
        assert_eq!(open.kind, TokenKind::LeftBrace);
        let mut stmts = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != TokenKind::RightBrace {
            stmts.push(self.parse_stmt()?);

            // The last statement in a block is allowed to omit the trailing
            // semicolon.
            if self.peek().unwrap_or(&EOF).kind == TokenKind::RightBrace {
                break;
            }
        }
        let close = self.next().unwrap_or(EOF.clone());
        assert_eq!(close.kind, TokenKind::RightBrace);
        let span = merge_spans(&open.span, &close.span);

        Ok(Block { span, stmts })
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().unwrap_or(&EOF).clone();

        let lhs = match &token.kind {
            TokenKind::NumLit(n) => {
                self.next(); // consume number
                Expr {
                    kind: ExprKind::Num(Num {
                        value: n.to_owned(),
                    }),
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::Identifier(id) => {
                self.next(); // consume identifier
                Expr {
                    kind: ExprKind::Ident(Ident {
                        name: id.to_owned(),
                        span: token.span,
                    }),
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::BoolLit(b) => {
                self.next(); // consume boolean
                Expr {
                    kind: ExprKind::Bool(Bool { value: *b }),
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::StrLit(s) => {
                self.next(); // consume string
                Expr {
                    kind: ExprKind::Str(Str {
                        value: s.to_owned(),
                        span: token.span,
                    }),
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::StrTemplateLit { parts, exprs } => {
                self.next(); // consume string template
                let kind = ExprKind::TemplateLiteral(TemplateLiteral {
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
                });

                Expr {
                    kind,
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::Null => {
                self.next(); // consume 'null'
                Expr {
                    kind: ExprKind::Null(Null {}),
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::Undefined => {
                self.next(); // consume 'undefined'
                Expr {
                    kind: ExprKind::Undefined(Undefined {}),
                    span: token.span,
                    inferred_type: None,
                }
            }
            TokenKind::LeftParen => self.parse_inside_parens(|p| p.parse_expr())?,
            TokenKind::LeftBracket => {
                self.next(); // consumes '['
                let start = token;
                let elements = self.parse_many(
                    |p| {
                        match p.peek().unwrap_or(&EOF).kind {
                            TokenKind::DotDotDot => {
                                p.next().unwrap_or(EOF.clone()); // consumes `...`
                                let expr = p.parse_expr()?;
                                Ok(ExprOrSpread::Spread(expr))
                            }
                            _ => {
                                let expr = p.parse_expr()?;
                                Ok(ExprOrSpread::Expr(expr))
                            }
                        }
                    },
                    TokenKind::Comma,
                    TokenKind::RightBracket,
                )?;

                assert_eq!(self.peek().unwrap_or(&EOF).kind, TokenKind::RightBracket);

                let end = self.next().unwrap_or(EOF.clone());

                Expr {
                    kind: ExprKind::Tuple(Tuple { elements }),
                    span: merge_spans(&start.span, &end.span),
                    inferred_type: None,
                }
            }
            TokenKind::LeftBrace => {
                self.next(); // consumes '{'
                let start = token;

                let properties = self.parse_many(
                    |p| {
                        let next = p.next().unwrap_or(EOF.clone());

                        match &next.kind {
                            TokenKind::DotDotDot => {
                                let expr = p.parse_expr()?;
                                Ok(PropOrSpread::Spread(expr))
                            }
                            TokenKind::Identifier(id)
                                if p.peek().unwrap_or(&EOF).kind == TokenKind::Comma
                                    || p.peek().unwrap_or(&EOF).kind == TokenKind::RightBrace =>
                            {
                                Ok(PropOrSpread::Prop(expr::Prop::Shorthand(id.to_owned())))
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
                                        let expr = p.parse_expr()?;
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

                                let value = p.parse_expr()?;

                                Ok(PropOrSpread::Prop(expr::Prop::Property { key, value }))
                            }
                        }
                    },
                    TokenKind::Comma,
                    TokenKind::RightBrace,
                )?;

                let end = self.next().unwrap_or(EOF.clone());

                Expr {
                    kind: ExprKind::Object(Object { properties }),
                    span: merge_spans(&start.span, &end.span),
                    inferred_type: None,
                }
            }
            TokenKind::Async => self.parse_function()?,
            TokenKind::Gen => self.parse_function()?,
            TokenKind::Fn => self.parse_function()?,
            TokenKind::If => {
                self.next(); // consumes 'if'
                let cond = self.parse_inside_parens(|p| p.parse_expr())?;
                let consequent = self.parse_block()?;

                if self.peek().unwrap_or(&EOF).kind == TokenKind::Else {
                    self.next().unwrap_or(EOF.clone());
                    let alternate = self.parse_block()?;
                    let span = merge_spans(&token.span, &alternate.span);
                    Expr {
                        kind: ExprKind::IfElse(IfElse {
                            cond: Box::new(cond),
                            consequent,
                            alternate: Some(alternate),
                        }),
                        span,
                        inferred_type: None,
                    }
                } else {
                    let span = merge_spans(&token.span, &consequent.span);
                    Expr {
                        kind: ExprKind::IfElse(IfElse {
                            cond: Box::new(cond),
                            consequent,
                            alternate: None,
                        }),
                        span,
                        inferred_type: None,
                    }
                }
            }
            TokenKind::Match => {
                let start = token;
                self.next(); // consumes 'match'
                let expr = self.parse_inside_parens(|p| p.parse_expr())?;

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::LeftBrace
                );

                let arms = self.parse_many(
                    |p| {
                        let pattern = p.parse_pattern()?;
                        assert_eq!(p.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);

                        let (body, end) = match p.peek().unwrap_or(&EOF).kind {
                            TokenKind::LeftBrace => {
                                let block = p.parse_block()?;
                                let span = block.span;
                                (BlockOrExpr::Block(block), span)
                            }
                            _ => {
                                let expr = p.parse_expr()?;
                                let span = expr.get_span();
                                (BlockOrExpr::Expr(Box::new(expr)), span)
                            }
                        };

                        Ok(MatchArm {
                            span: merge_spans(&pattern.span, &end),
                            pattern,
                            guard: None,
                            body,
                        })
                    },
                    TokenKind::Comma,
                    TokenKind::RightBrace,
                )?;

                let end = self.next().unwrap_or(EOF.clone());
                assert_eq!(end.kind, TokenKind::RightBrace);

                Expr {
                    kind: ExprKind::Match(Match {
                        expr: Box::new(expr),
                        arms,
                    }),
                    span: merge_spans(&start.span, &end.span),
                    inferred_type: None,
                }
            }
            TokenKind::Try => {
                let start = token;
                self.next(); // consumes 'try'
                let try_body = self.parse_block()?;

                match self.next().unwrap_or(EOF.clone()).kind {
                    TokenKind::Catch => {
                        let error = self.parse_inside_parens(|p| p.parse_pattern())?;
                        let catch_body = self.parse_block()?;

                        match self.peek().unwrap_or(&EOF).kind {
                            TokenKind::Finally => {
                                self.next().unwrap_or(EOF.clone());
                                let finally_body = self.parse_block()?;
                                let span = merge_spans(&start.span, &finally_body.span);

                                Expr {
                                    kind: ExprKind::Try(Try {
                                        body: try_body,
                                        catch: Some(CatchClause {
                                            param: Some(error),
                                            body: catch_body,
                                        }),
                                        finally: Some(finally_body),
                                    }),
                                    span,
                                    inferred_type: None,
                                }
                            }
                            _ => {
                                let span = merge_spans(&start.span, &catch_body.span);

                                Expr {
                                    kind: ExprKind::Try(Try {
                                        body: try_body,
                                        catch: Some(CatchClause {
                                            param: Some(error),
                                            body: catch_body,
                                        }),
                                        finally: None,
                                    }),
                                    span,
                                    inferred_type: None,
                                }
                            }
                        }
                    }
                    TokenKind::Finally => {
                        let finally_body = self.parse_block()?;
                        let span = merge_spans(&start.span, &finally_body.span);

                        Expr {
                            kind: ExprKind::Try(Try {
                                body: try_body,
                                catch: None,
                                finally: Some(finally_body),
                            }),
                            span,
                            inferred_type: None,
                        }
                    }
                    _ => {
                        panic!("expected catch or finally");
                    }
                }
            }
            TokenKind::Do => {
                self.next(); // consumes 'do'
                let body = self.parse_block()?;
                let span = merge_spans(&token.span, &body.span);

                Expr {
                    kind: ExprKind::Do(Do { body }),
                    span,
                    inferred_type: None,
                }
            }
            TokenKind::LessThan => {
                // HACK: We use self.scanner.peek() to lookahead further than
                // self.peek() will allow.  The reason why this is scanner.peek(0)
                // and not scanner.peek(1) is because the call to self.peek() at
                // the top of the method has already advanced the scanner's position.
                match self.scanner.peek(0) {
                    Some('>') => {
                        let fragment = self.parse_jsx_fragment()?;
                        let span = fragment.span;
                        Expr {
                            kind: ExprKind::JSXFragment(fragment),
                            span,
                            inferred_type: None,
                        }
                    }
                    _ => {
                        let element = self.parse_jsx_element()?;
                        let span = element.span;
                        Expr {
                            kind: ExprKind::JSXElement(element),
                            span,
                            inferred_type: None,
                        }
                    }
                }
            }
            TokenKind::Class => self.parse_class()?,
            t => {
                self.next(); // consume the token
                match get_prefix_precedence(&token) {
                    Some(precendence) => {
                        let rhs = self.parse_expr_with_precedence(precendence.0)?;
                        let span = merge_spans(&token.span, &rhs.get_span());

                        let kind = match t {
                            TokenKind::Plus => ExprKind::Unary(Unary {
                                op: UnaryOp::Plus,
                                right: Box::new(rhs),
                            }),
                            TokenKind::Minus => ExprKind::Unary(Unary {
                                op: UnaryOp::Minus,
                                right: Box::new(rhs),
                            }),
                            TokenKind::Not => ExprKind::Unary(Unary {
                                op: UnaryOp::Not,
                                right: Box::new(rhs),
                            }),
                            TokenKind::Await => ExprKind::Await(Await { arg: Box::new(rhs) }),
                            TokenKind::Yield => ExprKind::Yield(Yield { arg: Box::new(rhs) }),
                            _ => panic!("unexpected token: {:?}", t),
                        };

                        Expr {
                            kind,
                            span,
                            inferred_type: None,
                        }
                    }
                    None => panic!("unexpected token: {:?}", token),
                }
            }
        };

        Ok(lhs)
    }

    pub fn maybe_parse_type_params(&mut self) -> Result<Option<Vec<TypeParam>>, ParseError> {
        if self.peek().unwrap_or(&EOF).kind == TokenKind::LessThan {
            self.next(); // consumes '<'
            let type_params = self.parse_many(
                |p| p.parse_type_param(),
                TokenKind::Comma,
                TokenKind::GreaterThan,
            )?;
            assert_eq!(
                self.next().unwrap_or(EOF.clone()).kind,
                TokenKind::GreaterThan
            );
            Ok(Some(type_params))
        } else {
            Ok(None)
        }
    }

    fn parse_function(&mut self) -> Result<Expr, ParseError> {
        let start = self.peek().unwrap_or(&EOF).clone();

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

        let type_params = self.maybe_parse_type_params()?;
        let params = self.parse_params()?;

        let type_ann = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::Colon => {
                self.next().unwrap_or(EOF.clone());
                Some(self.parse_type_ann()?)
            }
            _ => None,
        };

        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Arrow);

        let (body, span) = match self.peek().unwrap_or(&EOF).kind {
            TokenKind::LeftBrace => {
                let block = self.parse_block()?;
                let span = merge_spans(&start.span, &block.span);
                (BlockOrExpr::Block(block), span)
            }
            _ => {
                let expr = self.parse_expr()?;
                let span = merge_spans(&start.span, &expr.get_span());
                (BlockOrExpr::Expr(Box::new(expr)), span)
            }
        };

        let kind = ExprKind::Function(Function {
            type_params,
            params,
            body,
            type_ann,
            is_async,
            is_gen,
        });

        Ok(Expr {
            kind,
            span,
            inferred_type: None,
        })
    }

    fn parse_type_param(&mut self) -> Result<TypeParam, ParseError> {
        let start = self.scanner.cursor();
        let name = match self.next().unwrap_or(EOF.clone()).kind {
            TokenKind::Identifier(name) => name,
            _ => panic!("expected identifier"),
        };
        let bound = if self.peek().unwrap_or(&EOF).kind == TokenKind::Colon {
            self.next().unwrap_or(EOF.clone());
            Some(self.parse_type_ann()?)
        } else {
            None
        };
        let end = self.scanner.cursor();

        Ok(TypeParam {
            span: Span { start, end },
            name,
            bound,
            default: None,
        })
    }

    fn parse_expr_with_precedence(&mut self, precedence: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_atom()?;

        loop {
            let next = self.peek().unwrap_or(&EOF).clone();
            if let TokenKind::Eof = next.kind {
                return Ok(lhs);
            }

            if let TokenKind::Semicolon = next.kind {
                return Ok(lhs);
            }

            if let Some(next_precedence) = get_postfix_precedence(&next) {
                if precedence >= next_precedence.0 {
                    return Ok(lhs);
                }

                if let Some(result) = self.parse_postfix(lhs.clone(), next_precedence)? {
                    lhs = result;
                    continue;
                }
            }

            if let Some(next_precedence) = get_infix_precedence(&next) {
                if precedence >= next_precedence.0 {
                    return Ok(lhs);
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
                    if !lhs.is_lvalue() {
                        panic!("expected lvalue");
                    }

                    let precedence = if next_precedence.1 == Associativity::Left {
                        next_precedence.0
                    } else {
                        next_precedence.0 - 1
                    };

                    let rhs = self.parse_expr_with_precedence(precedence)?;
                    let span = merge_spans(&lhs.get_span(), &rhs.get_span());

                    lhs = Expr {
                        kind: ExprKind::Assign(Assign {
                            op,
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        }),
                        span,
                        inferred_type: None,
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

                let rhs = self.parse_expr_with_precedence(precedence)?;
                let span = merge_spans(&lhs.get_span(), &rhs.get_span());

                lhs = Expr {
                    kind: ExprKind::Binary(Binary {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    }),
                    span,
                    inferred_type: None,
                };

                continue;
            }

            return Ok(lhs);
        }
    }

    // If we attempt to parse explicit type args for a function call and fail,
    // we return None and restore the parser state to what it was before the
    // attempt.
    fn parse_postfix(
        &mut self,
        lhs: Expr,
        next_precedence: (u8, Associativity),
    ) -> Result<Option<Expr>, ParseError> {
        let precedence = if next_precedence.1 == Associativity::Left {
            next_precedence.0
        } else {
            next_precedence.0 - 1
        };

        let token = self.peek().unwrap_or(&EOF).clone();

        let expr = match &token.kind {
            TokenKind::LeftBracket => {
                self.next(); // consumes '['
                let rhs = self.parse_expr()?;
                let span = merge_spans(&lhs.get_span(), &rhs.get_span());
                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::RightBracket
                );
                Expr {
                    kind: ExprKind::Member(Member {
                        object: Box::new(lhs),
                        property: MemberProp::Computed(ComputedPropName {
                            span,
                            expr: Box::new(rhs),
                        }),
                    }),
                    span,
                    inferred_type: None,
                }
            }
            TokenKind::LeftParen => {
                let args = self.parse_inside_parens(|p| {
                    p.parse_many(|p| p.parse_expr(), TokenKind::Comma, TokenKind::RightParen)
                })?;

                let end = self.scanner.cursor();
                let span = Span {
                    start: lhs.get_span().start,
                    end,
                };
                let kind = ExprKind::Call(Call {
                    callee: Box::new(lhs),
                    type_args: None,
                    args,
                });

                Expr {
                    kind,
                    span,
                    inferred_type: None,
                }
            }
            TokenKind::LessThan => {
                // Parsing explicit type args conflicts with parsing expressions
                // involving less-than and greater-than operators.  We can't know
                // ahead of time which one to parse, so we have to try both.
                let backup = self.clone();

                self.next(); // consumes '<'
                let type_args = self.parse_many(
                    |p| p.parse_type_ann(),
                    TokenKind::Comma,
                    TokenKind::GreaterThan,
                );
                let type_args = match type_args {
                    Ok(type_args) => type_args,
                    Err(_) => {
                        // If we failed to parse explicit type args, restore the
                        // parser state and continue parsing like nothing happened.
                        self.restore(backup);
                        return Ok(None);
                    }
                };

                assert_eq!(
                    self.next().unwrap_or(EOF.clone()).kind,
                    TokenKind::GreaterThan
                );

                let args = self.parse_inside_parens(|p| {
                    p.parse_many(|p| p.parse_expr(), TokenKind::Comma, TokenKind::RightParen)
                })?;

                let end = self.scanner.cursor();
                let span = Span {
                    start: lhs.get_span().start,
                    end,
                };
                let kind = ExprKind::Call(Call {
                    callee: Box::new(lhs),
                    type_args: Some(type_args),
                    args,
                });

                Expr {
                    kind,
                    span,
                    inferred_type: None,
                }
            }
            TokenKind::Dot => {
                self.next(); // consumes '.'
                let rhs = self.parse_expr_with_precedence(precedence)?;
                match &rhs.kind {
                    ExprKind::Ident(ident) => {
                        let span = merge_spans(&lhs.get_span(), &rhs.get_span());
                        Expr {
                            kind: ExprKind::Member(Member {
                                object: Box::new(lhs),
                                property: MemberProp::Ident(ident.to_owned()),
                            }),
                            span,
                            inferred_type: None,
                        }
                    }
                    _ => {
                        return Err(ParseError {
                            message: "expected identifier".to_string(),
                        });
                    }
                }
            }
            TokenKind::QuestionDot => {
                self.next(); // consumes '?.'
                let lhs_span = lhs.get_span();

                let base = match self.peek().unwrap_or(&EOF).kind {
                    TokenKind::LeftParen | TokenKind::LeftBracket => {
                        self.parse_postfix(lhs, next_precedence)?
                    }
                    _ => {
                        let rhs = self.parse_expr_with_precedence(precedence)?;
                        match &rhs.kind {
                            ExprKind::Ident(ident) => {
                                let span = merge_spans(&lhs.get_span(), &rhs.get_span());
                                let expr = Expr {
                                    kind: ExprKind::Member(Member {
                                        object: Box::new(lhs),
                                        property: MemberProp::Ident(ident.to_owned()),
                                    }),
                                    span,
                                    inferred_type: None,
                                };
                                Some(expr)
                            }
                            _ => {
                                return Err(ParseError {
                                    message: "expected identifier".to_string(),
                                });
                            }
                        }
                    }
                };

                let base = match base {
                    Some(base) => base,
                    None => {
                        return Err(ParseError {
                            message: "base is None when parsing optional chain".to_string(),
                        })
                    }
                };

                let span = merge_spans(&lhs_span, &base.get_span());

                Expr {
                    kind: ExprKind::OptionalChain(OptionalChain {
                        base: Box::new(base),
                    }),
                    span,
                    inferred_type: None,
                }
            }
            _ => panic!("unexpected token: {:?}", token),
        };

        Ok(Some(expr))
    }

    pub fn parse_inside_parens<T>(
        &mut self,
        callback: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
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
        mut callback: impl FnMut(&mut Self) -> Result<T, ParseError>,
        separator: TokenKind,
        terminator: TokenKind,
    ) -> Result<Vec<T>, ParseError> {
        let mut result = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != terminator {
            result.push(callback(self)?);

            let next = self.peek().unwrap_or(&EOF);

            if next.kind == terminator {
                break;
            } else if next.kind == separator {
                self.next().unwrap_or(EOF.clone());
            } else {
                return Err(ParseError {
                    message: format!(
                        "Expected {:?} or {:?}, got {:?}",
                        separator, terminator, next
                    ),
                });
            }
        }
        Ok(result)
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_with_precedence(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    pub fn parse(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.parse_expr().unwrap()
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
            match (obj.kind) {
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

    #[test]
    fn parse_func_calls_with_type_args() {
        insta::assert_debug_snapshot!(parse("id<number>(5)"));
        insta::assert_debug_snapshot!(parse(r#"fst<number, string>(5, "hello")"#));
    }

    #[test]
    fn parse_class() {
        insta::assert_debug_snapshot!(parse(
            r#"
            class {
                msg: string
                id = 5
                fn foo(self) {}
                async fn fetch(self, url: string) {}
                gen fn [Symbol.iterator](self) {}
            }
        "#
        ));
    }

    #[test]
    fn parse_getters_setters() {
        insta::assert_debug_snapshot!(parse(
            r#"
            class {
                get foo(self) {}
                set foo(self, value) {}
            }
        "#
        ));
    }

    #[test]
    fn parse_class_with_extends_and_type_params() {
        insta::assert_debug_snapshot!(parse(
            r#"
            class<T> extends Foo {
                fn bar<A>(self, a: A): T {}
            }
        "#
        ));
    }
}
