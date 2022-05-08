use super::super::syntax;
use super::ast::*;

pub fn build_js(prog: &syntax::Program) -> Program {
    let body: Vec<_> = prog
        .body
        .iter()
        .map(|child| match child {
            (syntax::Statement::Decl { pattern, value }, _) => {
                let pattern = build_pattern(&pattern.0);
                let value = build_expr(&value.0);
                Statement::Decl { pattern, value }
            }
            (syntax::Statement::Expr(expr), _) => {
                let expr = build_expr(&expr.0);
                Statement::Expression { expr }
            }
        })
        .collect();

    Program { body }
}

pub fn build_pattern(pattern: &syntax::Pattern) -> Pattern {
    match pattern {
        syntax::Pattern::Ident { name } => Pattern::Ident {
            name: name.to_owned(),
        },
    }
}

pub fn build_return_block(body: &syntax::Expr) -> Vec<Statement> {
    match body {
        // Avoids wrapping in an IIFE when it isn't necessary.
        syntax::Expr::Let { .. } => {
            let_to_children(body)
        }
        _ => vec![Statement::Return {
            arg: build_expr(body),
        }]
    }
}

pub fn build_expr(expr: &syntax::Expr) -> Expression {
    match expr {
        syntax::Expr::App { lam, args } => {
            let func = Box::from(build_expr(&lam.as_ref().0));
            let args: Vec<_> = args.iter().map(|arg| build_expr(&arg.0)).collect();

            Expression::Call { func, args }
        }
        syntax::Expr::Ident { name } => Expression::Ident {
            name: name.to_owned(),
        },
        syntax::Expr::Lam { args, body, .. } => {
            let params: Vec<_> = args
                .iter()
                .map(|(ident, _)| match ident {
                    syntax::BindingIdent::Ident { name } => Param::Ident {
                        name: name.to_owned(),
                    },
                    syntax::BindingIdent::Rest { name } => Param::Ident {
                        name: name.to_owned(),
                    },
                })
                .collect();
            let body = &body.as_ref().0;

            match body {
                // Avoids wrapping in an IIFE when it isn't necessary.
                syntax::Expr::Let { .. } => {
                    return Expression::Function {
                        params,
                        body: let_to_children(body),
                    }
                }
                _ => Expression::Function {
                    params,
                    // The last statement in the body of a function
                    // should always be a `return` statement.
                    body: vec![Statement::Return {
                        arg: build_expr(body),
                    }],
                },
            }
        }
        syntax::Expr::Let { .. } => {
            let children = let_to_children(expr);

            // Return an IIFE.
            Expression::Call {
                func: Box::from(Expression::Function {
                    params: vec![],
                    body: children,
                }),
                args: vec![],
            }
        }
        syntax::Expr::Lit { literal } => Expression::Literal {
            literal: literal.to_owned(),
        },
        syntax::Expr::Op { op, left, right } => {
            let op = match op {
                syntax::BinOp::Add => BinaryOp::Add,
                syntax::BinOp::Sub => BinaryOp::Sub,
                syntax::BinOp::Mul => BinaryOp::Mul,
                syntax::BinOp::Div => BinaryOp::Div,
            };
            let left = Box::from(build_expr(&left.0));
            let right = Box::from(build_expr(&right.0));

            Expression::Binary { op, left, right }
        }
        syntax::Expr::Fix { expr } => match expr.as_ref() {
            (syntax::Expr::Lam { body, .. }, _) => build_expr(&body.0),
            _ => panic!("Fix should only wrap a lambda"),
        },
        syntax::Expr::If {
            cond,
            consequent,
            alternate,
        } => {
            // Return an IIFE.
            // (() => {
            //    if (cond) {
            //        return consequent;
            //    } else {
            //        return alternate;
            //    }
            // })();
            Expression::Call {
                func: Box::from(Expression::Function {
                    params: vec![],
                    body: vec![Statement::Expression {
                        expr: Expression::IfElse {
                            cond: Box::from(build_expr(&cond.as_ref().0)),
                            consequent: build_return_block(&consequent.as_ref().0),
                            alternate: build_return_block(&alternate.as_ref().0),
                        },
                    }],
                }),
                args: vec![],
            }
        }
        syntax::Expr::Obj { properties } => {
            let properties: Vec<_> = properties.iter().map(|(prop, _)| {
                Property {
                    name: prop.name.clone(),
                    value: build_expr(&prop.value.0),
                }
            }).collect();

            Expression::Object { properties }
        },
    }
}

pub fn let_to_children(expr: &syntax::Expr) -> Vec<Statement> {
    if let syntax::Expr::Let {
        pattern,
        value,
        body,
    } = expr
    {
        // TODO: handle shadowed variables in the same scope by introducing
        // unique identifiers.
        let pattern = build_pattern(&pattern.0);
        let value = build_expr(&value.0);
        let decl = Statement::Decl { pattern, value };

        let mut children = vec![decl];
        let mut body = body.0.to_owned();

        while let syntax::Expr::Let {
            pattern,
            value,
            body: next_body,
        } = body
        {
            let pattern = build_pattern(&pattern.0);
            let value = build_expr(&value.0);
            let decl = Statement::Decl { pattern, value };
            children.push(decl);
            body = next_body.0.to_owned();
        }

        children.push(Statement::Return {
            arg: build_expr(&body),
        });

        children
    } else {
        panic!("was expecting an syntax::Expr::Let")
    }
}
