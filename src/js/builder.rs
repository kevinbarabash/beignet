use crate::ast;
use crate::js::ast::*;

pub fn build_js(prog: &ast::Program) -> Program {
    let body: Vec<_> = prog
        .body
        .iter()
        .map(|child| match child {
            ast::Statement::Decl { pattern, value, .. } => {
                let pattern = build_pattern(&pattern);
                let value = build_expr(&value);
                Statement::Decl { pattern, value }
            }
            ast::Statement::Expr { expr, .. } => {
                let expr = build_expr(&expr);
                Statement::Expression { expr }
            }
        })
        .collect();

    Program { body }
}

pub fn build_pattern(pattern: &ast::Pattern) -> Pattern {
    match pattern {
        ast::Pattern::Ident(ident) => Pattern::Ident {
            name: ident.name.to_owned(),
        },
    }
}

pub fn build_return_block(body: &ast::Expr) -> Vec<Statement> {
    match body {
        // Avoids wrapping in an IIFE when it isn't necessary.
        ast::Expr::Let { .. } => let_to_children(body),
        _ => vec![Statement::Return {
            arg: build_expr(body),
        }],
    }
}

pub fn build_expr(expr: &ast::Expr) -> Expression {
    match expr {
        ast::Expr::App(ast::App { lam, args, .. }) => {
            let func = Box::from(build_expr(&lam.as_ref()));
            let args: Vec<_> = args.iter().map(|arg| build_expr(&arg)).collect();

            Expression::Call { func, args }
        }
        ast::Expr::Ident(ast::Ident { name, .. }) => Expression::Ident {
            name: name.to_owned(),
        },
        ast::Expr::Lambda(ast::Lambda {
            args,
            body,
            is_async: r#async,
            ..
        }) => {
            let params: Vec<_> = args
                .iter()
                .map(|ident| match ident {
                    ast::BindingIdent::Ident(ident) => Param::Ident {
                        name: ident.name.to_owned(),
                    },
                    ast::BindingIdent::Rest { name, .. } => Param::Ident {
                        name: name.to_owned(),
                    },
                })
                .collect();
            let body = &body.as_ref();

            match body {
                // Avoids wrapping in an IIFE when it isn't necessary.
                ast::Expr::Let { .. } => {
                    return Expression::Function {
                        params,
                        body: let_to_children(body),
                        r#async: r#async.to_owned(),
                    }
                }
                _ => Expression::Function {
                    params,
                    // The last statement in the body of a function
                    // should always be a `return` statement.
                    body: vec![Statement::Return {
                        arg: build_expr(body),
                    }],
                    r#async: r#async.to_owned(),
                },
            }
        }
        ast::Expr::Let { .. } => {
            let children = let_to_children(expr);

            // Return an IIFE
            Expression::Call {
                func: Box::from(Expression::Function {
                    params: vec![],
                    body: children,
                    r#async: false,
                }),
                args: vec![],
            }
        }
        ast::Expr::Lit(lit) => Expression::Literal(lit.to_owned()),
        ast::Expr::Op(ast::Op {
            op, left, right, ..
        }) => {
            let op = match op {
                ast::BinOp::Add => BinaryOp::Add,
                ast::BinOp::Sub => BinaryOp::Sub,
                ast::BinOp::Mul => BinaryOp::Mul,
                ast::BinOp::Div => BinaryOp::Div,
            };
            let left = Box::from(build_expr(&left));
            let right = Box::from(build_expr(&right));

            Expression::Binary { op, left, right }
        }
        ast::Expr::Fix(ast::Fix { expr, .. }) => match expr.as_ref() {
            ast::Expr::Lambda(ast::Lambda { body, .. }) => build_expr(&body),
            _ => panic!("Fix should only wrap a lambda"),
        },
        ast::Expr::IfElse(ast::IfElse {
            cond,
            consequent,
            alternate,
            ..
        }) => {
            // Returns an IIFE that looks like:
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
                            cond: Box::from(build_expr(&cond.as_ref())),
                            consequent: build_return_block(&consequent.as_ref()),
                            alternate: build_return_block(&alternate.as_ref()),
                        },
                    }],
                    r#async: false,
                }),
                args: vec![],
            }
        }
        ast::Expr::Obj(ast::Obj { properties, .. }) => {
            let properties: Vec<_> = properties
                .iter()
                .map(|prop| Property {
                    name: prop.name.clone(),
                    value: build_expr(&prop.value),
                })
                .collect();

            Expression::Object { properties }
        }
        ast::Expr::Await(ast::Await { expr, .. }) => Expression::Await {
            expr: Box::from(build_expr(&expr.as_ref())),
        },
        ast::Expr::JSXElement(_) => todo!(),
    }
}

pub fn let_to_children(expr: &ast::Expr) -> Vec<Statement> {
    if let ast::Expr::Let(ast::Let {
        pattern,
        value,
        body,
        ..
    }) = expr
    {
        // TODO: handle shadowed variables in the same scope by introducing
        // unique identifiers.
        let pattern = build_pattern(&pattern);
        let value = build_expr(&value);
        let decl = Statement::Decl { pattern, value };

        let mut children = vec![decl];
        let mut body = body.to_owned();

        while let ast::Expr::Let(ast::Let {
            pattern,
            value,
            body: next_body,
            ..
        }) = body.as_ref()
        {
            let pattern = build_pattern(&pattern);
            let value = build_expr(&value);
            let decl = Statement::Decl { pattern, value };
            children.push(decl);
            body = next_body.to_owned();
        }

        children.push(Statement::Return {
            arg: build_expr(&body),
        });

        children
    } else {
        panic!("was expecting an ast::Expr::Let")
    }
}
