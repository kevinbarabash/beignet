use itertools::*;

use super::syntax::*;

// TODO: refactor this to use an io writer

#[allow(unstable_name_collisions)] // intersperse
pub fn codegen_prog(prog: &Program) -> String {
    prog.body
        .iter()
        .map(|child| match child {
            (Statement::Decl { pattern, value }, _) => {
                let pattern_output = codegen_pattern(pattern);
                let value_output = codegen_expr(value);
                match value {
                    (Expr::Let {..}, _) => format!("var {} = {{\n{}\n}}", pattern_output, value_output),
                    _ => format!("var {} = {}", pattern_output, value_output),
                }
            }
            (Statement::Expr(expr), _) => codegen_expr(expr),
        })
        .intersperse(String::from("\n"))
        .collect()
}

#[allow(unstable_name_collisions)] // intersperse
pub fn codegen_expr(expr: &WithSpan<Expr>) -> String {
    match expr {
        (Expr::App { lam, args }, _) => {
            let lam = codegen_expr(lam);
            let args: String = args
                .iter()
                .map(|arg| codegen_expr(arg))
                .intersperse(String::from(", "))
                .collect();

            format!("{}({})", lam, args)
        }
        (Expr::Ident { name }, _) => name.to_owned(),
        (Expr::Lam { args, body, .. }, _) => {
            let args: String = args
                .iter()
                .map(|(ident, _)| match ident {
                    BindingIdent::Ident { name } => name.to_owned(),
                    BindingIdent::Rest { name } => name.to_owned(),
                })
                .intersperse(String::from(", "))
                .collect();
            let body = codegen_expr(body);

            format!("({}) => {}", args, body)
        }
        (
            Expr::Let {
                pattern,
                value,
                body,
            },
            _,
        ) => {
            // TODO: handle shadowed variables in the same scope by introducing
            // unique identifiers.
            let pattern = codegen_pattern(pattern);
            let value = codegen_expr(value);
            let body_output = codegen_expr(body);

            // TODO: introduce a JavaScript AST that we can use as an intermediary step before
            // outputting a string.
            // NOTE: let-in is essentially a linked list so we could have a helper function that
            // converts that to a vector first.
            match body.as_ref() {
                (Expr::Let { .. }, _) => format!("var {} = {};\n{}", pattern, value, body_output),
                _ => format!("var {} = {};\nreturn {};", pattern, value, body_output),
            }
        }
        (Expr::Lit { literal }, _) => {
            format!("{}", literal)
        }
        (Expr::Op { op, left, right }, _) => {
            let wrap_right = match (&expr.0, &right.0) {
                (Expr::Op { op: parent_op, .. }, Expr::Op { op: right_op, .. }) => {
                    // Division and subtraction are not commutative operations so we
                    // need to have some additional logic to handle things like
                    // `a / (b / c)` and `a - (b - c)`.
                    match (parent_op, right_op) {
                        (BinOp::Div, BinOp::Div) => true,
                        (BinOp::Sub, BinOp::Sub) => true,
                        _ => get_precedence(right_op) < get_precedence(parent_op),
                    }
                }
                _ => false,
            };

            let wrap_left = match (&expr.0, &left.0) {
                (Expr::Op { op: parent_op, .. }, Expr::Op { op: right_op, .. }) => {
                    get_precedence(right_op) < get_precedence(parent_op)
                }
                _ => false,
            };

            let left = if wrap_left {
                format!("({})", codegen_expr(left))
            } else {
                codegen_expr(left)
            };
            let right = if wrap_right {
                format!("({})", codegen_expr(right))
            } else {
                codegen_expr(right)
            };

            let op = match op {
                BinOp::Add => String::from("+"),
                BinOp::Sub => String::from("-"),
                BinOp::Mul => String::from("*"),
                BinOp::Div => String::from("/"),
            };

            format!("{} {} {}", left, op, right)
        }
    }
}

fn codegen_pattern(pattern: &WithSpan<Pattern>) -> String {
    match pattern {
        (Pattern::Ident { name }, _) => name.to_owned(),
    }
}

fn get_precedence(op: &BinOp) -> i32 {
    match op {
        BinOp::Add => 1,
        BinOp::Sub => 1,
        BinOp::Mul => 2,
        BinOp::Div => 2,
    }
}

// NOTE: tests are in tests/codegen_test.rs
