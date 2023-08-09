use escalier_ast::*;
use generational_arena::Index;

struct ReturnVisitor {
    pub returns: Vec<Expr>,
}

impl Visitor for ReturnVisitor {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if let StmtKind::Return { arg: Some(arg) } = &stmt.kind {
            self.returns.push(arg.to_owned());
        }
        walk_stmt(self, stmt);
    }
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // Don't walk into functions, since we don't want to include returns
            // from nested functions
            ExprKind::Function(_) => {}
            _ => walk_expr(self, expr),
        }
    }
}

pub fn find_returns(body: &BlockOrExpr) -> Vec<Expr> {
    let mut visitor = ReturnVisitor { returns: vec![] };

    match body {
        BlockOrExpr::Block(block) => {
            for stmt in &block.stmts {
                visitor.visit_stmt(stmt);
            }
        }
        BlockOrExpr::Expr(expr) => visitor.visit_expr(expr),
    }

    visitor.returns
}

struct CallVisitor {
    pub throws: Vec<Index>,
}

impl Visitor for CallVisitor {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // Don't walk into functions, since we don't want to include returns
            // from nested functions
            ExprKind::Function(_) => {}
            ExprKind::Call(Call { throws, .. }) => {
                if let Some(throws) = throws {
                    self.throws.push(*throws);
                }
                walk_expr(self, expr);
            }
            _ => walk_expr(self, expr),
        }
    }
}

pub fn find_call_throws(body: &BlockOrExpr) -> Vec<Index> {
    let mut visitor = CallVisitor { throws: vec![] };

    match body {
        BlockOrExpr::Block(block) => {
            for stmt in &block.stmts {
                visitor.visit_stmt(stmt);
            }
        }
        BlockOrExpr::Expr(expr) => visitor.visit_expr(expr),
    }

    visitor.throws
}
