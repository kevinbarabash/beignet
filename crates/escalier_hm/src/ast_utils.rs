use escalier_ast::{self as syntax, *};

struct ReturnVisitor {
    pub returns: Vec<Expr>,
}

impl syntax::Visitor for ReturnVisitor {
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
