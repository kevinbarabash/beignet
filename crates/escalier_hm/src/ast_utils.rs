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

struct ThrowsVisitor {
    pub throws: Vec<Index>,
}

impl Visitor for ThrowsVisitor {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // Don't walk into functions, since we don't want to include throws
            // from nested functions
            ExprKind::Function(_) => {}
            // Don't walk into `try` blocks since these are supposed to catch
            // `throw`s and discard them.
            ExprKind::Try(Try {
                body: _,
                catch,
                finally,
            }) => {
                if let Some(catch) = catch {
                    walk_block(self, &catch.body);
                }
                if let Some(finally) = finally {
                    walk_block(self, finally);
                }
            }
            ExprKind::Call(Call { throws, .. }) => {
                if let Some(throws) = throws {
                    self.throws.push(*throws);
                }
                walk_expr(self, expr);
            }
            ExprKind::Throw(Throw { throws, .. }) => {
                if let Some(throws) = throws {
                    self.throws.push(*throws);
                }
                walk_expr(self, expr);
            }
            _ => walk_expr(self, expr),
        }
    }
}

pub fn find_throws(body: &BlockOrExpr) -> Vec<Index> {
    let mut visitor = ThrowsVisitor { throws: vec![] };

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

pub fn find_throws_in_block(block: &Block) -> Vec<Index> {
    let mut visitor = ThrowsVisitor { throws: vec![] };

    for stmt in &block.stmts {
        visitor.visit_stmt(stmt);
    }

    visitor.throws
}
