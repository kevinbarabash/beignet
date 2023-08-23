use crate::block::Block;
use crate::class::*;
use crate::expr::*;
use crate::pattern::*;
use crate::stmt::*;
use crate::type_ann::TypeAnn;

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr)
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        walk_pattern(self, pattern)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt)
    }

    fn visit_type_ann(&mut self, type_ann: &TypeAnn) {
        walk_type_ann(self, type_ann)
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        crate::ExprKind::Ident(_) => {}
        crate::ExprKind::Num(_) => {}
        crate::ExprKind::Str(_) => {}
        crate::ExprKind::Bool(_) => {}
        crate::ExprKind::Null(_) => {}
        crate::ExprKind::Undefined(_) => {}
        crate::ExprKind::TemplateLiteral(TemplateLiteral { parts: _, exprs }) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        }
        crate::ExprKind::Object(Object { properties }) => {
            for prop in properties {
                match prop {
                    crate::PropOrSpread::Prop(prop) => match prop {
                        Prop::Shorthand(_) => {}
                        Prop::Property { key: _, value } => {
                            visitor.visit_expr(value);
                        }
                    },
                    crate::PropOrSpread::Spread(expr) => visitor.visit_expr(expr),
                }
            }
        }
        crate::ExprKind::Tuple(Tuple { elements }) => {
            for expr in elements {
                match expr {
                    ExprOrSpread::Expr(expr) => visitor.visit_expr(expr),
                    ExprOrSpread::Spread(expr) => visitor.visit_expr(expr),
                }
            }
        }
        crate::ExprKind::Assign(Assign { left, op: _, right }) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        crate::ExprKind::Binary(Binary { left, op: _, right }) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        crate::ExprKind::Unary(Unary { op: _, right }) => {
            visitor.visit_expr(right);
        }
        crate::ExprKind::Function(Function {
            type_params,
            params,
            body,
            type_ann,
            throws,
            is_async: _,
            is_gen: _,
        }) => {
            if let Some(type_params) = type_params {
                for type_param in type_params {
                    if let Some(bound) = &type_param.bound {
                        visitor.visit_type_ann(bound);
                    }
                    if let Some(default) = &type_param.default {
                        visitor.visit_type_ann(default);
                    }
                }
            }

            for param in params {
                visitor.visit_pattern(&param.pattern);
                if let Some(type_ann) = &param.type_ann {
                    visitor.visit_type_ann(type_ann);
                }
            }

            walk_block_or_expr(visitor, body);

            if let Some(type_ann) = type_ann {
                visitor.visit_type_ann(type_ann);
            }

            if let Some(throws) = throws {
                visitor.visit_type_ann(throws);
            }
        }
        crate::ExprKind::Class(Class {
            span: _,
            type_params,
            super_class: _,
            super_type_args,
            body,
        }) => {
            if let Some(type_params) = type_params {
                for type_param in type_params {
                    if let Some(bound) = &type_param.bound {
                        visitor.visit_type_ann(bound);
                    }
                    if let Some(default) = &type_param.default {
                        visitor.visit_type_ann(default);
                    }
                }
            }

            if let Some(super_type_args) = super_type_args {
                for type_arg in super_type_args {
                    visitor.visit_type_ann(type_arg);
                }
            }

            // TODO
            for member in body {
                match member {
                    ClassMember::Method(_) => {}
                    ClassMember::Getter(_) => {}
                    ClassMember::Setter(_) => {}
                    ClassMember::Constructor(_) => {}
                    ClassMember::Field(_) => {}
                }
            }
        }
        crate::ExprKind::Call(Call {
            callee,
            type_args,
            args,
            opt_chain: _,
            throws: _,
        }) => {
            visitor.visit_expr(callee);
            if let Some(type_args) = type_args {
                for type_arg in type_args {
                    visitor.visit_type_ann(type_arg);
                }
            }
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        crate::ExprKind::Member(Member {
            object,
            property,
            opt_chain: _,
        }) => {
            visitor.visit_expr(object);
            match property {
                MemberProp::Ident(_) => {}
                MemberProp::Computed(ComputedPropName { span: _, expr }) => {
                    visitor.visit_expr(expr)
                }
            };
        }
        crate::ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            visitor.visit_expr(cond);
            walk_block(visitor, consequent);
            if let Some(alternate) = alternate {
                walk_block_or_expr(visitor, alternate);
            }
        }
        crate::ExprKind::Match(Match { expr, arms }) => {
            visitor.visit_expr(expr);
            for MatchArm {
                span: _,
                pattern,
                guard,
                body,
            } in arms
            {
                visitor.visit_pattern(pattern);
                if let Some(guard) = guard {
                    visitor.visit_expr(guard);
                }
                walk_block_or_expr(visitor, body);
            }
        }
        crate::ExprKind::Try(Try {
            body,
            catch,
            finally,
        }) => {
            walk_block(visitor, body);
            if let Some(catch) = catch {
                if let Some(param) = &catch.param {
                    visitor.visit_pattern(param);
                }
                walk_block(visitor, &catch.body);
            }
            if let Some(finally) = finally {
                walk_block(visitor, finally);
            }
        }
        crate::ExprKind::Do(Do { body }) => walk_block(visitor, body),
        crate::ExprKind::Await(Await { arg, throws: _ }) => visitor.visit_expr(arg),
        crate::ExprKind::Yield(Yield { arg }) => visitor.visit_expr(arg),
        crate::ExprKind::Throw(Throw { arg, throws: _ }) => visitor.visit_expr(arg),
        crate::ExprKind::JSXElement(_) => {}  // TODO
        crate::ExprKind::JSXFragment(_) => {} // TODO
    }
}

pub fn walk_pattern<V: Visitor>(visitor: &mut V, pattern: &Pattern) {
    match &pattern.kind {
        crate::PatternKind::Ident(_) => {}
        crate::PatternKind::Rest(RestPat { arg }) => visitor.visit_pattern(arg),
        crate::PatternKind::Object(ObjectPat { props, optional: _ }) => {
            for prop in props {
                match prop {
                    ObjectPatProp::KeyValue(KeyValuePatProp {
                        span: _,
                        key: _,
                        value,
                        init,
                    }) => {
                        visitor.visit_pattern(value);
                        if let Some(init) = init {
                            visitor.visit_expr(init);
                        }
                    }
                    ObjectPatProp::Shorthand(_) => {}
                    ObjectPatProp::Rest(RestPat { arg }) => {
                        visitor.visit_pattern(arg);
                    }
                }
            }
        }
        crate::PatternKind::Tuple(TuplePat { elems, optional: _ }) => {
            for elem in elems.iter().flatten() {
                visitor.visit_pattern(&elem.pattern);
                if let Some(init) = &elem.init {
                    visitor.visit_expr(init);
                }
            }
        }
        crate::PatternKind::Lit(_) => {}
        crate::PatternKind::Is(_) => {}
        crate::PatternKind::Wildcard => {}
    }
}

pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Expr(ExprStmt { expr }) => visitor.visit_expr(expr),
        StmtKind::For(ForStmt {
            left: _,
            right: _,
            body: _,
        }) => todo!(),
        StmtKind::Return(ReturnStmt { arg }) => {
            if let Some(arg) = arg {
                visitor.visit_expr(arg);
            }
        }

        StmtKind::VarDecl(crate::VarDecl {
            is_declare: _,
            is_var: _,
            pattern,
            expr,
            type_ann,
        }) => {
            visitor.visit_pattern(pattern);
            if let Some(expr) = expr {
                visitor.visit_expr(expr);
            }
            if let Some(type_ann) = type_ann {
                visitor.visit_type_ann(type_ann);
            }
        }
        StmtKind::TypeDecl(TypeDecl {
            name: _,
            type_ann,
            type_params,
        }) => {
            if let Some(type_params) = type_params {
                for type_param in type_params {
                    if let Some(bound) = &type_param.bound {
                        visitor.visit_type_ann(bound);
                    }
                    if let Some(default) = &type_param.default {
                        visitor.visit_type_ann(default);
                    }
                }
            }
            visitor.visit_type_ann(type_ann);
        }
    }
}

// TODO
pub fn walk_type_ann<V: Visitor>(_visitor: &mut V, type_ann: &TypeAnn) {
    match &type_ann.kind {
        crate::TypeAnnKind::BoolLit(_) => {}
        crate::TypeAnnKind::Boolean => {}
        crate::TypeAnnKind::NumLit(_) => {}
        crate::TypeAnnKind::Number => {}
        crate::TypeAnnKind::StrLit(_) => {}
        crate::TypeAnnKind::String => {}
        crate::TypeAnnKind::Symbol => {}
        crate::TypeAnnKind::Null => {}
        crate::TypeAnnKind::Undefined => {}
        crate::TypeAnnKind::Unknown => {}
        crate::TypeAnnKind::Never => {}
        crate::TypeAnnKind::Object(_) => {}
        crate::TypeAnnKind::Tuple(_) => {}
        crate::TypeAnnKind::Array(_) => {}
        crate::TypeAnnKind::TypeRef(_, _) => {}
        crate::TypeAnnKind::Function(_) => {}
        crate::TypeAnnKind::Union(_) => {}
        crate::TypeAnnKind::Intersection(_) => {}
        crate::TypeAnnKind::IndexedAccess(_, _) => {}
        crate::TypeAnnKind::KeyOf(_) => {}
        crate::TypeAnnKind::Rest(_) => {}
        crate::TypeAnnKind::TypeOf(_) => {}
        crate::TypeAnnKind::Condition(_) => {}
        crate::TypeAnnKind::Match(_) => {}
        crate::TypeAnnKind::Wildcard => {}
        crate::TypeAnnKind::Infer(_) => {}
        crate::TypeAnnKind::Binary(_) => {}
    }
}

pub fn walk_block<V: Visitor>(visitor: &mut V, block: &Block) {
    for stmt in &block.stmts {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_block_or_expr<V: Visitor>(visitor: &mut V, block_or_expr: &BlockOrExpr) {
    match block_or_expr {
        BlockOrExpr::Block(block) => walk_block(visitor, block),
        BlockOrExpr::Expr(expr) => visitor.visit_expr(expr),
    }
}
