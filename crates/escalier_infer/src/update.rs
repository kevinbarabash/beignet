use derive_visitor::{DriveMut, VisitorMut};

use escalier_ast::types::TypeKind;
use escalier_ast::values::*;

use crate::substitutable::{Subst, Substitutable};

#[derive(VisitorMut)]
#[visitor(Pattern(enter), Expr(enter), TypeAnn(enter))]
struct UpdateVisitor<'a> {
    s: &'a Subst,
}

impl UpdateVisitor<'_> {
    fn enter_expr(&mut self, expr: &mut Expr) {
        if let Some(t) = &expr.inferred_type {
            if let TypeKind::Var(_) = &t.kind {
                expr.inferred_type = Some(t.apply(self.s));
            }
        }
    }
    fn enter_pattern(&mut self, pattern: &mut Pattern) {
        if let Some(t) = &pattern.inferred_type {
            if let TypeKind::Var(_) = &t.kind {
                pattern.inferred_type = Some(t.apply(self.s));
            }
        }
    }
    fn enter_type_ann(&mut self, type_ann: &mut TypeAnn) {
        if let Some(t) = &type_ann.inferred_type {
            if let TypeKind::Var(_) = &t.kind {
                type_ann.inferred_type = Some(t.apply(self.s));
            }
        }
    }
}

pub fn update_expr(expr: &mut Expr, s: &Subst) {
    let mut visitor = UpdateVisitor { s };
    expr.drive_mut(&mut visitor);
}

pub fn update_pattern(pattern: &mut Pattern, s: &Subst) {
    let mut visitor = UpdateVisitor { s };
    pattern.drive_mut(&mut visitor);
}

pub fn update_type_ann(type_ann: &mut TypeAnn, s: &Subst) {
    let mut visitor = UpdateVisitor { s };
    type_ann.drive_mut(&mut visitor);
}
