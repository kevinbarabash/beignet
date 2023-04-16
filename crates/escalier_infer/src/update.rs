use derive_visitor::{DriveMut, VisitorMut};

use escalier_ast::types::{Type, TypeKind};
use escalier_ast::values::*;

use crate::checker::Checker;
use crate::substitutable::{Subst, Substitutable};

type PatternType = Pattern<Type>;
type ExprType = Expr<Type>;
type TypeAnnType = TypeAnn<Type>;

#[derive(VisitorMut)]
#[visitor(PatternType(enter), ExprType(enter), TypeAnnType(enter))]
struct UpdateVisitor<'a> {
    checker: &'a mut Checker,
    s: &'a Subst,
}

impl UpdateVisitor<'_> {
    fn enter_expr_type(&mut self, expr: &mut Expr<Type>) {
        if let Some(t) = &expr.inferred_type {
            if let TypeKind::Var(_) = &t.kind {
                expr.inferred_type = Some(t.apply(self.s, self.checker));
            }
        }
    }
    fn enter_pattern_type(&mut self, pattern: &mut Pattern<Type>) {
        if let Some(t) = &pattern.inferred_type {
            if let TypeKind::Var(_) = &t.kind {
                pattern.inferred_type = Some(t.apply(self.s, self.checker));
            }
        }
    }
    fn enter_type_ann_type(&mut self, type_ann: &mut TypeAnn<Type>) {
        if let Some(t) = &type_ann.inferred_type {
            if let TypeKind::Var(_) = &t.kind {
                type_ann.inferred_type = Some(t.apply(self.s, self.checker));
            }
        }
    }
}

pub fn update_expr(expr: &mut Expr<Type>, s: &Subst) {
    let mut checker = Checker::default();
    let mut visitor = UpdateVisitor {
        checker: &mut checker,
        s,
    };
    expr.drive_mut(&mut visitor);
}

pub fn update_pattern(pattern: &mut Pattern<Type>, s: &Subst) {
    let mut checker = Checker::default();
    let mut visitor = UpdateVisitor {
        checker: &mut checker,
        s,
    };
    pattern.drive_mut(&mut visitor);
}

pub fn update_type_ann(type_ann: &mut TypeAnn<Type>, s: &Subst) {
    let mut checker = Checker::default();
    let mut visitor = UpdateVisitor {
        checker: &mut checker,
        s,
    };
    type_ann.drive_mut(&mut visitor);
}
