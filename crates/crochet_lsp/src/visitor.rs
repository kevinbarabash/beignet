use crochet_ast::values::*;

pub trait Visitor {
    fn visit_program(&mut self, prog: &Program);
    fn visit_statement(&mut self, stmt: &Statement);
    fn visit_pattern(&mut self, pat: &Pattern);
    fn visit_type_ann(&mut self, type_ann: &TypeAnn);
    fn visit_expr(&mut self, expr: &Expr);

    // This is the main entry point
    fn visit(&mut self, prog: &mut Program) {
        self.visit_program(prog);

        prog.body
            .iter_mut()
            .for_each(|stmt| self._visit_statement(stmt));
    }

    fn _visit_statement(&mut self, stmt: &mut Statement) {
        self.visit_statement(stmt);

        match stmt {
            Statement::VarDecl {
                pattern,
                type_ann,
                init,
                ..
            } => {
                self._visit_pattern(pattern);
                if let Some(type_ann) = type_ann {
                    self._visit_type_ann(type_ann);
                }
                if let Some(init) = init {
                    self._visit_expr(init);
                }
            }
            Statement::TypeDecl {
                type_ann,
                type_params: _, // TODO
                ..
            } => {
                self._visit_type_ann(type_ann);
            }
            Statement::Expr { expr, .. } => {
                self._visit_expr(expr);
            }
        }
    }

    fn _visit_pattern(&mut self, pat: &mut Pattern) {
        self.visit_pattern(pat);

        // TODO: traverse children
    }

    fn _visit_type_ann(&mut self, type_ann: &mut TypeAnn) {
        self.visit_type_ann(type_ann);

        // TODO: traverse children
    }

    fn _visit_expr(&mut self, expr: &mut Expr) {
        self.visit_expr(expr);

        match &mut expr.kind {
            ExprKind::App(App { lam, args }) => {
                self._visit_expr(lam);
                args.iter_mut()
                    .for_each(|arg| self._visit_expr(&mut arg.expr));
            }
            ExprKind::New(_) => todo!(),
            ExprKind::Fix(_) => todo!(),
            ExprKind::Ident(_) => (), // leaf node
            ExprKind::IfElse(IfElse {
                cond,
                consequent,
                alternate,
            }) => {
                self._visit_expr(cond);
                self._visit_expr(consequent);
                if let Some(alternate) = alternate {
                    self._visit_expr(alternate);
                }
            }
            ExprKind::JSXElement(_) => todo!(),
            ExprKind::Lambda(Lambda {
                params,
                body,
                return_type,
                type_params: _, // TODO
                ..
            }) => {
                params.iter_mut().for_each(|param| {
                    // TODO: add visit_fn_param() method
                    let EFnParam { pat, type_ann, .. } = param;
                    self._visit_pattern(pat);
                    if let Some(type_ann) = type_ann {
                        self._visit_type_ann(type_ann);
                    }
                });
                if let Some(return_type) = return_type {
                    self._visit_type_ann(return_type);
                }
                // TODO: revisit how we store the body in the AST, maybe we can
                // defer converting it to `let-in` until later in the process.
                // The problem with the `let-in` form is that it results in a
                // of nesting that we could do with out.
                self._visit_expr(body);
            }
            ExprKind::Let(Let {
                pattern,
                type_ann,
                init,
                body,
            }) => {
                if let Some(pattern) = pattern {
                    self._visit_pattern(pattern);
                }
                if let Some(type_ann) = type_ann {
                    self._visit_type_ann(type_ann);
                }
                self._visit_expr(init);
                self._visit_expr(body);
            }
            ExprKind::Assign(Assign { left, right, .. }) => {
                self._visit_expr(left);
                self._visit_expr(right);
            }
            ExprKind::LetExpr(_) => todo!(),
            ExprKind::Lit(_) => (),     // leaf node
            ExprKind::Keyword(_) => (), // leaf node
            ExprKind::BinaryExpr(BinaryExpr { left, right, .. }) => {
                self._visit_expr(left);
                self._visit_expr(right);
            }
            ExprKind::UnaryExpr(UnaryExpr { arg, .. }) => {
                self._visit_expr(arg);
            }
            ExprKind::Obj(Obj { props }) => {
                props.iter_mut().for_each(|prop| match prop {
                    PropOrSpread::Spread(_) => todo!(),
                    PropOrSpread::Prop(prop) => match prop.as_mut() {
                        Prop::Shorthand(_) => todo!(),
                        Prop::KeyValue(KeyValueProp { key: _, value }) => {
                            self._visit_expr(value);
                        }
                    },
                });
            }
            ExprKind::Await(Await { expr }) => {
                self._visit_expr(expr);
            }
            ExprKind::Tuple(Tuple { elems }) => {
                elems
                    .iter_mut()
                    .for_each(|elem| self._visit_expr(&mut elem.expr));
            }
            ExprKind::Member(_) => todo!(),
            ExprKind::Empty => (), // leaf node
            ExprKind::TemplateLiteral(_) => todo!(),
            ExprKind::TaggedTemplateLiteral(_) => todo!(),
            ExprKind::Match(_) => todo!(),
        }
    }
}
