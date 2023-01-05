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
            Statement::ClassDecl {
                ident: _, class: _, ..
            } => {
                // TODO: add `visit_class()` method
            }
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

        match &mut pat.kind {
            PatternKind::Ident(_) => (), // leaf node
            PatternKind::Rest(RestPat { arg }) => {
                self._visit_pattern(arg);
            }
            PatternKind::Object(ObjectPat { props, optional: _ }) => {
                props.iter_mut().for_each(|prop| match prop {
                    ObjectPatProp::KeyValue(KeyValuePatProp {
                        key: _,
                        value,
                        init,
                        ..
                    }) => {
                        self._visit_pattern(value);
                        if let Some(init) = init {
                            self._visit_expr(init);
                        }
                    }
                    ObjectPatProp::Shorthand(ShorthandPatProp { ident: _, init, .. }) => {
                        if let Some(init) = init {
                            self._visit_expr(init);
                        }
                    }
                    ObjectPatProp::Rest(RestPat { arg }) => {
                        self._visit_pattern(arg);
                    }
                });
            }
            PatternKind::Array(ArrayPat { elems, optional: _ }) => {
                elems.iter_mut().for_each(|elem| {
                    if let Some(elem) = elem {
                        let ArrayPatElem { pattern, init } = elem;
                        self._visit_pattern(pattern);
                        if let Some(init) = init {
                            self._visit_expr(init);
                        }
                    }
                });
            }
            PatternKind::Lit(_) => (),   // leaf node
            PatternKind::Is(_) => (),    // leaf node
            PatternKind::Wildcard => (), // leaf node
        }
    }

    fn _visit_type_ann(&mut self, type_ann: &mut TypeAnn) {
        self.visit_type_ann(type_ann);

        match &mut type_ann.kind {
            TypeAnnKind::Lam(_) => (),
            TypeAnnKind::Lit(_) => (),
            TypeAnnKind::Keyword(_) => (),
            TypeAnnKind::Object(_) => (),
            TypeAnnKind::TypeRef(_) => (),
            TypeAnnKind::Union(_) => (),
            TypeAnnKind::Intersection(_) => (),
            TypeAnnKind::Tuple(TupleType { types }) => {
                types.iter_mut().for_each(|t| self._visit_type_ann(t))
            }
            TypeAnnKind::Array(ArrayType { elem_type }) => {
                self._visit_type_ann(elem_type);
            }
            TypeAnnKind::KeyOf(_) => (),
            TypeAnnKind::Query(_) => (),
            TypeAnnKind::IndexedAccess(_) => (),
            TypeAnnKind::Mapped(_) => (),
            TypeAnnKind::Conditional(_) => (),
            TypeAnnKind::Mutable(_) => (),
        }
    }

    fn _visit_expr(&mut self, expr: &mut Expr) {
        self.visit_expr(expr);

        match &mut expr.kind {
            ExprKind::App(App { lam, args }) => {
                self._visit_expr(lam);
                args.iter_mut()
                    .for_each(|arg| self._visit_expr(&mut arg.expr));
            }
            ExprKind::New(New { expr, args }) => {
                self._visit_expr(expr);
                args.iter_mut()
                    .for_each(|arg| self._visit_expr(&mut arg.expr));
            }
            ExprKind::Fix(Fix { expr }) => {
                self._visit_expr(expr);
            }
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
                type_params,
                ..
            }) => {
                if let Some(type_params) = type_params {
                    type_params.iter_mut().for_each(|type_param| {
                        // TODO: add visit_type_param() method
                        if let Some(constraint) = &mut type_param.constraint {
                            self._visit_type_ann(constraint);
                        }
                        if let Some(default) = &mut type_param.default {
                            self._visit_type_ann(default);
                        }
                    });
                };
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
            ExprKind::Member(Member { obj, prop: _ }) => {
                self._visit_expr(obj);
            }
            ExprKind::Empty => (), // leaf node
            ExprKind::TemplateLiteral(TemplateLiteral { exprs, quasis: _ }) => {
                exprs.iter_mut().for_each(|expr| self._visit_expr(expr));
            }
            ExprKind::TaggedTemplateLiteral(TaggedTemplateLiteral { tag: _, template }) => {
                let TemplateLiteral { exprs, quasis: _ } = template;
                exprs.iter_mut().for_each(|expr| self._visit_expr(expr));
            }
            ExprKind::Match(Match { expr, arms }) => {
                self._visit_expr(expr);
                arms.iter_mut().for_each(|arm| {
                    let Arm {
                        pattern,
                        guard,
                        body,
                        ..
                    } = arm;
                    self._visit_pattern(pattern);
                    if let Some(guard) = guard {
                        self._visit_expr(guard);
                    }
                    self._visit_expr(body);
                })
            }
            ExprKind::Class(Class { ident: _, body }) => {
                for member in body {
                    match member {
                        ClassMember::Constructor(Constructor { params, body }) => {
                            params.iter_mut().for_each(|param| {
                                // TODO: add visit_fn_param() method
                                let EFnParam { pat, type_ann, .. } = param;
                                self._visit_pattern(pat);
                                if let Some(type_ann) = type_ann {
                                    self._visit_type_ann(type_ann);
                                }
                            });
                            self._visit_expr(body);
                        }
                        ClassMember::Method(ClassMethod {
                            key: _,
                            kind: _,
                            lambda,
                        }) => {
                            lambda.params.iter_mut().for_each(|param| {
                                // TODO: add visit_fn_param() method
                                let EFnParam { pat, type_ann, .. } = param;
                                self._visit_pattern(pat);
                                if let Some(type_ann) = type_ann {
                                    self._visit_type_ann(type_ann);
                                }
                            });
                            self._visit_expr(&mut lambda.body);
                        }
                        ClassMember::Prop(ClassProp { key: _, value, .. }) => {
                            if let Some(value) = value {
                                self._visit_expr(value);
                            }
                        }
                    }
                }
            }
        }
    }
}
