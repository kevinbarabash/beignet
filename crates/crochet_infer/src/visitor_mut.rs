use crochet_ast::types::*;

pub trait VisitorMut {
    fn visit_type(&mut self, t: &mut Type);

    fn visit_obj_elem(&mut self, elem: &mut TObjElem) {
        match elem {
            TObjElem::Call(callable) | TObjElem::Constructor(callable) => {
                let TCallable {
                    params,
                    ret,
                    type_params,
                } = callable;
                if let Some(type_params) = type_params {
                    type_params
                        .iter_mut()
                        .for_each(|type_param| self.visit_type_param(type_param));
                }
                params
                    .iter_mut()
                    .for_each(|param| self.visit_fn_param(param));
                self.visit_children(ret.as_mut());
            }
            TObjElem::Method(TMethod {
                name: _,
                params,
                ret,
                type_params,
                is_mutating: _,
            }) => {
                if let Some(type_params) = type_params {
                    type_params
                        .iter_mut()
                        .for_each(|type_param| self.visit_type_param(type_param));
                }
                params
                    .iter_mut()
                    .for_each(|param| self.visit_fn_param(param));
                self.visit_children(ret.as_mut());
            }
            TObjElem::Getter(_) => {
                // TODO
            }
            TObjElem::Setter(_) => {
                // TODO
            }
            TObjElem::Index(_) => {
                // TODO
            }
            TObjElem::Prop(_) => {
                // TODO
            }
        }
    }

    fn visit_fn_param(&mut self, param: &mut TFnParam) {
        self.visit_children(&mut param.t);
    }

    fn visit_pattern(&mut self, _pattern: &mut TPat) {
        // TODO
    }

    fn visit_type_param(&mut self, _type_param: &mut TypeParam) {
        // TODO
    }

    fn visit_children(&mut self, t: &mut Type) {
        match &mut t.kind {
            TypeKind::Var(_) => (), // leaf node
            TypeKind::App(app) => {
                app.args.iter_mut().for_each(|arg| self.visit_children(arg));
                self.visit_children(app.ret.as_mut());
            }
            TypeKind::Lam(TLam {
                type_params,
                params,
                ret,
            }) => {
                if let Some(type_params) = type_params {
                    type_params.iter_mut().for_each(|type_param| {
                        if let Some(constraint) = &mut type_param.constraint {
                            self.visit_children(constraint);
                        }
                        if let Some(default) = &mut type_param.default {
                            self.visit_children(default);
                        }
                    });
                }
                params
                    .iter_mut()
                    .for_each(|TFnParam { t, .. }| self.visit_children(t));
                self.visit_children(ret.as_mut());
            }
            TypeKind::Lit(_) => (),     // leaf node
            TypeKind::Keyword(_) => (), // leaf node
            TypeKind::Union(types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Intersection(types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Object(_) => todo!(),
            TypeKind::Ref(alias) => {
                if let Some(type_args) = &mut alias.type_args {
                    type_args.iter_mut().for_each(|t| self.visit_children(t));
                }
            }
            TypeKind::Tuple(types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Array(t) => self.visit_children(t),
            TypeKind::Rest(t) => self.visit_children(t),
            TypeKind::This => (), // leaf node
            TypeKind::KeyOf(t) => self.visit_children(t),
            TypeKind::IndexAccess(access) => {
                self.visit_children(&mut access.object);
                self.visit_children(&mut access.index);
            }
            TypeKind::MappedType(mapped) => {
                self.visit_children(&mut mapped.t);
            }
            TypeKind::ConditionalType(TConditionalType {
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => {
                self.visit_children(check_type);
                self.visit_children(extends_type);
                self.visit_children(true_type);
                self.visit_children(false_type);
            }
            TypeKind::InferType(_) => (), // leaf node
        }

        self.visit_type(t);
    }
}
