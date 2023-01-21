use crochet_ast::types::*;

// TODO: rename this to VisitorMut and create a non-mutable Visitor
pub trait Visitor {
    fn visit_type(&mut self, t: &mut Type);

    fn visit_children(&mut self, t: &mut Type) {
        match &mut t.kind {
            TypeKind::Var(_) => (), // leaf node
            TypeKind::App(app) => {
                app.args.iter_mut().for_each(|arg| self.visit_children(arg));
                self.visit_children(app.ret.as_mut());
            }
            TypeKind::Lam(lam) => {
                lam.params
                    .iter_mut()
                    .for_each(|TFnParam { t, .. }| self.visit_children(t));
                self.visit_children(lam.ret.as_mut());
            }
            TypeKind::GenLam(TGenLam { type_params, lam }) => {
                type_params.iter_mut().for_each(|type_param| {
                    if let Some(constraint) = &mut type_param.constraint {
                        self.visit_children(constraint);
                    }
                    if let Some(default) = &mut type_param.default {
                        self.visit_children(default);
                    }
                });
                lam.params
                    .iter_mut()
                    .for_each(|TFnParam { t, .. }| self.visit_children(t));
                self.visit_children(lam.ret.as_mut());
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
