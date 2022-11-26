use crochet_ast::types::*;

pub trait Visitor {
    fn visit_type(&mut self, t: &mut Type);

    fn visit_children(&mut self, t: &mut Type) {
        match &mut t.kind {
            TypeKind::Var(_) => (), // no children
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
            TypeKind::Lit(_) => (),     // no children
            TypeKind::Keyword(_) => (), // no children
            TypeKind::Union(types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Intersection(types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Object(_) => todo!(),
            TypeKind::Ref(_) => todo!(),
            TypeKind::Tuple(types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Array(t) => self.visit_children(t),
            TypeKind::Rest(t) => self.visit_children(t),
            TypeKind::This => (), // no children
            TypeKind::KeyOf(t) => self.visit_children(t),
            TypeKind::IndexAccess(access) => {
                self.visit_children(&mut access.object);
                self.visit_children(&mut access.index);
            }
            TypeKind::MappedType(mapped) => {
                self.visit_children(&mut mapped.t);
            }
            TypeKind::Generic(gen) => {
                self.visit_children(&mut gen.t);
            }
        }

        self.visit_type(t);
    }
}
