use crochet_ast::types::*;

pub trait Visitor {
    // fn visit_var(&mut self, tv: &mut TVar) {}
    // fn visit_app(&mut self, app: &mut TApp) {}
    // fn visit_lam(&mut self, lam: &mut TLam) {}
    // fn visit_lit(&mut self, lit: &mut TLit) {}
    // fn visit_keyword(&mut self, keyword: &mut TKeyword) {}
    // fn visit_union(&mut self, union: &mut [Type]) {}
    // fn visit_intersection(&mut self, intersection: &mut [Type]) {}
    // fn visit_object(&mut self, object: &mut TObject) {}
    // fn visit_ref(&mut self, r#ref: &mut TRef) {}
    // fn visit_tuple(&mut self, tuple: &mut [Type]) {}
    // fn visit_array(&mut self, array: &mut [Type]) {}
    // fn visit_key_of(&mut self, key_of: &mut [Type]) {}
    // fn visit_mutable(&mut self, mutable: &mut Type) {}
    // fn visit_index_access(&mut self, index_access: &mut TIndexAccess) {}
    // fn visit_generic(&mut self, generic: &mut TGeneric) {}

    fn visit_type(&mut self, t: &mut Type);

    fn visit_children(&mut self, t: &mut Type) {
        match t.kind {
            TypeKind::Var(_) => (), // no children
            TypeKind::App(ref mut app) => {
                app.args.iter_mut().for_each(|arg| self.visit_children(arg));
                self.visit_children(app.ret.as_mut());
            }
            TypeKind::Lam(ref mut lam) => {
                lam.params
                    .iter_mut()
                    .for_each(|TFnParam { ref mut t, .. }| self.visit_children(t));
                self.visit_children(lam.ret.as_mut());
            }
            TypeKind::Lit(_) => (),     // no children
            TypeKind::Keyword(_) => (), // no children
            TypeKind::Union(ref mut types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Intersection(ref mut types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Object(_) => todo!(),
            TypeKind::Ref(_) => todo!(),
            TypeKind::Tuple(ref mut types) => {
                types.iter_mut().for_each(|t| self.visit_children(t));
            }
            TypeKind::Array(ref mut t) => self.visit_children(t),
            TypeKind::Rest(ref mut t) => self.visit_children(t),
            TypeKind::This => (), // no children
            TypeKind::KeyOf(ref mut t) => self.visit_children(t),
            TypeKind::Mutable(ref mut t) => self.visit_children(t),
            TypeKind::IndexAccess(ref mut access) => {
                self.visit_children(&mut access.object);
                self.visit_children(&mut access.index);
            }
            TypeKind::Generic(ref mut gen) => {
                self.visit_children(&mut gen.t);
            }
        }

        self.visit_type(t);
    }
}
