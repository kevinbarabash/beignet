use generational_arena::Index;

use crate::ast::Lit;
use crate::types::*;

pub trait KeyValueStore<K, V> {
    fn get_type(&mut self, idx: &K) -> (K, V);
    fn put_type(&mut self, t: V) -> K;
}

pub trait Visitor: KeyValueStore<Index, Type> {
    fn visit_type_var(&mut self, _: &Variable, idx: &Index) -> Index;

    fn visit_type_ref(&mut self, tref: &Constructor, _: &Index) -> Index {
        let t = Type {
            kind: TypeKind::Constructor(Constructor {
                types: self.visit_indexes(&tref.types),
                ..tref.to_owned()
            }),
        };
        self.put_type(t)
    }

    fn visit_literal(&self, lit: &Lit) -> Lit {
        lit.clone()
    }

    fn visit_object(&mut self, obj: &Object) -> Object {
        let props: Vec<_> = obj
            .props
            .iter()
            .map(|prop| match prop {
                TObjElem::Method(method) => TObjElem::Method(TMethod {
                    params: self.visit_indexes(&method.params),
                    ret: self.visit_index(&method.ret),
                    ..method.to_owned()
                }),
                TObjElem::Index(index) => TObjElem::Index(TIndex {
                    t: self.visit_index(&index.t),
                    ..index.clone()
                }),
                TObjElem::Prop(prop) => TObjElem::Prop(TProp {
                    t: self.visit_index(&prop.t),
                    ..prop.clone()
                }),
            })
            .collect();

        Object { props }
    }

    fn visit_function(&mut self, func: &Function) -> Function {
        Function {
            params: self.visit_indexes(&func.params),
            ret: self.visit_index(&func.ret),
            ..func.clone()
        }
    }

    fn visit_rest(&mut self, rest: &Rest) -> Rest {
        Rest {
            arg: self.visit_index(&rest.arg),
        }
    }

    fn visit_index(&mut self, idx: &Index) -> Index {
        let (idx, t) = self.get_type(idx);
        let kind = match &t.kind {
            TypeKind::Variable(tvar) => return self.visit_type_var(tvar, &idx),
            TypeKind::Constructor(tref) => return self.visit_type_ref(tref, &idx),
            TypeKind::Literal(lit) => TypeKind::Literal(self.visit_literal(lit)),
            TypeKind::Function(func) => TypeKind::Function(self.visit_function(func)),
            TypeKind::Object(obj) => TypeKind::Object(self.visit_object(obj)),
            TypeKind::Rest(rest) => TypeKind::Rest(self.visit_rest(rest)),
        };
        let new_t = Type { kind };
        self.put_type(new_t)
    }

    fn visit_indexes(&mut self, idxs: &[Index]) -> Vec<Index> {
        idxs.iter().map(|idx| self.visit_index(idx)).collect()
    }
}
