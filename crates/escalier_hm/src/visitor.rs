use generational_arena::Index;

use escalier_ast::Literal as Lit;

use crate::types::*;

pub trait KeyValueStore<K, V> {
    fn get_type(&mut self, idx: &K) -> (K, V);
    fn put_type(&mut self, t: V) -> K;
}

pub trait Visitor: KeyValueStore<Index, Type> {
    fn visit_type_var(&mut self, _: &Variable, idx: &Index) -> Index;

    fn visit_type_ref(&mut self, tref: &Constructor, idx: &Index) -> Index {
        let t = self.get_type(idx).1;
        let t = Type {
            kind: TypeKind::Constructor(Constructor {
                types: self.visit_indexes(&tref.types),
                ..tref.to_owned()
            }),
            provenance: t.provenance,
        };
        self.put_type(t)
    }

    fn visit_union(&mut self, union: &Union) -> Union {
        Union {
            types: self.visit_indexes(&union.types),
        }
    }

    fn visit_intersection(&mut self, union: &Intersection) -> Intersection {
        Intersection {
            types: self.visit_indexes(&union.types),
        }
    }

    fn visit_tuple(&mut self, tuple: &Tuple) -> Tuple {
        Tuple {
            types: self.visit_indexes(&tuple.types),
        }
    }

    fn visit_keyword(&self, keyword: &Keyword) -> Keyword {
        keyword.clone()
    }

    fn visit_primitive(&self, primitive: &Primitive) -> Primitive {
        primitive.clone()
    }

    fn visit_literal(&self, lit: &Lit) -> Lit {
        lit.clone()
    }

    fn visit_type_param(&mut self, type_param: &TypeParam) -> TypeParam {
        TypeParam {
            name: type_param.name.to_owned(),
            constraint: type_param
                .constraint
                .as_ref()
                .map(|constraint| self.visit_index(constraint)),
            default: type_param
                .default
                .as_ref()
                .map(|default| self.visit_index(default)),
        }
    }

    fn visit_type_params(
        &mut self,
        type_params: &Option<Vec<TypeParam>>,
    ) -> Option<Vec<TypeParam>> {
        type_params.as_ref().map(|type_params| {
            type_params
                .iter()
                .map(|type_param| self.visit_type_param(type_param))
                .collect::<Vec<_>>()
        })
    }

    fn visit_func_param(&mut self, func_param: &FuncParam) -> FuncParam {
        FuncParam {
            t: self.visit_index(&func_param.t),
            ..func_param.to_owned()
        }
    }

    fn visit_func_params(&mut self, params: &[FuncParam]) -> Vec<FuncParam> {
        params
            .iter()
            .map(|param| self.visit_func_param(param))
            .collect::<Vec<_>>()
    }

    fn visit_object(&mut self, obj: &Object) -> Object {
        let elems: Vec<_> = obj
            .elems
            .iter()
            .map(|elem| match elem {
                TObjElem::Constructor(TCallable {
                    params,
                    ret,
                    type_params,
                }) => TObjElem::Constructor(TCallable {
                    params: self.visit_func_params(params),
                    ret: self.visit_index(ret),
                    type_params: self.visit_type_params(type_params),
                }),
                TObjElem::Call(TCallable {
                    params,
                    ret,
                    type_params,
                }) => TObjElem::Call(TCallable {
                    params: self.visit_func_params(params),
                    ret: self.visit_index(ret),
                    type_params: self.visit_type_params(type_params),
                }),
                TObjElem::Method(TMethod {
                    name,
                    params,
                    ret,
                    type_params,
                    is_mutating,
                }) => TObjElem::Method(TMethod {
                    name: name.to_owned(),
                    params: self.visit_func_params(params),
                    ret: self.visit_index(ret),
                    type_params: self.visit_type_params(type_params),
                    is_mutating: *is_mutating,
                }),
                TObjElem::Getter(TGetter { name, ret }) => TObjElem::Getter(TGetter {
                    name: name.to_owned(),
                    ret: self.visit_index(ret),
                }),
                TObjElem::Setter(TSetter { name, param }) => TObjElem::Setter(TSetter {
                    name: name.to_owned(),
                    param: self.visit_func_param(param),
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

        Object { elems }
    }

    fn visit_function(&mut self, func: &Function) -> Function {
        let params = func
            .params
            .iter()
            .map(|param| self.visit_func_param(param))
            .collect::<Vec<_>>();
        let ret = self.visit_index(&func.ret);
        let type_params = self.visit_type_params(&func.type_params);
        Function {
            params,
            ret,
            type_params,
        }
    }

    fn visit_rest(&mut self, rest: &Rest) -> Rest {
        Rest {
            arg: self.visit_index(&rest.arg),
        }
    }

    fn visit_keyof(&mut self, keyof: &KeyOf) -> KeyOf {
        KeyOf {
            t: self.visit_index(&keyof.t),
        }
    }

    fn visit_indexed_access(&mut self, indexed_access: &IndexedAccess) -> IndexedAccess {
        IndexedAccess {
            obj: self.visit_index(&indexed_access.obj),
            index: self.visit_index(&indexed_access.index),
        }
    }

    fn visit_conditional(&mut self, conditional: &Conditional) -> Conditional {
        Conditional {
            check: self.visit_index(&conditional.check),
            extends: self.visit_index(&conditional.extends),
            true_type: self.visit_index(&conditional.true_type),
            false_type: self.visit_index(&conditional.false_type),
        }
    }

    fn visit_infer(&mut self, infer: &Infer, idx: Index) -> Index {
        idx // Do nothing by default
    }

    fn visit_index(&mut self, idx: &Index) -> Index {
        let (idx, t) = self.get_type(idx);
        let kind = match &t.kind {
            TypeKind::Variable(tvar) => return self.visit_type_var(tvar, &idx),
            TypeKind::Union(union) => TypeKind::Union(self.visit_union(union)),
            TypeKind::Intersection(intersection) => {
                TypeKind::Intersection(self.visit_intersection(intersection))
            }
            TypeKind::Tuple(tuple) => TypeKind::Tuple(self.visit_tuple(tuple)),
            TypeKind::Constructor(tref) => return self.visit_type_ref(tref, &idx),
            TypeKind::Keyword(keyword) => TypeKind::Keyword(self.visit_keyword(keyword)),
            TypeKind::Primitive(primitive) => TypeKind::Primitive(self.visit_primitive(primitive)),
            TypeKind::Literal(lit) => TypeKind::Literal(self.visit_literal(lit)),
            TypeKind::Function(func) => TypeKind::Function(self.visit_function(func)),
            TypeKind::Object(obj) => TypeKind::Object(self.visit_object(obj)),
            TypeKind::Rest(rest) => TypeKind::Rest(self.visit_rest(rest)),
            TypeKind::KeyOf(keyof) => TypeKind::KeyOf(self.visit_keyof(keyof)),
            TypeKind::IndexedAccess(indexed_access) => {
                TypeKind::IndexedAccess(self.visit_indexed_access(indexed_access))
            }
            TypeKind::Conditional(conditional) => {
                TypeKind::Conditional(self.visit_conditional(conditional))
            }
            TypeKind::Infer(infer) => return self.visit_infer(infer, idx),
        };
        let new_t = Type {
            kind,
            provenance: t.provenance,
        };
        self.put_type(new_t)
    }

    fn visit_indexes(&mut self, idxs: &[Index]) -> Vec<Index> {
        idxs.iter().map(|idx| self.visit_index(idx)).collect()
    }
}
