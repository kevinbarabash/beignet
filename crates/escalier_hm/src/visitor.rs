use generational_arena::Index;

use escalier_ast::Literal as Lit;

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
        let props: Vec<_> = obj
            .props
            .iter()
            .map(|prop| match prop {
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

        Object { props }
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

    fn visit_utility(&mut self, utility: &Utility) -> Index {
        let t = Type {
            kind: TypeKind::Utility(Utility {
                types: self.visit_indexes(&utility.types),
                ..utility.to_owned()
            }),
        };
        self.put_type(t)
    }

    fn visit_mutable(&mut self, mutable: &Mutable) -> Mutable {
        Mutable {
            t: self.visit_index(&mutable.t),
        }
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
            TypeKind::Literal(lit) => TypeKind::Literal(self.visit_literal(lit)),
            TypeKind::Function(func) => TypeKind::Function(self.visit_function(func)),
            TypeKind::Object(obj) => TypeKind::Object(self.visit_object(obj)),
            TypeKind::Rest(rest) => TypeKind::Rest(self.visit_rest(rest)),
            TypeKind::Utility(utility) => return self.visit_utility(utility),
            TypeKind::Mutable(mutable) => TypeKind::Mutable(self.visit_mutable(mutable)),
        };
        let new_t = Type { kind };
        self.put_type(new_t)
    }

    fn visit_indexes(&mut self, idxs: &[Index]) -> Vec<Index> {
        idxs.iter().map(|idx| self.visit_index(idx)).collect()
    }
}
