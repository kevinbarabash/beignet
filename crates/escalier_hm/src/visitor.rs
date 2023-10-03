use generational_arena::Index;

use crate::key_value_store::KeyValueStore;
use crate::types::*;

pub trait Visitor: KeyValueStore<Index, Type> + Sized {
    fn visit_index(&mut self, index: &Index) {
        walk_index(self, index)
    }
}

pub fn walk_index<V: Visitor>(visitor: &mut V, index: &Index) {
    let t = visitor.get_type(index);

    match &t.kind {
        TypeKind::TypeVar(TypeVar {
            id: _,
            instance,
            constraint,
        }) => {
            instance.map(|instance| visitor.visit_index(&instance));
            constraint.map(|constraint| visitor.visit_index(&constraint));
        }
        TypeKind::TypeRef(TypeRef { name: _, types }) => {
            walk_indexes(visitor, types);
        }
        TypeKind::Union(Union { types }) => {
            walk_indexes(visitor, types);
        }
        TypeKind::Intersection(Intersection { types }) => {
            walk_indexes(visitor, types);
        }
        TypeKind::Tuple(Tuple { types }) => {
            walk_indexes(visitor, types);
        }
        TypeKind::Array(Array { t }) => {
            visitor.visit_index(t);
        }
        TypeKind::Keyword(_) => (),
        TypeKind::Primitive(_) => (),
        TypeKind::Literal(_) => (),
        TypeKind::Function(function) => walk_function(visitor, function),
        TypeKind::Object(Object { elems }) => {
            elems.iter().for_each(|elem| match elem {
                TObjElem::Constructor(function) => walk_function(visitor, function),
                TObjElem::Call(function) => walk_function(visitor, function),
                TObjElem::Mapped(mapped) => {
                    visitor.visit_index(&mapped.key);
                    visitor.visit_index(&mapped.value);
                    visitor.visit_index(&mapped.source);
                    if let Some(check) = mapped.check {
                        visitor.visit_index(&check)
                    }
                    if let Some(extends) = mapped.extends {
                        visitor.visit_index(&extends)
                    }
                }
                TObjElem::Method(method) => {
                    walk_func_params(visitor, &method.params);
                    visitor.visit_index(&method.ret);
                    walk_type_params(visitor, &method.type_params);
                    method.throws.map(|throws| visitor.visit_index(&throws));
                }
                TObjElem::Prop(prop) => visitor.visit_index(&prop.t),
            });
        }
        TypeKind::Rest(Rest { arg }) => {
            visitor.visit_index(arg);
        }
        TypeKind::KeyOf(KeyOf { t }) => {
            visitor.visit_index(t);
        }
        TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
            visitor.visit_index(obj);
            visitor.visit_index(index);
        }
        TypeKind::Conditional(Conditional {
            check,
            extends,
            true_type,
            false_type,
        }) => {
            visitor.visit_index(check);
            visitor.visit_index(extends);
            visitor.visit_index(true_type);
            visitor.visit_index(false_type);
        }
        TypeKind::Infer(_) => (),
        TypeKind::Wildcard => (),
        TypeKind::Binary(BinaryT { op: _, left, right }) => {
            visitor.visit_index(left);
            visitor.visit_index(right);
        }
    }
}

pub fn walk_indexes<V: Visitor>(visitor: &mut V, indexes: &[Index]) {
    indexes.iter().for_each(|index| visitor.visit_index(index))
}

fn walk_func_params<V: Visitor>(visitor: &mut V, params: &[FuncParam]) {
    params
        .iter()
        .for_each(|param| walk_func_param(visitor, param))
}

fn walk_func_param<V: Visitor>(visitor: &mut V, func_param: &FuncParam) {
    visitor.visit_index(&func_param.t)
}

fn walk_type_params<V: Visitor>(visitor: &mut V, type_params: &Option<Vec<TypeParam>>) {
    if let Some(type_params) = type_params {
        type_params
            .iter()
            .for_each(|type_param| walk_type_param(visitor, type_param))
    }
}

fn walk_type_param<V: Visitor>(visitor: &mut V, type_param: &TypeParam) {
    if let Some(constraint) = type_param.constraint {
        visitor.visit_index(&constraint);
    }
    if let Some(default) = type_param.default {
        visitor.visit_index(&default);
    }
}

fn walk_function<V: Visitor>(visitor: &mut V, function: &Function) {
    let Function {
        params,
        ret,
        type_params,
        throws,
    } = function;

    walk_func_params(visitor, params);
    visitor.visit_index(ret);
    walk_type_params(visitor, type_params);
    throws.map(|throws| visitor.visit_index(&throws));
}
