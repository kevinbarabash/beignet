use generational_arena::Index;

use crate::key_value_store::KeyValueStore;
use crate::types::*;

pub trait Folder: KeyValueStore<Index, Type> + Sized {
    fn fold_index(&mut self, index: &Index) -> Index {
        walk_index(self, index)
    }
}

pub fn walk_index<F: Folder>(folder: &mut F, index: &Index) -> Index {
    let t = folder.get_type(index);

    let kind = match &t.kind {
        TypeKind::TypeVar(TypeVar {
            id,
            instance,
            constraint,
        }) => {
            let new_instance = instance.map(|instance| folder.fold_index(&instance));
            let new_constraint = constraint.map(|constraint| folder.fold_index(&constraint));

            if new_instance == *instance && new_constraint == *constraint {
                return *index;
            }

            // This doesn't seem right. We're creating a new type here, but
            // the `id` is the same.
            TypeKind::TypeVar(TypeVar {
                id: *id,
                instance: new_instance,
                constraint: new_constraint,
            })
        }
        TypeKind::TypeRef(TypeRef { name, types }) => {
            let new_types = walk_indexes(folder, types);

            if new_types == *types {
                return *index;
            }

            TypeKind::TypeRef(TypeRef {
                name: name.to_owned(),
                types: new_types,
            })
        }
        TypeKind::Union(Union { types }) => {
            let new_types = walk_indexes(folder, types);

            if new_types == *types {
                return *index;
            }

            TypeKind::Union(Union { types: new_types })
        }
        TypeKind::Intersection(Intersection { types }) => {
            let new_types = walk_indexes(folder, types);

            if new_types == *types {
                return *index;
            }

            TypeKind::Intersection(Intersection { types: new_types })
        }
        TypeKind::Tuple(Tuple { types }) => {
            let new_types = walk_indexes(folder, types);

            if new_types == *types {
                return *index;
            }

            TypeKind::Tuple(Tuple { types: new_types })
        }
        TypeKind::Array(Array { t }) => {
            let new_t = folder.fold_index(t);

            if new_t == *t {
                return *index;
            }

            TypeKind::Array(Array { t: new_t })
        }
        TypeKind::Keyword(_) => return *index,
        TypeKind::Primitive(_) => return *index,
        TypeKind::Literal(_) => return *index,
        TypeKind::Function(Function {
            params,
            ret,
            type_params,
            throws,
        }) => {
            let new_params = walk_func_params(folder, params);
            let new_ret = folder.fold_index(ret);
            let new_type_params = walk_type_params(folder, type_params);
            let new_throws = throws.map(|throws| folder.fold_index(&throws));

            if new_params == *params
                && new_ret == *ret
                && new_type_params == *type_params
                && new_throws == *throws
            {
                return *index;
            }

            TypeKind::Function(Function {
                params: new_params,
                ret: new_ret,
                type_params: new_type_params,
                throws: new_throws,
            })
        }
        TypeKind::Object(Object { elems }) => {
            let elems: Vec<_> = elems
                .iter()
                .map(|elem| match elem {
                    TObjElem::Constructor(TCallable {
                        params,
                        ret,
                        type_params,
                        throws,
                    }) => TObjElem::Constructor(TCallable {
                        params: walk_func_params(folder, params),
                        ret: folder.fold_index(ret),
                        type_params: walk_type_params(folder, type_params),
                        throws: throws.map(|throws| folder.fold_index(&throws)),
                    }),
                    TObjElem::Call(TCallable {
                        params,
                        ret,
                        type_params,
                        throws,
                    }) => TObjElem::Call(TCallable {
                        params: walk_func_params(folder, params),
                        ret: folder.fold_index(ret),
                        type_params: walk_type_params(folder, type_params),
                        throws: throws.map(|throws| folder.fold_index(&throws)),
                    }),
                    TObjElem::Mapped(MappedType {
                        key,
                        value,
                        target,
                        source,
                        optional,
                        check,
                        extends,
                    }) => {
                        let new_key = folder.fold_index(key);
                        let new_value = folder.fold_index(value);
                        let new_source = folder.fold_index(source);

                        let new_check = check.map(|check| folder.fold_index(&check));
                        let new_extends = extends.map(|extends| folder.fold_index(&extends));

                        TObjElem::Mapped(MappedType {
                            key: new_key,
                            value: new_value,
                            target: target.to_owned(),
                            source: new_source,
                            optional: optional.to_owned(),
                            check: new_check,
                            extends: new_extends,
                        })
                    }
                    // TObjElem::Index(index) => TObjElem::Index(TIndex {
                    //     t: folder.fold_index(&index.t),
                    //     ..index.clone()
                    // }),
                    TObjElem::Prop(prop) => TObjElem::Prop(TProp {
                        t: folder.fold_index(&prop.t),
                        ..prop.clone()
                    }),
                })
                .collect();

            TypeKind::Object(Object { elems })
        }
        TypeKind::Rest(Rest { arg }) => {
            let new_arg = folder.fold_index(arg);

            if new_arg == *arg {
                return *index;
            }

            TypeKind::Rest(Rest { arg: new_arg })
        }
        TypeKind::KeyOf(KeyOf { t }) => {
            let new_t = folder.fold_index(t);

            if new_t == *t {
                return *index;
            }

            TypeKind::KeyOf(KeyOf { t: new_t })
        }
        TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
            let new_obj = folder.fold_index(obj);
            let new_index = folder.fold_index(index);

            if new_obj == *obj && new_index == *index {
                return *index;
            }

            TypeKind::IndexedAccess(IndexedAccess {
                obj: new_obj,
                index: new_index,
            })
        }
        TypeKind::Conditional(Conditional {
            check,
            extends,
            true_type,
            false_type,
        }) => {
            let new_check = folder.fold_index(check);
            let new_extends = folder.fold_index(extends);
            let new_true_type = folder.fold_index(true_type);
            let new_false_type = folder.fold_index(false_type);

            if new_check == *check
                && new_extends == *extends
                && new_true_type == *true_type
                && new_false_type == *false_type
            {
                return *index;
            }

            TypeKind::Conditional(Conditional {
                check: new_check,
                extends: new_extends,
                true_type: new_true_type,
                false_type: new_false_type,
            })
        }
        TypeKind::Infer(_) => return *index,
        TypeKind::Wildcard => return *index,
        TypeKind::Binary(BinaryT { op, left, right }) => {
            let new_left = folder.fold_index(left);
            let new_right = folder.fold_index(right);

            if new_left == *left && new_right == *right {
                return *index;
            }

            TypeKind::Binary(BinaryT {
                op: *op,
                left: new_left,
                right: new_right,
            })
        }
    };

    folder.put_type(Type {
        kind,
        provenance: t.provenance,
    })
}

pub fn walk_indexes<F: Folder>(folder: &mut F, indexes: &[Index]) -> Vec<Index> {
    indexes
        .iter()
        .map(|index| folder.fold_index(index))
        .collect::<Vec<_>>()
}

fn walk_func_params<F: Folder>(folder: &mut F, params: &[FuncParam]) -> Vec<FuncParam> {
    params
        .iter()
        .map(|param| walk_func_param(folder, param))
        .collect::<Vec<_>>()
}

fn walk_func_param<F: Folder>(folder: &mut F, func_param: &FuncParam) -> FuncParam {
    FuncParam {
        t: folder.fold_index(&func_param.t),
        ..func_param.to_owned()
    }
}

fn walk_type_params<F: Folder>(
    folder: &mut F,
    type_params: &Option<Vec<TypeParam>>,
) -> Option<Vec<TypeParam>> {
    type_params.as_ref().map(|type_params| {
        type_params
            .iter()
            .map(|type_param| walk_type_param(folder, type_param))
            .collect::<Vec<_>>()
    })
}

fn walk_type_param<F: Folder>(folder: &mut F, type_param: &TypeParam) -> TypeParam {
    TypeParam {
        name: type_param.name.to_owned(),
        constraint: type_param
            .constraint
            .as_ref()
            .map(|constraint| folder.fold_index(constraint)),
        default: type_param
            .default
            .as_ref()
            .map(|default| folder.fold_index(default)),
    }
}
