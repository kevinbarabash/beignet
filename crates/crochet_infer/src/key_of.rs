use crochet_ast::types::{
    TFnParam, TGeneric, TIndex, TKeyword, TLit, TObjElem, TObject, Type, TypeKind,
};
use error_stack::{Report, Result, ResultExt};

use crate::context::Context;
use crate::type_error::TypeError;
use crate::util::union_many_types;

const NEVER_TYPE: Type = Type {
    kind: TypeKind::Keyword(TKeyword::Never),
    provenance: None,
    mutable: false,
};

// TODO: try to dedupe with infer_property_type()
pub fn key_of(t: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match &t.kind {
        TypeKind::Generic(TGeneric { t, type_params: _ }) => key_of(t, ctx),
        TypeKind::Var(_) => Err(Report::new(TypeError)
            .attach_printable("There isn't a way to infer a type from its keys")),
        TypeKind::Ref(alias) => {
            let t = ctx
                .lookup_ref_and_instantiate(alias)
                .change_context(TypeError)?;
            key_of(&t, ctx)
        }
        TypeKind::Object(TObject { elems }) => {
            let elems: Vec<_> = elems
                .iter()
                .filter_map(|elem| -> Option<Type> {
                    match elem {
                        TObjElem::Call(_) => None,
                        TObjElem::Constructor(_) => None,
                        TObjElem::Index(TIndex {
                            key: TFnParam { t, .. },
                            ..
                        }) => Some(t.to_owned()),
                        TObjElem::Prop(prop) => {
                            Some(Type::from(TypeKind::Lit(TLit::Str(prop.name.to_owned()))))
                        }
                    }
                })
                .collect();

            Ok(union_many_types(&elems))
        }
        TypeKind::Lit(lit) => {
            let t = match lit {
                TLit::Num(_) => ctx.lookup_type_and_instantiate("Number")?,
                TLit::Bool(_) => ctx.lookup_type_and_instantiate("Boolean")?,
                TLit::Str(_) => ctx.lookup_type_and_instantiate("String")?,
            };
            key_of(&t, ctx)
        }
        TypeKind::Keyword(keyword) => {
            let t = match keyword {
                TKeyword::Number => ctx.lookup_type_and_instantiate("Number")?,
                TKeyword::Boolean => ctx.lookup_type_and_instantiate("Boolean")?,
                TKeyword::String => ctx.lookup_type_and_instantiate("String")?,
                TKeyword::Symbol => ctx.lookup_type_and_instantiate("Symbol")?,
                TKeyword::Null => return Ok(NEVER_TYPE),
                TKeyword::Undefined => return Ok(NEVER_TYPE),
                TKeyword::Never => return Ok(NEVER_TYPE),
            };
            key_of(&t, ctx)
        }
        TypeKind::Tuple(tuple) => {
            let mut elems: Vec<Type> = vec![];
            for i in 0..tuple.len() {
                elems.push(Type::from(TypeKind::Lit(TLit::Num(i.to_string()))))
            }
            let array_inferface = match t.mutable {
                true => "Array",
                false => "ReadonlyArray",
            };
            elems.push(key_of(
                &ctx.lookup_type_and_instantiate(array_inferface)?,
                ctx,
            )?);
            Ok(union_many_types(&elems))
        }
        TypeKind::Array(_) => {
            let array_inferface = match t.mutable {
                true => "Array",
                false => "ReadonlyArray",
            };
            key_of(&ctx.lookup_type_and_instantiate(array_inferface)?, ctx)
        }
        TypeKind::Lam(_) => key_of(&ctx.lookup_type_and_instantiate("Function")?, ctx),
        TypeKind::App(_) => todo!(), // What does this even mean?
        TypeKind::Union(_) => todo!(),
        TypeKind::Intersection(elems) => {
            let elems: Result<Vec<_>, TypeError> =
                elems.iter().map(|elem| key_of(elem, ctx)).collect();
            Ok(union_many_types(&elems?))
        }
        TypeKind::Rest(_) => todo!(), // What does this even mean?
        TypeKind::This => todo!(),    // Depends on what this is referencing
        TypeKind::KeyOf(t) => key_of(t, ctx),
        TypeKind::IndexAccess(_) => {
            todo!() // We have to evaluate the IndexAccess first
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::infer;
    use crochet_parser::*;

    fn infer_prog(input: &str) -> Context {
        let mut prog = parse(input).unwrap();
        let mut ctx: Context = Context::default();
        infer::infer_prog(&mut prog, &mut ctx).unwrap()
    }

    fn get_key_of(name: &str, ctx: &Context) -> String {
        match ctx.lookup_type(name) {
            Ok(t) => {
                let t = key_of(&t, ctx).unwrap();
                format!("{t}")
            }
            Err(_) => panic!("Couldn't find type with name '{name}'"),
        }
    }

    #[test]
    fn test_object() {
        let src = r#"
        type t = {x: number, y: number};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""x" | "y""#);
    }

    #[test]
    fn test_intersection() {
        let src = r#"
        type t = {a: number, b: boolean} & {b: string, c: number};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""a" | "b" | "c""#);
    }

    #[test]
    fn test_number() {
        let src = r#"
        type Number = {
            toFixed: () => string,
            toString: () => string,
        };
        type t = number;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""toFixed" | "toString""#);
    }

    #[test]
    fn test_string() {
        let src = r#"
        type String = {
            length: () => number,
            toLowerCase: () => string,
            toUpperCase: () => string,
        };
        type t = string;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_key_of("t", &ctx),
            r#""length" | "toLowerCase" | "toUpperCase""#
        );
    }

    #[test]
    fn test_array() {
        let src = r#"
        type ReadonlyArray<T> = {
            [key: number]: T;
            length: number;
            map: (item: T, index: number, array: ReadonlyArray<T>) => null;
        };
        type t = number[];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""length" | "map" | number"#);
    }

    #[test]
    fn test_mutable_array() {
        let src = r#"
        type Array<T> = {
            [key: number]: T;
            length: number;
            map: (item: T, index: number, array: ReadonlyArray<T>) => null;
            sort: () => mut T[];
        };
        type t = mut number[];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_key_of("t", &ctx),
            r#""length" | "map" | "sort" | number"#
        );
    }

    #[test]
    fn test_tuple() {
        let src = r#"
        type ReadonlyArray<T> = {
            length: number,
            map: (item: T, index: number, array: ReadonlyArray<T>) => null,
        };
        type t = [1, 2, 3];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""length" | "map" | 0 | 1 | 2"#);
    }

    #[test]
    fn test_function() {
        let src = r#"
        type Function = {
            call: () => null,
            apply: () => null,
            bind: () => null,
        };
        type t = () => boolean;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""apply" | "bind" | "call""#);
    }
}
