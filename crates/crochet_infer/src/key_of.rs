use crochet_ast::types::{TGeneric, TKeyword, TLit, TObjElem, TObject, Type, TypeKind};

use crate::context::Context;
use crate::util::union_many_types;

// TODO: try to dedupe with infer_property_type()
pub fn key_of(t: &Type, ctx: &Context) -> Result<Type, String> {
    match &t.kind {
        TypeKind::Generic(TGeneric { t, type_params: _ }) => key_of(t, ctx),
        TypeKind::Var(_) => Err(String::from(
            "There isn't a way to infer a type from its keys",
        )),
        TypeKind::Ref(alias) => {
            let t = ctx.lookup_ref_and_instantiate(alias)?;
            key_of(&t, ctx)
        }
        TypeKind::Object(TObject { elems }) => {
            let elems: Vec<_> = elems
                .iter()
                .filter_map(|elem| match elem {
                    TObjElem::Call(_) => None,
                    TObjElem::Constructor(_) => None,
                    TObjElem::Index(_) => todo!(),
                    TObjElem::Prop(prop) => Some(Type {
                        kind: TypeKind::Lit(TLit::Str(prop.name.to_owned())),
                    }),
                })
                .collect();

            Ok(union_many_types(&elems))
        }
        TypeKind::Lit(lit) => match lit {
            TLit::Num(_) => key_of(&ctx.lookup_type_and_instantiate("Number")?, ctx),
            TLit::Bool(_) => key_of(&ctx.lookup_type_and_instantiate("Boolean")?, ctx),
            TLit::Str(_) => key_of(&ctx.lookup_type_and_instantiate("String")?, ctx),
        },
        TypeKind::Tuple(tuple) => {
            let mut elems: Vec<Type> = vec![];
            for i in 0..tuple.len() {
                elems.push(Type {
                    kind: TypeKind::Lit(TLit::Num(i.to_string())),
                })
            }
            elems.push(key_of(
                &ctx.lookup_type_and_instantiate("ReadonlyArray")?,
                ctx,
            )?);
            Ok(union_many_types(&elems))
        }
        TypeKind::Array(_) => Ok(union_many_types(&[
            Type {
                kind: TypeKind::Keyword(TKeyword::Number),
            },
            key_of(&ctx.lookup_type_and_instantiate("ReadonlyArray")?, ctx)?,
        ])),
        TypeKind::Lam(_) => key_of(&ctx.lookup_type_and_instantiate("Function")?, ctx),
        TypeKind::App(_) => {
            todo!() // What does this even mean?
        }
        TypeKind::Keyword(keyword) => match keyword {
            TKeyword::Number => key_of(&ctx.lookup_type_and_instantiate("Number")?, ctx),
            TKeyword::Boolean => key_of(&ctx.lookup_type_and_instantiate("Boolean")?, ctx),
            TKeyword::String => key_of(&ctx.lookup_type_and_instantiate("String")?, ctx),
            TKeyword::Symbol => key_of(&ctx.lookup_type_and_instantiate("Symbol")?, ctx),
            TKeyword::Null => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Never),
            }),
            TKeyword::Undefined => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Never),
            }),
            TKeyword::Never => Ok(Type {
                kind: TypeKind::Keyword(TKeyword::Never),
            }),
        },
        TypeKind::Union(_) => todo!(),
        TypeKind::Intersection(elems) => {
            let elems: Result<Vec<_>, String> =
                elems.iter().map(|elem| key_of(elem, ctx)).collect();
            Ok(union_many_types(&elems?))
        }
        TypeKind::Rest(_) => {
            todo!() // What does this even mean?
        }
        TypeKind::This => {
            todo!() // Depends on what this is referencing
        }
        TypeKind::KeyOf(t) => key_of(&key_of(t, ctx)?, ctx),
        TypeKind::Mutable(t) => key_of(t, ctx),
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
            length: number,
            map: (item: T, index: number, array: ReadonlyArray<T>) => null,
        };
        type t = number[];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_key_of("t", &ctx), r#""length" | "map" | number"#);
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
