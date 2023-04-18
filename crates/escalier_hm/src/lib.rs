// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod ast;
mod context;
mod errors;
mod infer;
mod parser;
mod types;
mod unify;
mod util;

pub use crate::infer::{infer_expression, infer_program};
pub use crate::parser::parse;

#[cfg(test)]
mod tests {
    use generational_arena::{Arena, Index};

    use crate::ast::*;
    use crate::parser::parse;

    use crate::ast::{self as syntax};
    use crate::context::*;
    use crate::errors::*;
    use crate::infer::*;
    use crate::types::*;

    fn new_bool_lit_type(arena: &mut Arena<Type>, value: bool) -> Index {
        arena.insert(Type {
            kind: TypeKind::Literal(Lit::Bool(Bool {
                value,
                loc: DUMMY_LOC,
                span: Span::default(),
            })),
        })
    }

    fn new_num_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
        arena.insert(Type {
            kind: TypeKind::Literal(Lit::Num(Num {
                value: value.to_owned(),
                loc: DUMMY_LOC,
                span: Span::default(),
            })),
        })
    }

    fn new_str_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
        arena.insert(Type {
            kind: TypeKind::Literal(Lit::Str(Str {
                value: value.to_owned(),
                loc: DUMMY_LOC,
                span: Span::default(),
            })),
        })
    }

    fn test_env() -> (Arena<Type>, Context) {
        (Arena::new(), Context::default())
    }

    /// Sets up some predefined types using the type constructors TypeVariable,
    /// TypeOperator and Function.  Creates a list of example expressions to be
    /// evaluated. Evaluates the expressions, printing the type or errors arising
    /// from each.

    #[test]
    fn test_factorial() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        // factorial
        let src = r#"
        let rec fact = (n) => {
            return if (n == 0) {
                1
            } else {
                n * fact(n - 1)
            };
        };
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("fact").unwrap();

        // TODO: simplify union types
        assert_eq!(arena[*t].as_string(&arena), r#"(number) => 1 | number"#);
        Ok(())
    }

    // #[test]
    // fn test_mutual_recursion() -> Result<(), Errors> {
    //     let (mut arena, mut my_ctx) = test_env();

    //     // NOTE: The definitions of "even" and "odd" are correct from a types
    //     // perspective, but incorrect semantically.
    //     let mut syntax = new_letrec(
    //         &[
    //             (
    //                 "even".to_string(),
    //                 new_lambda(
    //                     &["x"], // (x) => times(1, odd(x - 1))
    //                     &new_apply(
    //                         // this casts odd(x - 1) to a number
    //                         new_identifier("times"),
    //                         &[
    //                             new_number("1"),
    //                             new_apply(
    //                                 new_identifier("odd"),
    //                                 &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
    //                             ),
    //                         ],
    //                     ),
    //                 ),
    //             ),
    //             (
    //                 "odd".to_string(),
    //                 new_lambda(
    //                     &["x"], // (x) => times(1, even(x - 1))
    //                     &new_apply(
    //                         // this casts even(x - 1) to a number
    //                         new_identifier("times"),
    //                         &[
    //                             new_number("1"),
    //                             new_apply(
    //                                 new_identifier("even"),
    //                                 &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
    //                             ),
    //                         ],
    //                     ),
    //                 ),
    //             ),
    //         ],
    //         new_identifier("odd"),
    //     );

    //     let t = infer_expression(&mut arena, &mut syntax, &mut my_ctx)?;
    //     assert_eq!(arena[t].as_string(&arena), r#"(number) => number"#);

    //     Ok(())
    // }

    #[should_panic]
    #[test]
    fn test_mismatch() {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"(x) => [x(3), x(true)];"#;

        let mut program = parse(src).unwrap();
        infer_program(&mut arena, &mut program, &mut my_ctx).unwrap();
    }

    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"Undefined symbol \\\"f\\\"\")"]
    #[test]
    fn test_pair() {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"[f(3), f(true)];"#;

        let mut program = parse(src).unwrap();
        infer_program(&mut arena, &mut program, &mut my_ctx).unwrap();
    }

    #[test]
    fn test_mul() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
            let f = (x) => x;
            let result = [f(4), f(true)];
        "#;

        let mut program = parse(src).unwrap();
        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"[4, true]"#);
        Ok(())
    }

    #[should_panic = "recursive unification"]
    #[test]
    fn test_recursive() {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"(f) => f(f);"#;

        let mut program = parse(src).unwrap();
        infer_program(&mut arena, &mut program, &mut my_ctx).unwrap();
    }

    #[test]
    fn test_number_literal() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let g = (f) => 5;
        let result = g(g);
        "#;

        let mut program = parse(src).unwrap();
        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"5"#);
        Ok(())
    }

    #[test]
    fn test_generic_nongeneric() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let result = (g) => {
            let f = (x) => g;
            return [f(3), f(true)];
        };"#;

        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"<A>(A) => [A, A]"#);
        Ok(())
    }

    #[test]
    fn test_basic_generics() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        // example that demonstrates generic and non-generic variables:
        let src = r#"let result = (x) => x;"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"<A>(A) => A"#);

        Ok(())
    }

    #[test]
    fn test_composition() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        // Function composition
        // fn f (fn g (fn arg (f g arg)))
        let src = r#"let result = (f) => (g) => (arg) => g(f(arg));"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(
            arena[*t].as_string(&arena),
            r#"<A, B, C>((A) => B) => ((B) => C) => (A) => C"#
        );
        Ok(())
    }

    #[test]
    fn test_composition_with_statements() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        // Function composition
        let src = r#"
        let result = (f) => {
            let mantel = (g) => {
                let core = (arg) => g(f(arg));
                return core;
            };
            return mantel;
        };
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(
            arena[*t].as_string(&arena),
            r#"<A, B, C>((A) => B) => ((B) => C) => (A) => C"#
        );
        Ok(())
    }

    #[test]
    fn test_subtype() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let times = (x, y) => x * y;
        let result = times(5, 10);
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"number"#);
        Ok(())
    }

    #[test]
    fn test_callback_subtyping() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let num = new_constructor(&mut arena, "number", &[]);
        let bool = new_constructor(&mut arena, "boolean", &[]);
        let str = new_constructor(&mut arena, "string", &[]);

        // foo: ((number, string) => boolean) => boolean
        let cb = new_func_type(&mut arena, &[num, str], bool, None);
        my_ctx.env.insert(
            "foo".to_string(),
            new_func_type(&mut arena, &[cb], bool, None),
        );

        // bar: (number | string) => true
        // It's okay for the callback arg to take fewer params since extra params
        // are ignored.  It's also okay for its params to be supertypes of the
        // expected params since the callback will only be called with the expected
        // types.  Lastly, it's okay for the return type to be a subtype of the
        // expected return type since it still conforms to the expected type.
        let num_or_str = new_union_type(&mut arena, &[num, str]);
        let true_type = new_bool_lit_type(&mut arena, true);
        my_ctx.env.insert(
            "bar".to_string(),
            new_func_type(&mut arena, &[num_or_str], true_type, None),
        );

        let src = r#"let result = foo(bar);"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);
        Ok(())
    }

    #[test]
    fn test_callback_error_too_many_params() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let num = new_constructor(&mut arena, "number", &[]);
        let bool = new_constructor(&mut arena, "boolean", &[]);
        let str = new_constructor(&mut arena, "string", &[]);

        // foo: ((number) => boolean) => boolean
        let cb = new_func_type(&mut arena, &[num], bool, None);
        my_ctx.env.insert(
            "foo".to_string(),
            new_func_type(&mut arena, &[cb], bool, None),
        );

        // bar: (number, string) => true
        my_ctx.env.insert(
            "bar".to_string(),
            new_func_type(&mut arena, &[num, str], bool, None),
        );

        let src = r#"let result = foo(bar);"#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);
        assert_eq!(
            result,
            Err(Errors::InferenceError("(number, string) => boolean is not a subtype of (number) => boolean since it requires more params".to_string())),
        );
        Ok(())
    }

    #[test]
    fn test_union_subtype() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let lit1 = new_num_lit_type(&mut arena, "5");
        let lit2 = new_num_lit_type(&mut arena, "10");
        my_ctx
            .env
            .insert("foo".to_string(), new_union_type(&mut arena, &[lit1, lit2]));

        let src = r#"
        let times = (x, y) => x * y;
        let result = times(foo, 2);
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"number"#);
        Ok(())
    }

    #[test]
    fn test_calling_a_union() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let bool = new_constructor(&mut arena, "boolean", &[]);
        let str = new_constructor(&mut arena, "string", &[]);
        let fn1 = new_func_type(&mut arena, &[], bool, None);
        let fn2 = new_func_type(&mut arena, &[], str, None);
        my_ctx
            .env
            .insert("foo".to_string(), new_union_type(&mut arena, &[fn1, fn2]));

        let src = r#"let result = foo();"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"boolean | string"#);
        Ok(())
    }

    #[test]
    fn call_with_too_few_args() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let times = (x, y) => x * y;
        let result = times();
        "#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "too few arguments to function: expected 2, got 0".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn literal_isnt_callable() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let lit = new_num_lit_type(&mut arena, "5");
        my_ctx.env.insert("foo".to_string(), lit);

        let src = r#"let result = foo();"#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "literal 5 is not callable".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn infer_basic_tuple() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"let result = [5, "hello"];"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), "[5, \"hello\"]".to_string(),);

        Ok(())
    }

    #[test]
    fn tuple_member() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let tuple = [5, "hello"];
        let result = tuple[1];
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), "\"hello\"".to_string(),);

        Ok(())
    }

    #[test]
    fn tuple_member_error_out_of_bounds() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let tuple = [5, "hello"];
        let result = tuple[2];
        "#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "2 was outside the bounds 0..2 of the tuple".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn tuple_subtyping() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let num = new_constructor(&mut arena, "number", &[]);
        let str = new_constructor(&mut arena, "string", &[]);
        let param_type = new_tuple_type(&mut arena, &[num, str]);
        let bool = new_constructor(&mut arena, "boolean", &[]);
        let func = new_func_type(&mut arena, &[param_type], bool, None);
        my_ctx.env.insert("foo".to_string(), func);

        let src = r#"let result = foo([5, "hello", true]);"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), "boolean".to_string(),);

        Ok(())
    }

    #[test]
    fn tuple_subtyping_not_enough_elements() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let num = new_constructor(&mut arena, "number", &[]);
        let str = new_constructor(&mut arena, "string", &[]);
        let param_type = new_tuple_type(&mut arena, &[num, str]);
        let bool = new_constructor(&mut arena, "boolean", &[]);
        let func = new_func_type(&mut arena, &[param_type], bool, None);
        my_ctx.env.insert("foo".to_string(), func);

        let src = r#"let result = foo([5]);"#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "Expected tuple of length 2, got tuple of length 1".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn infer_basic_object() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"let result = {a: 5, b: "hello"};"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();

        assert_eq!(
            arena[*t].as_string(&arena),
            "{a: 5, b: \"hello\"}".to_string(),
        );

        Ok(())
    }

    #[test]
    fn object_member() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let obj = {a: 5, b: "hello"};
        let result = obj.a;
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();

        assert_eq!(arena[*t].as_string(&arena), "5".to_string(),);

        Ok(())
    }

    #[test]
    fn object_member_missing_prop() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let obj = {a: 5, b: "hello"};
        let result = obj.c;
        "#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "Couldn't find property 'c' on object".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn object_subtyping() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let num = new_constructor(&mut arena, "number", &[]);
        let str = new_constructor(&mut arena, "string", &[]);
        let param_type = new_object_type(
            &mut arena,
            &[("a".to_string(), num), ("b".to_string(), str)],
        );
        let bool = new_constructor(&mut arena, "boolean", &[]);
        let func = new_func_type(&mut arena, &[param_type], bool, None);
        my_ctx.env.insert("foo".to_string(), func);

        // Each prop must be a subtype of the expected element type
        // It's okay to pass an object with extra props
        let src = r#"let result = foo({a: 5, b: "hello", c: true});"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("result").unwrap();

        assert_eq!(arena[*t].as_string(&arena), "boolean".to_string(),);

        Ok(())
    }

    #[test]
    fn object_subtyping_missing_prop() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let num = new_constructor(&mut arena, "number", &[]);
        let str = new_constructor(&mut arena, "string", &[]);
        let param_type = new_object_type(
            &mut arena,
            &[("a".to_string(), num), ("b".to_string(), str)],
        );
        let bool = new_constructor(&mut arena, "boolean", &[]);
        let func = new_func_type(&mut arena, &[param_type], bool, None);
        my_ctx.env.insert("foo".to_string(), func);

        let src = r#"let result = foo({b: "hello"});"#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "'a' is missing in {b: \"hello\"}".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn test_subtype_error() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let times = (x, y) => x * y;
        let result = times(5, "hello");
        "#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "type mismatch: unify(\"hello\", number) failed".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn test_union_subtype_error() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let lit1 = new_num_lit_type(&mut arena, "5");
        let lit2 = new_str_lit_type(&mut arena, "hello");
        my_ctx
            .env
            .insert("foo".to_string(), new_union_type(&mut arena, &[lit1, lit2]));

        let src = r#"
        let times = (x, y) => x * y;
        let result = times(foo, "world");
        "#;
        let mut program = parse(src).unwrap();

        let result = infer_program(&mut arena, &mut program, &mut my_ctx);

        assert_eq!(
            result,
            Err(Errors::InferenceError(
                "type mismatch: unify(\"hello\", number) failed".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn test_program() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let num = 5;
        let str = "hello";
        num * num;
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("num").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"5"#);

        let t = my_ctx.env.get("str").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#""hello""#);

        // TODO: implement std::fmt for Program et al
        // eprintln!("program = {program}");

        // insta::assert_snapshot!(program.to_string(), @r###"
        // let num = 5
        // let str = "hello"
        // times(num, num)
        // "###);

        Ok(())
    }

    #[test]
    fn test_program_with_generic_func() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let id = (x) => x;
        let a = id(5);
        let b = id("hello");
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("id").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"<A>(A) => A"#);

        let t = my_ctx.env.get("a").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"5"#);

        let t = my_ctx.env.get("b").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#""hello""#);

        Ok(())
    }

    #[test]
    fn test_program_with_generic_func_multiple_type_params() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let fst = (x, y) => x;
        let snd = (x, y) => y;
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("fst").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"<A, B>(A, B) => A"#);

        let t = my_ctx.env.get("snd").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"<A, B>(A, B) => B"#);

        Ok(())
    }

    #[test]
    fn test_function_with_multiple_statements() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"
        let result = () => {
            let x = 5;
            let y = 10;
            return x * y;
        };
        "#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(arena[*t].as_string(&arena), r#"() => number"#);

        if let StmtKind::VarDecl(VarDecl {
            init: Some(init), ..
        }) = &program.statements[0].kind
        {
            if let ExprKind::Lambda(syntax::Lambda {
                body: BlockOrExpr::Block(Block { stmts: _, .. }),
                ..
            }) = &init.as_ref().kind
            {
                // TODO: check that the first two statements are var decls and
                // then grab the first pattern and check its inferred type.
                // let x_t = stmts[0].inferred_type.unwrap();
                // let y_t = stmts[1].inferred_type.unwrap();

                // assert_eq!(a[x_t].as_string(&arena), "5");
                // assert_eq!(a[y_t].as_string(&arena), "10");
            } else {
                panic!("expected a lambda");
            }
        } else {
            panic!("expected a variable declaration");
        }

        // TODO: implement std::fmt for Program et al
        // insta::assert_snapshot!(syntax.to_string(), @r###"
        // fn () => {let x = 5
        // let y = 10
        // return times(x, y)}
        // "###);

        Ok(())
    }

    #[test]
    fn test_inferred_type_on_ast_nodes() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"let result = (x, y) => x * y;"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;

        if let StmtKind::VarDecl(VarDecl {
            init: Some(init), ..
        }) = &program.statements[0].kind
        {
            if let ExprKind::Lambda(Lambda { params, .. }) = &init.kind {
                let x_t = params[0].pat.inferred_type.unwrap();
                let y_t = params[1].pat.inferred_type.unwrap();

                assert_eq!(arena[x_t].as_string(&arena), "number");
                assert_eq!(arena[y_t].as_string(&arena), "number");
            } else {
                panic!("expected a lambda");
            }
        } else {
            panic!("expected a variable declaration");
        }

        Ok(())
    }

    #[test]
    fn test_unary_op() -> Result<(), Errors> {
        let (mut arena, mut my_ctx) = test_env();

        let src = r#"let neg = (x) => -x;"#;
        let mut program = parse(src).unwrap();

        infer_program(&mut arena, &mut program, &mut my_ctx)?;
        let t = my_ctx.env.get("neg").unwrap();

        assert_eq!(arena[*t].as_string(&arena), r#"(number) => number"#);
        Ok(())
    }
}
