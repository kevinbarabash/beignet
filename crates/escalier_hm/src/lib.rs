// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod context;
mod errors;
mod infer;
mod literal;
mod syntax;
mod types;
mod unify;
mod util;

pub use crate::infer::{infer_expression, infer_program};

#[cfg(test)]
mod tests {
    use generational_arena::{Arena, Index};

    use crate::context::*;
    use crate::errors::*;
    use crate::infer::*;
    use crate::literal::*;
    use crate::syntax::{self, *};
    use crate::types::*;

    pub fn new_num_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
        new_lit_type(arena, &Literal::Number(value.to_string()))
    }

    pub fn new_str_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
        new_lit_type(arena, &Literal::String(value.to_string()))
    }

    pub fn new_bool_lit_type(arena: &mut Arena<Type>, value: bool) -> Index {
        new_lit_type(arena, &Literal::Boolean(value))
    }

    pub fn new_lambda(params: &[&str], body: &Expression) -> Expression {
        let kind = ExprKind::Lambda(Lambda {
            params: params
                .iter()
                .map(|name| FuncParam {
                    pattern: new_binding(name),
                })
                .collect(),
            body: BlockOrExpr::Expr(Box::from(body.to_owned())),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_function(params: &[&str], body: &[Statement]) -> Expression {
        let kind = ExprKind::Lambda(Lambda {
            params: params
                .iter()
                .map(|name| FuncParam {
                    pattern: new_binding(name),
                })
                .collect(),
            body: BlockOrExpr::Block(Block {
                stmts: body.to_owned(),
            }),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_apply(func: Expression, args: &[Expression]) -> Expression {
        let kind = ExprKind::Apply(Apply {
            func: Box::new(func),
            args: args.to_owned(),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_letrec(decls: &[(String, Expression)], body: Expression) -> Expression {
        let kind = ExprKind::Letrec(Letrec {
            decls: decls
                .iter()
                .map(|(var, defn)| (var.to_owned(), Box::new(defn.to_owned())))
                .collect(),
            body: Box::new(body),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_identifier(name: &str) -> Expression {
        // TODO: Check that `name` is a valid identifier
        let kind = ExprKind::Identifier(Identifier {
            name: name.to_string(),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_number(value: &str) -> Expression {
        let kind = ExprKind::Literal(Literal::Number(value.to_owned()));
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_string(value: &str) -> Expression {
        let kind = ExprKind::Literal(Literal::String(value.to_owned()));
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_boolean(value: bool) -> Expression {
        let kind = ExprKind::Literal(Literal::Boolean(value));
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_tuple(elems: &[Expression]) -> Expression {
        let kind = ExprKind::Tuple(syntax::Tuple {
            elems: elems.to_owned(),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_object(props: &[(String, Expression)]) -> Expression {
        let kind = ExprKind::Object(syntax::Object {
            props: props.to_owned(),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_member(obj: &Expression, prop: &Expression) -> Expression {
        let kind = ExprKind::Member(Member {
            obj: Box::from(obj.to_owned()),
            prop: Box::new(prop.to_owned()),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_if_else(
        cond: Expression,
        consequent: Expression,
        alternate: Expression,
    ) -> Expression {
        let kind = ExprKind::IfElse(IfElse {
            cond: Box::new(cond),
            consequent: Box::new(consequent),
            alternate: Box::new(alternate),
        });
        Expression {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_binding(name: &str) -> Pattern {
        // TODO: Check that `name` is a valid identifier
        let kind = PatternKind::Ident(BindingIdent {
            name: name.to_string(),
            mutable: false,
        });
        Pattern {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_declaration(name: &str, value: &Expression) -> Statement {
        let kind = StmtKind::Declaration(Declaration {
            pattern: new_binding(name),
            defn: Box::new(value.to_owned()),
        });
        Statement {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_return(value: &Expression) -> Statement {
        let kind = StmtKind::Return(Return {
            expr: Box::new(value.to_owned()),
        });
        Statement {
            kind,
            inferred_type: None,
        }
    }

    pub fn new_stmt_expr(expr: &Expression) -> Statement {
        let kind = StmtKind::Expression(expr.to_owned());
        Statement {
            kind,
            inferred_type: None,
        }
    }

    fn test_env() -> (Arena<Type>, Context) {
        let mut a: Arena<Type> = Arena::new();
        let mut my_ctx = Context::default();

        let var1 = new_var_type(&mut a);
        let var2 = new_var_type(&mut a);
        let pair_type = new_constructor(&mut a, "*", &[var1, var2]);
        my_ctx.env.insert(
            "pair".to_string(),
            new_func_type(&mut a, &[var1, var2], pair_type),
        );

        let int_t = new_constructor(&mut a, "number", &[]);
        let bool_t = new_constructor(&mut a, "boolean", &[]);
        my_ctx
            .env
            .insert("zero".to_string(), new_func_type(&mut a, &[int_t], bool_t));

        let num1_t = new_constructor(&mut a, "number", &[]);
        let num2_t = new_constructor(&mut a, "number", &[]);
        my_ctx
            .env
            .insert("pred".to_string(), new_func_type(&mut a, &[num1_t], num2_t));

        // It isn't necessary to create separate instances of `number` for each of
        // these.  When interferring types from code we'll want separate instances
        // so that we can link each back to the expression from which it was
        // inferred.
        let num1_t = new_constructor(&mut a, "number", &[]);
        let num2_t = new_constructor(&mut a, "number", &[]);
        let num3_t = new_constructor(&mut a, "number", &[]);
        my_ctx.env.insert(
            "times".to_string(),
            new_func_type(&mut a, &[num1_t, num2_t], num3_t),
        );
        my_ctx.env.insert(
            "add".to_string(),
            new_func_type(&mut a, &[num1_t, num2_t], num3_t),
        );

        my_ctx
            .env
            .insert("true".to_string(), new_bool_lit_type(&mut a, true));
        my_ctx
            .env
            .insert("false".to_string(), new_bool_lit_type(&mut a, false));

        (a, my_ctx)
    }

    /// Sets up some predefined types using the type constructors TypeVariable,
    /// TypeOperator and Function.  Creates a list of example expressions to be
    /// evaluated. Evaluates the expressions, printing the type or errors arising
    /// from each.

    #[test]
    fn test_factorial() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        // factorial
        let mut syntax = new_letrec(
            &[(
                "factorial".to_string(),
                new_lambda(
                    &["n"], // (n) => times(n, factorial(pred(n))
                    &new_if_else(
                        new_apply(new_identifier("zero"), &[new_identifier("n")]),
                        new_number("1"),
                        new_apply(
                            // times(n, factorial(pred(n))
                            new_identifier("times"),
                            &[
                                new_identifier("n"),
                                new_apply(
                                    new_identifier("factorial"),
                                    &[new_apply(new_identifier("pred"), &[new_identifier("n")])],
                                ),
                            ],
                        ),
                    ),
                ),
            )],
            new_identifier("factorial"),
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"(number) => number"#);
        Ok(())
    }

    #[test]
    fn test_mutual_recursion() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        // NOTE: The definitions of "even" and "odd" are correct from a types
        // perspective, but incorrect semantically.
        let mut syntax = new_letrec(
            &[
                (
                    "even".to_string(),
                    new_lambda(
                        &["x"], // (x) => times(1, odd(x - 1))
                        &new_apply(
                            // this casts odd(x - 1) to a number
                            new_identifier("times"),
                            &[
                                new_number("1"),
                                new_apply(
                                    new_identifier("odd"),
                                    &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
                                ),
                            ],
                        ),
                    ),
                ),
                (
                    "odd".to_string(),
                    new_lambda(
                        &["x"], // (x) => times(1, even(x - 1))
                        &new_apply(
                            // this casts even(x - 1) to a number
                            new_identifier("times"),
                            &[
                                new_number("1"),
                                new_apply(
                                    new_identifier("even"),
                                    &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
                                ),
                            ],
                        ),
                    ),
                ),
            ],
            new_identifier("odd"),
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"(number) => number"#);

        Ok(())
    }

    #[should_panic]
    #[test]
    fn test_mismatch() {
        let (mut a, mut my_ctx) = test_env();

        // fn x => (pair(x(3) (x(true)))
        let mut syntax = new_lambda(
            &["x"],
            &new_apply(
                new_identifier("pair"),
                &[
                    new_apply(new_identifier("x"), &[new_number("3")]),
                    new_apply(new_identifier("x"), &[new_boolean(true)]),
                ],
            ),
        );

        infer_expression(&mut a, &mut syntax, &mut my_ctx).unwrap();
    }

    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"Undefined symbol \\\"f\\\"\")"]
    #[test]
    fn test_pair() {
        let (mut a, mut my_ctx) = test_env();

        // pair(f(3), f(true))
        let mut syntax = new_apply(
            new_identifier("pair"),
            &[
                new_apply(new_identifier("f"), &[new_number("4")]),
                new_apply(new_identifier("f"), &[new_boolean(true)]),
            ],
        );

        infer_expression(&mut a, &mut syntax, &mut my_ctx).unwrap();
    }

    #[test]
    fn test_mul() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let pair = new_apply(
            new_identifier("pair"),
            &[
                new_apply(new_identifier("f"), &[new_number("4")]),
                new_apply(new_identifier("f"), &[new_boolean(true)]),
            ],
        );

        let mut program = Program {
            statements: vec![
                // let f = (x) => x
                new_declaration("f", &new_lambda(&["x"], &new_identifier("x"))),
                // let result = ((pair (f 4)) (f true))
                new_declaration("result", &pair),
            ],
        };

        infer_program(&mut a, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(a[*t].as_string(&a), r#"(4 * true)"#);
        Ok(())
    }

    #[should_panic = "recursive unification"]
    #[test]
    fn test_recursive() {
        let (mut a, mut my_ctx) = test_env();

        // fn f => f f (fail)
        let mut syntax = new_lambda(
            &["f"],
            &new_apply(new_identifier("f"), &[new_identifier("f")]),
        );

        infer_expression(&mut a, &mut syntax, &mut my_ctx).unwrap();
    }

    #[test]
    fn test_number_literal() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut program = Program {
            statements: vec![
                // let g = (f) => 5
                new_declaration("g", &new_lambda(&["f"], &new_number("5"))),
                // let result = g(g)
                new_declaration(
                    "result",
                    &new_apply(new_identifier("g"), &[new_identifier("g")]),
                ),
            ],
        };

        infer_program(&mut a, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("result").unwrap();
        assert_eq!(a[*t].as_string(&a), r#"5"#);
        Ok(())
    }

    #[test]
    fn test_generic_nongeneric() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_function(
            &["g"],
            &[
                // let f = (x) => g
                new_declaration("f", &new_lambda(&["x"], &new_identifier("g"))),
                // pair (f 3, f true)
                new_return(&new_apply(
                    new_identifier("pair"),
                    &[
                        new_apply(new_identifier("f"), &[new_number("3")]),
                        new_apply(new_identifier("f"), &[new_boolean(true)]),
                    ],
                )),
            ],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"(t21) => (t21 * t21)"#);
        Ok(())
    }

    #[test]
    fn test_basic_generics() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        // example that demonstrates generic and non-generic variables:
        // fn x => x
        let mut syntax = new_lambda(&["x"], &new_identifier("x"));

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"(t17) => t17"#);
        let t = &a[t];
        eprintln!("t = {t:#?}");
        Ok(())
    }

    #[test]
    fn test_composition() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        // Function composition
        // fn f (fn g (fn arg (f g arg)))
        let mut syntax = new_lambda(
            &["f"],
            &new_lambda(
                &["g"],
                &new_lambda(
                    &["arg"],
                    &new_apply(
                        new_identifier("g"),
                        &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
                    ),
                ),
            ),
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(
            a[t].as_string(&a),
            r#"((t19) => t20) => ((t20) => t22) => (t19) => t22"#
        );
        Ok(())
    }

    #[test]
    fn test_composition_with_statements() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        // Function composition
        // let compose = (f) => {
        //     let mantel = (g) => {
        //         let core = (arg) => f(g(arg));
        //         return core;
        //     };
        //     return mantel;
        // }
        let mut syntax = new_function(
            &["f"],
            &[
                new_declaration(
                    "mantel",
                    &new_function(
                        &["g"],
                        &[
                            new_declaration(
                                "core",
                                &new_lambda(
                                    &["arg"],
                                    &new_apply(
                                        new_identifier("g"),
                                        &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
                                    ),
                                ),
                            ),
                            new_return(&new_identifier("core")),
                        ],
                    ),
                ),
                new_return(&new_identifier("mantel")),
            ],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(
            a[t].as_string(&a),
            r#"((t19) => t20) => ((t20) => t26) => (t19) => t26"#
        );
        Ok(())
    }

    #[test]
    fn test_fun() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        // Function composition
        // (fn (f, g, arg) -> (f g arg))
        let mut syntax = new_lambda(
            &["f", "g", "arg"],
            &new_apply(
                new_identifier("g"),
                &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
            ),
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(
            a[t].as_string(&a),
            r#"((t19) => t20, (t20) => t22, t19) => t22"#
        );
        Ok(())
    }

    #[test]
    fn test_subtype() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_number("10")],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"number"#);
        Ok(())
    }

    #[test]
    fn test_callback_subtyping() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let num = new_constructor(&mut a, "number", &[]);
        let bool = new_constructor(&mut a, "boolean", &[]);
        let str = new_constructor(&mut a, "string", &[]);

        // foo: ((number, string) => boolean) => boolean
        let cb = new_func_type(&mut a, &[num, str], bool);
        my_ctx
            .env
            .insert("foo".to_string(), new_func_type(&mut a, &[cb], bool));

        // bar: (number | string) => true
        // It's okay for the callback arg to take fewer params since extra params
        // are ignored.  It's also okay for its params to be supertypes of the
        // expected params since the callback will only be called with the expected
        // types.  Lastly, it's okay for the return type to be a subtype of the
        // expected return type since it still conforms to the expected type.
        let num_or_str = new_union_type(&mut a, &[num, str]);
        let true_type = new_bool_lit_type(&mut a, true);
        my_ctx.env.insert(
            "bar".to_string(),
            new_func_type(&mut a, &[num_or_str], true_type),
        );

        // foo(bar)
        let mut syntax = new_apply(new_identifier("foo"), &[new_identifier("bar")]);

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"boolean"#);
        Ok(())
    }

    #[test]
    fn test_callback_error_too_many_params() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let num = new_constructor(&mut a, "number", &[]);
        let bool = new_constructor(&mut a, "boolean", &[]);
        let str = new_constructor(&mut a, "string", &[]);

        // foo: ((number) => boolean) => boolean
        let cb = new_func_type(&mut a, &[num], bool);
        my_ctx
            .env
            .insert("foo".to_string(), new_func_type(&mut a, &[cb], bool));

        // bar: (number, string) => true
        my_ctx
            .env
            .insert("bar".to_string(), new_func_type(&mut a, &[num, str], bool));

        // foo(bar)
        let mut syntax = new_apply(new_identifier("foo"), &[new_identifier("bar")]);

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);
        assert_eq!(
            result,
            Err(Errors::InferenceError("(number, string) => boolean is not a subtype of (number) => boolean since it requires more params".to_string())),
        );
        Ok(())
    }

    #[test]
    fn test_union_subtype() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let lit1 = new_num_lit_type(&mut a, "5");
        let lit2 = new_num_lit_type(&mut a, "10");
        my_ctx
            .env
            .insert("foo".to_string(), new_union_type(&mut a, &[lit1, lit2]));

        let mut syntax = new_apply(
            new_identifier("times"),
            &[new_identifier("foo"), new_number("2")],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"number"#);
        Ok(())
    }

    #[test]
    fn test_calling_a_union() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let bool = new_constructor(&mut a, "boolean", &[]);
        let str = new_constructor(&mut a, "string", &[]);
        let fn1 = new_func_type(&mut a, &[], bool);
        let fn2 = new_func_type(&mut a, &[], str);
        my_ctx
            .env
            .insert("foo".to_string(), new_union_type(&mut a, &[fn1, fn2]));

        let mut syntax = new_apply(new_identifier("foo"), &[]);

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;
        assert_eq!(a[t].as_string(&a), r#"boolean | string"#);
        Ok(())
    }

    #[test]
    fn call_with_too_few_args() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_apply(new_identifier("times"), &[]);

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let lit = new_num_lit_type(&mut a, "5");
        my_ctx.env.insert("foo".to_string(), lit);

        let mut syntax = new_apply(new_identifier("foo"), &[]);

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_tuple(&[new_number("5"), new_string("hello")]);

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), "[5, \"hello\"]".to_string(),);

        Ok(())
    }

    #[test]
    fn tuple_member() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let tuple = new_tuple(&[new_number("5"), new_string("hello")]);
        let mut syntax = new_member(&tuple, &new_number("1"));

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), "\"hello\"".to_string(),);

        Ok(())
    }

    #[test]
    fn tuple_member_error_out_of_bounds() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let tuple = new_tuple(&[new_number("5"), new_string("hello")]);
        let mut syntax = new_member(&tuple, &new_number("2"));

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let num = new_constructor(&mut a, "number", &[]);
        let str = new_constructor(&mut a, "string", &[]);
        let param_type = new_tuple_type(&mut a, &[num, str]);
        let bool = new_constructor(&mut a, "boolean", &[]);
        let func = new_func_type(&mut a, &[param_type], bool);
        my_ctx.env.insert("foo".to_string(), func);

        let mut syntax = new_apply(
            new_identifier("foo"),
            &[new_tuple(&[
                // Each element must be a subtype of the expected element type
                new_number("5"),
                new_string("hello"),
                // It's okay to pass a tuple with extra elements
                new_boolean(true),
            ])],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), "boolean".to_string(),);

        Ok(())
    }

    #[test]
    fn tuple_subtyping_not_enough_elements() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let num = new_constructor(&mut a, "number", &[]);
        let str = new_constructor(&mut a, "string", &[]);
        let param_type = new_tuple_type(&mut a, &[num, str]);
        let bool = new_constructor(&mut a, "boolean", &[]);
        let func = new_func_type(&mut a, &[param_type], bool);
        my_ctx.env.insert("foo".to_string(), func);

        let mut syntax = new_apply(new_identifier("foo"), &[new_tuple(&[new_number("5")])]);

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_object(&[
            ("a".to_string(), new_number("5")),
            ("b".to_string(), new_string("hello")),
        ]);

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), "{a: 5, b: \"hello\"}".to_string(),);

        Ok(())
    }

    #[test]
    fn object_member() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let object = new_object(&[
            ("a".to_string(), new_number("5")),
            ("b".to_string(), new_string("hello")),
        ]);
        let mut syntax = new_member(&object, &new_string("a"));

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), "5".to_string(),);

        Ok(())
    }

    #[test]
    fn object_member_missing_prop() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let object = new_object(&[
            ("a".to_string(), new_number("5")),
            ("b".to_string(), new_string("hello")),
        ]);
        let mut syntax = new_member(&object, &new_string("c"));

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let num = new_constructor(&mut a, "number", &[]);
        let str = new_constructor(&mut a, "string", &[]);
        let param_type = new_object_type(&mut a, &[("a".to_string(), num), ("b".to_string(), str)]);
        let bool = new_constructor(&mut a, "boolean", &[]);
        let func = new_func_type(&mut a, &[param_type], bool);
        my_ctx.env.insert("foo".to_string(), func);

        let mut syntax = new_apply(
            new_identifier("foo"),
            &[new_object(&[
                // Each prop must be a subtype of the expected element type
                ("a".to_string(), new_number("5")),
                ("b".to_string(), new_string("hello")),
                // It's okay to pass an object with extra props
                ("c".to_string(), new_boolean(true)),
            ])],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), "boolean".to_string(),);

        Ok(())
    }

    #[test]
    fn object_subtyping_missing_prop() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let num = new_constructor(&mut a, "number", &[]);
        let str = new_constructor(&mut a, "string", &[]);
        let param_type = new_object_type(&mut a, &[("a".to_string(), num), ("b".to_string(), str)]);
        let bool = new_constructor(&mut a, "boolean", &[]);
        let func = new_func_type(&mut a, &[param_type], bool);
        my_ctx.env.insert("foo".to_string(), func);

        let mut syntax = new_apply(
            new_identifier("foo"),
            &[new_object(&[("b".to_string(), new_string("hello"))])],
        );

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_string("hello")],
        );

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let lit1 = new_lit_type(&mut a, &Literal::Number("5".to_string()));
        let lit2 = new_lit_type(&mut a, &Literal::String("hello".to_string()));
        my_ctx
            .env
            .insert("foo".to_string(), new_union_type(&mut a, &[lit1, lit2]));

        let mut syntax = new_apply(
            new_identifier("times"),
            &[new_identifier("foo"), new_number("2")],
        );

        let result = infer_expression(&mut a, &mut syntax, &mut my_ctx);

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
        let (mut a, mut my_ctx) = test_env();

        let mut program = Program {
            statements: vec![
                new_declaration("num", &new_number("5")),
                new_declaration("str", &new_string("hello")),
                new_stmt_expr(&new_apply(
                    new_identifier("times"),
                    &[new_identifier("num"), new_identifier("num")],
                )),
            ],
        };

        infer_program(&mut a, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("num").unwrap();
        assert_eq!(a[*t].as_string(&a), r#"5"#);

        let t = my_ctx.env.get("str").unwrap();
        assert_eq!(a[*t].as_string(&a), r#""hello""#);

        eprintln!("program = {program}");

        insta::assert_snapshot!(program.to_string(), @r###"
        let num = 5
        let str = "hello"
        times(num, num)
        "###);

        Ok(())
    }

    #[test]
    fn test_program_with_generic_func() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut program = Program {
            statements: vec![
                new_declaration("id", &new_lambda(&["x"], &new_identifier("x"))),
                new_declaration("a", &new_apply(new_identifier("id"), &[new_number("5")])),
                new_declaration(
                    "b",
                    &new_apply(new_identifier("id"), &[new_string("hello")]),
                ),
            ],
        };

        infer_program(&mut a, &mut program, &mut my_ctx)?;

        let t = my_ctx.env.get("a").unwrap();
        assert_eq!(a[*t].as_string(&a), r#"5"#);

        let t = my_ctx.env.get("b").unwrap();
        assert_eq!(a[*t].as_string(&a), r#""hello""#);

        Ok(())
    }

    #[test]
    fn test_function_with_multiple_statements() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_function(
            &[],
            &[
                new_declaration("x", &new_number("5")),
                new_declaration("y", &new_number("10")),
                new_return(&new_apply(
                    new_identifier("times"),
                    &[new_identifier("x"), new_identifier("y")],
                )),
            ],
        );

        let t = infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        assert_eq!(a[t].as_string(&a), r#"() => number"#);

        if let ExprKind::Lambda(Lambda {
            body: BlockOrExpr::Block(Block { stmts }),
            ..
        }) = &syntax.kind
        {
            let x_t = stmts[0].inferred_type.unwrap();
            let y_t = stmts[1].inferred_type.unwrap();

            assert_eq!(a[x_t].as_string(&a), "5");
            assert_eq!(a[y_t].as_string(&a), "10");
        } else {
            panic!("expected a lambda");
        }

        insta::assert_snapshot!(syntax.to_string(), @r###"
        fn () => {let x = 5
        let y = 10
        return times(x, y)}
        "###);

        Ok(())
    }

    #[test]
    fn test_inferred_type_on_ast_nodes() -> Result<(), Errors> {
        let (mut a, mut my_ctx) = test_env();

        let mut syntax = new_function(
            &["x", "y"],
            &[new_return(&new_apply(
                new_identifier("times"),
                &[new_identifier("x"), new_identifier("y")],
            ))],
        );

        infer_expression(&mut a, &mut syntax, &mut my_ctx)?;

        if let ExprKind::Lambda(Lambda { params, .. }) = &syntax.kind {
            let x_t = params[0].pattern.inferred_type.unwrap();
            let y_t = params[0].pattern.inferred_type.unwrap();

            assert_eq!(a[x_t].as_string(&a), "number");
            assert_eq!(a[y_t].as_string(&a), "number");
        } else {
            panic!("expected a lambda");
        }

        Ok(())
    }
}
