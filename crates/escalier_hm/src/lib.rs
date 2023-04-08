// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod env;
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
    use std::collections::{HashMap, HashSet};

    use crate::env::*;
    use crate::errors::*;
    use crate::infer::*;
    use crate::literal::*;
    use crate::syntax::{self, *};
    use crate::types::*;

    pub fn new_lambda(params: &[&str], body: &[Statement]) -> Expression {
        Expression::Lambda(Lambda {
            params: params.iter().map(|x| x.to_string()).collect(),
            body: body.to_owned(),
        })
    }

    pub fn new_apply(func: Expression, args: &[Expression]) -> Expression {
        Expression::Apply(Apply {
            func: Box::new(func),
            args: args.to_owned(),
        })
    }

    pub fn new_let(var: &str, defn: Expression, body: Expression) -> Expression {
        Expression::Let(Let {
            var: var.to_string(),
            defn: Box::new(defn),
            body: Box::new(body),
        })
    }

    pub fn new_letrec(decls: &[(String, Expression)], body: Expression) -> Expression {
        Expression::Letrec(Letrec {
            decls: decls
                .iter()
                .map(|(var, defn)| (var.to_owned(), Box::new(defn.to_owned())))
                .collect(),
            body: Box::new(body),
        })
    }

    pub fn new_identifier(name: &str) -> Expression {
        // TODO: Check that `name` is a valid identifier
        Expression::Identifier(Identifier {
            name: name.to_string(),
        })
    }

    pub fn new_number(value: &str) -> Expression {
        Expression::Literal(Literal::Number(value.to_owned()))
    }

    pub fn new_string(value: &str) -> Expression {
        Expression::Literal(Literal::String(value.to_owned()))
    }

    pub fn new_boolean(value: bool) -> Expression {
        Expression::Literal(Literal::Boolean(value))
    }

    pub fn new_tuple(elems: &[Expression]) -> Expression {
        Expression::Tuple(syntax::Tuple {
            elems: elems.to_owned(),
        })
    }

    pub fn new_object(props: &[(String, Expression)]) -> Expression {
        Expression::Object(syntax::Object {
            props: props.to_owned(),
        })
    }

    pub fn new_member(obj: &Expression, prop: &Expression) -> Expression {
        Expression::Member(Member {
            obj: Box::from(obj.to_owned()),
            prop: Box::new(prop.to_owned()),
        })
    }

    pub fn new_if_else(
        cond: Expression,
        consequent: Expression,
        alternate: Expression,
    ) -> Expression {
        Expression::IfElse(IfElse {
            cond: Box::new(cond),
            consequent: Box::new(consequent),
            alternate: Box::new(alternate),
        })
    }

    pub fn new_expr_stmt(expr: Expression) -> Statement {
        Statement::Expression(expr)
    }

    fn test_env() -> (Vec<Type>, Env) {
        let mut a = vec![];
        let mut my_env = Env::default();

        let var1 = new_var_type(&mut a);
        let var2 = new_var_type(&mut a);
        let pair_type = new_constructor(&mut a, "*", &[var1, var2]);
        my_env.0.insert(
            "pair".to_string(),
            new_func_type(&mut a, &[var1, var2], pair_type),
        );

        let int_t = new_constructor(&mut a, "number", &[]);
        let bool_t = new_constructor(&mut a, "boolean", &[]);
        my_env
            .0
            .insert("zero".to_string(), new_func_type(&mut a, &[int_t], bool_t));

        let num1_t = new_constructor(&mut a, "number", &[]);
        let num2_t = new_constructor(&mut a, "number", &[]);
        my_env
            .0
            .insert("pred".to_string(), new_func_type(&mut a, &[num1_t], num2_t));

        // It isn't necessary to create separate instances of `number` for each of
        // these.  When interferring types from code we'll want separate instances
        // so that we can link each back to the expression from which it was
        // inferred.
        let num1_t = new_constructor(&mut a, "number", &[]);
        let num2_t = new_constructor(&mut a, "number", &[]);
        let num3_t = new_constructor(&mut a, "number", &[]);
        my_env.0.insert(
            "times".to_string(),
            new_func_type(&mut a, &[num1_t, num2_t], num3_t),
        );
        my_env.0.insert(
            "add".to_string(),
            new_func_type(&mut a, &[num1_t, num2_t], num3_t),
        );

        my_env
            .0
            .insert("true".to_string(), new_bool_lit_type(&mut a, true));
        my_env
            .0
            .insert("false".to_string(), new_bool_lit_type(&mut a, false));

        (a, my_env)
    }

    /// Sets up some predefined types using the type constructors TypeVariable,
    /// TypeOperator and Function.  Creates a list of example expressions to be
    /// evaluated. Evaluates the expressions, printing the type or errors arising
    /// from each.

    #[test]
    fn test_factorial() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // factorial
        let syntax = new_letrec(
            &[(
                "factorial".to_string(),
                new_lambda(
                    &["n"], // fn n =>
                    &[new_expr_stmt(new_if_else(
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
                    ))],
                ),
            )],
            new_identifier("factorial"),
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(number) => number"#
        );
        Ok(())
    }

    #[test]
    fn test_mutual_recursion() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // NOTE: The definitions of "even" and "odd" are correct from a types
        // perspective, but incorrect semantically.
        let syntax = new_letrec(
            &[
                (
                    "even".to_string(),
                    new_lambda(
                        &["x"], // (x) =>
                        &[new_expr_stmt(new_apply(
                            // times(1, odd(x - 1)) - this casts it to a number
                            new_identifier("times"),
                            &[
                                new_number("1"),
                                new_apply(
                                    new_identifier("odd"),
                                    &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
                                ),
                            ],
                        ))],
                    ),
                ),
                (
                    "odd".to_string(),
                    new_lambda(
                        &["x"], // (x) =>
                        &[new_expr_stmt(new_apply(
                            // times(1, even(x - 1)) - this casts it to a number
                            new_identifier("times"),
                            &[
                                new_number("1"),
                                new_apply(
                                    new_identifier("even"),
                                    &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
                                ),
                            ],
                        ))],
                    ),
                ),
            ],
            new_identifier("odd"),
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(number) => number"#
        );

        Ok(())
    }

    #[should_panic]
    #[test]
    fn test_mismatch() {
        let (mut a, mut my_env) = test_env();

        // fn x => (pair(x(3) (x(true)))
        let syntax = new_lambda(
            &["x"],
            &[new_expr_stmt(new_apply(
                new_identifier("pair"),
                &[
                    new_apply(new_identifier("x"), &[new_number("3")]),
                    new_apply(new_identifier("x"), &[new_boolean(true)]),
                ],
            ))],
        );

        infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }

    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"Undefined symbol \\\"f\\\"\")"]
    #[test]
    fn test_pair() {
        let (mut a, mut my_env) = test_env();

        // pair(f(3), f(true))
        let syntax = new_apply(
            new_identifier("pair"),
            &[
                new_apply(new_identifier("f"), &[new_number("4")]),
                new_apply(new_identifier("f"), &[new_boolean(true)]),
            ],
        );

        infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }

    #[test]
    fn test_mul() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let pair = new_apply(
            new_identifier("pair"),
            &[
                new_apply(new_identifier("f"), &[new_number("4")]),
                new_apply(new_identifier("f"), &[new_boolean(true)]),
            ],
        );

        // let f = (fn x => x) in ((pair (f 4)) (f true))
        let syntax = new_let(
            "f",
            new_lambda(&["x"], &[new_expr_stmt(new_identifier("x"))]),
            pair,
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(4 * true)"#
        );
        Ok(())
    }

    #[should_panic = "recursive unification"]
    #[test]
    fn test_recursive() {
        let (mut a, mut my_env) = test_env();

        // fn f => f f (fail)
        let syntax = new_lambda(
            &["f"],
            &[new_expr_stmt(new_apply(
                new_identifier("f"),
                &[new_identifier("f")],
            ))],
        );

        infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }

    #[test]
    fn test_number_literal() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // let g = fn f => 5 in g g
        let syntax = new_let(
            "g",
            new_lambda(&["f"], &[new_expr_stmt(new_number("5"))]),
            new_apply(new_identifier("g"), &[new_identifier("g")]),
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"5"#
        );
        Ok(())
    }

    #[test]
    fn test_generic_nongeneric() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // example that demonstrates generic and non-generic variables:
        // fn g => let f = fn x => g in pair (f 3, f true)
        let syntax = new_lambda(
            &["g"],
            &[new_expr_stmt(new_let(
                "f",
                new_lambda(&["x"], &[new_expr_stmt(new_identifier("g"))]),
                new_apply(
                    new_identifier("pair"),
                    &[
                        new_apply(new_identifier("f"), &[new_number("3")]),
                        new_apply(new_identifier("f"), &[new_boolean(true)]),
                    ],
                ),
            ))],
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(a) => (a * a)"#
        );
        Ok(())
    }

    #[test]
    fn test_basic_generics() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // example that demonstrates generic and non-generic variables:
        // fn x => x
        let syntax = new_lambda(&["x"], &[new_expr_stmt(new_identifier("x"))]);

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(a) => a"#
        );
        let t = &a[t];
        eprintln!("t = {t:#?}");
        Ok(())
    }

    #[test]
    fn test_composition() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // Function composition
        // fn f (fn g (fn arg (f g arg)))
        let syntax = new_lambda(
            &["f"],
            &[new_expr_stmt(new_lambda(
                &["g"],
                &[new_expr_stmt(new_lambda(
                    &["arg"],
                    &[new_expr_stmt(new_apply(
                        new_identifier("g"),
                        &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
                    ))],
                ))],
            ))],
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"((a) => b) => ((b) => c) => (a) => c"#
        );
        Ok(())
    }

    #[test]
    fn test_fun() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // Function composition
        // (fn (f, g, arg) -> (f g arg))
        let syntax = new_lambda(
            &["f", "g", "arg"],
            &[new_expr_stmt(new_apply(
                new_identifier("g"),
                &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
            ))],
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"((a) => b, (b) => c, a) => c"#
        );
        Ok(())
    }

    #[test]
    fn test_subtype() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_number("10")],
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"number"#
        );
        Ok(())
    }

    #[test]
    fn test_union_subtype() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let lit1 = new_num_lit_type(&mut a, "5");
        let lit2 = new_num_lit_type(&mut a, "10");
        my_env
            .0
            .insert("foo".to_string(), new_union_type(&mut a, &[lit1, lit2]));

        let syntax = new_apply(
            new_identifier("times"),
            &[new_identifier("foo"), new_number("2")],
        );

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"number"#
        );
        Ok(())
    }

    #[test]
    fn test_calling_a_union() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let bool = new_constructor(&mut a, "boolean", &[]);
        let str = new_constructor(&mut a, "string", &[]);
        let fn1 = new_func_type(&mut a, &[], bool);
        let fn2 = new_func_type(&mut a, &[], str);
        my_env
            .0
            .insert("foo".to_string(), new_union_type(&mut a, &[fn1, fn2]));

        let syntax = new_apply(new_identifier("foo"), &[]);

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"boolean | string"#
        );
        Ok(())
    }

    #[test]
    fn call_with_too_few_args() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let syntax = new_apply(new_identifier("times"), &[]);

        let result = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default());

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
        let (mut a, mut my_env) = test_env();

        let lit = new_num_lit_type(&mut a, "5");
        my_env.0.insert("foo".to_string(), lit);

        let syntax = new_apply(new_identifier("foo"), &[]);

        let result = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default());

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
        let (mut a, mut my_env) = test_env();

        let syntax = new_tuple(&[new_number("5"), new_string("hello")]);

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;

        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            "[5, \"hello\"]".to_string(),
        );

        Ok(())
    }

    #[test]
    fn tuple_member() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let tuple = new_tuple(&[new_number("5"), new_string("hello")]);
        let syntax = new_member(&tuple, &new_number("1"));

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;

        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            "\"hello\"".to_string(),
        );

        Ok(())
    }

    #[test]
    fn infer_basic_object() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let syntax = new_object(&[
            ("a".to_string(), new_number("5")),
            ("b".to_string(), new_string("hello")),
        ]);

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;

        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            "{a: 5, b: \"hello\"}".to_string(),
        );

        Ok(())
    }

    #[test]
    fn object_member() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let object = new_object(&[
            ("a".to_string(), new_number("5")),
            ("b".to_string(), new_string("hello")),
        ]);
        let syntax = new_member(&object, &new_string("a"));

        let t = infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default())?;

        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            "5".to_string(),
        );

        Ok(())
    }

    #[test]
    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"type mismatch: unify(Type { id: 22, kind: Literal(String(\\\"hello\\\")) }, Type { id: 18, kind: Constructor(Constructor { name: \\\"number\\\", types: [] }) }) failed\")"]
    fn test_subtype_error() {
        let (mut a, mut my_env) = test_env();

        let syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_string("hello")],
        );

        infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }

    #[test]
    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"type mismatch: unify(Type { id: 25, kind: Literal(String(\\\"hello\\\")) }, Type { id: 20, kind: Constructor(Constructor { name: \\\"number\\\", types: [] }) }) failed\")"]
    fn test_union_subtype_error() {
        let (mut a, mut my_env) = test_env();

        let lit1 = new_lit_type(&mut a, &Literal::Number("5".to_string()));
        let lit2 = new_lit_type(&mut a, &Literal::String("hello".to_string()));
        my_env
            .0
            .insert("foo".to_string(), new_union_type(&mut a, &[lit1, lit2]));

        let syntax = new_apply(
            new_identifier("times"),
            &[new_identifier("foo"), new_number("2")],
        );

        infer_expression(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }

    #[test]
    fn test_program() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let program = Program {
            statements: vec![
                Statement::Declaration(Declaration {
                    var: "num".to_string(),
                    defn: Box::new(new_number("5")),
                }),
                Statement::Declaration(Declaration {
                    var: "str".to_string(),
                    defn: Box::new(new_string("hello")),
                }),
                Statement::Expression(new_apply(
                    new_identifier("times"),
                    &[new_identifier("num"), new_identifier("num")],
                )),
            ],
        };

        infer_program(&mut a, &program, &mut my_env)?;

        let t = my_env.0.get("num").unwrap();
        assert_eq!(
            a[*t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"5"#
        );

        let t = my_env.0.get("str").unwrap();
        assert_eq!(
            a[*t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#""hello""#
        );

        Ok(())
    }

    #[test]
    fn test_program_with_generic_func() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let program = Program {
            statements: vec![
                Statement::Declaration(Declaration {
                    var: "id".to_string(),
                    defn: Box::new(new_lambda(&["x"], &[new_expr_stmt(new_identifier("x"))])),
                }),
                Statement::Declaration(Declaration {
                    var: "a".to_string(),
                    defn: Box::new(new_apply(new_identifier("id"), &[new_number("5")])),
                }),
                Statement::Declaration(Declaration {
                    var: "b".to_string(),
                    defn: Box::new(new_apply(new_identifier("id"), &[new_string("hello")])),
                }),
            ],
        };

        infer_program(&mut a, &program, &mut my_env)?;

        let t = my_env.0.get("a").unwrap();
        assert_eq!(
            a[*t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"5"#
        );

        let t = my_env.0.get("b").unwrap();
        assert_eq!(
            a[*t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#""hello""#
        );

        Ok(())
    }

    #[test]
    fn test_lambda_with_multiple_statements() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let lambda = new_lambda(
            &[],
            &[
                Statement::Declaration(Declaration {
                    var: "x".to_string(),
                    defn: Box::new(new_number("5")),
                }),
                Statement::Declaration(Declaration {
                    var: "y".to_string(),
                    defn: Box::new(new_number("10")),
                }),
                Statement::Return(Return {
                    expr: Box::new(new_apply(
                        new_identifier("times"),
                        &[new_identifier("x"), new_identifier("y")],
                    )),
                }),
            ],
        );

        let t = infer_expression(&mut a, &lambda, &mut my_env, &HashSet::default())?;

        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"() => number"#
        );

        Ok(())
    }
}
