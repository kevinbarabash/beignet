// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod env;
mod errors;
mod infer;
mod literal;
mod syntax;
mod types;
mod unify;
mod util;

pub use crate::infer::infer;

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use crate::env::*;
    use crate::errors::*;
    use crate::infer::*;
    use crate::literal::*;
    use crate::syntax::*;
    use crate::types::*;

    pub fn new_lambda(params: &[&str], body: Syntax) -> Syntax {
        Syntax::Lambda(Lambda {
            params: params.iter().map(|x| x.to_string()).collect(),
            body: Box::new(body),
        })
    }

    pub fn new_apply(func: Syntax, args: &[Syntax]) -> Syntax {
        Syntax::Apply(Apply {
            func: Box::new(func),
            args: args.to_owned(),
        })
    }

    pub fn new_let(var: &str, defn: Syntax, body: Syntax) -> Syntax {
        Syntax::Let(Let {
            var: var.to_string(),
            defn: Box::new(defn),
            body: Box::new(body),
        })
    }

    pub fn new_letrec(decls: &[(String, Syntax)], body: Syntax) -> Syntax {
        Syntax::Letrec(Letrec {
            decls: decls
                .iter()
                .map(|(var, defn)| (var.to_owned(), Box::new(defn.to_owned())))
                .collect(),
            body: Box::new(body),
        })
    }

    pub fn new_identifier(name: &str) -> Syntax {
        // TODO: Check that `name` is a valid identifier
        Syntax::Identifier(Identifier {
            name: name.to_string(),
        })
    }

    pub fn new_number(value: &str) -> Syntax {
        Syntax::Literal(Literal::Number(value.to_owned()))
    }

    pub fn new_string(value: &str) -> Syntax {
        Syntax::Literal(Literal::String(value.to_owned()))
    }

    pub fn new_boolean(value: bool) -> Syntax {
        Syntax::Literal(Literal::Boolean(value))
    }

    pub fn new_if_else(cond: Syntax, consequent: Syntax, alternate: Syntax) -> Syntax {
        Syntax::IfElse(IfElse {
            cond: Box::new(cond),
            consequent: Box::new(consequent),
            alternate: Box::new(alternate),
        })
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
                    new_if_else(
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

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(number -> number)"#
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
                        new_apply(
                            // times(1, odd(x - 1)) - this casts it to a number
                            new_identifier("times"),
                            &[new_apply(
                                new_identifier("odd"),
                                &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
                            )],
                        ),
                    ),
                ),
                (
                    "odd".to_string(),
                    new_lambda(
                        &["x"], // (x) =>
                        new_apply(
                            // times(1, even(x - 1)) - this casts it to a number
                            new_identifier("times"),
                            &[new_apply(
                                new_identifier("even"),
                                &[new_apply(new_identifier("pred"), &[new_identifier("x")])],
                            )],
                        ),
                    ),
                ),
            ],
            new_identifier("odd"),
        );

        infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;

        let t = my_env.0.get("even").unwrap();
        assert_eq!(
            a[*t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(number -> number)"#
        );
        let t = my_env.0.get("odd").unwrap();
        assert_eq!(
            a[*t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(number -> number)"#
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
            new_apply(
                new_identifier("pair"),
                &[
                    new_apply(new_identifier("x"), &[new_number("3")]),
                    new_apply(new_identifier("x"), &[new_boolean(true)]),
                ],
            ),
        );

        infer(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
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

        infer(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
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
        let syntax = new_let("f", new_lambda(&["x"], new_identifier("x")), pair);

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
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
            new_apply(new_identifier("f"), &[new_identifier("f")]),
        );

        infer(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }

    #[test]
    fn test_number_literal() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // let g = fn f => 5 in g g
        let syntax = new_let(
            "g",
            new_lambda(&["f"], new_number("5")),
            new_apply(new_identifier("g"), &[new_identifier("g")]),
        );

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
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
            new_let(
                "f",
                new_lambda(&["x"], new_identifier("g")),
                new_apply(
                    new_identifier("pair"),
                    &[
                        new_apply(new_identifier("f"), &[new_number("3")]),
                        new_apply(new_identifier("f"), &[new_boolean(true)]),
                    ],
                ),
            ),
        );

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(a -> (a * a))"#
        );
        Ok(())
    }

    #[test]
    fn test_composition() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        // Function composition
        // fn f (fn g (fn arg (f g arg)))
        let syntax = new_lambda(
            &["f"],
            new_lambda(
                &["g"],
                new_lambda(
                    &["arg"],
                    new_apply(
                        new_identifier("g"),
                        &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
                    ),
                ),
            ),
        );

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"((a -> b) -> ((b -> c) -> (a -> c)))"#
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
            new_apply(
                new_identifier("g"),
                &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
            ),
        );

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"((a -> b), (b -> c), a -> c)"#
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

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
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

        let t = infer(&mut a, &syntax, &mut my_env, &HashSet::default())?;
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
    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"type mismatch: unify(Type { id: 22, kind: Literal(String(\\\"hello\\\")) }, Type { id: 18, kind: Constructor(Constructor { name: \\\"number\\\", types: [] }) }) failed\")"]
    fn test_subtype_error() {
        let (mut a, mut my_env) = test_env();

        let syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_string("hello")],
        );

        infer(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
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

        infer(&mut a, &syntax, &mut my_env, &HashSet::default()).unwrap();
    }
}
