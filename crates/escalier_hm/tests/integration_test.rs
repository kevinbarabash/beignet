use generational_arena::{Arena, Index};

use escalier_ast::{self as syntax, Literal as Lit, *};
use escalier_parser::{ParseError, Parser};

use escalier_hm::checker::Checker;
use escalier_hm::context::*;
use escalier_hm::type_error::TypeError;
use escalier_hm::types::{self, *};

pub fn parse_script(input: &str) -> Result<Script, ParseError> {
    let mut parser = Parser::new(input);
    parser.parse_script()
}

pub fn parse_module(input: &str) -> Result<Module, ParseError> {
    let mut parser = Parser::new(input);
    parser.parse_module()
}

fn assert_no_errors(checker: &Checker) -> Result<(), TypeError> {
    if !checker.current_report.diagnostics.is_empty() {
        return Err(TypeError {
            message: format!(
                "expected no errors, found: {:?}",
                checker.current_report.diagnostics
            ),
        });
    }

    Ok(())
}

fn new_num_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
    arena.insert(Type::from(TypeKind::Literal(Lit::Number(value.to_owned()))))
}

fn new_str_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
    arena.insert(Type::from(TypeKind::Literal(Lit::String(value.to_owned()))))
}

fn test_env() -> (Checker, Context) {
    let mut checker = Checker::default();
    let mut context = Context::default();

    let number = checker.new_primitive(Primitive::Number);
    let type_param_t = checker.new_type_ref("T", None, &[]);

    let never = checker.new_keyword(Keyword::Never);
    let push_t = checker.new_func_type(
        &[types::FuncParam {
            pattern: TPat::Ident(BindingIdent {
                name: "item".to_string(),
                mutable: false,
                span: Span { start: 0, end: 0 },
            }),
            t: type_param_t,
            optional: false,
        }],
        number,
        &None,
        &never,
    );

    // [P]: T for P in number;
    let mapped = types::TObjElem::Mapped(types::MappedType {
        key: checker.new_type_ref("P", None, &[]),
        value: checker.new_type_ref("T", None, &[]),
        target: "P".to_string(),
        source: checker.new_primitive(Primitive::Number),
        optional: None,
        check: None,
        extends: None,
    });

    let array_interface = checker.new_object_type(&[
        // .push(item: T) -> number;
        types::TObjElem::Prop(types::TProp {
            name: types::TPropKey::StringKey("push".to_string()),
            optional: false,
            readonly: false,
            t: push_t,
        }),
        // .length: number;
        types::TObjElem::Prop(types::TProp {
            name: types::TPropKey::StringKey("length".to_string()),
            optional: false,
            readonly: false,
            t: number,
        }),
        mapped,
    ]);
    let array_scheme = Scheme {
        type_params: Some(vec![types::TypeParam {
            name: "T".to_string(),
            constraint: None,
            default: None,
        }]),
        t: array_interface,
        is_type_param: false,
    };

    context.schemes.insert("Array".to_string(), array_scheme);

    (checker, context)
}

/// Sets up some predefined types using the type constructors TypeVariable,
/// TypeOperator and Function.  Creates a list of example expressions to be
/// evaluated. Evaluates the expressions, printing the type or errors arising
/// from each.

#[test]
fn test_complex_logic() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let a: number
    declare let b: number
    declare let c: number
    let result = a > b || b >= c || c != a && c != b
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn test_string_equality() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let a: string
    declare let b: string
    let eq = a == b
    let neq = a != b
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("eq").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);
    let binding = my_ctx.values.get("neq").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn test_if_else() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let cond: boolean
    let result = if (cond) { 5 } else { 10 }
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(checker.print_type(&binding.index), r#"5 | 10"#);
    assert_no_errors(&checker)
}

#[test]
fn test_chained_if_else() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let cond1: boolean
    declare let cond2: boolean
    let result = if (cond1) { 5 } else if (cond2) { 10 } else { 15 }
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(checker.print_type(&binding.index), r#"5 | 10 | 15"#);
    assert_no_errors(&checker)
}

#[test]
fn test_factorial() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // factorial
    let src = r#"
    let fact = fn (n) {
        return if (n == 0) {
            1
        } else {
            n * fact(n - 1)
        }
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("fact").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        r#"(n: number) -> 1 | number"#
    );
    assert_no_errors(&checker)
}

#[test]
fn test_mutual_recursion() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let even = fn (x) => if (x == 0) {
        true
    } else {
        !odd(x - 1)
    }

    let odd = fn (x) => if (x == 1) {
        true
    } else {
        !even(x - 1)
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("even").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> true | boolean"#
    );
    let binding = my_ctx.values.get("odd").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> true | boolean"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mutual_recursion_using_destructuring() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let {even, odd} = {
        even: fn (x) => if (x == 0) {
            true
        } else {
            !odd(x - 1)
        },
        odd: fn (x) => if (x == 1) {
            true
        } else {
            !even(x - 1)
        },
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("even").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> true | boolean"#
    );
    let binding = my_ctx.values.get("odd").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> true | boolean"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_mutual_rec_decl() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = "let [foo, bar] = [
        fn (x) => if (x > 0) { bar(x - 1) } else { true },
        fn (x) => if (x > 0) { foo(x - 1) } else { false },
    ]";
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = checker.print_type(&my_ctx.values.get("foo").unwrap().index);
    insta::assert_snapshot!(result, @"(x: number) -> false | true | false | true");

    let result = checker.print_type(&my_ctx.values.get("bar").unwrap().index);
    insta::assert_snapshot!(result, @"(x: number) -> false | true | false");

    Ok(())
}

// NOTE: The snapshots are slightly different for the following two tests.  They
// should be the same after #706 and #707.
#[test]
fn infer_mutual_rec_decl_in_module() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = "let [foo, bar] = [
        fn (x) => if (x > 0) { bar(x - 1) } else { true },
        fn (x) => if (x > 0) { foo(x - 1) } else { false },
    ]";
    let mut module = parse_module(src).unwrap();
    checker.infer_module(&mut module, &mut my_ctx)?;

    let result = checker.print_type(&my_ctx.values.get("foo").unwrap().index);
    insta::assert_snapshot!(result, @"(x: number) -> false | true | false | true");

    let result = checker.print_type(&my_ctx.values.get("bar").unwrap().index);
    insta::assert_snapshot!(result, @"(x: number) -> false | true | false");

    Ok(())
}

#[test]
fn infer_mutual_rec_separate_decls_in_module() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = "
    let foo = fn (x) => if (x > 0) { bar(x - 1) } else { true }
    let bar = fn (x) => if (x > 0) { foo(x - 1) } else { false }
    ";
    let mut module = parse_module(src).unwrap();
    checker.infer_module(&mut module, &mut my_ctx)?;

    let result = checker.print_type(&my_ctx.values.get("foo").unwrap().index);
    insta::assert_snapshot!(result, @"(x: number) -> true | false | true");

    let result = checker.print_type(&my_ctx.values.get("bar").unwrap().index);
    insta::assert_snapshot!(result, @"(x: number) -> true | false | true | false");

    Ok(())
}

#[test]
fn infer_type_in_module() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = "
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    ";
    let mut module = parse_module(src).unwrap();
    checker.infer_module(&mut module, &mut my_ctx)?;

    let result = checker.print_type(&my_ctx.values.get("p").unwrap().index);
    insta::assert_snapshot!(result, @"Point");

    Ok(())
}

#[test]
fn top_level_for_loop_in_module_errors() -> Result<(), TypeError> {
    let (_, _) = test_env();

    let src = "
    let tuple = [1, 2, 3]
    for (i in tuple) {
        // do something
    }
    ";
    let result = parse_module(src);

    assert_eq!(
        result,
        Err(ParseError {
            message: "expected module item".to_string()
        })
    );

    Ok(())
}

#[test]
fn infer_mutual_rec_decls() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = "
    let foo = fn (x) => if (x > 0) { bar(x - 1) } else { true }
    let bar = fn (x) => if (x > 0) { foo(x - 1) } else { false }
    ";
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = checker.print_type(&my_ctx.values.get("foo").unwrap().index);
    insta::assert_snapshot!(result, @"<A>(x: number) -> A | true");

    let result = checker.print_type(&my_ctx.values.get("bar").unwrap().index);
    insta::assert_snapshot!(result, @"<A>(x: number) -> A | true | false");

    Ok(())
}

#[test]
fn test_no_top_level_redeclaration() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let id = fn (x) => x
    let id = fn (y) => y
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "id cannot be redeclared at the top-level".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mismatch() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"fn (x) => [x(3), x(true)]"#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: true != 3
    "###);

    Ok(())
}

#[test]
fn test_multiple_incorrect_args() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (x: number, y: string) => x
    foo(true, false)
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    ├ TypeError: type mismatch: unify(true, number) failed
    └ TypeError: type mismatch: unify(false, string) failed

    "###);

    Ok(())
}

#[test]
fn test_pair() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"[f(3), f(true)]"#;

    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Undefined symbol \"f\"".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mul() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        let f = fn (x) => x
        let result = [f(4), f(true)]
    "#;

    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"[4, true]"#);
    assert_no_errors(&checker)
}

#[should_panic = "recursive unification"]
#[test]
fn test_recursive() {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"fn (f) => f(f)"#;

    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx).unwrap();
}

#[test]
fn test_fib() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        let fib = fn (n) => if (n == 0) {
            0
        } else if (n == 1) {
            1
        } else {
            fib(n - 1) + fib(n - 2)
        }
    "#;

    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx).unwrap();

    let binding = my_ctx.values.get("fib").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(n: number) -> 0 | 1 | number"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_number_literal() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let g = fn (f) => 5
    let result = g(g)
    "#;

    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);
    assert_no_errors(&checker)
}

#[test]
fn test_generic_nongeneric() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let result = fn (g) {
        let f = fn (x) => g
        return [f(3), f(true)]
    }"#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"<A>(g: A) -> [A, A]"#);
    assert_no_errors(&checker)
}

#[test]
fn test_basic_generics() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // example that demonstrates generic and non-generic variables:
    let src = r#"let result = fn (x) => x"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"<A>(x: A) -> A"#);

    assert_no_errors(&checker)
}

#[test]
fn test_composition() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // Function composition
    // fn f (fn g (fn arg (f g arg)))
    let src = r#"let result = fn (f) => fn (g) => fn (arg) => g(f(arg))"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B, C>(f: (arg0: A) -> B) -> (g: (arg0: B) -> C) -> (arg: A) -> C"#
    );
    assert_no_errors(&checker)
}

#[test]
fn test_skk() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))
    let K = fn (x) => fn (y) => x
    let I = S(K)(K)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("S").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B, C>(f: (arg0: A) -> (arg0: B) -> C) -> (g: (arg0: A) -> B) -> (x: A) -> C"#
    );
    let binding = my_ctx.values.get("K").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<B, A>(x: A) -> (y: B) -> A"#
    );
    let binding = my_ctx.values.get("I").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"<A>(x: A) -> A"#);

    assert_no_errors(&checker)
}

#[test]
fn test_composition_with_statements() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // Function composition
    let src = r#"
    let result = fn (f) {
        let mantel = fn (g) {
            let core = fn (arg) => g(f(arg))
            return core
        }
        return mantel
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B, C>(f: (arg0: A) -> B) -> (g: (arg0: B) -> C) -> (arg: A) -> C"#
    );
    assert_no_errors(&checker)
}

#[test]
fn test_subtype() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(5, 10)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);
    assert_no_errors(&checker)
}

#[test]
fn test_callback_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // It's okay for the callback arg to take fewer params since extra params
    // are ignored.  It's also okay for its params to be supertypes of the
    // expected params since the callback will only be called with the expected
    // types.  Lastly, it's okay for the return type to be a subtype of the
    // expected return type since it still conforms to the expected type.
    let src = r#"
    declare let foo: fn (cb: fn (a: number, b: string) -> boolean) -> boolean
    declare let bar: fn (x: number | string) -> boolean
    let result = foo(bar)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);
    assert_no_errors(&checker)
}

#[test]
fn test_callback_error_too_many_params() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (cb: fn (x: number) -> boolean) -> boolean
    declare let bar: fn (a: number, b: string) -> boolean
    let result = foo(bar)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: (a: number, b: string) -> boolean is not a subtype of (x: number) -> boolean since it requires more params
    "###);

    Ok(())
}

#[test]
fn infer_param_types_with_union_return_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (cond, a, b) -> number | string =>
        if (cond) { a } else { b }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(cond: boolean, a: number | string, b: number | string) -> number | string"#
    );
    assert_no_errors(&checker)
}

#[test]
fn test_union_subtype() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let lit1 = new_num_lit_type(&mut checker.arena, "5");
    let lit2 = new_num_lit_type(&mut checker.arena, "10");
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: checker.new_union_type(&[lit1, lit2]),
            is_mut: false,
        },
    );

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(foo, 2)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);
    assert_no_errors(&checker)
}

#[test]
fn test_calling_a_union() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let bool = checker.new_primitive(Primitive::Boolean);
    let str = checker.new_primitive(Primitive::String);
    let never = checker.new_keyword(Keyword::Never);
    let fn1 = checker.new_func_type(&[], bool, &None, &never);
    let fn2 = checker.new_func_type(&[], str, &None, &never);
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: checker.new_union_type(&[fn1, fn2]),
            is_mut: false,
        },
    );

    let src = r#"let result = foo()"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean | string"#);
    assert_no_errors(&checker)
}

#[test]
fn call_with_too_few_args() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times()
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "too few arguments to function: expected 2, got 0".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn literal_isnt_callable() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let lit = new_num_lit_type(&mut checker.arena, "5");
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: lit,
            is_mut: false,
        },
    );

    let src = r#"let result = foo()"#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "literal Number(\n    \"5\",\n) is not callable".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_basic_tuple() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"let result = [5, "hello"]"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        "[5, \"hello\"]".to_string(),
    );

    assert_no_errors(&checker)
}

#[test]
fn tuple_member() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"]
    let second = tuple[1]
    declare let index: number
    let any = tuple[index]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("second").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#""hello""#.to_string(),);
    let binding = my_ctx.values.get("any").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"5 | "hello" | undefined"#.to_string(),
    );

    assert_no_errors(&checker)
}

#[test]
fn tuple_member_invalid_index() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"]
    let second = tuple["foo"]
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't access property on non-object type".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn array_member() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<number>
    let first = array[0]
    declare let index: number
    let any = array[0]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("first").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        "number | undefined".to_string(),
    );
    let binding = my_ctx.values.get("any").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        "number | undefined".to_string(),
    );

    assert_no_errors(&checker)
}

#[test]
fn tuple_member_error_out_of_bounds() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"]
    let result = tuple[2]
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "2 was outside the bounds 0..2 of the tuple".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn tuple_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (x: [number, string]) -> boolean
    let result = foo([5, "hello", true])
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), "boolean".to_string(),);

    assert_no_errors(&checker)
}

// TODO(#654): update how we unify tuples with arrays and other tuples
#[test]
#[ignore]
fn more_tuple_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let tuple1: [number, ...string[]] = [5]
    let tuple2: [number, ...string[]] = [5, "hello"]
    let tuple3: [number, ...string[]] = [5, "hello", "world"]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn tuple_subtyping_not_enough_elements() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (x: [number, string]) -> boolean
    let result = foo([5])
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: Expected tuple of length 2, got tuple of length 1
    "###);

    Ok(())
}

#[test]
fn infer_basic_object() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"let result = {a: 5, b: "hello"}"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        "{a: 5, b: \"hello\"}".to_string(),
    );

    assert_no_errors(&checker)
}

#[test]
fn object_member() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let obj = {a: 5, b: "hello"}
    let result = obj.a
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(checker.print_type(&binding.index), "5".to_string(),);

    assert_no_errors(&checker)
}

#[test]
fn object_member_string_key() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let obj = {a: 5, b: "hello"}
    declare let key: string
    let result = obj[key]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        "5 | \"hello\" | undefined".to_string(),
    );

    assert_no_errors(&checker)
}

#[test]
fn object_member_missing_prop() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let obj = {a: 5, b: "hello"}
    let result = obj.c
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Couldn't find property 'c' on object".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn object_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // Each prop must be a subtype of the expected element type
    // It's okay to pass an object with extra props
    let src = r#"
    declare let foo: fn (x: {a: number, b: string}) -> boolean
    let result = foo({a: 5, b: "hello", c: true})
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(checker.print_type(&binding.index), "boolean".to_string(),);

    assert_no_errors(&checker)
}

#[test]
fn object_signatures() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // Each prop must be a subtype of the expected element type
    // It's okay to pass an object with extra props
    let src = r#"
    declare let obj: {
        fn (a: number) -> string,
        foo: fn (a: number) -> string,
        fn bar(self, a: number) -> string,
        get baz(self) -> string,
        set baz(mut self, value: string) -> undefined,
        [P]: number for P in string,
        qux: string,
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("obj").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        "{fn(a: number) -> string, foo: (a: number) -> string, bar(self, a: number) -> string, get baz(self) -> string, set baz(mut self, string), [P]: number for P in string, qux: string}".to_string(),
    );

    assert_no_errors(&checker)
}

#[test]
fn object_callable_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn (a: number | string) -> string,
    }
    let bar: {
        fn (a: number) -> number | string,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

// TODO: This should fail but doesn't, we need to check unify callable
// signatures in object types
#[test]
#[ignore]
fn object_callable_subtyping_failure_case() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn (a: string) -> string,
    }
    let bar: {
        fn (a: number) -> number,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Expected type number, found type string".to_string(),
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn object_method_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn method(self, a: number | string) -> string,
    }
    let bar: {
        fn method(self, a: number) -> number | string,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn object_property_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn method(self, a: number) -> string,
        x: number,
        y: boolean,
    }
    let bar: {
        fn method(self, a: number) -> string,
        x: number | string,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn object_mapped_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        [P]: number for P in string | number,
    }
    let bar: {
        [P]: number | string for P in string
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn object_methods_and_properties_should_unify() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn foo(self, a: number) -> string,
    }
    let bar: {
        foo: fn (a: number) -> string,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn object_mappeds_should_unify_with_all_named_obj_elems() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        a: fn () -> number,
        b?: fn () -> number,
        get c(self) -> (fn () -> number),
        fn d(self) -> number,
    }
    let bar: {
        [P]: fn () -> number for P in string,
    } = foo
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn object_mappeds_and_properties_unify_failure() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        x: number,
    }
    let bar: {
        [P]: boolean for P in string
    } = foo
    "#;

    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(number, boolean | undefined) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

// NOTE: Getters are readonly while bar.foo is not readonly so this
// assignment should not be allowed, but we're not handling readonly-ness
// yet.
#[test]
fn object_properties_and_getter_should_unify() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        get foo(self) -> number,
    }
    let bar: {
        foo: number,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
#[ignore]
fn mutable_object_properties_unify_with_getters_setters() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let mut foo: {
        x: number,
    }
    let mut bar: {
        get x(self) -> number,
        set x(mut self, value: number) -> undefined,
    } = foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn object_subtyping_missing_prop() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (x: {a: number, b: string}) -> boolean
    let result = foo({b: "hello"})
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: 'a' is missing in {b: "hello"}
    "###);

    Ok(())
}

#[test]
fn test_subtype_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(5, "hello")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: unify("hello", number) failed
    "###);

    Ok(())
}

#[test]
fn test_union_subtype_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let lit1 = new_num_lit_type(&mut checker.arena, "5");
    let lit2 = new_str_lit_type(&mut checker.arena, "hello");
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: checker.new_union_type(&[lit1, lit2]),
            is_mut: false,
        },
    );

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(foo, "world")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    ├ TypeError: type mismatch: unify("hello", number) failed
    └ TypeError: type mismatch: unify("world", number) failed

    "###);

    Ok(())
}

#[test]
fn test_union_subtype_error_with_type_ann() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let x: number | string = true
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(true, number | string) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_program() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let num = 5
    let str = "hello"
    num * num
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("num").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);

    let binding = my_ctx.values.get("str").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#""hello""#);

    // TODO: implement std::fmt for Program et al
    // eprintln!("script = {script}");

    // insta::assert_snapshot!(script.to_string(), @r###"
    // let num = 5
    // let str = "hello"
    // times(num, num)
    // "###);

    assert_no_errors(&checker)
}

#[test]
fn test_program_with_generic_func() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let id = fn (x) => x
    let a = id(5)
    let b = id("hello")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("id").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"<A>(x: A) -> A"#);

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#""hello""#);

    assert_no_errors(&checker)
}

#[test]
fn test_program_with_generic_func_multiple_type_params() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let fst = fn (x, y) => x
    let snd = fn (x, y) => y
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<B, A>(x: A, y: B) -> A"#
    );

    let binding = my_ctx.values.get("snd").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B>(x: A, y: B) -> B"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_function_with_multiple_statements() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let result = fn () {
        let x = 5
        let y = 10
        return x * y
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"() -> 50"#);

    if let StmtKind::Decl(Decl {
        kind: DeclKind::VarDecl(VarDecl {
            expr: Some(init), ..
        }),
        ..
    }) = &script.stmts[0].kind
    {
        if let ExprKind::Function(syntax::Function {
            body: BlockOrExpr::Block(Block { stmts: _, .. }),
            ..
        }) = &init.kind
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
    // fn () {let x = 5
    // let y = 10
    // return times(x, y)}
    // "###);

    assert_no_errors(&checker)
}

#[test]
fn test_inferred_type_on_ast_nodes() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"let result = fn (x, y) => x * y"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    if let StmtKind::Decl(Decl {
        kind: DeclKind::VarDecl(VarDecl {
            expr: Some(init), ..
        }),
        ..
    }) = &script.stmts[0].kind
    {
        if let ExprKind::Function(expr::Function { params, .. }) = &init.kind {
            let x_t = params[0].pattern.inferred_type.unwrap();
            let y_t = params[1].pattern.inferred_type.unwrap();

            assert_eq!(checker.print_type(&x_t), "number");
            assert_eq!(checker.print_type(&y_t), "number");
        } else {
            panic!("expected a lambda");
        }
    } else {
        panic!("expected a variable declaration");
    }

    assert_no_errors(&checker)
}

#[test]
fn test_unary_op() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"let neg = fn (x) => -x"#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("neg").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> number"#
    );
    assert_no_errors(&checker)
}

#[test]
fn test_async_return_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => 5
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("foo").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> Promise<5, never>"#
    );
    assert_no_errors(&checker)
}

#[test]
fn throws_in_async() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn (cond) => if (cond) { throw "error" } else { 5 }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("foo").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        r#"(cond: boolean) -> Promise<5, "error">"#
    );
    assert_no_errors(&checker)
}

#[test]
fn await_async_func_with_throw() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn (cond) => if (cond) { throw "error" } else { 5 }
    let bar = async fn () => await foo(true)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(cond: boolean) -> Promise<5, "error">"#
    );

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> Promise<5, "error">"#
    );

    assert_no_errors(&checker)
}

#[test]
fn catch_await_async_func_that_throws() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn (cond) => if (cond) { throw "error" } else { 5 }
    let bar = async fn () {
        return try {
            await foo(true)
        } catch (e) {
            10
        }
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(cond: boolean) -> Promise<5, "error">"#
    );

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> Promise<5 | 10, never>"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_async_without_return() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () {
        let sum = 5 + 10
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("foo").unwrap();

    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> Promise<undefined, never>"#
    );
    assert_no_errors(&checker)
}

#[test]
fn test_await_in_async() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => 5
    let bar = async fn () {
        let x = await foo()
        return x
    }
    let baz = async fn () => foo()
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> Promise<5, never>"#
    );

    let binding = my_ctx.values.get("baz").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> Promise<5, never>"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_await_outside_of_async() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => 5
    let bar = fn () {
        let x = await foo()
        return x
    }
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);
    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't use await outside of an async function".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_await_non_promise() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => await 5
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);
    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(5, Promise<t11, t12>) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

// TODO: write a test to ensure that Promise<5> is a subtype of Promise<number>
// In general, generic types should be covariant across their type parameters.

#[test]
fn test_do_expr() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let sum = do {
        let msg = do {
            "hello"
        }
        let x = 5
        let y = 10
        let result = [msg, x + y]
        result
    }
    "#;
    // The following is ambiguous:
    // let y = 10
    // [msg]
    // TODO: If there's a newline before a postfix operator, we should
    // ignore the postfix operator.
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"["hello", 15]"#);

    assert_no_errors(&checker)
}

#[test]
fn test_empty_do_expr() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"let sum = do {}"#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn test_let_with_type_ann() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let x: number = 5
    let flag: boolean = true
    let foo: fn () -> number = fn () => 10
    let bar: fn () -> undefined = fn () {}
    let arr1: number[] = [1, 2, 3]
    let arr2: Array<string> = ["hello", "world"]
    let p: { x: number, y: number } = { x: 5, y: 10 }
    let tuple: [number, string] = [5, "hello"]
    let union: number | string = 5
    let union_arr: (number | string)[] = [5, "hello"]
    "#;
    // TODO: add support for comments
    // This should be valid, but we don't support it yet
    // let baz: (number) => number = <A>(a: A) => a;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn test_function_overloads() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let add: (fn (a: number, b: number) -> number) & (fn (a: string, b: string) -> string)
    let sum = add(5, 10)
    let msg = add("hello, ", "world")
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    let binding = my_ctx.values.get("msg").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_function_no_valid_overload() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let add: (fn (a: number, b: number) -> number) & (fn (a: string, b: string) -> string)
    add(5, "world")
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "no valid overload for args".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_declare_cant_have_initializer() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let add: fn (a: number, b: number) -> number = fn (a, b) => a + b
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Variable declarations using `declare` cannot have an initializer".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_declare_must_have_type_annotations() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let add
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Variable declarations using `declare` must have a type annotation"
                .to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_normal_decl_must_have_initializer() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let add: fn (a: number, b: number) -> number
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Variable declarations not using `declare` must have an initializer"
                .to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_pattern_matching_is_patterns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string
    let name = match (expr) {
        a is number => a + 1,
        b is string => "bar"
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("name").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | "bar""#);

    assert_no_errors(&checker)
}

#[test]
fn test_pattern_matching_does_not_refine_expr() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string
    let name = match (expr) {
        x is number => expr + 1,
        x is string => "bar"
    }
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: string != number".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_pattern_not_a_subtype_of_expr() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string
    let name = match (expr) {
        x is number => "foo",
        x is string => "bar",
        x is boolean => "baz"
    }
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(boolean, number | string) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_pattern_matching_array() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let array: Array<number>
    let result = match (array) {
        [] => 0,
        [a] => a,
        [a, b] => a + b,
        [_, _, ...rest] => rest
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        // TODO: update unions to merge elements whenever possible
        r#"0 | number | number | number[]"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_pattern_matching_object() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let action: {type: "insert", key: string, value: string} | {type: "delete", key: string}
    let key = match (action) {
        {type: "insert", key, value} => key,
        {type: "delete", key} => key
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("key").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string | string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_pattern_matching_object_event() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    // TODO: add support for omitting fields in object patterns
    let src = r#"
    type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string}
    declare let event: Event
    let result = match (event) {
        {type: "mousedown", x, y} => `mousedown: (${x}, ${y})`,
        {type: "keydown", key} if (key != "Escape") => key
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string | string"#);

    assert_no_errors(&checker)
}

#[test]
fn member_access_on_union() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let obj: {a: number, b: string} | {b: boolean}
    let b = obj.b
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string | boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn member_access_optional_property() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a?: number, b: string}
    let a = obj.a
    let b = obj.b
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);
    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn unifying_object_with_optional_properties() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Obj = {a?: number, b: string}
    let obj: Obj = {b: "hello"}
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn unifying_null_literals() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = "let d: null = null";

    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn member_access_on_unknown_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: unknown
    let a = obj.a
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't access properties on unknown".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn member_access_on_type_variable() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let get_a = fn (x) => x.a
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't access properties on t10".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_object_destructuring_assignment() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let obj: {a?: number, b: string, c: boolean}
    let {a, b, c} = obj
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn test_object_destructuring_assignment_with_rest() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a?: number, b: string, c: boolean}
    let {a, ...rest} = obj
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);
    let binding = my_ctx.values.get("rest").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"{b: string, c: boolean}"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_object_nested_destructuring_assignment() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a: {b: {c: string}}}
    let {a: {b: {c}}} = obj
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_eq!(my_ctx.values.get("a"), None);
    assert_eq!(my_ctx.values.get("b"), None);

    assert_no_errors(&checker)
}

#[test]
fn test_tuple_destrcuturing_assignment() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, string, boolean]
    let [a, b, c] = tuple
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_tuple_destructuring_assignment_with_rest() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, string, boolean]
    let [a, ...tuple_rest] = tuple
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);
    let binding = my_ctx.values.get("tuple_rest").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"[string, boolean]"#);

    assert_no_errors(&checker)
}

#[test]
fn test_array_destructuring_assignment_with_rest() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<string>
    let [a, ...array_rest] = array
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string | undefined"#);
    let binding = my_ctx.values.get("array_rest").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string[]"#);

    assert_no_errors(&checker)
}

#[test]
fn test_tuple_nested_destrcuturing_assignment() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, [string, [boolean]]]
    let [_, [_, [c]]] = tuple
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn test_explicit_type_params() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn (x) => x
    let x = identity<number>(5)
    let y = identity<string>("hello")
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);
    let binding = my_ctx.values.get("y").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_explicit_type_params_type_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn (x) => x
    identity<number>("hello")
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: unify("hello", number) failed
    "###);

    Ok(())
}

#[test]
fn test_explicit_type_params_too_many_type_args() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn (x) => x
    identity<number, string>(5)
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "wrong number of type args".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_type_param_with_constraint() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn <T: number | string>(x: T) -> T => x
    let x = identity(5)
    let y = identity("hello")
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("identity").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<T:number | string>(x: T) -> T"#
    );
    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);
    let binding = my_ctx.values.get("y").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#""hello""#);

    assert_no_errors(&checker)
}

#[test]
fn test_mix_explicit_implicit_type_params() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let fst = fn <B>(a, b: B) => a
    let snd = fn <B>(a, b: B) -> B => b
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<B, A>(a: A, b: B) -> A"#
    );
    let binding = my_ctx.values.get("snd").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<B, A>(a: A, b: B) -> B"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_duplicate_type_param_names_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let fst = fn <T, T>(a: T, b: T) -> T => a
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type param identifiers must be unique".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_type_param_with_violated_constraint() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn <T: number | string>(x: T) -> T => x
    identity(true)
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: unify(true, number | string) failed
    "###);

    Ok(())
}

#[test]
fn test_type_ann_func_with_type_constraint() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity: fn <T: number | string>(x: T) -> T = fn (x) => x
    let x = identity<number>(5)
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("identity").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<T:number | string>(x: T) -> T"#
    );
    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn test_type_ann_func_with_type_constraint_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let id1 = fn <T: number | string>(x: T) -> T => x
    let id2: fn <T: boolean>(x: T) -> T = id1
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(boolean, number | string) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_callback_with_type_param_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (callback: fn <T: number>(x: T) -> T) -> boolean
    let identity = fn <T: number | string>(x: T) -> T => x
    let result = foo(identity)
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn test_callback_with_type_param_subtyping_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (callback: fn <T: number | string>(x: T) -> T) -> boolean
    let identity = fn <T: number>(x: T) -> T => x
    let result = foo(identity)
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: string != number
    "###);

    Ok(())
}

#[test]
fn test_return_type_checking() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn () -> string => "hello"
    let result = foo()
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"() -> string"#);
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_return_value_is_not_subtype_of_return_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn () -> number => "hello"
    "#;
    let mut script = parse_script(src).unwrap();
    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(\"hello\", number) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_multiple_returns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (x) {
        if (x > 5) {
            return true
        }
        return "hello"
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> true | "hello""#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_no_returns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (x) {
        if (x > 5) { }
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> undefined"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_multiple_returns_with_nested_functions() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (x) {
        if (x > 5) {
            return fn () {
                return true
            }
        }
        return "hello"
    }
    "#;
    let mut script = parse_script(src).unwrap();
    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(x: number) -> () -> true | "hello""#
    );

    assert_no_errors(&checker)
}

#[test]
fn type_alias() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("p").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Point"#);

    assert_no_errors(&checker)
}

#[test]
fn type_alias_with_params_with_destructuring() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string> = {value: "hello"}
    let {value} = node
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("node").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Node<string>"#);
    let binding = my_ctx.values.get("value").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn type_alias_with_params_with_member_access() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string> = {value: "hello"}
    let value = node.value
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("node").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Node<string>"#);
    let binding = my_ctx.values.get("value").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn type_alias_with_params_with_computed_member_access() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string> = {value: "hello"}
    let key = "value"
    let value = node[key]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("node").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Node<string>"#);
    let binding = my_ctx.values.get("value").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn instantiate_type_alias_with_too_many_type_args() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string, number> = {value: "hello"}
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Node expects 1 type args, but was passed 2".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn instantiate_type_alias_with_args_when_it_has_no_type_params() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point<number> = {x: 5, y: 10}
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Point expects 0 type args, but was passed 1".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn property_accesses_on_unions() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    declare let object_union: {x: number, y: number} | {x: string}
    let elem = tuple_union[0]
    let x1 = object_union.x
    let key = "x"
    let x2 = object_union[key]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("elem").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | string"#);
    let binding = my_ctx.values.get("x1").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | string"#);
    let binding = my_ctx.values.get("x2").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | string"#);

    assert_no_errors(&checker)
}

#[test]
fn maybe_property_accesses_on_unions() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    declare let object_union: {x: number, y: number} | {x: string}
    let maybe_elem = tuple_union[1]
    let maybe_y = object_union.y
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("maybe_elem").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);
    let binding = my_ctx.values.get("maybe_y").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn optional_chaining() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: string}
    let p: Point | undefined = {x: 5, y: "hello"}
    let q: Point | null = {x: 5, y: "hello"}
    let x = p?.x
    let y = q?.["y"]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);

    let binding = my_ctx.values.get("y").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string | undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn optional_chaining_with_alias() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type OptPoint = {x: number, y: string} | undefined
    let p: OptPoint = {x: 5, y: "hello"}
    let x = p?.x
    let y = p?.["y"]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);

    let binding = my_ctx.values.get("y").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string | undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn optional_chaining_unnecessary_chaining() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Obj = {msg: string}
    let obj: Obj = {msg: "hello"}
    let msg = obj?.msg
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("msg").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn optional_chaining_multiple_levels() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Obj = {a?: {b?: {c?: number}}}
    let obj: Obj = {a: {b: {c: 5}}}
    let c = obj?.a?.b?.c
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn calling_variable_whose_type_is_aliased_function_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Fn = fn () -> number
    declare let foo: Fn
    let result = foo()
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn optional_chaining_call() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Fn = fn () -> number
    declare let foo: Fn | undefined | null
    let result = foo?.()
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number | undefined"#);

    assert_no_errors(&checker)
}

#[test]
#[ignore]
fn destructuring_unions() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    declare let object_union: {x: number, y: number} | {x: string}
    let [fst, snd] = tuple_union
    let {x, y} = object_union
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    // TODO: write assertions for this test once the desired
    // behavior has been implemented.

    assert_no_errors(&checker)
}

#[test]
fn missing_property_accesses_on_union_of_tuples() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    let elem = tuple_union[2]
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Couldn't find property on object".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn missing_property_accesses_on_union_of_objects() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let object_union: {x: number, y: number} | {x: string}
    let z = object_union.z
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Couldn't find property \"z\" on object".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn methods_on_arrays() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let num_array: Array<number> = []
    num_array.push(5)
    let str_array: Array<string> = []
    str_array.push("hello")

    let len = str_array.length
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;
    let binding = my_ctx.values.get("len").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn properties_on_tuple() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let tuple: [number, string] = [5, "hello"]
    let len = tuple.length
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("len").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn set_array_element() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let mut array: Array<number>
    array[0] = 5
    array[1] = 10
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
#[ignore]
fn set_tuple_element() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, number]
    tuple[0] = 5
    tuple[1] = 10
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("len").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn methods_on_arrays_incorrect_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let num_array: Array<number> = []
    num_array.push("hello")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    insta::assert_display_snapshot!(checker.current_report, @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: unify("hello", number) failed
    "###);

    Ok(())
}

#[test]
fn test_unknown() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let a: unknown = 5
    let b: unknown = "hello"
    let c: unknown = true
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"unknown"#);
    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"unknown"#);
    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"unknown"#);

    assert_no_errors(&checker)
}

#[test]
fn test_unknown_assignment_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let a: unknown = 5
    let b: number = a
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(unknown, number) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_type_param_explicit_unknown_constraint() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let add = fn <T: unknown>(a: T, b: T) -> T {
        return a + b
    }
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(unknown, number) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_type_param_implicit_unknown_constraint() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let add = fn <T>(a: T, b: T) -> T {
        return a + b
    }
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(unknown, number) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_optional_function_params() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (a: number, b?: number) -> number {
        return a
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number, b?: number) -> number"#
    );

    assert_no_errors(&checker)
}

#[test]
fn passing_undefined_to_an_optional_param() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (a: number, b?: number) -> number
    foo(5, undefined)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn passing_undefined_to_an_optional_param_with_rest_param() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (a: number, b?: number, ...c: number[]) -> number
    foo(5, undefined)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn args_for_optional_params_can_be_omitted() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (a: number, b?: number) -> number
    foo(5)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn args_for_optional_params_can_be_omitted_with_rest_param() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (a: number, b?: number, ...c: number[]) -> number
    foo(5)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_func_param_patterns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn ({ a: x, b }: { a: number, b: string }) {
        return x
    }
    let bar = fn ([a, b]: [number, string]) {
        return b
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"({a, b}: {a: number, b: string}) -> number"#
    );
    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"([a, b]: [number, string]) -> string"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_func_param_object_rest_patterns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn ({ a, ...rest }: { a: number, b: string }) {
        return rest.b
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"({a, ...rest}: {a: number, b: string}) -> string"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_func_param_object_multiple_rest_patterns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn ({ a, ...rest1, ...rest2 }: { a: number, b: string }) {
        return rest.b
    }
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Maximum one rest pattern allowed in object patterns".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_func_param_tuple_rest_patterns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let bar = fn ([a, ...rest]: [number, string, boolean]) {
        return rest[1]
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"([a, ...rest]: [number, string, boolean]) -> boolean"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b?: number, [P]: boolean for P in string}
    type A = Foo["a"]
    type B = Foo["b"]
    type C = Foo["c"]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    assert_eq!(checker.print_type(&scheme.t), r#"Foo["a"]"#);
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"string"#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"number | undefined"#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"boolean | undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_using_string_as_mapped() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b: number, c: boolean}
    type T = Foo[string]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(
        checker.print_type(&t),
        r#"string | number | boolean | undefined"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_using_number_as_mapped() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b: number, [P]: boolean for P in number}
    type T = Foo[number]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"boolean | undefined"#);

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_missing_property() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b?: number}
    type C = Foo["c"]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("C").unwrap();
    let result = checker.expand_type(&my_ctx, scheme.t);

    // TODO: check that the index access is valid where it's inferred
    assert_eq!(
        result,
        Err(TypeError {
            message: "Couldn't find property 'c' on object".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_missing_mapped() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {[P]: string for P in number}
    type C = Foo["c"]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("C").unwrap();
    let result = checker.expand_type(&my_ctx, scheme.t);

    // TODO: check that the index access is valid where it's inferred
    assert_eq!(
        result,
        Err(TypeError {
            message: "Couldn't find property c in object".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_number_mapped() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {[P]: string for P in number}
    type T = Foo[1]
    let t: T = "hello"
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"string | undefined"#);

    let binding = my_ctx.values.get("t").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"T"#);

    assert_no_errors(&checker)
}

#[test]
fn test_mapped_type_pick() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Pick<T, K : keyof T> = {[P]: T[P] for P in K}
    type Obj = {a?: string, b: number, c: boolean}
    type Result = Pick<Obj, "a" | "b">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"{a?: string, b: number}"#);

    assert_no_errors(&checker)
}

#[test]
fn test_pick_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Pick<T, K> = {[P]: T[P] for P in K}
    type Obj = {a: string, b: number, c: boolean}
    type Result = Pick<Obj, "a" | "b">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"{a: string, b: number}"#);

    assert_no_errors(&checker)
}

#[test]
fn test_exclude_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Exclude<T, U> = if (T : U) { never } else { T }
    type Obj = {a: string, b: number, c: boolean}
    type Result = Exclude<keyof Obj, "c">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""a" | "b""#);

    assert_no_errors(&checker)
}

#[test]
fn test_omit_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Pick<T, K> = {[P]: T[P] for P in K}
    type Exclude<T, U> = if (T : U) { never } else { T }
    type Omit<T, K> = Pick<T, Exclude<keyof T, K>>
    type Obj = {a: string, b: number, c: boolean}
    type Result = Omit<Obj, "c">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"{a: string, b: number}"#);

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_on_tuple() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[1]
    let t: T = "hello"
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("t").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"T"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(checker.print_type(&t), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_type_on_tuple_with_number_key() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[number]
    let t: T = "hello"
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("t").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"T"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"number | string | boolean | undefined"#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_out_of_bounds_on_tuple() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[3]
    let t: T = "hello"
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "3 was outside the bounds 0..3 of the tuple".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_index_access_not_usize() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[1.5]
    let t: T = "hello"
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "1.5 isn't a valid index".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_typeof() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    let foo = {a: "hello", b: 5, c: true}
    type Foo = typeof foo
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"{a: "hello", b: 5, c: true}"#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_obj() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    let a = {x: "hello", y: 5, z: true}
    type A = keyof typeof a
    let b = {x: "hello"}
    type B = keyof typeof b
    let c = {}
    type C = keyof typeof c
    type D = keyof {[P]: number for P in string, x: number}
    type E = keyof {[P]: boolean for P in number, x: boolean}
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""x" | "y" | "z""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""x""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"never"#);

    let scheme = my_ctx.schemes.get("D").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"string"#);

    let scheme = my_ctx.schemes.get("E").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"number | "x""#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_array_tuple() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = keyof Array<number>
    type Bar = keyof ["hello", 5, true]
    type Baz = keyof ["hello"]
    type Qux = keyof []
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"number"#);

    let scheme = my_ctx.schemes.get("Bar").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"0 | 1 | 2"#);

    let scheme = my_ctx.schemes.get("Baz").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"0"#);

    let scheme = my_ctx.schemes.get("Qux").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"never"#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_alias() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    type Foo = keyof Point
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""x" | "y""#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_literal() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type String = {
        length: number,
        slice: fn (start: number, end: number) -> string,
    }
    type Number = {
        toFixed: fn (precision: number) -> string,
        toString: fn () -> string,
    }
    type Boolean = {
        valueOf: fn () -> boolean,
    }
    type A = keyof "hello"
    type B = keyof 5
    type C = keyof true
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""length" | "slice""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""toFixed" | "toString""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""valueOf""#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_primitive() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type String = {
        length: number,
        slice: fn (start: number, end: number) -> string,
    }
    type Number = {
        toFixed: fn (precision: number) -> string,
        toString: fn () -> string,
    }
    type Boolean = {
        valueOf: fn () -> boolean,
    }
    type A = keyof string
    type B = keyof number
    type C = keyof boolean
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""length" | "slice""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""toFixed" | "toString""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""valueOf""#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_unknown_undefined_null() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type A = keyof unknown
    type B = keyof undefined
    type C = keyof null
    type D = keyof never
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"never"#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"never"#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"never"#);

    let scheme = my_ctx.schemes.get("D").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"string | number | symbol"#);

    assert_no_errors(&checker)
}

#[test]
fn test_keyof_intersection() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type A = keyof ({a: number} & {b: string})
    type B = keyof ({a: number, b: number} & {b: string, c: boolean})
    type C = keyof ({a: number} & undefined)
    type D = keyof ({a: number} & {[P]: number for P in string})
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""a" | "b""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""a" | "b" | "c""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#""a""#);

    let scheme = my_ctx.schemes.get("D").unwrap();
    let t = checker.expand_type(&my_ctx, scheme.t)?;
    assert_eq!(checker.print_type(&t), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn test_mutually_recursive_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();
    let src = r#"
    type A = {
        a: number,
        b: B | null,
    }
    
    type B = {
        a: A | null,
        b: string,
    }
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_mutually_recursive_type_with_index_access_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();
    let src = r#"
    type Foo = {
        a: number,
        b: Bar["b"],
    }
    
    type Bar = {
        a: Foo["a"],
        b: string,
    }    

    let foo: Foo = {a: 5, b: "hello"}
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_type_alias_with_undefined_def() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();
    let src = r#"
    type A = B
    "#;

    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "B is not in scope".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_error_arg_passing() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    type Point = {x: number, y: number}
    let scale = fn (mut p: Point, factor: number) {
        return p
    }
    let p: Point = {x: 5, y: 10}
    scale(p, 2)
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't assign immutable value to mutable binding".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
#[ignore]
fn test_infer_array_element_type_from_assignment() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    let numbers: Array<_> = [1, 2, 3]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let numbers = my_ctx.values.get("numbers").unwrap();
    assert_eq!(checker.print_type(&numbers.index), r#"Array<1 | 2 | 3>"#);

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_error_arg_passing_with_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    declare let foo: fn (mut items: Array<number | string>) -> undefined
    let mut numbers: Array<number> = [1, 2, 3]
    foo(numbers)
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "unify_mut: number[] != number | string[]".to_string(),
        }),
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_ok_arg_passing() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    type Point = {x: number, y: number}
    let mut_scale = fn (mut p: Point, factor: number) {
        return p
    }
    let scale = fn (p: Point, factor: number) {
        return p
    }

    let main = fn () {
        let mut p: Point = {x: 5, y: 10}
        mut_scale(p, 2)
        mut_scale({x: 5, y: 10}, 2)

        let p: Point = {x: 5, y: 10}
        let mut q: Point = {x: 5, y: 10}
        scale(p, 2)
        scale(q, 2)
        scale({x: 5, y: 10}, 2)

        let mut p = scale(p, 2)
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_error_arg_passing_declared_fn() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    declare let scale: fn (mut p: Point, factor: number) -> Point
    let p: Point = {x: 5, y: 10}
    scale(p, 2)
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't assign immutable value to mutable binding".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_ok_arg_passing_declared_fns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    type Point = {x: number, y: number}
    declare let mut_scale: fn (mut p: Point, factor: number) -> Point
    declare let scale: fn (p: Point, factor: number) -> Point

    let main = fn () {
        let mut p: Point = {x: 5, y: 10}
        mut_scale(p, 2)
        mut_scale({x: 5, y: 10}, 2)

        let p: Point = {x: 5, y: 10}
        let mut q: Point = {x: 5, y: 10}
        scale(p, 2)
        scale(q, 2)
        scale({x: 5, y: 10}, 2)

        let mut p = scale(p, 2)
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_error_assignment() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    let mut q = p
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Can't assign immutable value to mutable binding".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_ok_assignments() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    
    let main = fn () {
        let mut p: Point = {x: 5, y: 10}
        let mut q = p
    
        let p: Point = {x: 5, y: 10}
        let q = p

        let mut p: Point = {x: 5, y: 10}
        let q = p
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_invalid_assignments() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"    
    let mut arr1: Array<number> = [1, 2, 3]
    let mut arr2: Array<number | string>  = arr1
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "unify_mut: number[] != number | string[]".to_string(),
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn test_sub_objects_are_mutable() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Obj = {a: {b: {c: string}}}
    declare let obj1: Obj
    declare let mut obj2: Obj
    let b1 = obj1.a.b
    let b2 = obj2.a.b
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("b1").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"{c: string}"#);

    let binding = my_ctx.values.get("b2").unwrap();
    // TODO: create helper function to print bindings, not just types
    assert_eq!(checker.print_type(&binding.index), r#"{c: string}"#);

    assert_no_errors(&checker)
}

#[test]
fn test_tuple_type_equality() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let a: [number, string]
    declare let b: [number, string]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let a = my_ctx.values.get("a").unwrap();
    let b = my_ctx.values.get("b").unwrap();

    assert!(checker.equals(&a.index, &b.index));

    assert_no_errors(&checker)
}

#[test]
fn test_function_type_equality() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let add: fn(a: number, b: number) -> number
    declare let sub: fn(a: number, b: number) -> number
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let add = my_ctx.values.get("add").unwrap();
    let sub = my_ctx.values.get("sub").unwrap();

    assert!(checker.equals(&add.index, &sub.index));

    assert_no_errors(&checker)
}

#[test]
fn test_literal_type_equality() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let a = 5
    let b = 5
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let a = my_ctx.values.get("a").unwrap();
    let b = my_ctx.values.get("b").unwrap();

    assert!(checker.equals(&a.index, &b.index));

    assert_no_errors(&checker)
}

#[test]
fn test_mutable_object_type_equality() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let mut p: Point = {x: 5, y: 10}
    let mut q: Point = {x: 0, y: 1}
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let p = my_ctx.values.get("p").unwrap();
    let q = my_ctx.values.get("q").unwrap();

    assert!(checker.equals(&p.index, &q.index));

    assert_no_errors(&checker)
}

#[test]
fn test_mutating_mutable_object() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let mut p: Point = {x: 5, y: 10}
    p.x = 0
    p["y"] = 0
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn test_mutating_immutable_object_errors() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    p.x = 0
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "Cannot assign to immutable lvalue".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn conditional_type_exclude() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Exclude<T, U> = if (T: U) { never } else { T }
    type Result = Exclude<"a" | "b" | "c" | "d" | "e", "a" | "e">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""b" | "c" | "d""#);

    assert_no_errors(&checker)
}

#[test]
fn chained_conditional_types() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Foo<T> = if (T: string) { 
        "str"
    } else if (T: number) {
        "num"
    } else {
        "?"
    }
    type Result = Foo<5 | "hello">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""num" | "str""#);

    assert_no_errors(&checker)
}

#[test]
fn match_type_with_catchall() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Foo<T> = match (T) {
        string => "str",
        number => "num",
        _ => "other",
    }
    type Result = Foo<5 | "hello">
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""num" | "str""#);

    assert_no_errors(&checker)
}

#[test]
fn match_type_without_catchall() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Foo<T> = match (T) {
        string => "str",
        number => "num",
    }
    type True = Foo<5 | true>
    type Never = Foo<true>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("True").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""num""#);

    let result = my_ctx.schemes.get("Never").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"never"#);

    assert_no_errors(&checker)
}

#[test]
fn match_type_with_tuples() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // NOTE: The arrays need to be matched in reverse order
    // because they're open.  This means that the first match
    // all arrays size 4 and greater.
    let src = r#"
    type Bar<T: Array<_>> = match (T) {
        [_, _, _, _] => "n-tuple",
        [_, _, _] => "triplet",
        [_, _] => "pair",
        [_] => "1-tuple",
        [] => "unit",
    }
    type Tuple0 = Bar<[]>
    type Tuple1 = Bar<[5]>
    type Tuple2 = Bar<[5, "hello"]>
    type Tuple3 = Bar<[5, "hello", true]>
    type Tuple4 = Bar<[5, "hello", true, 10]>
    type Tuple5 = Bar<[5, "hello", true, 10, "world"]>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Tuple0").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""unit""#);

    let result = my_ctx.schemes.get("Tuple1").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""1-tuple""#);

    let result = my_ctx.schemes.get("Tuple2").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""pair""#);

    let result = my_ctx.schemes.get("Tuple3").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""triplet""#);

    let result = my_ctx.schemes.get("Tuple4").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""n-tuple""#);

    let result = my_ctx.schemes.get("Tuple5").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#""n-tuple""#);

    assert_no_errors(&checker)
}

#[test]
fn conditional_type_with_placeholders() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        type IsArray<T> = if (T: Array<_>) { true } else { false }
        type T = IsArray<Array<number>>
        type F = IsArray<number>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("T").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"true"#);

    let result = my_ctx.schemes.get("F").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"false"#);

    assert_no_errors(&checker)
}

#[test]
fn conditional_type_with_constraint() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        type IsArrayOfNumbers<T> = if (T: Array<number>) { true } else { false }
        type T = IsArrayOfNumbers<[1, 2, 3]>
        type F = IsArrayOfNumbers<["hello", "world"]>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("T").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"true"#);

    let result = my_ctx.schemes.get("F").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"false"#);

    assert_no_errors(&checker)
}

#[test]
fn unify_tuple_and_array() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        let tuple = [1, 2, 3]
        let array: Array<number> = tuple
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn conditional_type_with_function_subtyping() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        type IsFunction<T> = if (T: fn (...args: _) -> _) { true } else { false }
        type T = IsFunction<fn (a: number) -> string>
        type F = IsFunction<number>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("T").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"true"#);

    let result = my_ctx.schemes.get("F").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"false"#);

    assert_no_errors(&checker)
}

#[test]
fn return_type_rest_placeholder() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: introduce a placeholder type that will unify with anything
    // we need to make sure we aren't adding `_` to non_generic
    let src = r#"
        type ReturnType<
            T: fn (...args: _) -> _
        > = if (T: fn (...args: _) -> infer R) {
            R
        } else {
            never
        }
        type RT1 = ReturnType<fn (a: string, b: number) -> boolean>
        type RT2 = ReturnType<fn (a: string) -> number>
        type RT3 = ReturnType<fn () -> string>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("RT1").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"boolean"#);

    let result = my_ctx.schemes.get("RT2").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"number"#);

    let result = my_ctx.schemes.get("RT3").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"string"#);

    assert_no_errors(&checker)
}

#[test]
fn return_type_of_union() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
        type ReturnType<
            T: fn (...args: _) -> _
        > = if (T: fn (...args: _) -> infer R) {
            R
        } else {
            never
        }
        type Result = ReturnType<(fn () -> number) | (fn () -> string)>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"number | string"#);

    assert_no_errors(&checker)
}

#[test]
fn parameters_utility_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Parameters<T : fn (...args: _) -> _> = if (
        T: fn (...args: infer P) -> _
    ) { 
        P
    } else { 
        never
    }
    type P1 = Parameters<fn (a: string, b: number) -> boolean>
    type P2 = Parameters<fn (a: string, ...rest: Array<number>) -> boolean>
    type P3 = Parameters<fn (a: string, ...rest: [number, boolean]) -> boolean>
    type P4 = Parameters<fn (a: string, ...rest: [number, boolean, ...string[]]) -> boolean>
    "#;

    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("P1").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"[string, number]"#);

    let result = my_ctx.schemes.get("P2").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"[string, ...number[]]"#);

    let result = my_ctx.schemes.get("P3").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"[string, number, boolean]"#);

    let result = my_ctx.schemes.get("P4").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(
        checker.print_type(&t),
        r#"[string, number, boolean, ...string[]]"#
    );

    assert_no_errors(&checker)
}

#[test]
fn function_subtyping_with_rest_placeholder() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let none: fn (...args: _) -> boolean = fn () => true
    let one: fn (...args: _) -> boolean = fn (a: string) => true
    let many: fn (...args: _) -> boolean = fn (a: string, b: number) => true
    let one_req: fn (a: string, ...args: _) -> boolean = fn (a: string, b: number) => true
    let array: fn (...args: Array<_>) -> boolean = fn (...args: Array<number>) => true
    let tuple1: fn (...args: [_, _]) -> boolean = fn (...args: [number, string]) => true
    let tuple2: fn (...args: [_, _]) -> boolean = fn (a: string, b: number) => true
    let tuple3: fn (...args: [string, number]) -> boolean = fn (a: string, b: number) => true
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn function_subtyping_with_rest_array_fails() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let result: fn (...args: Array<_>) -> boolean = fn (a: string, b: number) => true
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(undefined, string) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn function_multiple_rest_params_in_type_fails() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let result: fn (...args: _, ...moar_args: _) -> boolean = fn (a: string, b: number) => true
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "multiple rest params in function".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn function_multiple_rest_params_function_fails() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let result: fn (...args: _) -> boolean = fn (...args: _, ...moar_args: _) => true
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "multiple rest params in function".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn arithmetic_op_const_folding() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let a = 3
    let b = 2
    let sum = a + b
    let diff = a - b
    let prod = a * b
    let quot = a / b
    let rem = a % b
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);

    let binding = my_ctx.values.get("diff").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"1"#);

    let binding = my_ctx.values.get("prod").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"6"#);

    let binding = my_ctx.values.get("quot").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"1.5"#);

    let binding = my_ctx.values.get("rem").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"1"#);

    assert_no_errors(&checker)
}

#[test]
fn comparison_op_const_folding() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let a = 3
    let b = 2
    let gt = a > b
    let gte = a >= b
    let lt = a < b
    let lte = a <= b
    let eq = a == b
    let neq = a != b
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("gt").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"true"#);

    let binding = my_ctx.values.get("gte").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"true"#);

    let binding = my_ctx.values.get("lt").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"false"#);

    let binding = my_ctx.values.get("lte").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"false"#);

    let binding = my_ctx.values.get("eq").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"false"#);

    let binding = my_ctx.values.get("neq").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"true"#);

    assert_no_errors(&checker)
}

#[test]
fn other_equality_checks() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let a = "foo" == "bar"
    let b = "foo" != "bar"
    let c = "hello" == 5
    let d = "hello" != 5
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"false"#);

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"true"#);

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    let binding = my_ctx.values.get("d").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"boolean"#);

    assert_no_errors(&checker)
}

#[test]
fn type_level_arithmetic() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type A = 10 + 5
    type B = 10 - 5
    type C = 10 * 5
    type D = 10 / 5
    type E = 10 % 5
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("A").unwrap();
    assert_eq!(checker.print_type(&result.t), r#"10 + 5"#);
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"15"#);

    let result = my_ctx.schemes.get("B").unwrap();
    assert_eq!(checker.print_type(&result.t), r#"10 - 5"#);
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"5"#);

    let result = my_ctx.schemes.get("C").unwrap();
    assert_eq!(checker.print_type(&result.t), r#"10 * 5"#);
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"50"#);

    let result = my_ctx.schemes.get("D").unwrap();
    assert_eq!(checker.print_type(&result.t), r#"10 / 5"#);
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"2"#);

    let result = my_ctx.schemes.get("E").unwrap();
    assert_eq!(checker.print_type(&result.t), r#"10 % 5"#);
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"0"#);

    assert_no_errors(&checker)
}

#[test]
fn type_level_arithmetic_incorrect_operands() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Sum = "hello" + true
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(\"hello\", number) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn check_type_constraints() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Add<A: number, B: number> = A + B
    type A = Add<"hello", "world">
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: unify(\"hello\", number) failed".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn type_level_arithmetic_with_alias() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Add<A: number, B: number> = A + B
    type A = Add<5, 10>
    type B = Add<5, number>
    type C = Add<number, 10>
    type D = Add<number, number>
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let result = my_ctx.schemes.get("A").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"15"#);

    let result = my_ctx.schemes.get("B").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"number"#);

    let result = my_ctx.schemes.get("C").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"number"#);

    let result = my_ctx.schemes.get("D").unwrap();
    let t = checker.expand_type(&my_ctx, result.t)?;
    assert_eq!(checker.print_type(&t), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn type_level_arithmetic_with_incorrect_types() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Eagerly check arithmetic in type aliases
    let src = r#"
    type Sum = string + number
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: string != number".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn type_args_are_eagerly_checked() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Foo<A: number> = A
    type Bar = Foo<string>
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "type mismatch: string != number".to_string()
        })
    );

    assert_no_errors(&checker)
}

#[test]
fn for_in_loop() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let mut sum: number = 0
    for (num in [1, 2, 3]) {
        sum = sum + num
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn for_in_loop_with_patterns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    declare let points: Array<Point>
    let mut centroid: Point = {x: 0, y: 0}
    for ({x, y} in points) {
        centroid.x += x
        centroid.y += y
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn function_call_func_wth_rest_arg_array() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (a: Array<number>, ...rest: Array<string>) => true
    foo([5, 10], "hello", "world")
    foo([5, 10], "hello")
    foo([5, 10])
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn function_call_func_wth_rest_arg_tuple() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (a: Array<number>, ...rest: [string, boolean]) => true
    foo([5, 10], "hello", true)
    foo([5, 10], "hello", true, "world")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn function_call_func_wth_rest_arg_tuple_not_enough_args() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (a: Array<number>, ...rest: [string, boolean]) => true
    foo([5, 10], "hello")
    "#;
    let mut script = parse_script(src).unwrap();

    let result = checker.infer_script(&mut script, &mut my_ctx);

    assert_eq!(
        result,
        Err(TypeError {
            message: "too few arguments to function: expected 3, got 2".to_string()
        })
    );

    Ok(())
}

// TODO(#676): handle array/tuple spread in function call
#[test]
#[ignore]
fn function_call_with_spread_args() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (a: number, b: string) => true
    let args = [5, "hello"]
    let result = foo(...args)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

#[test]
fn tagged_template_literal() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn(strings: Array<string>, ...args: Array<number>) -> number
    let result = foo`hello ${1} world ${5}`
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"number"#);

    assert_no_errors(&checker)
}

#[test]
fn tagged_template_literal_with_throw() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn(strings: Array<string>, ...args: Array<number>) -> number throws "RangeError"
    let bar = fn () => foo`hello ${1} world ${5}`
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"() -> number throws "RangeError""#
    );

    assert_no_errors(&checker)
}

#[test]
fn test_generalization_inside_function() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    // this is foo
    let foo = fn () {
        // TODO: generalize `bar` before returning it
        let bar = fn (x) => x // `bar` is the identity function
        return [bar(5), bar("hello"), bar] // we include `bar` in the return value
    }
    let bar = foo()
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    // TODO: do let-generalization within functions, not just
    // at the top level.
    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A>() -> [5, "hello", (x: A) -> A]"#
    );
    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"[5, "hello", (x: t50) -> t50]"#
    );

    assert_no_errors(&checker)
}

#[test]
fn higher_rank_type_1() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let f = fn (g: fn<A>(x: A) -> A) => [g(5), g("hello")]
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("f").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(g: <A>(x: A) -> A) -> [5, "hello"]"#
    );

    assert_no_errors(&checker)
}

#[test]
fn higher_rank_type_2() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn <T>(item: T) -> T => item     // generic
    let plusOne = fn (x: number) -> number => x + 1 // concrete

    type Id<T> = fn (item: T) -> T
    let x: Id<number> = identity   // fine
    let y: Id<number> = plusOne    // fine

    type IdHigherRank = fn <T>(item: T) -> T
    let z: IdHigherRank = identity // fine
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

// TODO: this should error but doesn't
#[test]
#[ignore]
fn higher_rank_type_expected_error() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let plusOne = fn (x: number) -> number => x + 1 // concrete
    type IdHigherRank = fn <T>(item: T) -> T
    let zz: IdHigherRank = plusOne // error
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    assert_no_errors(&checker)
}

// TODO: If a type param is used for multiple params or if it's
// used in a spread param, create multiple type variables and unify
// the params independently.  The result of each can be union-ed
// together to determine the actual value of `T`.
// #[test]
// fn unify_multiple_args_with_same_type_param() -> Result<(), TypeError> {
//     let (mut checker, mut my_ctx) = test_env();

//     let src = r#"
//     declare let foo: fn<T>(a: T, b: T, c: T) -> T
//     foo(1, 2, 3)
//     "#;
//     let mut script = parse_script(src).unwrap();

//     checker.infer_script(&mut script, &mut my_ctx)?;

//     assert_no_errors(&checker)
// }

#[test]
fn infer_simple_class() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let Point = class {
        x: number
        y: number
        fn constructor(mut self, x: number, y: number) {
            self.x = x
            self.y = y
        }
        fn add(mut self, other: Self) -> Self {
            self.x += other.x
            self.y += other.y
            return self
        }
    }
    let mut p = new Point(5, 10)
    let q = new Point(1, 0)
    let {x, y} = p
    let r = p.add(q)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("Point").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"{new fn(x: number, y: number) -> Self}"#
    );

    let binding = my_ctx.values.get("p").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{x: number, y: number, add(mut self, other: Self) -> Self}"#
    );
    let binding = my_ctx.values.get("Point").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"{new fn(x: number, y: number) -> Self}"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_simple_class_and_param_types() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let Point = class {
        x: number
        y: number
        fn constructor(mut self, x, y) {
            self.x = x
            self.y = y
        }
        fn add(mut self, other: Self) {
            self.x += other.x
            self.y += other.y
            return self
        }
    }
    let mut p = new Point(5, 10)
    let q = new Point(1, 0)
    let {x, y} = p
    let r = p.add(q)
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("Point").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"{new fn(x: number, y: number) -> Self}"#
    );

    let binding = my_ctx.values.get("p").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{x: number, y: number, add(mut self, other: Self) -> Self}"#
    );
    let binding = my_ctx.values.get("Point").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"{new fn(x: number, y: number) -> Self}"#
    );

    assert_no_errors(&checker)
}

// TODO: class without an explicit constructor

#[test]
fn infer_class_with_generic_method() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let Foo = class {
        x: number
        fn constructor(mut self) {
            self.x = 0
        }
        fn fst(self, a, b) {
            return a
        }
        fn inc(mut self) {
            self.x += 1
            return self
        }
    }
    let mut foo = new Foo()
    let bar = foo.inc()
    let x = foo.inc().fst(5, "hello")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{x: number, fst<B, A>(self, a: A, b: B) -> A, inc(mut self) -> Self}"#
    );

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{x: number, fst<B, A>(self, a: A, b: B) -> A, inc(mut self) -> Self}"#
    );

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);

    assert_no_errors(&checker)
}

#[test]
fn infer_class_with_generic_method_with_type_anns() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let Foo = class {
        fn constructor(mut self) {}
        fn fst<T, U>(self, a: T, b: U) -> T {
            return a
        }
    }
    let foo = new Foo()
    let x = foo.fst(5, "hello")
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{fst<T, U>(self, a: T, b: U) -> T}"#
    );

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"5"#);

    assert_no_errors(&checker)
}

#[test]
#[ignore]
fn infer_class_async_and_throw() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let Foo = class {
        fn constructor(mut self) {}
        fn bar(self, a: number) -> number throws "RangeError" {
            if (a < 0) {
                throw "RangeError"
            }
            return a
        }
        fn baz(self, a) {
            return self.bar(a)
        }
        fn qux(self, a) {
            return self.baz(a)
        }
    }
    let foo = new Foo()
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{bar(self, a: number) -> number throws "RangeError", baz(self, a: number) -> number throws "RangeError"}"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_many_methods() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let Foo = class {
        fn constructor(mut self) {}
        fn bar(self, a: number) {
            return a
        }
        fn baz(self, a) {
            return self.bar(a)
        }
        fn qux(self, a) {
            return self.baz(a)
        }
    }
    let foo = new Foo()
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Self"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(
        checker.print_type(&t),
        r#"{bar(self, a: number) -> number, baz(self, a: number) -> number, qux(self, a: number) -> number}"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_many_functions() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let bar = fn(a: number) {
        return a
    }
    let baz = fn(a) {
        return bar(a)
    }
    let qux = fn(a) {
        return baz(a)
    }
    "#;
    let mut module = parse_module(src).unwrap();

    checker.infer_module(&mut module, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number"#
    );
    let binding = my_ctx.values.get("baz").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number"#
    );
    let binding = my_ctx.values.get("qux").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_many_functions_with_throw() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let bar = fn(a: number) throws "RangeError" {
        if (a < 0) {
            throw "RangeError"
        }
        return a
    }
    let qux = fn(a) {
        return baz(a)
    }
    let baz = fn(a) {
        return bar(a)
    }
    "#;
    let mut module = parse_module(src).unwrap();

    checker.infer_module(&mut module, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number throws "RangeError""#
    );
    let binding = my_ctx.values.get("baz").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number throws "RangeError""#
    );
    let binding = my_ctx.values.get("qux").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number throws "RangeError""#
    );

    assert_no_errors(&checker)
}

#[test]
#[ignore]
fn infer_many_functions_on_object() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    let src = r#"
    let foo = {
        bar: fn(a: number) {
            return a
        },
        baz: fn(a) {
            return foo.bar(a)
        },
        qux: fn(a) {
            return foo.baz(a)
        }
    }
    "#;
    let mut module = parse_module(src).unwrap();

    checker.infer_module(&mut module, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number"#
    );

    assert_no_errors(&checker)
}

#[test]
fn typecheck_simple_throws() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let bar = fn(a: number) -> number throws "RangeError" {
        if (a < 0) {
            throw "RangeError"
        }
        return a
    }
    let baz = fn(a) {
        return bar(a)
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number throws "RangeError""#
    );
    let binding = my_ctx.values.get("baz").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"(a: number) -> number throws "RangeError""#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_generic_func_with_type_ann() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let fst = fn<T, U>(a: T, b: U) -> T {
        return a
    }
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<T, U>(a: T, b: U) -> T"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_generic_that_call_each_other_in_script() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let fst2 = fn (a, b) => fst1(a, b)
    let fst1 = fn (a, b) => a
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst1").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B>(a: A, b: B) -> A"#
    );
    let binding = my_ctx.values.get("fst2").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B, C>(a: A, b: B) -> C"#
    );

    assert_no_errors(&checker)
}

#[test]
fn infer_generic_that_call_each_other_in_module() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let fst2 = fn (a, b) => fst1(a, b)
    let fst1 = fn (a, b) => a
    "#;
    let mut module = parse_module(src).unwrap();

    checker.infer_module(&mut module, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst1").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B>(a: A, b: B) -> A"#
    );
    let binding = my_ctx.values.get("fst2").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        r#"<A, B>(a: A, b: B) -> A"#
    );

    assert_no_errors(&checker)
}

#[test]
fn use_value_with_private_type() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let make_point = fn () {
        type Point = {x: number, y: number}
        let p: Point = {x: 5, y: 10}
        return p
    }
    let p = make_point()
    let {x, y} = p
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("p").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Point"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(checker.print_type(&t), r#"{x: number, y: number}"#);

    assert_no_errors(&checker)
}

#[test]
fn use_value_with_private_type_on_obj() -> Result<(), TypeError> {
    let (mut checker, mut my_ctx) = test_env();

    // TODO: Allow comments in class bodies
    let src = r#"
    let make_point = fn () {
        type Point = {x: number, y: number}
        let obj: {p: Point} = {p: {x: 5, y: 10}}
        return obj
    }
    let {p} = make_point()
    let {x, y} = p
    "#;
    let mut script = parse_script(src).unwrap();

    checker.infer_script(&mut script, &mut my_ctx)?;

    let binding = my_ctx.values.get("p").unwrap();
    assert_eq!(checker.print_type(&binding.index), r#"Point"#);
    let t = checker.expand_type(&my_ctx, binding.index)?;
    assert_eq!(checker.print_type(&t), r#"{x: number, y: number}"#);

    assert_no_errors(&checker)
}
