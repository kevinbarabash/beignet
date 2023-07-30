use generational_arena::{Arena, Index};

use escalier_ast::{self as syntax, Literal as Lit, *};
use escalier_parser::parse;

use escalier_hm::context::*;
use escalier_hm::errors::*;
use escalier_hm::infer::*;
use escalier_hm::types::{self, *};
use escalier_hm::util::expand_type;

fn new_num_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
    arena.insert(Type::from(TypeKind::Literal(Lit::Number(value.to_owned()))))
}

fn new_str_lit_type(arena: &mut Arena<Type>, value: &str) -> Index {
    arena.insert(Type::from(TypeKind::Literal(Lit::String(value.to_owned()))))
}

fn test_env() -> (Arena<Type>, Context) {
    let mut arena = Arena::new();
    let mut context = Context::default();

    let number = new_primitive(&mut arena, Primitive::Number);
    let type_param_t = new_constructor(&mut arena, "T", &[]);

    let array_interface = new_object_type(
        &mut arena,
        &[
            // .push(item: T): number;
            types::TObjElem::Method(types::TMethod {
                name: types::TPropKey::StringKey("push".to_string()),
                params: vec![types::FuncParam {
                    pattern: TPat::Ident(BindingIdent {
                        name: "item".to_string(),
                        mutable: false,
                        span: Span { start: 0, end: 0 },
                    }),
                    t: type_param_t,
                    optional: false,
                }],
                ret: number,
                type_params: None,
                is_mutating: true,
            }),
            // .length: number;
            types::TObjElem::Prop(types::TProp {
                name: types::TPropKey::StringKey("length".to_string()),
                optional: false,
                mutable: false,
                t: number,
            }),
            // [n: number]: T;
            types::TObjElem::Index(types::TIndex {
                key: types::TIndexKey {
                    name: "n".to_string(),
                    t: number,
                },
                mutable: true, // we should have some sets to set properties on an array
                t: type_param_t,
            }),
        ],
    );
    let array_scheme = Scheme {
        type_params: Some(vec![types::TypeParam {
            name: "T".to_string(),
            constraint: None,
            default: None,
        }]),
        t: array_interface,
    };

    context.schemes.insert("Array".to_string(), array_scheme);

    (arena, context)
}

/// Sets up some predefined types using the type constructors TypeVariable,
/// TypeOperator and Function.  Creates a list of example expressions to be
/// evaluated. Evaluates the expressions, printing the type or errors arising
/// from each.

#[test]
fn test_complex_logic() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let a: number
    declare let b: number
    declare let c: number
    let result = a > b || b >= c || c != a && c != b
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_string_equality() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let a: string
    declare let b: string
    let eq = a == b
    let neq = a != b
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("eq").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);
    let binding = my_ctx.values.get("neq").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_if_else() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let cond: boolean
    let result = if (cond) { 5 } else { 10 }
    "#;

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(arena[binding.index].as_string(&arena), r#"5 | 10"#);
    Ok(())
}

#[test]
fn test_chained_if_else() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let cond1: boolean
    declare let cond2: boolean
    let result = if (cond1) { 5 } else if (cond2) { 10 } else { 15 }
    "#;

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(arena[binding.index].as_string(&arena), r#"5 | 10 | 15"#);
    Ok(())
}

#[test]
fn test_factorial() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // factorial
    let src = r#"
    let fact = fn (n) => {
        return if (n == 0) {
            1
        } else {
            n * fact(n - 1)
        }
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("fact").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(n: number) => 1 | number"#
    );
    Ok(())
}

#[test]
fn test_mutual_recursion() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

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
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("even").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );
    let binding = my_ctx.values.get("odd").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );

    Ok(())
}

#[test]
fn test_mutual_recursion_using_destructuring() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

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
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("even").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );
    let binding = my_ctx.values.get("odd").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );

    Ok(())
}

#[test]
fn test_no_top_level_redeclaration() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let id = fn (x) => x
    let id = fn (y) => y
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "id cannot be redeclared at the top-level".to_string()
        ))
    );

    Ok(())
}

#[should_panic]
#[test]
fn test_mismatch() {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"fn (x) => [x(3), x(true)]"#;

    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx).unwrap();
}

#[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"Undefined symbol \\\"f\\\"\")"]
#[test]
fn test_pair() {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"[f(3), f(true)]"#;

    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx).unwrap();
}

#[test]
fn test_mul() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
        let f = fn (x) => x
        let result = [f(4), f(true)]
    "#;

    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"[4, true]"#);
    Ok(())
}

#[should_panic = "recursive unification"]
#[test]
fn test_recursive() {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"fn (f) => f(f)"#;

    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx).unwrap();
}

#[test]
fn test_number_literal() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let g = fn (f) => 5
    let result = g(g)
    "#;

    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"5"#);
    Ok(())
}

#[test]
fn test_generic_nongeneric() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let result = fn (g) => {
        let f = fn (x) => g
        return [f(3), f(true)]
    }"#;

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A>(g: A) => [A, A]"#
    );
    Ok(())
}

#[test]
fn test_basic_generics() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // example that demonstrates generic and non-generic variables:
    let src = r#"let result = fn (x) => x"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"<A>(x: A) => A"#);

    Ok(())
}

#[test]
fn test_composition() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // Function composition
    // fn f (fn g (fn arg (f g arg)))
    let src = r#"let result = fn (f) => fn (g) => fn (arg) => g(f(arg))"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A, C, B>(f: (arg0: A) => B) => (g: (arg0: B) => C) => (arg: A) => C"#
    );
    Ok(())
}

#[test]
fn test_skk() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))
    let K = fn (x) => fn (y) => x
    let I = S(K)(K)
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("S").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A, C, B>(f: (arg0: A) => (arg0: B) => C) => (g: (arg0: A) => B) => (x: A) => C"#
    );
    let binding = my_ctx.values.get("K").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A, B>(x: A) => (y: B) => A"#
    );
    let binding = my_ctx.values.get("I").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"<A>(x: A) => A"#);

    Ok(())
}

#[test]
fn test_composition_with_statements() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // Function composition
    let src = r#"
    let result = fn (f) => {
        let mantel = fn (g) => {
            let core = fn (arg) => g(f(arg))
            return core
        }
        return mantel
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A, B, C>(f: (arg0: A) => B) => (g: (arg0: B) => C) => (arg: A) => C"#
    );
    Ok(())
}

#[test]
fn test_subtype() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(5, 10)
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);
    Ok(())
}

#[test]
fn test_callback_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // It's okay for the callback arg to take fewer params since extra params
    // are ignored.  It's also okay for its params to be supertypes of the
    // expected params since the callback will only be called with the expected
    // types.  Lastly, it's okay for the return type to be a subtype of the
    // expected return type since it still conforms to the expected type.
    let src = r#"
    declare let foo: fn (cb: fn (a: number, b: string) => boolean) => boolean
    declare let bar: fn (x: number | string) => boolean
    let result = foo(bar)
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);
    Ok(())
}

#[test]
fn test_callback_error_too_many_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (cb: fn (x: number) => boolean) => boolean
    declare let bar: fn (a: number, b: string) => boolean
    let result = foo(bar)
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);
    assert_eq!(
        result,
        Err(Errors::InferenceError("(a: number, b: string) => boolean is not a subtype of (x: number) => boolean since it requires more params".to_string())),
    );
    Ok(())
}

#[test]
fn test_union_subtype() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let lit1 = new_num_lit_type(&mut arena, "5");
    let lit2 = new_num_lit_type(&mut arena, "10");
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: new_union_type(&mut arena, &[lit1, lit2]),
            is_mut: false,
        },
    );

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(foo, 2)
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);
    Ok(())
}

#[test]
fn test_calling_a_union() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let bool = new_primitive(&mut arena, Primitive::Boolean);
    let str = new_primitive(&mut arena, Primitive::String);
    let fn1 = new_func_type(&mut arena, &[], bool, &None);
    let fn2 = new_func_type(&mut arena, &[], str, &None);
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: new_union_type(&mut arena, &[fn1, fn2]),
            is_mut: false,
        },
    );

    let src = r#"let result = foo()"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"boolean | string"#
    );
    Ok(())
}

#[test]
fn call_with_too_few_args() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times()
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
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: lit,
            is_mut: false,
        },
    );

    let src = r#"let result = foo()"#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "literal Number(\n    \"5\",\n) is not callable".to_string()
        ))
    );

    Ok(())
}

#[test]
fn infer_basic_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"let result = [5, "hello"]"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        "[5, \"hello\"]".to_string(),
    );

    Ok(())
}

#[test]
fn tuple_member() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"]
    let second = tuple[1]
    declare let index: number
    let any = tuple[index]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("second").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#""hello""#.to_string(),
    );
    let binding = my_ctx.values.get("any").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"5 | "hello" | undefined"#.to_string(),
    );

    Ok(())
}

#[test]
fn tuple_member_invalid_index() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"]
    let second = tuple["foo"]
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't access property on non-object type".to_string()
        ))
    );

    Ok(())
}

#[test]
fn array_member() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<number>
    let first = array[0]
    declare let index: number
    let any = array[0]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("first").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        "number | undefined".to_string(),
    );
    let binding = my_ctx.values.get("any").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        "number | undefined".to_string(),
    );

    Ok(())
}

#[test]
fn tuple_member_error_out_of_bounds() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"]
    let result = tuple[2]
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

    let src = r#"
    declare let foo: fn (x: [number, string]) => boolean
    let result = foo([5, "hello", true])
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        "boolean".to_string(),
    );

    Ok(())
}

#[test]
fn tuple_subtyping_not_enough_elements() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (x: [number, string]) => boolean
    let result = foo([5])
    "#;
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

    let src = r#"let result = {a: 5, b: "hello"}"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        "{a: 5, b: \"hello\"}".to_string(),
    );

    Ok(())
}

#[test]
fn object_member() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let obj = {a: 5, b: "hello"}
    let result = obj.a
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(arena[binding.index].as_string(&arena), "5".to_string(),);

    Ok(())
}

#[test]
fn object_member_string_key() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let obj = {a: 5, b: "hello"}
    declare let key: string
    let result = obj[key]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        "5 | \"hello\" | undefined".to_string(),
    );

    Ok(())
}

#[test]
fn object_member_missing_prop() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let obj = {a: 5, b: "hello"}
    let result = obj.c
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

    // Each prop must be a subtype of the expected element type
    // It's okay to pass an object with extra props
    let src = r#"
    declare let foo: fn (x: {a: number, b: string}) => boolean
    let result = foo({a: 5, b: "hello", c: true})
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("result").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        "boolean".to_string(),
    );

    Ok(())
}

#[test]
fn object_signatures() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // Each prop must be a subtype of the expected element type
    // It's okay to pass an object with extra props
    let src = r#"
    declare let obj: {
        fn (a: number): string,
        fn foo(a: number): string,
        fn bar(self, a: number): string,
        get baz(self): string,
        set baz(self, value: string): undefined,
        [key: string]: number,
        qux: string,
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("obj").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        "{fn(a: number): string, fn foo(a: number): string, fn bar(self: t9, a: number): string, get baz(self): string, set baz(self, value: string): undefined, [key: string]: number, qux: string}".to_string(),
    );

    Ok(())
}

#[test]
fn object_callable_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn (self, a: number | string): string,
    }
    let bar: {
        fn (self, a: number): number | string,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

// TODO: This fail, we need to check unify callable siagntures in
// object types
#[test]
#[ignore]
fn object_callable_subtyping_failure_case() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn (self, a: string): string,
    }
    let bar: {
        fn (self, a: number): number,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Err(Errors::InferenceError(
        "Expected type number, found type string".to_string(),
    ))
}

#[test]
fn object_method_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn method(self, a: number | string): string,
    }
    let bar: {
        fn method(self, a: number): number | string,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn object_property_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn method(self, a: number): string,
        x: number,
        y: boolean,
    }
    let bar: {
        fn method(self, a: number): string,
        x: number | string,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn object_indexer_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        [key: string | number]: number,
    }
    let bar: {
        [key: string]: number | string,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

// TODO
#[test]
fn object_methods_and_properties_should_unify() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        fn foo(self, a: number): string,
    }
    let bar: {
        foo: fn (a: number) => string,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn object_indexers_should_unify_with_all_named_obj_elems() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        a: fn () => number,
        b?: fn () => number,
        get c(self): fn () => number,
        fn d(self): number,
    }
    let bar: {
        [key: string]: fn () => number,
    } = foo
    "#;

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn object_indexers_and_properties_unify_failure() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        x: number,
    }
    let bar: {
        [key: string]: boolean,
    } = foo
    "#;

    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(number, boolean | undefined) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn object_properties_and_getter_should_unify() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: {
        get foo(self): number,
    }
    let bar: {
        foo: number,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

// TODO
#[test]
#[ignore]
fn mutable_object_properties_unify_with_getters_setters() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let mut foo: {
        x: number,
    }
    let mut bar: {
        get x(self): number,
        set x(self, value: number): undefined,
    } = foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn object_subtyping_missing_prop() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (x: {a: number, b: string}) => boolean
    let result = foo({b: "hello"})
    "#;
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
    let times = fn (x, y) => x * y
    let result = times(5, "hello")
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
    my_ctx.values.insert(
        "foo".to_string(),
        Binding {
            index: new_union_type(&mut arena, &[lit1, lit2]),
            is_mut: false,
        },
    );

    let src = r#"
    let times = fn (x, y) => x * y
    let result = times(foo, "world")
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
fn test_union_subtype_error_with_type_ann() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let x: number | string = true
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(true, number | string) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_program() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let num = 5
    let str = "hello"
    num * num
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("num").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"5"#);

    let binding = my_ctx.values.get("str").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#""hello""#);

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
    let id = fn (x) => x
    let a = id(5)
    let b = id("hello")
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("id").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"<A>(x: A) => A"#);

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"5"#);

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#""hello""#);

    Ok(())
}

#[test]
fn test_program_with_generic_func_multiple_type_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let fst = fn (x, y) => x
    let snd = fn (x, y) => y
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A, B>(x: A, y: B) => A"#
    );

    let binding = my_ctx.values.get("snd").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<A, B>(x: A, y: B) => B"#
    );

    Ok(())
}

#[test]
fn test_function_with_multiple_statements() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let result = fn () => {
        let x = 5
        let y = 10
        return x * y
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"() => number"#);

    if let StmtKind::Let {
        expr: Some(init), ..
    } = &program.stmts[0].kind
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
    // fn () => {let x = 5
    // let y = 10
    // return times(x, y)}
    // "###);

    Ok(())
}

#[test]
fn test_inferred_type_on_ast_nodes() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"let result = fn (x, y) => x * y"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    if let StmtKind::Let {
        expr: Some(init), ..
    } = &program.stmts[0].kind
    {
        if let ExprKind::Function(expr::Function { params, .. }) = &init.kind {
            let x_t = params[0].pattern.inferred_type.unwrap();
            let y_t = params[1].pattern.inferred_type.unwrap();

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

    let src = r#"let neg = fn (x) => -x"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("neg").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(x: number) => number"#
    );
    Ok(())
}

#[test]
fn test_async_return_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => 5
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("foo").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"() => Promise<5>"#
    );
    Ok(())
}

#[test]
fn test_async_without_return() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => {
        let sum = 5 + 10
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("foo").unwrap();

    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"() => Promise<undefined>"#
    );
    Ok(())
}

#[test]
fn test_await_in_async() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => 5
    let bar = async fn () => {
        let x = await foo()
        return x
    }
    let baz = async fn () => foo()
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"() => Promise<5>"#
    );

    let binding = my_ctx.values.get("baz").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"() => Promise<5>"#
    );

    Ok(())
}

#[test]
fn test_await_outside_of_async() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => 5
    let bar = fn () => {
        let x = await foo()
        return x
    }
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);
    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't use await outside of an async function".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_await_non_promise() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async fn () => await 5
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);
    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(5, Promise<t6>) failed".to_string()
        ))
    );

    Ok(())
}

// TODO: write a test to ensure that Promise<5> is a subtype of Promise<number>
// In general, generic types should be covariant across their type parameters.

#[test]
fn test_do_expr() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

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
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"["hello", number]"#
    );

    Ok(())
}

#[test]
fn test_empty_do_expr() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"let sum = do {}"#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"undefined"#);

    Ok(())
}

#[test]
fn test_let_with_type_ann() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let x: number = 5
    let flag: boolean = true
    let foo: fn () => number = fn () => 10
    let bar: fn () => undefined = fn () => {}
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
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn test_function_overloads() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add: (fn (a: number, b: number) => number) & (fn (a: string, b: string) => string)
    let sum = add(5, 10)
    let msg = add("hello, ", "world")
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("sum").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    let binding = my_ctx.values.get("msg").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_function_no_valid_overload() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add: (fn (a: number, b: number) => number) & (fn (a: string, b: string) => string)
    add(5, "world")
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "no valid overload for args".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_declare_cant_have_initializer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add: fn (a: number, b: number) => number = fn (a, b) => a + b
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Variable declarations using `declare` cannot have an initializer".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_declare_must_have_type_annotations() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Variable declarations using `declare` must have a type annotation".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_normal_decl_must_have_initializer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let add: fn (a: number, b: number) => number
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Variable declarations not using `declare` must have an initializer".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_pattern_matching_is_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string
    let name = match (expr) {
        a is number => a + 1,
        b is string => "bar"
    }
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("name").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number | "bar""#);

    Ok(())
}

#[test]
fn test_pattern_matching_does_not_refine_expr() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string
    let name = match (expr) {
        x is number => expr + 1,
        x is string => "bar"
    }
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: string != number".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_pattern_not_a_subtype_of_expr() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string
    let name = match (expr) {
        x is number => "foo",
        x is string => "bar",
        x is boolean => "baz"
    }
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(boolean, number | string) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_pattern_matching_array() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

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
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        // TODO: update unions to merge elements whenever possible
        r#"0 | number | number | Array<number>"#
    );

    Ok(())
}

#[test]
fn test_pattern_matching_object() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let action: {kind: "insert", key: string, value: string} | {kind: "delete", key: string}
    let key = match (action) {
        {kind: "insert", key, value} => key,
        {kind: "delete", key} => key
    }
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("key").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string | string"#);

    Ok(())
}

#[test]
fn member_access_on_union() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let obj: {a: number, b: string} | {b: boolean}
    let b = obj.b
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"string | boolean"#
    );

    Ok(())
}

#[test]
fn member_access_optional_property() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a?: number, b: string}
    let a = obj.a
    let b = obj.b
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"number | undefined"#
    );
    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn member_access_on_unknown_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: unknown
    let a = obj.a
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't access properties on unknown".to_string()
        ))
    );

    Ok(())
}

#[test]
fn member_access_on_type_variable() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let get_a = fn (x) => x.a
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't access properties on t5".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_object_destructuring_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let obj: {a?: number, b: string, c: boolean}
    let {a, b, c} = obj
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"number | undefined"#
    );

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_object_destructuring_assignment_with_rest() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a?: number, b: string, c: boolean}
    let {a, ...rest} = obj
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"number | undefined"#
    );
    let binding = my_ctx.values.get("rest").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"{b: string, c: boolean}"#
    );

    Ok(())
}

#[test]
fn test_object_nested_destructuring_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a: {b: {c: string}}}
    let {a: {b: {c}}} = obj
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    assert_eq!(my_ctx.values.get("a"), None);
    assert_eq!(my_ctx.values.get("b"), None);

    Ok(())
}

#[test]
fn test_tuple_destrcuturing_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, string, boolean]
    let [a, b, c] = tuple
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_tuple_destructuring_assignment_with_rest() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, string, boolean]
    let [a, ...tuple_rest] = tuple
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);
    let binding = my_ctx.values.get("tuple_rest").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"[string, boolean]"#
    );

    Ok(())
}

#[test]
fn test_array_destructuring_assignment_with_rest() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<string>
    let [a, ...array_rest] = array
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"string | undefined"#
    );
    let binding = my_ctx.values.get("array_rest").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"Array<string>"#);

    Ok(())
}

#[test]
fn test_tuple_nested_destrcuturing_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, [string, [boolean]]]
    let [_, [_, [c]]] = tuple
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_explicit_type_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn (x) => x
    let x = identity<number>(5)
    let y = identity<string>("hello")
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);
    let binding = my_ctx.values.get("y").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_explicit_type_params_type_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn (x) => x
    identity<number>("hello")
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
fn test_explicit_type_params_too_many_type_args() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn (x) => x
    identity<number, string>(5)
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "wrong number of type args".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_param_with_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn <T: number | string>(x: T): T => x
    let x = identity(5)
    let y = identity("hello")
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("identity").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<T:number | string>(x: T) => T"#
    );
    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"5"#);
    let binding = my_ctx.values.get("y").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#""hello""#);

    Ok(())
}

#[test]
fn test_mix_explicit_implicit_type_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let fst = fn <B>(a, b: B) => a
    let snd = fn <B>(a, b: B): B => b
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("fst").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<B, A>(a: A, b: B) => A"#
    );
    let binding = my_ctx.values.get("snd").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<B, A>(a: A, b: B) => B"#
    );

    Ok(())
}

#[test]
fn test_duplicate_type_param_names_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let fst = fn <T, T>(a: T, b: T): T => a
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type param identifiers must be unique".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_param_with_violated_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = fn <T: number | string>(x: T): T => x
    identity(true)
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(true, number | string) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_ann_func_with_type_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity: fn <T: number | string>(x: T) => T = fn (x) => x
    let x = identity<number>(5)
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("identity").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"<T:number | string>(x: T) => T"#
    );
    let binding = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn test_type_ann_func_with_type_constraint_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let id1 = fn <T: number | string>(x: T): T => x
    let id2: fn <T: boolean>(x: T) => T = id1
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(boolean, number | string) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_callback_with_type_param_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (callback: fn <T: number>(x: T) => T) => boolean
    let identity = fn <T: number | string>(x: T): T => x
    let result = foo(identity)
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_callback_with_type_param_subtyping_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: fn (callback: fn <T: number | string>(x: T) => T) => boolean
    let identity = fn <T: number>(x: T): T => x
    let result = foo(identity)
    "#;
    let mut program = parse(src).unwrap();
    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: string != number".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_return_type_checking() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (): string => "hello"
    let result = foo()
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"() => string"#);
    let binding = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_return_value_is_not_subtype_of_return_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (): number => "hello"
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
fn type_alias() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("p").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"{x: number, y: number}"#
    );

    Ok(())
}

#[test]
fn type_alias_with_params_with_destructuring() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string> = {value: "hello"}
    let {value} = node
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("node").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"{value: string}"#);
    let binding = my_ctx.values.get("value").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn type_alias_with_params_with_member_access() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string> = {value: "hello"}
    let value = node.value
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("node").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"{value: string}"#);
    let binding = my_ctx.values.get("value").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn type_alias_with_params_with_computed_member_access() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string> = {value: "hello"}
    let key = "value"
    let value = node[key]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("node").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"{value: string}"#);
    let binding = my_ctx.values.get("value").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn instantiate_type_alias_with_too_many_type_args() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T}
    let node: Node<string, number> = {value: "hello"}
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Node expects 1 type args, but was passed 2".to_string()
        ))
    );

    Ok(())
}

#[test]
fn instantiate_type_alias_with_args_when_it_has_no_type_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point<number> = {x: 5, y: 10}
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Point doesn't require any type args".to_string()
        ))
    );

    Ok(())
}

#[test]
fn property_accesses_on_unions() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    declare let object_union: {x: number, y: number} | {x: string}
    let elem = tuple_union[0]
    let x1 = object_union.x
    let key = "x"
    let x2 = object_union[key]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("elem").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number | string"#);
    let binding = my_ctx.values.get("x1").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number | string"#);
    let binding = my_ctx.values.get("x2").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number | string"#);

    Ok(())
}

#[test]
fn maybe_property_accesses_on_unions() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    declare let object_union: {x: number, y: number} | {x: string}
    let maybe_elem = tuple_union[1]
    let maybe_y = object_union.y
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("maybe_elem").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"number | undefined"#
    );
    let binding = my_ctx.values.get("maybe_y").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"number | undefined"#
    );

    Ok(())
}

#[test]
#[ignore]
fn destructuring_unions() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    declare let object_union: {x: number, y: number} | {x: string}
    let [fst, snd] = tuple_union
    let {x, y} = object_union
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    // TODO: write assertions for this test once the desired
    // behavior has been implemented.

    Ok(())
}

#[test]
fn missing_property_accesses_on_union_of_tuples() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string]
    let elem = tuple_union[2]
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property on object".to_string()
        ))
    );

    Ok(())
}

#[test]
fn missing_property_accesses_on_union_of_objects() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let object_union: {x: number, y: number} | {x: string}
    let z = object_union.z
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property \"z\" on object".to_string()
        ))
    );

    Ok(())
}

#[test]
fn methods_on_arrays() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let num_array: Array<number> = []
    num_array.push(5)
    let str_array: Array<string> = []
    str_array.push("hello")

    let len = str_array.length
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let binding = my_ctx.values.get("len").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn properties_on_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let tuple: [number, string] = [5, "hello"]
    let len = tuple.length
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("len").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
#[ignore]
fn set_array_element() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<number>
    array[0] = 5
    array[1] = 10
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
#[ignore]
fn set_tuple_element() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, number]
    tuple[0] = 5
    tuple[1] = 10
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("len").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn methods_on_arrays_incorrect_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let num_array: Array<number> = []
    num_array.push("hello")
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
fn test_unknown() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let a: unknown = 5
    let b: unknown = "hello"
    let c: unknown = true
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"unknown"#);
    let binding = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"unknown"#);
    let binding = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"unknown"#);

    Ok(())
}

#[test]
fn test_unknown_assignment_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let a: unknown = 5
    let b: number = a
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(unknown, number) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_param_explicit_unknown_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let add = fn <T: unknown>(a: T, b: T): T => {
        return a + b
    }
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(unknown, number) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_param_implicit_unknown_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let add = fn <T>(a: T, b: T): T => {
        return a + b
    }
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unify(unknown, number) failed".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_optional_function_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn (a: number, b?: number): number => {
        return a
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"(a: number, b?: number) => number"#
    );

    Ok(())
}

#[test]
fn test_func_param_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn ({ a: x, b }: { a: number, b: string }) => {
        return x
    }
    let bar = fn ([a, b]: [number, string]) => {
        return b
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"({a, b}: {a: number, b: string}) => number"#
    );
    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"([a, b]: [number, string]) => string"#
    );

    Ok(())
}

#[test]
fn test_func_param_object_rest_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn ({ a, ...rest }: { a: number, b: string }) => {
        return rest.b
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"({a, ...rest}: {a: number, b: string}) => string"#
    );

    Ok(())
}

#[test]
fn test_func_param_object_multiple_rest_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = fn ({ a, ...rest1, ...rest2 }: { a: number, b: string }) => {
        return rest.b
    }
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Maximum one rest pattern allowed in object patterns".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_func_param_tuple_rest_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let bar = fn ([a, ...rest]: [number, string, boolean]) => {
        return rest[1]
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"([a, ...rest]: [number, string, boolean]) => boolean"#
    );

    Ok(())
}

#[test]
fn test_index_access_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b?: number, [key: string]: boolean}
    type A = Foo["a"]
    type B = Foo["b"]
    type C = Foo["c"]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    assert_eq!(arena[scheme.t].as_string(&arena), r#"Foo["a"]"#);
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"string"#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"number | undefined"#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"boolean | undefined"#);

    Ok(())
}

#[test]
fn test_index_access_type_using_string_as_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b: number, c: boolean}
    type T = Foo[string]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(
        arena[t].as_string(&arena),
        r#"string | number | boolean | undefined"#
    );

    Ok(())
}

#[test]
fn test_index_access_type_using_number_as_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b: number, [key: number]: boolean}
    type T = Foo[number]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"boolean | undefined"#);

    Ok(())
}

#[test]
fn test_index_access_type_missing_property() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b?: number}
    type C = Foo["c"]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("C").unwrap();
    let result = expand_type(&mut arena, &my_ctx, scheme.t);

    // TODO: check that the index access is valid where it's inferred
    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property 'c' on object".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_index_access_type_missing_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {[key: number]: string}
    type C = Foo["c"]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("C").unwrap();
    let result = expand_type(&mut arena, &my_ctx, scheme.t);

    // TODO: check that the index access is valid where it's inferred
    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property c in object".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_index_access_type_number_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {[key: number]: string}
    type T = Foo[1]
    let t: T = "hello"
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"string | undefined"#);

    let binding = my_ctx.values.get("t").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"string | undefined"#
    );

    Ok(())
}

#[test]
fn test_index_access_type_on_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[1]
    let t: T = "hello"
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("t").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_index_access_type_on_tuple_with_number_key() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[number]
    let t: T = "hello"
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("t").unwrap();
    assert_eq!(
        arena[binding.index].as_string(&arena),
        r#"number | string | boolean | undefined"#
    );

    Ok(())
}

#[test]
fn test_index_access_out_of_bounds_on_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[3]
    let t: T = "hello"
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "3 was outside the bounds 0..3 of the tuple".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_index_access_not_usize() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean]
    type T = Foo[1.5]
    let t: T = "hello"
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "1.5 isn't a valid index".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_typeof() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    let foo = {a: "hello", b: 5, c: true}
    type Foo = typeof foo
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"{a: "hello", b: 5, c: true}"#);

    Ok(())
}

#[test]
fn test_keyof_obj() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    let a = {x: "hello", y: 5, z: true}
    type A = keyof typeof a
    let b = {x: "hello"}
    type B = keyof typeof b
    let c = {}
    type C = keyof typeof c
    type D = keyof {[key: string]: number, x: number}
    type E = keyof {[key: number]: boolean, x: boolean}
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""x" | "y" | "z""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""x""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"never"#);

    let scheme = my_ctx.schemes.get("D").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"string"#);

    let scheme = my_ctx.schemes.get("E").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"number | "x""#);

    Ok(())
}

#[test]
fn test_keyof_array_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = keyof Array<number>
    type Bar = keyof ["hello", 5, true]
    type Baz = keyof ["hello"]
    type Qux = keyof []
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"number"#);

    let scheme = my_ctx.schemes.get("Bar").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"0 | 1 | 2"#);

    let scheme = my_ctx.schemes.get("Baz").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"0"#);

    let scheme = my_ctx.schemes.get("Qux").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"never"#);

    Ok(())
}

#[test]
fn test_keyof_alias() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    type Foo = keyof Point
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""x" | "y""#);

    Ok(())
}

#[test]
fn test_keyof_literal() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type String = {
        length: number,
        slice: fn (start: number, end: number) => string,
    }
    type Number = {
        toFixed: fn (precision: number) => string,
        toString: fn () => string,
    }
    type Boolean = {
        valueOf: fn () => boolean,
    }
    type A = keyof "hello"
    type B = keyof 5
    type C = keyof true
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""length" | "slice""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""toFixed" | "toString""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""valueOf""#);

    Ok(())
}

#[test]
fn test_keyof_primitive() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type String = {
        length: number,
        slice: fn (start: number, end: number) => string,
    }
    type Number = {
        toFixed: fn (precision: number) => string,
        toString: fn () => string,
    }
    type Boolean = {
        valueOf: fn () => boolean,
    }
    type A = keyof string
    type B = keyof number
    type C = keyof boolean
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""length" | "slice""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""toFixed" | "toString""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""valueOf""#);

    Ok(())
}

#[test]
fn test_keyof_unknown_undefined_null() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type A = keyof unknown
    type B = keyof undefined
    type C = keyof null
    type D = keyof never
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"never"#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"never"#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"never"#);

    let scheme = my_ctx.schemes.get("D").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"string | number | symbol"#);

    Ok(())
}

#[test]
fn test_keyof_intersection() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type A = keyof ({a: number} & {b: string})
    type B = keyof ({a: number, b: number} & {b: string, c: boolean})
    type C = keyof ({a: number} & undefined)
    type D = keyof ({a: number} & {[key: string]: number})
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("A").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""a" | "b""#);

    let scheme = my_ctx.schemes.get("B").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""a" | "b" | "c""#);

    let scheme = my_ctx.schemes.get("C").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""a""#);

    let scheme = my_ctx.schemes.get("D").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_mutually_recursive_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();
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

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_mutually_recursive_type_with_index_access_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();
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

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_type_alias_with_undefined_def() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();
    let src = r#"
    type A = B
    "#;

    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError("B is not in scope".to_string()))
    );

    Ok(())
}

#[test]
fn test_mutable_error_arg_passing() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    type Point = {x: number, y: number}
    let scale = fn (mut p: Point, factor: number) => {
        return p
    }
    let p: Point = {x: 5, y: 10}
    scale(p, 2)
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't assign immutable value to mutable binding".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_mutable_error_arg_passing_with_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    declare let foo: fn (mut items: Array<number | string>) => undefined
    let mut numbers: Array<number> = [1, 2, 3]
    foo(numbers)
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "unify_mut: Array<number> != Array<number | string>".to_string(),
        )),
    );

    Ok(())
}

#[test]
fn test_mutable_ok_arg_passing() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    type Point = {x: number, y: number}
    let mut_scale = fn (mut p: Point, factor: number) => {
        return p
    }
    let scale = fn (p: Point, factor: number) => {
        return p
    }

    let main = fn () => {
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
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_mutable_error_arg_passing_declared_fn() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    declare let scale: fn (mut p: Point, factor: number) => Point
    let p: Point = {x: 5, y: 10}
    scale(p, 2)
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't assign immutable value to mutable binding".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_mutable_ok_arg_passing_declared_fns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: handle `declare let scale: fn(mut p: Point, ...);
    let src = r#"
    type Point = {x: number, y: number}
    declare let mut_scale: fn (mut p: Point, factor: number) => Point
    declare let scale: fn (p: Point, factor: number) => Point

    let main = fn () => {
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
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_mutable_error_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    let mut q = p
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Can't assign immutable value to mutable binding".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_mutable_ok_assignments() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    
    let main = fn () => {
        let mut p: Point = {x: 5, y: 10}
        let mut q = p
    
        let p: Point = {x: 5, y: 10}
        let q = p

        let mut p: Point = {x: 5, y: 10}
        let q = p
    }
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_mutable_invalid_assignments() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"    
    let mut arr1: Array<number> = [1, 2, 3]
    let mut arr2: Array<number | string>  = arr1
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "unify_mut: Array<number> != Array<number | string>".to_string(),
        ))
    );

    Ok(())
}

#[test]
fn test_sub_objects_are_mutable() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Obj = {a: {b: {c: string}}}
    declare let obj1: Obj
    declare let mut obj2: Obj
    let b1 = obj1.a.b
    let b2 = obj2.a.b
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let binding = my_ctx.values.get("b1").unwrap();
    assert_eq!(arena[binding.index].as_string(&arena), r#"{c: string}"#);

    let binding = my_ctx.values.get("b2").unwrap();
    // TODO: create helper function to print bindings, not just types
    assert_eq!(arena[binding.index].as_string(&arena), r#"{c: string}"#);

    Ok(())
}

#[test]
fn test_tuple_type_equality() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let a: [number, string]
    declare let b: [number, string]
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let a = my_ctx.values.get("a").unwrap();
    let a_t = arena[a.index].clone();
    let b = my_ctx.values.get("b").unwrap();
    let b_t = arena[b.index].clone();

    assert!(a_t.equals(&b_t, &arena));

    Ok(())
}

#[test]
fn test_function_type_equality() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add: fn(a: number, b: number) => number
    declare let sub: fn(a: number, b: number) => number
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let add = my_ctx.values.get("add").unwrap();
    let add_t = arena[add.index].clone();
    let sub = my_ctx.values.get("sub").unwrap();
    let sub_t = arena[sub.index].clone();

    assert!(add_t.equals(&sub_t, &arena));

    Ok(())
}

#[test]
fn test_literal_type_equality() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let a = 5
    let b = 5
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let a = my_ctx.values.get("a").unwrap();
    let a_t = arena[a.index].clone();
    let b = my_ctx.values.get("b").unwrap();
    let b_t = arena[b.index].clone();

    assert!(a_t.equals(&b_t, &arena));

    Ok(())
}

#[test]
fn test_mutable_object_type_equality() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let mut p: Point = {x: 5, y: 10}
    let mut q: Point = {x: 0, y: 1}
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let p = my_ctx.values.get("p").unwrap();
    let p_t = arena[p.index].clone();
    let q = my_ctx.values.get("q").unwrap();
    let q_t = arena[q.index].clone();

    assert!(p_t.equals(&q_t, &arena));

    Ok(())
}

#[test]
fn test_mutating_mutable_object() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let mut p: Point = {x: 5, y: 10}
    p.x = 0
    p["y"] = 0
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_mutating_immutable_object_errors() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Point = {x: number, y: number}
    let p: Point = {x: 5, y: 10}
    p.x = 0
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Cannot assign to immutable lvalue".to_string()
        ))
    );

    Ok(())
}

#[test]
fn conditional_type_exclude() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Exclude<T, U> = if (T extends U) { never } else { T }
    type Result = Exclude<"a" | "b" | "c" | "d" | "e", "a" | "e">
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""b" | "c" | "d""#);

    Ok(())
}

#[test]
fn chained_conditional_types() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Foo<T> = if (T extends string) { 
        "str"
    } else if (T extends number) {
        "num"
    } else {
        "?"
    }
    type Result = Foo<5 | "hello">
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""num" | "str""#);

    Ok(())
}

#[test]
fn conditional_type_with_placeholders() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: introduce a placeholder type that will unify with anything
    let src = r#"
        type IsArray<T> = if (T extends Array<_>) { true } else { false }
        type T = IsArray<Array<number>>
        type F = IsArray<number>
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let result = my_ctx.schemes.get("T").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"true"#);

    let result = my_ctx.schemes.get("F").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"false"#);

    Ok(())
}

#[test]
#[ignore]
fn conditional_type_with_function_subtyping() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
        type IsFunction<T> = if (T extends fn (...args: Array<_>) => _) { true } else { false }
        type T = IsFunction<fn (a: number) => string>
        type F = IsFunction<number>
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let result = my_ctx.schemes.get("T").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"true"#);

    let result = my_ctx.schemes.get("F").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"false"#);

    Ok(())
}

#[test]
fn conditional_type_with_infer_and_placeholders() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: introduce a placeholder type that will unify with anything
    let src = r#"
        type ReturnType<
            T : fn (...args: Array<_>) => _
        > = if (T extends fn (...args: Array<_>) => infer R) { 
            R 
        } else {
            never
        }
        type Result = ReturnType<fn () => boolean> 
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let result = my_ctx.schemes.get("Result").unwrap();
    let t = expand_type(&mut arena, &my_ctx, result.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"boolean"#);

    Ok(())
}
