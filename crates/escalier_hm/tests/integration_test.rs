use generational_arena::{Arena, Index};

use escalier_hm::ast::*;
use escalier_hm::parser::parse;

use escalier_hm::ast::{self as syntax};
use escalier_hm::context::*;
use escalier_hm::errors::*;
use escalier_hm::infer::*;
use escalier_hm::types::{self, *};
use escalier_hm::util::expand_type;

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
    let mut arena = Arena::new();
    let mut context = Context::default();

    let number = new_constructor(&mut arena, "number", &[]);
    let type_param_t = new_constructor(&mut arena, "T", &[]);

    let array_interface = new_object_type(
        &mut arena,
        &[
            // .push(item: T): number;
            types::TObjElem::Method(types::TMethod {
                name: types::TPropKey::StringKey("push".to_string()),
                params: vec![FuncParam {
                    pattern: TPat::Ident(BindingIdent {
                        name: "item".to_string(),
                        mutable: false,
                        span: 0..0,
                        loc: DUMMY_LOC,
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
    declare let a: number;
    declare let b: number;
    declare let c: number;
    let result = a > b || b >= c || c != a && c != b;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_string_equality() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let a: string;
    declare let b: string;
    let eq = a == b;
    let neq = a != b;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("eq").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);
    let t = my_ctx.values.get("neq").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_factorial() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // factorial
    let src = r#"
    let fact = (n) => {
        return if (n == 0) {
            1
        } else {
            n * fact(n - 1)
        };
    };
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("fact").unwrap();

    assert_eq!(arena[*t].as_string(&arena), r#"(n: number) => 1 | number"#);
    Ok(())
}

#[test]
fn test_mutual_recursion() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let even = (x) => if (x == 0) {
        true
    } else {
        !odd(x - 1)
    };

    let odd = (x) => if (x == 1) {
        true
    } else {
        !even(x - 1)
    };
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("even").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );
    let t = my_ctx.values.get("odd").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );

    Ok(())
}

#[test]
fn test_mutual_recursion_using_destructuring() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let {even, odd} = {
        even: (x) => if (x == 0) {
            true
        } else {
            !odd(x - 1)
        },
        odd: (x) => if (x == 1) {
            true
        } else {
            !even(x - 1)
        },
    };
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("even").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );
    let t = my_ctx.values.get("odd").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"(x: number) => true | boolean"#
    );

    Ok(())
}

#[test]
fn test_no_top_level_redeclaration() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let id = (x) => x;
    let id = (y) => y;
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

    let t = my_ctx.values.get("result").unwrap();
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

    let t = my_ctx.values.get("result").unwrap();
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

    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<A>(g: A) => [A, A]"#);
    Ok(())
}

#[test]
fn test_basic_generics() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // example that demonstrates generic and non-generic variables:
    let src = r#"let result = (x) => x;"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<A>(x: A) => A"#);

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
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"<A, B, C>(f: (arg0: A) => B) => (g: (arg0: B) => C) => (arg: A) => C"#
    );
    Ok(())
}

#[test]
fn test_skk() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let S = (f) => (g) => (x) => f(x)(g(x));
    let K = (x) => (y) => x;
    let I = S(K)(K);
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("S").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"<A, B, C>(f: (arg0: A) => (arg0: B) => C) => (g: (arg0: A) => B) => (x: A) => C"#
    );
    let t = my_ctx.values.get("K").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"<A, B>(x: A) => (y: B) => A"#
    );
    let t = my_ctx.values.get("I").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<A>(x: A) => A"#);

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
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"<A, B, C>(f: (arg0: A) => B) => (g: (arg0: B) => C) => (arg: A) => C"#
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
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);
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
    declare let foo: (cb: (a: number, b: string) => boolean) => boolean;
    declare let bar: (x: number | string) => boolean;
    let result = foo(bar);
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);
    Ok(())
}

#[test]
fn test_callback_error_too_many_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: (cb: (x: number) => boolean) => boolean;
    declare let bar: (a: number, b: string) => boolean;
    let result = foo(bar);
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
    my_ctx
        .values
        .insert("foo".to_string(), new_union_type(&mut arena, &[lit1, lit2]));

    let src = r#"
    let times = (x, y) => x * y;
    let result = times(foo, 2);
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();
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
        .values
        .insert("foo".to_string(), new_union_type(&mut arena, &[fn1, fn2]));

    let src = r#"let result = foo();"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();
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
    my_ctx.values.insert("foo".to_string(), lit);

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
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), "[5, \"hello\"]".to_string(),);

    Ok(())
}

#[test]
fn tuple_member() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let tuple = [5, "hello"];
    let second = tuple[1];
    declare let index: number;
    let any = tuple[index];
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("second").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#""hello""#.to_string(),);
    let t = my_ctx.values.get("any").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"5 | "hello""#.to_string(),);

    Ok(())
}

#[test]
fn array_member() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<number>;
    let first = array[0];
    declare let index: number;
    let any = array[0];
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("first").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        "number | undefined".to_string(),
    );
    let t = my_ctx.values.get("any").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        "number | undefined".to_string(),
    );

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

    let src = r#"
    declare let foo: (x: [number, string]) => boolean;
    let result = foo([5, "hello", true]);
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), "boolean".to_string(),);

    Ok(())
}

#[test]
fn tuple_subtyping_not_enough_elements() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: (x: [number, string]) => boolean;
    let result = foo([5]);
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

    let src = r#"let result = {a: 5, b: "hello"};"#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();

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
    let t = my_ctx.values.get("result").unwrap();

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

    // Each prop must be a subtype of the expected element type
    // It's okay to pass an object with extra props
    let src = r#"
    declare let foo: (x: {a: number, b: string}) => boolean;
    let result = foo({a: 5, b: "hello", c: true});
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("result").unwrap();

    assert_eq!(arena[*t].as_string(&arena), "boolean".to_string(),);

    Ok(())
}

#[test]
fn object_subtyping_missing_prop() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: (x: {a: number, b: string}) => boolean;
    let result = foo({b: "hello"});
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
        .values
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
fn test_union_subtype_error_with_type_ann() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let x: number | string = true;
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
    let num = 5;
    let str = "hello";
    num * num;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("num").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"5"#);

    let t = my_ctx.values.get("str").unwrap();
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

    let t = my_ctx.values.get("id").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<A>(x: A) => A"#);

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"5"#);

    let t = my_ctx.values.get("b").unwrap();
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

    let t = my_ctx.values.get("fst").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<A, B>(x: A, y: B) => A"#);

    let t = my_ctx.values.get("snd").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<A, B>(x: A, y: B) => B"#);

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

    let t = my_ctx.values.get("result").unwrap();
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
    let t = my_ctx.values.get("neg").unwrap();

    assert_eq!(arena[*t].as_string(&arena), r#"(x: number) => number"#);
    Ok(())
}

#[test]
fn test_async_return_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async () => 5;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("foo").unwrap();

    assert_eq!(arena[*t].as_string(&arena), r#"() => Promise<5>"#);
    Ok(())
}

#[test]
fn test_async_without_return() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async () => {
        let sum = 5 + 10;
    };
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("foo").unwrap();

    assert_eq!(arena[*t].as_string(&arena), r#"() => Promise<undefined>"#);
    Ok(())
}

#[test]
fn test_await_in_async() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async () => 5;
    let bar = async () => {
        let x = await foo();
        return x;
    };
    let baz = async () => foo();
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("bar").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"() => Promise<5>"#);

    let t = my_ctx.values.get("baz").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"() => Promise<5>"#);

    Ok(())
}

#[test]
fn test_await_outside_of_async() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = async () => 5;
    let bar = () => {
        let x = await foo();
        return x;
    };
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
    let foo = async () => await 5;
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
            "hello";
        };
        let x = 5;
        let y = 10;
        [msg, x + y];
    };
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("sum").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"["hello", number]"#);

    Ok(())
}

#[test]
fn test_empty_do_expr() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let sum = do {};
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("sum").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"undefined"#);

    Ok(())
}

#[test]
fn test_let_with_type_ann() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let x: number = 5;
    let flag: boolean = true;
    let foo: () => number = () => 10;
    let bar: () => undefined = () => {};
    let arr1: number[] = [1, 2, 3];
    let arr2: Array<string> = ["hello", "world"];
    let p: { x: number, y: number } = { x: 5, y: 10 };
    let tuple: [number, string] = [5, "hello"];
    let union: number | string = 5;
    let union_arr: (number | string)[] = [5, "hello"];

    // This should be valid, but we don't support it yet
    // let baz: (number) => number = <A>(a: A) => a;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn test_function_overloads() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string);
    let sum = add(5, 10);
    let msg = add("hello, ", "world");
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("sum").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    let t = my_ctx.values.get("msg").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_function_no_valid_overload() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string);
    add(5, "world");
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
    declare let add: (a: number, b: number) => number = (a, b) => a + b;
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
    declare let add;
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
    let add: (a: number, b: number) => number;
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
    declare let expr: number | string;
    let name = match (expr) {
        x is number -> x + 1,
        x is string -> "bar"
    };
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("name").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | "bar""#);

    Ok(())
}

#[test]
fn test_pattern_matching_does_not_refine_expr() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    let src = r#"
    declare let expr: number | string;
    let name = match (expr) {
        x is number -> expr + 1,
        x is string -> "bar"
    };
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
    declare let expr: number | string;
    let name = match (expr) {
        x is number -> "foo",
        x is string -> "bar",
        x is boolean -> "baz"
    };
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
    declare let array: Array<number>;
    let result = match (array) {
        [] -> 0,
        [a] -> a,
        [a, b] -> a + b,
        [_, _, ...rest] -> rest
    };
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
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
    declare let action: {type: "insert", key: string, value: string} | {type: "delete", key: string};
    let key = match (action) {
        {type: "insert", key, value} -> key,
        {type: "delete", key} -> key
    };
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("key").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string | string"#);

    Ok(())
}

#[test]
fn member_access_on_union() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: allow trailing `,` when doing pattern matching
    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let obj: {a: number, b: string} | {b: boolean};
    let b = obj.b;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string | boolean"#);

    Ok(())
}

#[test]
fn member_access_optional_property() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a?: number, b: string};
    let a = obj.a;
    let b = obj.b;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | undefined"#);
    let t = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_object_destructuring_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    // TODO: add support for omitting fields in object patterns
    let src = r#"
    declare let obj: {a?: number, b: string, c: boolean};
    let {a, b, c} = obj;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | undefined"#);

    let t = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    let t = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_object_destructuring_assignment_with_rest() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a?: number, b: string, c: boolean};
    let {a, ...rest} = obj;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | undefined"#);
    let t = my_ctx.values.get("rest").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"{b: string, c: boolean}"#);

    Ok(())
}

#[test]
fn test_object_nested_destructuring_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let obj: {a: {b: {c: string}}};
    let {a: {b: {c}}} = obj;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    assert_eq!(my_ctx.values.get("a"), None);
    assert_eq!(my_ctx.values.get("b"), None);

    Ok(())
}

#[test]
fn test_tuple_destrcuturing_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, string, boolean];
    let [a, b, c] = tuple;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    let t = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_tuple_destructuring_assignment_with_rest() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, string, boolean];
    let [a, ...tuple_rest] = tuple;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);
    let t = my_ctx.values.get("tuple_rest").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"[string, boolean]"#);

    Ok(())
}

#[test]
fn test_array_destructuring_assignment_with_rest() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<string>;
    let [a, ...array_rest] = array;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string | undefined"#);
    let t = my_ctx.values.get("array_rest").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"Array<string>"#);

    Ok(())
}

#[test]
fn test_tuple_nested_destrcuturing_assignment() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple: [number, [string, [boolean]]];
    let [_, [_, [c]]] = tuple;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_explicit_type_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = (x) => x;
    let x = identity<number>(5);
    let y = identity<string>("hello");
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);
    let t = my_ctx.values.get("y").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_explicit_type_params_type_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let identity = (x) => x;
    identity<number>("hello");
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
    let identity = (x) => x;
    identity<number, string>(5);
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
    let identity = <T extends number | string>(x: T): T => x;
    let x = identity(5);
    let y = identity("hello");
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("identity").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"<T:number | string>(x: T) => T"#
    );
    let t = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"5"#);
    let t = my_ctx.values.get("y").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#""hello""#);

    Ok(())
}

#[test]
fn test_mix_explicit_implicit_type_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let fst = <B>(a, b: B) => a;
    let snd = <B>(a, b: B): B => b;
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("fst").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<B, A>(a: A, b: B) => A"#);
    let t = my_ctx.values.get("snd").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"<B, A>(a: A, b: B) => B"#);

    Ok(())
}

#[test]
fn test_duplicate_type_param_names_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let fst = <T, T>(a: T, b: T): T => a;
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
    let identity = <T extends number | string>(x: T): T => x;
    identity(true);
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
    let identity: <T extends number | string>(x: T) => T = (x) => x;
    let x = identity<number>(5);
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("identity").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"<T:number | string>(x: T) => T"#
    );
    let t = my_ctx.values.get("x").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn test_type_ann_func_with_type_constraint_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let id1 = <T extends number | string>(x: T): T => x;
    let id2: <T extends boolean>(x: T) => T = id1;
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
    declare let foo: (callback: <T extends number>(x: T) => T) => boolean;
    let identity = <T extends number | string>(x: T): T => x;
    let result = foo(identity);
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"boolean"#);

    Ok(())
}

#[test]
fn test_callback_with_type_param_subtyping_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let foo: (callback: <T extends number | string>(x: T) => T) => boolean;
    let identity = <T extends number>(x: T): T => x;
    let result = foo(identity);
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
    let foo = (): string => "hello";
    let result = foo();
    "#;
    let mut program = parse(src).unwrap();
    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("foo").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"() => string"#);
    let t = my_ctx.values.get("result").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_return_value_is_not_subtype_of_return_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = (): number => "hello";
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
    type Point = {x: number, y: number};
    let p: Point = {x: 5, y: 10};
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("p").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"{x: number, y: number}"#);

    Ok(())
}

#[test]
fn type_alias_with_params_with_destructuring() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T};
    let node: Node<string> = {value: "hello"};
    let {value} = node;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("node").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"{value: string}"#);
    let t = my_ctx.values.get("value").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn type_alias_with_params_with_member_access() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T};
    let node: Node<string> = {value: "hello"};
    let value = node.value;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("node").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"{value: string}"#);
    let t = my_ctx.values.get("value").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn type_alias_with_params_with_computed_member_access() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T};
    let node: Node<string> = {value: "hello"};
    let key = "value";
    let value = node[key];
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("node").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"{value: string}"#);
    let t = my_ctx.values.get("value").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn instantiate_type_alias_with_too_many_type_args() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    type Node<T> = {value: T};
    let node: Node<string, number> = {value: "hello"};
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
    type Point = {x: number, y: number};
    let p: Point<number> = {x: 5, y: 10};
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
    declare let tuple_union: [number, number] | [string];
    declare let object_union: {x: number, y: number} | {x: string};
    let elem = tuple_union[0];
    let x1 = object_union.x;
    let key = "x";
    let x2 = object_union[key];
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("elem").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | string"#);
    let t = my_ctx.values.get("x1").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | string"#);
    let t = my_ctx.values.get("x2").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | string"#);

    Ok(())
}

#[test]
fn maybe_property_accesses_on_unions() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string];
    declare let object_union: {x: number, y: number} | {x: string};
    let maybe_elem = tuple_union[1];
    let maybe_y = object_union.y;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("maybe_elem").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | undefined"#);
    let t = my_ctx.values.get("maybe_y").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number | undefined"#);

    Ok(())
}

#[test]
#[ignore]
fn destructuring_unions() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let tuple_union: [number, number] | [string];
    declare let object_union: {x: number, y: number} | {x: string};
    let [fst, snd] = tuple_union;
    let {x, y} = object_union;
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
    declare let tuple_union: [number, number] | [string];
    let elem = tuple_union[2];
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
    declare let object_union: {x: number, y: number} | {x: string};
    let z = object_union.z;
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property z on object".to_string()
        ))
    );

    Ok(())
}

#[test]
fn methods_on_arrays() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let num_array: Array<number> = [];
    num_array.push(5);
    let str_array: Array<string> = [];
    str_array.push("hello");

    let len = str_array.length;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;
    let t = my_ctx.values.get("len").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn properties_on_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let tuple: [number, string] = [5, "hello"];
    let len = tuple.length;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("len").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
#[ignore]
fn set_array_element() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    declare let array: Array<number>;
    array[0] = 5;
    array[1] = 10;
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
    declare let tuple: [number, number];
    tuple[0] = 5;
    tuple[1] = 10;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("len").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"number"#);

    Ok(())
}

#[test]
fn methods_on_arrays_incorrect_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let num_array: Array<number> = [];
    num_array.push("hello");
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
    let a: unknown = 5;
    let b: unknown = "hello";
    let c: unknown = true;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("a").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"unknown"#);
    let t = my_ctx.values.get("b").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"unknown"#);
    let t = my_ctx.values.get("c").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"unknown"#);

    Ok(())
}

#[test]
fn test_unknown_assignment_error() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let a: unknown = 5;
    let b: number = a;
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unknown != number".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_param_explicit_unknown_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let add = <T extends unknown>(a: T, b: T): T => {
        return a + b;
    };
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unknown != number".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_type_param_implicit_unknown_constraint() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let add = <T>(a: T, b: T): T => {
        return a + b;
    };
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "type mismatch: unknown != number".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_optional_function_params() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = (a: number, b?: number): number => {
        return a;
    };
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"(a: number, b?: number) => number"#
    );

    Ok(())
}

#[test]
fn test_func_param_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = ({ a: x, b }: { a: number, b: string }) => {
        return x;
    };
    let bar = ([a, b]: [number, string]) => {
        return b;
    };
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"({a, b}: {a: number, b: string}) => number"#
    );
    let t = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"([a, b]: [number, string]) => string"#
    );

    Ok(())
}

#[test]
fn test_func_param_object_rest_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = ({ a, ...rest }: { a: number, b: string }) => {
        return rest.b;
    };
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("foo").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"({a, ...rest}: {a: number, b: string}) => string"#
    );

    Ok(())
}

#[test]
fn test_func_param_object_multiple_rest_patterns() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"
    let foo = ({ a, ...rest1, ...rest2 }: { a: number, b: string }) => {
        return rest.b;
    };
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
    let bar = ([a, ...rest]: [number, string, boolean]) => {
        return rest[1];
    };
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("bar").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"([a, ...rest]: [number, string, boolean]) => boolean"#
    );

    Ok(())
}

#[test]
fn test_index_access_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b?: number, [key: string]: boolean};
    type A = Foo["a"];
    type B = Foo["b"];
    type C = Foo["c"];
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

// TODO: make this test pass
#[test]
#[ignore]
fn test_index_access_type_using_string_as_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b: number, c: boolean};
    type T = Foo[string];
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

// TODO: make this test pass
#[test]
#[ignore]
fn test_index_access_type_using_number_as_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {a: string, b: number, [key: number]: boolean]};
    type T = Foo[number];
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
    type Foo = {a: string, b?: number};
    type C = Foo["c"];
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("C").unwrap();
    let result = expand_type(&mut arena, &my_ctx, scheme.t);

    // TODO: check that the index access is valid where it's inferred
    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property c in object {a: string, b?: number}".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_index_access_type_missing_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {[key: number]: string};
    type C = Foo["c"];
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("C").unwrap();
    let result = expand_type(&mut arena, &my_ctx, scheme.t);

    // TODO: check that the index access is valid where it's inferred
    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Couldn't find property c in object {[key: number]: string}".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_index_access_type_number_indexer() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = {[key: number]: string};
    type T = Foo[1];
    let t: T = "hello";
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("T").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"string | undefined"#);

    let t = my_ctx.values.get("t").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string | undefined"#);

    Ok(())
}

#[test]
fn test_index_access_type_on_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean];
    type T = Foo[1];
    let t: T = "hello";
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("t").unwrap();
    assert_eq!(arena[*t].as_string(&arena), r#"string"#);

    Ok(())
}

#[test]
fn test_index_access_type_on_tuple_with_number_key() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean];
    type T = Foo[number];
    let t: T = "hello";
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let t = my_ctx.values.get("t").unwrap();
    assert_eq!(
        arena[*t].as_string(&arena),
        r#"number | string | boolean | undefined"#
    );

    Ok(())
}

#[test]
fn test_index_access_out_of_bounds_on_tuple() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean];
    type T = Foo[3];
    let t: T = "hello";
    "#;
    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError(
            "Index 3 out of bounds for tuple [number, string, boolean]".to_string()
        ))
    );

    Ok(())
}

#[test]
fn test_index_access_not_usize() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    type Foo = [number, string, boolean];
    type T = Foo[1.5];
    let t: T = "hello";
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
    let foo = {a: "hello", b: 5, c: true};
    type Foo = typeof foo;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"{a: "hello", b: 5, c: true}"#);

    Ok(())
}

#[test]
fn test_keyof() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();

    let src = r#"   
    let foo = {a: "hello", b: 5, c: true};
    type Foo = keyof typeof foo;
    let bar = {a: "hello"};
    type Bar = keyof typeof bar;
    let baz = {};
    type Baz = keyof typeof baz;
    "#;
    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    let scheme = my_ctx.schemes.get("Foo").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""a" | "b" | "c""#);

    let scheme = my_ctx.schemes.get("Bar").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#""a""#);

    let scheme = my_ctx.schemes.get("Baz").unwrap();
    let t = expand_type(&mut arena, &my_ctx, scheme.t)?;
    assert_eq!(arena[t].as_string(&arena), r#"never"#);

    Ok(())
}

#[test]
fn test_mutually_recursive_type() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();
    let src = r#"
    type A = {
        a: number,
        b: B | null,
    };
    
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

    let foo: Foo = {a: 5, b: "hello"};
    "#;

    let mut program = parse(src).unwrap();

    infer_program(&mut arena, &mut program, &mut my_ctx)?;

    Ok(())
}

#[test]
fn test_type_alias_with_undefined_def() -> Result<(), Errors> {
    let (mut arena, mut my_ctx) = test_env();
    let src = r#"
    type A = B;
    "#;

    let mut program = parse(src).unwrap();

    let result = infer_program(&mut arena, &mut program, &mut my_ctx);

    assert_eq!(
        result,
        Err(Errors::InferenceError("B is not in scope".to_string()))
    );

    Ok(())
}
