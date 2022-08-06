use chumsky::prelude::*;

use crochet_ast::{Program, Statement};
use crochet_codegen::*;
use crochet_infer::*;
use crochet_parser::parser;

fn infer(input: &str) -> String {
    let mut ctx = crochet_infer::Context::default();
    let prog = parser().parse(input).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = match stmt {
        Statement::Expr { expr, .. } => infer_expr(&mut ctx, expr),
        _ => Err(String::from("We can't infer decls yet")),
    };
    format!("{}", result.unwrap())
}

fn infer_prog(src: &str) -> (Program, crochet_infer::Context) {
    let result = parser().parse(src);
    let prog = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };
    // println!("prog = {:#?}", &prog);
    // let prog = token_parser(&spans).parse(tokens).unwrap();
    let ctx = crochet_infer::infer_prog(&prog).unwrap();

    (prog, ctx)
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5"), "5");
}

#[test]
fn infer_lam() {
    insta::assert_snapshot!(infer("(x) => x"), @"<t0>(t0) => t0");
}

#[test]
fn infer_let_inside_function() {
    assert_eq!(infer("() => {let x = 5; x}"), "() => 5");
}

#[test]
fn infer_op() {
    assert_eq!(infer("5 + 10"), "number");
}

#[test]
fn infer_fn_param() {
    assert_eq!(
        infer("(f, x) => f(x) + x"),
        "((number) => number, number) => number"
    );
}

#[test]
fn infer_fn_with_param_types() {
    assert_eq!(infer("(a: 5, b: 10) => a + b"), "(5, 10) => number");
}

#[test]
fn infer_let_fn_with_param_types() {
    let src = "let add = (a: 5, b: 10) => a + b";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("add").unwrap());
    assert_eq!(result, "(5, 10) => number");
}

#[test]
#[should_panic = "Unification failure"]
fn infer_fn_with_incorrect_param_types() {
    infer("(a: string, b: boolean) => a + b");
}

#[test]
#[should_panic = "Unification failure"]
fn infer_let_fn_with_incorrect_param_types() {
    let src = "let add = (a: string, b: boolean) => a + b";
    infer_prog(src);
}

#[test]
fn infer_fn_param_used_with_multiple_other_params() {
    insta::assert_snapshot!(
        infer("(f, x, y) => f(x) + f(y)"),
        @"<t0>((t0) => number, t0, t0) => number"
    );
}

#[test]
fn infer_i_combinator() {
    let (_, ctx) = infer_prog("let I = (x) => x");
    let result = format!("{}", ctx.values.get("I").unwrap());
    insta::assert_snapshot!(result, @"<t0>(t0) => t0");
}

#[test]
fn infer_k_combinator_not_curried() {
    let (program, ctx) = infer_prog("let K = (x, y) => x");
    let result = format!("{}", ctx.values.get("K").unwrap());
    insta::assert_snapshot!(result, @"<t0, t1>(t0, t1) => t0");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const K: <A, B>(x: A, y: B) => A;\n");
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, ctx) = infer_prog("let S = (f, g, x) => f(x, g(x))");
    let result = format!("{}", ctx.values.get("S").unwrap());
    insta::assert_snapshot!(result, @"<t0, t1, t2>((t0, t1) => t2, (t0) => t1, t0) => t2");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, ctx) = infer_prog("let K = (x) => (y) => x");
    let result = format!("{}", ctx.values.get("K").unwrap());
    insta::assert_snapshot!(result, @"<t0, t1>(t0) => (t1) => t0");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, ctx) = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x))");
    let result = format!("{}", ctx.values.get("S").unwrap());
    insta::assert_snapshot!(
        result,
        @"<t0, t1, t2>((t0) => (t1) => t2) => ((t0) => t1) => (t0) => t2"
    );
}

#[test]
fn infer_skk() {
    let src = r#"
    let S = (f) => (g) => (x) => f(x)(g(x))
    let K = (x) => (y) => x
    let I = S(K)(K)
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("I").unwrap());
    insta::assert_snapshot!(result, @"<t0>(t0) => t0");
}

#[test]
fn infer_adding_variables() {
    let src = r#"
    let x = 5
    let y = 10
    let z = x + y
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("z").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_decl() {
    let src = r#"
    let foo = (a, b) => a + b
    let bar = "hello"
    "#;
    let (program, ctx) = infer_prog(src);
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const foo: (a: number, b: number) => number;
    export declare const bar: "hello";
    "###);
}

#[test]
fn infer_with_subtyping() {
    let src = r#"
    let foo = (a, b) => a + b
    let bar = foo(5, 10)
    "#;
    let (program, ctx) = infer_prog(src);
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const foo: (a: number, b: number) => number;
    export declare const bar: number;
    "###);
}

#[test]
fn infer_if_else_without_widening() {
    let (_, ctx) = infer_prog("let x = if true { 5 } else { 5 }");
    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_if_else_with_widening() {
    let (_, ctx) = infer_prog("let x = if (true) { 5 } else { 10 }");
    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "5 | 10");
}

#[test]
fn infer_only_if_must_be_undefined() {
    let (_, ctx) = infer_prog("let x = if true { let a = 5; }");
    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "undefined");
}

#[test]
#[should_panic = "Consequent for 'if' without 'else' must not return a value"]
fn infer_only_if_must_be_undefined_error() {
    infer_prog("let x = if true { let a = 5; a }");
}

#[test]
fn infer_if_else_with_widening_of_top_level_vars() {
    let src = r#"
    let a = 5
    let b = 10
    let x = if true { a } else { b }
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "5 | 10");
}

#[test]
fn infer_if_else_with_multiple_widenings() {
    let src = r#"
    let x = if true { 5 } else if false { 10 } else { 15 }
    "#;
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "5 | 10 | 15");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @r###"
    export declare const x: 5 | 10 | 15;
    "###);
}

#[test]
fn infer_equal_with_numbers() {
    let (_, ctx) = infer_prog("let cond = 5 == 10");
    let result = format!("{}", ctx.values.get("cond").unwrap());
    assert_eq!(result, "boolean");
}

// TODO: update definition of "!=" to be <A>(A, A) => boolean
#[test]
#[ignore]
fn infer_not_equal_with_variables() {
    let (_, ctx) = infer_prog("let neq = (a, b) => a != b");
    let result = format!("{}", ctx.values.get("neq").unwrap());
    insta::assert_snapshot!(result, @"<t0>(t0, t0) => boolean");
}

#[test]
fn infer_inequalities() {
    let src = r###"
    let lt = (a, b) => a < b
    let lte = (a, b) => a <= b
    let gt = (a, b) => a > b
    let gte = (a, b) => a >= b
    "###;
    let (_, ctx) = infer_prog(src);
    assert_eq!(
        format!("{}", ctx.values.get("lt").unwrap()),
        "(number, number) => boolean"
    );
    assert_eq!(
        format!("{}", ctx.values.get("lte").unwrap()),
        "(number, number) => boolean"
    );
    assert_eq!(
        format!("{}", ctx.values.get("gt").unwrap()),
        "(number, number) => boolean"
    );
    assert_eq!(
        format!("{}", ctx.values.get("gte").unwrap()),
        "(number, number) => boolean"
    );
}

#[test]
fn infer_let_rec_until() {
    let src = "let rec until = (p, f, x) => if p(x) { x } else { until(p, f, f(x)) }";
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("until").unwrap());
    insta::assert_snapshot!(result, @"<t0>((t0) => boolean, (t0) => t0, t0) => t0");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const until: <A>(p: (arg0: A) => boolean, f: (arg0: A) => A, x: A) => A;\n");
}

#[test]
fn infer_fib() {
    let src = r###"
    let rec fib = (n) => if n == 0 {
        0
    } else if n == 1 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
    "###;

    let (_, ctx) = infer_prog(src);
    let fib_type = ctx.values.get("fib").unwrap();
    assert_eq!(format!("{}", fib_type), "(number) => number");
}

#[test]
fn infer_obj() {
    let src = r#"
    let point = {x: 5, y: 10, msg: "Hello, world!"}
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("point").unwrap());
    assert_eq!(result, "{x: 5, y: 10, msg: \"Hello, world!\"}");
}

#[test]
fn infer_async() {
    let src = "let foo = async () => 10";
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("foo").unwrap());
    assert_eq!(result, "() => Promise<10>");
}

#[test]
fn infer_async_math() {
    let src = "let add = async (a, b) => await a() + await b()";
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("add").unwrap());
    assert_eq!(
        result,
        "(() => Promise<number>, () => Promise<number>) => Promise<number>"
    );

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const add: (a: () => Promise<number>, b: () => Promise<number>) => Promise<number>;\n");
}

#[test]
fn codegen_let_rec() {
    let src = "let rec f = () => f()";
    let (program, ctx) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @"export const f = ()=>f();
");

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const f: <A>() => A;\n");
}

#[test]
fn codegen_if_else() {
    let src = r#"
    let cond = true
    let result = if cond { 5 } else { 5 }
    "#;
    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const cond = true;
    let $temp_0;
    if (cond) {
        $temp_0 = 5;
    } else {
        $temp_0 = 5;
    }
    export const result = $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const cond: true;
    export declare const result: 5;
    "###);
}

#[test]
fn codegen_object() {
    let src = "let point = {x: 5, y: 10}";
    let (program, ctx) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @r###"
    export const point = {
        x: 5,
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const point: {
        x: 5;
        y: 10;
    };
    "###);
}

#[test]
fn codegen_async_math() {
    let src = "let add = async (a, b) => await a() + await b()";
    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @"export const add = async (a, b)=>await a() + await b();
");

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const add: (a: () => Promise<number>, b: () => Promise<number>) => Promise<number>;\n");
}

#[test]
fn infer_let_decl_with_type_ann() {
    let src = "let x: number = 10";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
// TODO: improve this error by checking the flags on the types before reporting
// "Unification failure".
#[should_panic = "called `Result::unwrap()` on an `Err` value: \"Unification failure\""]
fn infer_let_decl_with_incorrect_type_ann() {
    let src = "let x: string = 10";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_declare() {
    let src = "declare let x: number";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_expr_using_declared_var() {
    let src = r#"
    declare let x: number
    let y = x + 5
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("y").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn() {
    let src = r#"
    declare let add: (a: number, b: number) => number
    let sum = add(5, 10)
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("sum").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn_with_obj_param() {
    let src = r#"
    declare let mag: (p: {x: number, y: number}) => number
    let result = mag({x: 5, y: 10})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn calling_a_fn_with_an_obj_subtype() {
    let src = r#"
    declare let mag: (p: {x: number, y: number}) => number
    let result = mag({x: 5, y: 10, z: 15})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
#[should_panic = "Unification failure"]
fn calling_a_fn_with_an_obj_missing_a_property() {
    let src = r#"
    declare let mag: (p: {x: number, y: number}) => number
    let result = mag({x: 5})
    "#;
    infer_prog(src);
}

#[test]
fn infer_literal_tuple() {
    let src = r#"let tuple = [1, "two", true]"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("tuple").unwrap());
    assert_eq!(result, "[1, \"two\", true]");
}

#[test]
fn infer_tuple_with_type_annotation() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", true]"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("tuple").unwrap());
    assert_eq!(result, "[number, string, boolean]");
}

#[test]
fn infer_tuple_with_type_annotation_and_extra_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", true, "ignored"]"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("tuple").unwrap());
    assert_eq!(result, "[number, string, boolean]");
}

#[test]
// TODO: improve this error by checking the flags on the types before reporting
// "Unification failure".
#[should_panic = "called `Result::unwrap()` on an `Err` value: \"Unification failure\""]
fn infer_tuple_with_type_annotation_and_incorrect_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", 3]"#;
    infer_prog(src);
}

#[test]
#[should_panic = "not enough elements to unpack"]
fn infer_tuple_with_not_enough_elements() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two"]"#;
    infer_prog(src);
}

#[test]
fn infer_var_with_union_type_annotation() {
    let src = r#"
    let a: number | string = 5
    let b: number | string = "ten"
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "number | string");
    let b = format!("{}", ctx.values.get("b").unwrap());
    assert_eq!(b, "number | string");
}

#[test]
fn infer_widen_tuple_return() {
    let src = r#"
    let result = (cond) => {
        if cond {
            [1, 2]
        } else {
            [true, false]
        }
    }
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "(boolean) => [1, 2] | [true, false]");
}

#[ignore]
#[test]
fn infer_widen_tuples_with_type_annotations() {
    let src = r#"
    let result = (cond) => {
        if cond {
            let x: [number, number] = [1, 2] in
            x
        } else {
            let y: [boolean, boolean] = [true, false] in
            y
        }
    }
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(
        result,
        "(boolean) => [number | number] | [boolean | boolean]"
    );
}

#[test]
fn infer_member_access() {
    let src = r#"
    let point = {x: 5, y: 10}
    let x = point.x
    let y = point.y
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(x, "5");
    let y = format!("{}", ctx.values.get("y").unwrap());
    assert_eq!(y, "10");
}

#[test]
#[should_panic = "Record literal doesn't contain property"]
fn infer_incorrect_member_access() {
    let src = r#"
    let point = {x: 5, y: 10}
    let z = point.z
    "#;
    infer_prog(src);
}

#[test]
fn infer_member_access_on_obj_lit() {
    let src = r#"let x = {x: 5, y: 10}.x"#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(x, "5");
}

#[test]
fn infer_fn_using_type_decl() {
    let src = r#"
    type Point = {x: number, y: number}
    let mag = (p: Point) => p.x * p.x + p.y * p.y
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("mag").unwrap());
    assert_eq!(result, "(Point) => number");
}

#[test]
fn infer_react_component() {
    let src = r#"
    type Props = {name: string}
    let Foo = (props: Props) => {
        <div>Hello, world</div>
    }
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("Foo").unwrap());
    assert_eq!(result, "(Props) => JSXElement");
}

#[test]
fn codegen_code_with_type_delcarations() {
    let src = r#"
    type Point = {x: number, y: number}
    let point: Point = {x: 5, y: 10}
    "#;
    let (program, ctx) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @r###"
    ;
    export const point = {
        x: 5,
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    type Point = {
        x: number;
        y: number;
    };
    export declare const point: Point;
    "###);
}

#[test]
fn infer_mem_access_with_optional_prop() {
    let src = r#"
    declare let point: {x?: number, y: number}
    let x = point.x
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "number | undefined");
}

#[test]
#[should_panic = "not yet implemented"]
fn recursive_mem_access_on_optional_prop_should_fail() {
    let src = r#"
    declare let foo: {a?: {b?: number}}
    let b = foo.a.b
    "#;
    infer_prog(src);
}

#[test]
fn infer_assigning_to_obj_with_optional_props() {
    let src = r#"
    let p: {x?: number, y: number} = {y: 10}
    let x = p.x
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(x, "number | undefined");
}

#[test]
fn infer_assigning_an_obj_lit_with_extra_props() {
    let src = r#"
    let point: {x: number, y: number} = {x: 5, y: 10, z: 15}
    "#;
    let (_, ctx) = infer_prog(src);

    let point = format!("{}", ctx.values.get("point").unwrap());
    assert_eq!(point, "{x: number, y: number}");
}

#[test]
fn infer_function_overloading() {
    let src = r#"
    declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string)
    let num = add(5, 10)
    let str = add("hello, ", "world")
    "#;
    let (_, ctx) = infer_prog(src);

    let num_result = format!("{}", ctx.values.get("num").unwrap());
    assert_eq!(num_result, "number");
    let str_result = format!("{}", ctx.values.get("str").unwrap());
    assert_eq!(str_result, "string");
}

#[test]
#[should_panic = "Couldn't unify lambda with intersection"]
fn infer_function_overloading_with_incorrect_args() {
    let src = r#"
    declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string)
    let bool = add(true, false)
    "#;
    infer_prog(src);
}

#[test]
fn codegen_object_type_with_optional_property() {
    let src = r#"
    type Point = {x?: number, y: number}
    let point: Point = {y: 10}
    "#;
    let (program, ctx) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @r###"
    ;
    export const point = {
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    type Point = {
        x?: number;
        y: number;
    };
    export declare const point: Point;
    "###);
}

#[test]
fn infer_nested_block() {
    let src = "let result = {let sum = {let x = 5; let y = 10; x + y}; sum}";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_block_with_multiple_non_let_lines() {
    // TODO:
    // instead of inferring that `x` should be a `number` when we see
    // `x + 0`, we should instead infer that it's a `subtype of number`.
    // that way when we reconcile it with the other inferred type of `x`
    // which is `5`, the final inferred type will be `5`.
    let src = "let result = {let x = 5; x + 0; x}";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn codegen_block_with_multiple_non_let_lines() {
    let src = "let result = {let x = 5; x + 0; x}";
    let (program, ctx) = infer_prog(src);
    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @r###"
    let $temp_0;
    {
        const x = 5;
        x + 0;
        $temp_0 = x;
    }export const result = $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const result: 5;
");
}

#[test]
fn infer_type_alias_with_param() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let foo: Foo<string>
    let bar = foo.bar
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("bar").unwrap());
    assert_eq!(result, "string");
}

#[test]
fn infer_fn_param_with_type_alias_with_param() {
    let src = r#"
    type Foo<T> = {bar: T}
    let get_bar = (foo: Foo<string>) => foo.bar
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("bar").unwrap());
    assert_eq!(result, "string");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_2() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: <T>(foo: Foo<T>) => T
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("get_bar").unwrap());
    // TODO: normalize the scheme before inserting it into the context
    insta::assert_snapshot!(result, @"<t7>(Foo<t7>) => t7");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_3() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: <T>(foo: Foo<T>) => T
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("bar").unwrap());
    assert_eq!(result, "\"hello\"");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_4() {
    let src = r#"
    type Foo<T> = {bar: T}
    let get_bar = <T>(foo: Foo<T>) => foo.bar
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("get_bar").unwrap());
    insta::assert_snapshot!(result, @"<t0>(Foo<t0>) => t0");

    let result = format!("{}", ctx.values.get("bar").unwrap());
    assert_eq!(result, "\"hello\"");
}

#[test]
fn infer_destructure_all_object_properties() {
    let src = r#"
    let point = {x: 5, y: 10}
    let {x, y} = point
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "5");
    let result = format!("{}", ctx.values.get("y").unwrap());
    assert_eq!(result, "10");
}

#[test]
fn infer_destructure_some_object_properties() {
    let src = r#"
    let point = {x: 5, y: 10}
    let {x} = point
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_destructure_some_object_properties_with_renaming() {
    let src = r#"
    let point = {x: 5, y: 10}
    let {x: a} = point
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_destructure_object_with_type_alias() {
    let src = r#"
    type Point = {x: number, y: number}
    let point: Point = {x: 5, y: 10}
    let {x, y} = point
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_inside_fn() {
    let src = r#"
    type FooBar = {foo: number, bar: string}
    let get_foo = (x: FooBar) => {
        let {foo, bar} = x;
        foo
    }
    let foo = get_foo({foo: 5, bar: "hello"})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("foo").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_inside_fn_2() {
    let src = r#"
    type FooBar = {foo: number, bar: string}
    let get_foo = (x: FooBar) => {
        let {foo, bar} = x;
        foo
    }
    let foo = get_foo({foo: 5, bar: "hello"})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("foo").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_param() {
    let src = r#"
    let foo = ({a, b}: {a: string, b: number}) => a
    let a = foo({a: "hello", b: 5})
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "string");
}

#[test]
fn infer_destructure_object_param_2() {
    let src = r#"
    let foo = ({a, b}: {a: string, b: number}) => {
        {a: a, b: b}
    }
    let {a, b} = foo({a: "hello", b: 5})
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "string");

    let b = format!("{}", ctx.values.get("b").unwrap());
    assert_eq!(b, "number");
}

#[test]
fn return_an_object() {
    let src = r#"
    let foo = () => {
        {a: "hello", b: 5}
    }
    let {a, b} = foo()
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "\"hello\"");

    let b = format!("{}", ctx.values.get("b").unwrap());
    assert_eq!(b, "5");
}

#[test]
fn object_property_shorthand() {
    let src = r#"
    let a = "hello"
    let b = 5
    let c = {a, b}
    "#;
    let (_, ctx) = infer_prog(src);

    let c = format!("{}", ctx.values.get("c").unwrap());
    assert_eq!(c, "{a: \"hello\", b: 5}");
}

#[test]
fn infer_destructuring_with_optional_properties() {
    let src = r#"
    let p: {x?: number, y: number} = {y: 10}
    let {x, y: _} = p
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(x, "number | undefined");
    assert_eq!(ctx.values.get("y"), None);
}

#[test]
fn infer_destructure_tuple() {
    let src = r#"
    let [a, b] = ["hello", 5]
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "\"hello\"");

    let b = format!("{}", ctx.values.get("b").unwrap());
    assert_eq!(b, "5");
}

#[test]
#[should_panic = "not enough elements to unpack"]
fn infer_destructure_tuple_too_many_identifiers() {
    let src = r#"
    let [a, b, c] = ["hello", 5]
    "#;
    infer_prog(src);
}

#[test]
fn infer_destructure_tuple_extra_values_are_ignored() {
    let src = r#"
    let [a, b] = ["hello", 5, true]
    "#;
    infer_prog(src);
}

#[test]
fn lam_param_tuple() {
    let src = r#"
    let foo = (bar: [string, number]) => bar
    let [a, b] = foo(["hello", 5])
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "string");

    let b = format!("{}", ctx.values.get("b").unwrap());
    assert_eq!(b, "number");
}

#[test]
fn destructure_lam_param_tuple() {
    let src = r#"
    let foo = ([a, b]: [string, number]) => [a, b]
    let [a, b] = foo(["hello", 5])
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.values.get("a").unwrap());
    assert_eq!(a, "string");

    let b = format!("{}", ctx.values.get("b").unwrap());
    assert_eq!(b, "number");
}

#[test]
fn infer_jsx() {
    let src = r#"
    type JSXElement = {}
    let point = {x: 5, y: 10}
    let msg = "world"
    let elem = <div point={point} id="point">Hello, {msg}</div>
    "#;
    let (_, ctx) = infer_prog(src);

    let elem = format!("{}", ctx.values.get("elem").unwrap());
    assert_eq!(elem, "JSXElement");
}

#[test]
#[should_panic = "Unification failure"]
fn incorrect_args() {
    let src = r#"
    let add = (a, b) => a + b
    add("hello", "world")
    "#;
    infer_prog(src);
}

#[test]
fn return_empty() {
    let src = r#"
    let foo = () => {}
    "#;
    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.values.get("foo").unwrap()),
        "() => undefined"
    );
}

#[test]
fn return_empty_with_body() {
    let src = r#"
    let foo = () => {
        let a = 5;
    }
    "#;
    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.values.get("foo").unwrap()),
        "() => undefined"
    );
}

#[test]
fn infer_if_let() {
    let src = r#"
    let p = {x: 5, y: 10}
    if let {x, y} = p {
        x + y;
    }
    "#;

    let (_, ctx) = infer_prog(src);

    assert_eq!(format!("{}", ctx.values.get("p").unwrap()), "{x: 5, y: 10}");
    // Ensures we aren't polluting the outside context
    assert!(ctx.values.get("x").is_none());
    assert!(ctx.values.get("y").is_none());
}

#[test]
fn infer_if_let_with_is() {
    let src = r#"
    declare let b: string | number
    if let a is string = b {
        a;
    }
    "#;

    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.values.get("b").unwrap()),
        "string | number"
    );
    // Ensures we aren't polluting the outside context
    assert!(ctx.values.get("a").is_none());
}

#[test]
fn codegen_if_let() {
    let src = r#"
    let p = {x: 5, y: 10}
    if let {x, y} = p {
        x + y;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const p = {
        x: 5,
        y: 10
    };
    let $temp_0;
    const $temp_1 = p;
    {
        const { x , y  } = $temp_1;
        x + y;
        $temp_0 = undefined;
    }$temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const p: {
        x: 5;
        y: 10;
    };
    ;
    "###);
}

#[test]
fn codegen_if_let_with_rename() {
    let src = r#"
    let p = {x: 5, y: 10}
    if let {x: a, y: b} = p {
        a + b;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const p = {
        x: 5,
        y: 10
    };
    let $temp_0;
    const $temp_1 = p;
    {
        const { x: a , y: b  } = $temp_1;
        a + b;
        $temp_0 = undefined;
    }$temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const p: {
        x: 5;
        y: 10;
    };
    ;
    "###);
}

#[test]
fn infer_if_let_with_type_error() {
    let src = r#"
    let p = {x: "hello", y: "world"}
    if let {x, y} = p {
        x + y;
    }
    "#;

    infer_prog(src);
}

#[test]
fn infer_if_let_refutable_pattern_obj() {
    let src = r#"
    let p = {x: 5, y: 10}
    if let {x: 5, y} = p {
        y;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    assert_eq!(format!("{}", ctx.values.get("p").unwrap()), "{x: 5, y: 10}");

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const p = {
        x: 5,
        y: 10
    };
    let $temp_0;
    const $temp_1 = p;
    if ($temp_1.x === 5) {
        const { y  } = $temp_1;
        y;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const p: {
        x: 5;
        y: 10;
    };
    ;
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_nested_obj() {
    let src = r#"
    let action = {type: "moveto", point: {x: 5, y: 10}}
    if let {type: "moveto", point: {x, y}} = action {
        x + y;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const action = {
        type: "moveto",
        point: {
            x: 5,
            y: 10
        }
    };
    let $temp_0;
    const $temp_1 = action;
    if ($temp_1.type === "moveto") {
        const { point: { x , y  }  } = $temp_1;
        x + y;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const action: {
        type: "moveto";
        point: {
            x: 5;
            y: 10;
        };
    };
    ;
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_with_disjoint_union() {
    let src = r#"
    type Point = {x: number, y: number}
    type Action = {type: "moveto", point: Point} | {type: "lineto", point: Point}
    declare let action: Action
    if let {type: "moveto", point: {x, y}} = action {
        x + y;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    ;
    ;
    ;
    let $temp_0;
    const $temp_1 = action;
    if ($temp_1.type === "moveto") {
        const { point: { x , y  }  } = $temp_1;
        x + y;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    type Point = {
        x: number;
        y: number;
    };
    type Action = {
        type: "moveto";
        point: Point;
    } | {
        type: "lineto";
        point: Point;
    };
    export declare const action: Action;
    ;
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_array() {
    let src = r#"
    let p = [5, 10]
    if let [5, y] = p {
        y;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    assert_eq!(format!("{}", ctx.values.get("p").unwrap()), "[5, 10]");

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const p = [
        5,
        10
    ];
    let $temp_0;
    const $temp_1 = p;
    if ($temp_1[0] === 5) {
        const [, y] = $temp_1;
        y;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const p: [5, 10];
    ;
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_nested_array() {
    let src = r#"
    let action = ["moveto", [5, 10]]
    if let ["moveto", [x, y]] = action {
        x + y;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const action = [
        "moveto",
        [
            5,
            10
        ]
    ];
    let $temp_0;
    const $temp_1 = action;
    if ($temp_1[0] === "moveto") {
        const [, [x, y]] = $temp_1;
        x + y;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const action: ["moveto", [5, 10]];
    ;
    "###);
}

#[test]
fn codegen_if_let_with_is_prim() {
    let src = r#"
    declare let b: string | number
    if let a is number = b {
        a + 5;
    }
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    ;
    let $temp_0;
    const $temp_1 = b;
    if (typeof $temp_1 === "number") {
        const a = $temp_1;
        a + 5;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const b: string | number;
    ;
    "###);
}

#[test]
fn codegen_if_let_with_is_class() {
    // NOTE: TypeScript treats classes as both types and values.
    // The type represents the type of an instance of the class.
    let src = r#"
    type Foo = {
        getNum: () => number,
    }
    type Bar = {
        getStr: () => string,
    }
    declare let foo: Foo
    let Foo = {
        constructor: () => foo,
    }
    declare let bar: Bar
    let Bar = {
        constructor: () => bar,
    }
    declare let b: Foo | Bar
    if let a is Foo = b {
        a.getNum() + 5;
    }
    "#;

    let result = parser().parse(src);
    let program = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    ;
    ;
    ;
    export const Foo = {
        constructor: ()=>foo
    };
    ;
    export const Bar = {
        constructor: ()=>bar
    };
    ;
    let $temp_0;
    const $temp_1 = b;
    if ($temp_1 instanceof Foo) {
        const a = $temp_1;
        a.getNum() + 5;
        $temp_0 = undefined;
    }
    $temp_0;
    "###);
}


#[test]
fn codegen_array() {
    let src = r#"
    let arr: string[] = ["hello", "world"]
    "#;

    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const arr = [
        "hello",
        "world"
    ];
    "###);

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const arr: string[];
");
}
