use chumsky::prelude::*;

use crochet::ast::{Program, Statement};
use crochet::codegen::*;
use crochet::infer::*;
use crochet::parser::parser;

fn infer(input: &str) -> String {
    let ctx = Context::default();
    let prog = parser().parse(input).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = match stmt {
        Statement::Expr { expr, .. } => infer_expr(&ctx, expr),
        _ => Err(String::from("We can't infer decls yet")),
    };
    format!("{}", result.unwrap())
}

fn infer_prog(src: &str) -> (Program, Context) {
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
    let ctx = crochet::infer::infer_prog(&prog).unwrap();

    (prog, ctx)
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5"), "5");
}

#[test]
fn infer_lam() {
    assert_eq!(infer("(x) => x"), "<A>(A) => A");
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
#[should_panic = "unification failed"]
fn infer_fn_with_incorrect_param_types() {
    infer("(a: string, b: boolean) => a + b");
}

#[test]
#[should_panic = "unification failed"]
fn infer_let_fn_with_incorrect_param_types() {
    let src = "let add = (a: string, b: boolean) => a + b";
    infer_prog(src);
}

#[test]
fn infer_fn_param_used_with_multiple_other_params() {
    assert_eq!(
        infer("(f, x, y) => f(x) + f(y)"),
        "<A>((A) => number, A, A) => number"
    );
}

#[test]
fn infer_i_combinator() {
    let (_, ctx) = infer_prog("let I = (x) => x");
    let result = format!("{}", ctx.values.get("I").unwrap());
    assert_eq!(result, "<A>(A) => A");
}

#[test]
fn infer_k_combinator_not_curried() {
    let (program, ctx) = infer_prog("let K = (x, y) => x");
    let result = format!("{}", ctx.values.get("K").unwrap());
    assert_eq!(result, "<A, B>(A, B) => A");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const K: <A, B>(x: A, y: B) => A;\n");
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, ctx) = infer_prog("let S = (f, g, x) => f(x, g(x))");
    let result = format!("{}", ctx.values.get("S").unwrap());
    assert_eq!(result, "<A, B, C>((A, B) => C, (A) => B, A) => C");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, ctx) = infer_prog("let K = (x) => (y) => x");
    let result = format!("{}", ctx.values.get("K").unwrap());
    assert_eq!(result, "<A, B>(A) => (B) => A");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, ctx) = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x))");
    let result = format!("{}", ctx.values.get("S").unwrap());
    assert_eq!(
        result,
        "<A, B, C>((A) => (B) => C) => ((A) => B) => (A) => C"
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
    assert_eq!(result, "<A>(A) => A");
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
    let (_, ctx) = infer_prog("let x = if (true) { 5 } else { 5 }");
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
fn infer_if_else_with_multiple_widenings() {
    let src = r#"
    let x = if (true) { 5 } else { 10 }
    let y = if (false) { x } else { 15 }
    "#;
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("y").unwrap());
    assert_eq!(result, "5 | 10 | 15");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @r###"
    export declare const x: 5 | 10;
    export declare const y: 5 | 10 | 15;
    "###);
}

#[test]
fn infer_equal_with_numbers() {
    let (_, ctx) = infer_prog("let cond = 5 == 10");
    let result = format!("{}", ctx.values.get("cond").unwrap());
    assert_eq!(result, "boolean");
}

#[test]
fn infer_not_equal_with_variables() {
    let (_, ctx) = infer_prog("let neq = (a, b) => a != b");
    let result = format!("{}", ctx.values.get("neq").unwrap());
    assert_eq!(result, "<A>(A, A) => boolean");
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
    let src = "let rec until = (p, f, x) => if (p(x)) { x } else { until(p, f, f(x)) }";
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.values.get("until").unwrap());
    assert_eq!(result, "<A>((A) => boolean, (A) => A, A) => A");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const until: <A>(p: (arg0: A) => boolean, f: (arg0: A) => A, x: A) => A;\n");
}

#[test]
fn infer_fib() {
    let src = r###"
    let rec fib = (n) => if (n == 0) {
        0
    } else {
        if (n == 1) {
            1
        } else {
            fib(n - 1) + fib(n - 2)
        }
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
    let result = if (cond) { 5 } else { 5 }
    "#;
    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);
    insta::assert_snapshot!(js, @r###"
    export const cond = true;
    export const result = (()=>{
        if (cond) {
            return 5;
        } else {
            return 5;
        }
    })();
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
#[should_panic = "value is not a subtype of decl's declared type"]
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
    declare let add: (number, number) => number
    let sum = add(5, 10)
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("sum").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn_with_obj_param() {
    let src = r#"
    declare let mag: ({x: number, y: number}) => number
    let result = mag({x: 5, y: 10})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn calling_a_fn_with_an_obj_subtype() {
    let src = r#"
    declare let mag: ({x: number, y: number}) => number
    let result = mag({x: 5, y: 10, z: 15})
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
#[should_panic = "unification failed"]
fn calling_a_fn_with_an_obj_missing_a_property() {
    let src = r#"
    declare let mag: ({x: number, y: number}) => number
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
#[should_panic = "value is not a subtype of decl's declared type"]
fn infer_tuple_with_type_annotation_and_incorrect_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", 3]"#;
    infer_prog(src);
}

#[test]
#[should_panic = "t1 contain at least the same number of elements as t2"]
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
        if (cond) {
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

// TODO: meditate on how to allow if-else to return a widened type
// if both branches return a frozen type.
// IDEA: we could make a copy of the type an unfreeze it.
// When do we care about the frozeness of a type:
// - assignment
// - application
// The problem is that the constraint solver doesn't know about these.
// We should consider tagging constraints when we infer them.  We need
// to do this anyways to handle subtyping of functions so that callbacks
// are handled correctly.
#[ignore]
#[test]
fn infer_widen_tuples_with_type_annotations() {
    let src = r#"
    let result = (cond) => {
        if (cond) {
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
    assert_eq!(result, "(boolean) => [number | number] | [boolean | boolean]");
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
fn infer_member_access_nested_obj() {
    let src = r#"
    let obj = {
        point: {x: 5, y: 10},
    }
    let x = obj.point.x
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.values.get("x").unwrap());
    assert_eq!(x, "5");
}

#[test]
fn infer_obj_type_based_on_member_access() {
    let src = r#"let foo = (point) => point.x + point.y"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("foo").unwrap());
    assert_eq!(result, "({x: number, y: number}) => number");
}

#[test]
fn infer_obj_type_based_on_nested_member_access() {
    let src = r#"let slope = (line) => (line.p1.y - line.p0.y) / (line.p1.x - line.p0.x)"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("slope").unwrap());
    assert_eq!(result, "({p1: {x: number, y: number}, p0: {x: number, y: number}}) => number");
}

#[test]
fn infer_obj_type_based_on_member_access_with_type_var_intersection() {
    let src = r#"let foo = (point) => point.x * point.x + point.y * point.y"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("foo").unwrap());
    assert_eq!(result, "({x: number, y: number}) => number");
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
    declare let add: ((number, number) => number) & ((string, string) => string)
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
#[should_panic = "unification failed"]
fn infer_function_overloading_with_incorrect_args() {
    let src = r#"
    declare let add: ((number, number) => number) & ((string, string) => string)
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
    export const result = (()=>{
        const x = 5;
        x + 0;
        return x;
    })();
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
    declare let get_bar: <T>(Foo<T>) => T
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.values.get("get_bar").unwrap());
    // TODO: normalize the scheme before inserting it into the context
    assert_eq!(result, "<D>(Foo<D>) => D");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_3() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: <T>(Foo<T>) => T
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
    assert_eq!(result, "<A>(Foo<A>) => A");

    let result = format!("{}", ctx.values.get("bar").unwrap());
    assert_eq!(result, "\"hello\"");
}