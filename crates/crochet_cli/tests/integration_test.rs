use error_stack::Report;

use crochet_ast::values::{Program, Statement};
use crochet_codegen::*;
use crochet_infer::TypeError;
use crochet_infer::*;
use crochet_parser::parse;

fn infer(input: &str) -> String {
    let mut ctx = crochet_infer::Context::default();
    let prog = parse(input).unwrap();
    let stmt = prog.body.get(0).unwrap();
    let result = match stmt {
        Statement::Expr { expr, .. } => {
            let mut expr = expr.to_owned();
            infer_expr(&mut ctx, &mut expr)
        }
        _ => Err(Report::new(TypeError).attach_printable("We can't infer decls yet")),
    };
    format!("{}", result.unwrap())
}

fn infer_prog(src: &str) -> (Program, crochet_infer::Context) {
    let result = parse(src);
    let mut prog = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };
    // println!("prog = {:#?}", &prog);
    let mut ctx = crochet_infer::Context::default();
    let ctx = crochet_infer::infer_prog(&mut prog, &mut ctx).unwrap();

    (prog, ctx)
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5;"), "5");
}

#[test]
fn infer_lam() {
    insta::assert_snapshot!(infer("(x) => x;"), @"<t0>(x: t0) => t0");
}

#[test]
fn infer_let_inside_function() {
    assert_eq!(infer("() => {let x = 5; x};"), "() => 5");
}

#[test]
fn infer_op() {
    assert_eq!(infer("5 + 10;"), "number");
}

#[test]
fn infer_fn_param() {
    assert_eq!(
        infer("(f, x) => f(x) + x;"),
        "(f: (number) => number, x: number) => number"
    );
}

#[test]
fn infer_fn_with_param_types() {
    assert_eq!(infer("(a: 5, b: 10) => a + b;"), "(a: 5, b: 10) => number");
}

#[test]
fn infer_let_fn_with_param_types() {
    let src = "let add = (a: 5, b: 10) => a + b;";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("add").unwrap());
    assert_eq!(result, "(a: 5, b: 10) => number");
}

#[test]
#[should_panic = "Can't unify string with number"]
fn infer_fn_with_incorrect_param_types() {
    infer("(a: string, b: boolean) => a + b;");
}

#[test]
#[should_panic = "Can't unify string with number"]
fn infer_let_fn_with_incorrect_param_types() {
    let src = "let add = (a: string, b: boolean) => a + b;";
    infer_prog(src);
}

#[test]
fn infer_fn_param_used_with_multiple_other_params() {
    insta::assert_snapshot!(
        infer("(f, x, y) => f(x) + f(y);"),
        @"<t0>(f: (t0) => number, x: t0, y: t0) => number"
    );
}

#[test]
fn infer_i_combinator() {
    let (_, ctx) = infer_prog("let I = (x) => x;");
    let result = format!("{}", ctx.lookup_value("I").unwrap());
    insta::assert_snapshot!(result, @"<t0>(x: t0) => t0");
}

#[test]
fn infer_k_combinator_not_curried() {
    let (program, ctx) = infer_prog("let K = (x, y) => x;");
    let result = format!("{}", ctx.lookup_value("K").unwrap());
    insta::assert_snapshot!(result, @"<t0, t1>(x: t0, y: t1) => t0");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const K: <A, B>(x: A, y: B) => A;\n");
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, ctx) = infer_prog("let S = (f, g, x) => f(x, g(x));");
    let result = format!("{}", ctx.lookup_value("S").unwrap());
    insta::assert_snapshot!(result, @"<t0, t1, t2>(f: (t0, t1) => t2, g: (t0) => t1, x: t0) => t2");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, ctx) = infer_prog("let K = (x) => (y) => x;");
    let result = format!("{}", ctx.lookup_value("K").unwrap());
    insta::assert_snapshot!(result, @"<t0, t1>(x: t0) => (y: t1) => t0");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, ctx) = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x));");
    let result = format!("{}", ctx.lookup_value("S").unwrap());
    insta::assert_snapshot!(
        result,
        @"<t0, t1, t2>(f: (t0) => (t1) => t2) => (g: (t0) => t1) => (x: t0) => t2"
    );
}

#[test]
fn infer_skk() {
    let src = r#"
    let S = (f) => (g) => (x) => f(x)(g(x));
    let K = (x) => (y) => x;
    let I = S(K)(K);
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("I").unwrap());
    insta::assert_snapshot!(result, @"<t0>(x: t0) => t0");
}

#[test]
fn infer_adding_variables() {
    let src = r#"
    let x = 5;
    let y = 10;
    let z = x + y;
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("z").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_decl() {
    let src = r#"
    let foo = (a, b) => a + b;
    let bar = "hello";
    "#;
    let (program, ctx) = infer_prog(src);
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const bar: "hello";
    export declare const foo: (a: number, b: number) => number;
    "###);
}

#[test]
fn infer_with_subtyping() {
    let src = r#"
    let foo = (a, b) => a + b;
    let bar = foo(5, 10);
    "#;
    let (program, ctx) = infer_prog(src);
    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @r###"
    export declare const bar: number;
    export declare const foo: (a: number, b: number) => number;
    "###);
}

#[test]
fn infer_if_else_without_widening() {
    let (_, ctx) = infer_prog("let x = if (true) { 5 } else { 5 };");
    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_if_else_with_widening() {
    let (_, ctx) = infer_prog("let x = if (true) { 5 } else { 10 };");
    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "10 | 5");
}

#[test]
fn infer_only_if_must_be_undefined() {
    let (_, ctx) = infer_prog("let x = if (true) { let a = 5; };");
    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "undefined");
}

#[test]
#[should_panic = "Consequent for 'if' without 'else' must not return a value"]
fn infer_only_if_must_be_undefined_error() {
    infer_prog("let x = if (true) { let a = 5; a };");
}

#[test]
fn infer_if_else_with_widening_of_top_level_vars() {
    let src = r#"
    let a = 5;
    let b = 10;
    let x = if (true) { a } else { b };
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "10 | 5");
}

#[test]
fn infer_if_else_with_multiple_widenings() {
    let src = r#"
    let x = if (true) { 5 } else if (false) { 10 } else { 15 };
    "#;
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "10 | 15 | 5");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const x: 10 | 15 | 5;
");
}

#[test]
fn infer_equal_with_numbers() {
    let (_, ctx) = infer_prog("let cond = 5 == 10;");
    let result = format!("{}", ctx.lookup_value("cond").unwrap());
    assert_eq!(result, "boolean");
}

// TODO: update definition of "!=" to be <A>(A, A) => boolean
#[test]
#[ignore]
fn infer_not_equal_with_variables() {
    let (_, ctx) = infer_prog("let neq = (a, b) => a != b;");
    let result = format!("{}", ctx.lookup_value("neq").unwrap());
    insta::assert_snapshot!(result, @"<t0>(t0, t0) => boolean");
}

#[test]
fn infer_inequalities() {
    let src = r###"
    let lt = (a, b) => a < b;
    let lte = (a, b) => a <= b;
    let gt = (a, b) => a > b;
    let gte = (a, b) => a >= b;
    "###;
    let (_, ctx) = infer_prog(src);
    assert_eq!(
        format!("{}", ctx.lookup_value("lt").unwrap()),
        "(a: number, b: number) => boolean"
    );
    assert_eq!(
        format!("{}", ctx.lookup_value("lte").unwrap()),
        "(a: number, b: number) => boolean"
    );
    assert_eq!(
        format!("{}", ctx.lookup_value("gt").unwrap()),
        "(a: number, b: number) => boolean"
    );
    assert_eq!(
        format!("{}", ctx.lookup_value("gte").unwrap()),
        "(a: number, b: number) => boolean"
    );
}

#[test]
fn infer_let_rec_until() {
    let src = "let rec until = (p, f, x) => if (p(x)) { x } else { until(p, f, f(x)) };";
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("until").unwrap());
    insta::assert_snapshot!(result, @"<t0>(p: (t0) => boolean, f: (t0) => t0, x: t0) => t0");

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const until: <A>(p: (arg0: A) => boolean, f: (arg0: A) => A, x: A) => A;
");
}

#[test]
fn infer_fib() {
    let src = r###"
    let rec fib = (n) => if (n == 0) {
        0
    } else if (n == 1) {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    };
    "###;

    let (_, ctx) = infer_prog(src);
    let fib_type = ctx.lookup_value("fib").unwrap();
    assert_eq!(format!("{}", fib_type), "(n: number) => number");
}

#[test]
fn infer_obj() {
    let src = r#"
    let point = {x: 5, y: 10, msg: "Hello, world!"};
    "#;
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("point").unwrap());
    assert_eq!(result, "{x: 5, y: 10, msg: \"Hello, world!\"}");
}

#[test]
fn infer_async() {
    let src = "let foo = async () => 10;";
    let (_, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("foo").unwrap());
    assert_eq!(result, "() => Promise<10>");
}

#[test]
fn infer_async_math() {
    let src = "let add = async (a, b) => await a() + await b();";
    let (program, ctx) = infer_prog(src);
    let result = format!("{}", ctx.lookup_value("add").unwrap());
    assert_eq!(
        result,
        "(a: () => Promise<number>, b: () => Promise<number>) => Promise<number>"
    );

    let result = codegen_d_ts(&program, &ctx);
    insta::assert_snapshot!(result, @"export declare const add: (a: () => Promise<number>, b: () => Promise<number>) => Promise<number>;\n");
}

#[test]
fn codegen_let_rec() {
    let src = "let rec f = () => f();";
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
    let cond = true;
    let result = if (cond) { 5 } else { 5 };
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
    let src = "let point = {x: 5, y: 10};";
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
        readonly x: 5;
        readonly y: 10;
    };
    "###);
}

#[test]
fn codegen_async_math() {
    let src = "let add = async (a, b) => await a() + await b();";
    let (program, ctx) = infer_prog(src);

    let js = codegen_js(&program);

    insta::assert_snapshot!(js, @"export const add = async (a, b)=>await a() + await b();
");

    let result = codegen_d_ts(&program, &ctx);

    insta::assert_snapshot!(result, @"export declare const add: (a: () => Promise<number>, b: () => Promise<number>) => Promise<number>;\n");
}

#[test]
fn infer_let_decl_with_type_ann() {
    let src = "let x: number = 10;";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
// TODO: improve this error by checking the flags on the types before reporting
// "Unification failure".
#[should_panic = "called `Result::unwrap()` on an `Err` value: TypeError { msg: \"Unification failure\" }"]
fn infer_let_decl_with_incorrect_type_ann() {
    let src = "let x: string = 10;";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_declare() {
    let src = "declare let x: number;";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_expr_using_declared_var() {
    let src = r#"
    declare let x: number;
    let y = x + 5;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("y").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn() {
    let src = r#"
    declare let add: (a: number, b: number) => number;
    let sum = add(5, 10);
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("sum").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn_with_obj_param() {
    let src = r#"
    declare let mag: (p: {x: number, y: number}) => number;
    let result = mag({x: 5, y: 10});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn calling_a_fn_with_an_obj_subtype() {
    let src = r#"
    declare let mag: (p: {x: number, y: number}) => number;
    let result = mag({x: 5, y: 10, z: 15});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
#[should_panic = "Unification failure"]
fn calling_a_fn_with_an_obj_missing_a_property() {
    let src = r#"
    declare let mag: (p: {x: number, y: number}) => number;
    let result = mag({x: 5});
    "#;
    infer_prog(src);
}

#[test]
fn infer_literal_tuple() {
    let src = r#"let tuple = [1, "two", true];"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("tuple").unwrap());
    assert_eq!(result, "[1, \"two\", true]");
}

#[test]
fn infer_tuple_with_type_annotation() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", true];"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("tuple").unwrap());
    assert_eq!(result, "[number, string, boolean]");
}

#[test]
fn infer_tuple_with_type_annotation_and_extra_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", true, "ignored"];"#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("tuple").unwrap());
    assert_eq!(result, "[number, string, boolean]");
}

#[test]
// TODO: improve this error by checking the flags on the types before reporting
// "Unification failure".
#[should_panic = "called `Result::unwrap()` on an `Err` value: TypeError { msg: \"Unification failure\" }"]
fn infer_tuple_with_type_annotation_and_incorrect_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", 3];"#;
    infer_prog(src);
}

#[test]
#[should_panic = "not enough elements to unpack"]
fn infer_tuple_with_not_enough_elements() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two"];"#;
    infer_prog(src);
}

#[test]
fn infer_var_with_union_type_annotation() {
    let src = r#"
    let a: number | string = 5;
    let b: number | string = "ten";
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "number | string");
    let b = format!("{}", ctx.lookup_value("b").unwrap());
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
    };
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("result").unwrap());
    assert_eq!(result, "(cond: boolean) => [1, 2] | [true, false]");
}

#[ignore]
#[test]
fn infer_widen_tuples_with_type_annotations() {
    let src = r#"
    let result = (cond) => {
        if (cond) {
            let x: [number, number] = [1, 2];
            x
        } else {
            let y: [boolean, boolean] = [true, false];
            y
        }
    };
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("result").unwrap());
    assert_eq!(
        result,
        "(boolean) => [number | number] | [boolean | boolean]"
    );
}

#[test]
fn infer_member_access() {
    let src = r#"
    let point = {x: 5, y: 10};
    let x = point.x;
    let y = point.y;
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(x, "5");
    let y = format!("{}", ctx.lookup_value("y").unwrap());
    assert_eq!(y, "10");
}

#[test]
fn infer_member_access_on_obj_lit() {
    let src = r#"let x = {x: 5, y: 10}.x;"#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(x, "5");
}

#[test]
fn infer_fn_using_type_decl() {
    let src = r#"
    type Point = {x: number, y: number};
    let mag = (p: Point) => p.x * p.x + p.y * p.y;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("mag").unwrap());
    assert_eq!(result, "(p: Point) => number");
}

#[test]
fn infer_react_component() {
    let src = r#"
    type Props = {name: string};
    let Foo = (props: Props) => {
        <div>Hello, world</div>
    };
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("Foo").unwrap());
    assert_eq!(result, "(props: Props) => JSXElement");
}

#[test]
fn codegen_code_with_type_delcarations() {
    let src = r#"
    type Point = {x: number, y: number};
    let point: Point = {x: 5, y: 10};
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
    declare type Point = {
        readonly x: number;
        readonly y: number;
    };
    export declare const point: Point;
    "###);
}

#[test]
fn infer_mem_access_with_optional_prop() {
    let src = r#"
    declare let point: {x?: number, y: number};
    let x = point.x;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "number | undefined");
}

#[test]
#[should_panic = "not yet implemented"]
fn recursive_mem_access_on_optional_prop_should_fail() {
    let src = r#"
    declare let foo: {a?: {b?: number}};
    let b = foo.a.b;
    "#;
    infer_prog(src);
}

#[test]
fn infer_assigning_to_obj_with_optional_props() {
    let src = r#"
    let p: {x?: number, y: number} = {y: 10};
    let x = p.x;
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(x, "number | undefined");
}

#[test]
fn infer_assigning_an_obj_lit_with_extra_props() {
    let src = r#"
    let point: {x: number, y: number} = {x: 5, y: 10, z: 15};
    "#;
    let (_, ctx) = infer_prog(src);

    let point = format!("{}", ctx.lookup_value("point").unwrap());
    assert_eq!(point, "{x: number, y: number}");
}

#[test]
fn infer_function_overloading() {
    let src = r#"
    declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string);
    let num = add(5, 10);
    let str = add("hello, ", "world");
    "#;
    let (_, ctx) = infer_prog(src);

    let num_result = format!("{}", ctx.lookup_value("num").unwrap());
    assert_eq!(num_result, "number");
    let str_result = format!("{}", ctx.lookup_value("str").unwrap());
    assert_eq!(str_result, "string");
}

#[test]
#[should_panic = "Couldn't unify lambda with intersection"]
fn infer_function_overloading_with_incorrect_args() {
    let src = r#"
    declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string);
    let bool = add(true, false);
    "#;
    infer_prog(src);
}

#[test]
fn codegen_object_type_with_optional_property() {
    let src = r#"
    type Point = {x?: number, y: number};
    let point: Point = {y: 10};
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
    declare type Point = {
        readonly x?: number;
        readonly y: number;
    };
    export declare const point: Point;
    "###);
}

#[test]
fn infer_nested_block() {
    let src = "let result = do {let sum = do {let x = 5; let y = 10; x + y }; sum };";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("result").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_block_with_multiple_non_let_lines() {
    // TODO:
    // instead of inferring that `x` should be a `number` when we see
    // `x + 0`, we should instead infer that it's a `subtype of number`.
    // that way when we reconcile it with the other inferred type of `x`
    // which is `5`, the final inferred type will be `5`.
    let src = "let result = do {let x = 5; x + 0; x };";
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("result").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn codegen_block_with_multiple_non_let_lines() {
    let src = "let result = do {let x = 5; x + 0; x };";
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
    type Foo<T> = {bar: T};
    declare let foo: Foo<string>;
    let bar = foo.bar;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("bar").unwrap());
    assert_eq!(result, "string");
}

#[test]
fn infer_fn_param_with_type_alias_with_param() {
    let src = r#"
    type Foo<T> = {bar: T};
    let get_bar = (foo: Foo<string>) => foo.bar;
    let bar = get_bar({bar: "hello"});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("bar").unwrap());
    assert_eq!(result, "string");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_2() {
    let src = r#"
    type Foo<T> = {bar: T};
    declare let get_bar: <T>(foo: Foo<T>) => T;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("get_bar").unwrap());
    // TODO: normalize the type before inserting it into the context
    insta::assert_snapshot!(result, @"<t0>(foo: Foo<t0>) => t0");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_3() {
    let src = r#"
    type Foo<T> = {bar: T};
    declare let get_bar: <T>(foo: Foo<T>) => T;
    let bar = get_bar({bar: "hello"});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("bar").unwrap());
    assert_eq!(result, "\"hello\"");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_4() {
    let src = r#"
    type Foo<T> = {bar: T};
    let get_bar = <T>(foo: Foo<T>) => foo.bar;
    let bar = get_bar({bar: "hello"});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("get_bar").unwrap());
    insta::assert_snapshot!(result, @"<t0>(foo: Foo<t0>) => t0");

    let result = format!("{}", ctx.lookup_value("bar").unwrap());
    assert_eq!(result, "\"hello\"");
}

#[test]
fn infer_generic_type_aliases() {
    // What's the difference between these two?
    let src = r#"
    type Identity1<T> = <T>(foo: T) => T;
    type Identity2 = <T>(foo: T) => T;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_type("Identity1").unwrap());
    // TODO: normalize the type before inserting it into the context
    insta::assert_snapshot!(result, @"<t0>(foo: t0) => t0");

    let result = format!("{}", ctx.lookup_type("Identity2").unwrap());
    // TODO: normalize the type before inserting it into the context
    insta::assert_snapshot!(result, @"<t0>(foo: t0) => t0");
}

#[test]
fn infer_destructure_all_object_properties() {
    let src = r#"
    let point = {x: 5, y: 10};
    let {x, y} = point;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "5");
    let result = format!("{}", ctx.lookup_value("y").unwrap());
    assert_eq!(result, "10");
}

#[test]
fn infer_destructure_some_object_properties() {
    let src = r#"
    let point = {x: 5, y: 10};
    let {x} = point;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_destructure_some_object_properties_with_renaming() {
    let src = r#"
    let point = {x: 5, y: 10};
    let {x: a} = point;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(result, "5");
}

#[test]
fn infer_destructure_object_with_type_alias() {
    let src = r#"
    type Point = {x: number, y: number};
    let point: Point = {x: 5, y: 10};
    let {x, y} = point;
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_inside_fn() {
    let src = r#"
    type FooBar = {foo: number, bar: string};
    let get_foo = (x: FooBar) => {
        let {foo, bar} = x;
        foo
    };
    let foo = get_foo({foo: 5, bar: "hello"});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("foo").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_inside_fn_2() {
    let src = r#"
    type FooBar = {foo: number, bar: string};
    let get_foo = (x: FooBar) => {
        let {foo, bar} = x;
        foo
    };
    let foo = get_foo({foo: 5, bar: "hello"});
    "#;
    let (_, ctx) = infer_prog(src);

    let result = format!("{}", ctx.lookup_value("foo").unwrap());
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_param() {
    let src = r#"
    let foo = ({a, b}: {a: string, b: number}) => a;
    let a = foo({a: "hello", b: 5});
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "string");
}

#[test]
fn infer_destructure_object_param_2() {
    let src = r#"
    let foo = ({a, b}: {a: string, b: number}) => {
        {a: a, b: b}
    };
    let {a, b} = foo({a: "hello", b: 5});
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "string");

    let b = format!("{}", ctx.lookup_value("b").unwrap());
    assert_eq!(b, "number");
}

#[test]
fn return_an_object() {
    let src = r#"
    let foo = () => {
        {a: "hello", b: 5}
    };
    let {a, b} = foo();
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "\"hello\"");

    let b = format!("{}", ctx.lookup_value("b").unwrap());
    assert_eq!(b, "5");
}

#[test]
fn object_property_shorthand() {
    let src = r#"
    let a = "hello";
    let b = 5;
    let c = {a, b};
    "#;
    let (_, ctx) = infer_prog(src);

    let c = format!("{}", ctx.lookup_value("c").unwrap());
    assert_eq!(c, "{a: \"hello\", b: 5}");
}

#[test]
fn infer_destructuring_with_optional_properties() {
    let src = r#"
    let p: {x?: number, y: number} = {y: 10};
    let {x, y: _} = p;
    "#;
    let (_, ctx) = infer_prog(src);

    let x = format!("{}", ctx.lookup_value("x").unwrap());
    assert_eq!(x, "number | undefined");
    assert!(ctx.lookup_value("y").is_err());
}

#[test]
fn infer_destructure_tuple() {
    let src = r#"
    let [a, b] = ["hello", 5];
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "\"hello\"");

    let b = format!("{}", ctx.lookup_value("b").unwrap());
    assert_eq!(b, "5");
}

#[test]
#[should_panic = "not enough elements to unpack"]
fn infer_destructure_tuple_too_many_identifiers() {
    let src = r#"
    let [a, b, c] = ["hello", 5];
    "#;
    infer_prog(src);
}

#[test]
fn infer_destructure_tuple_extra_values_are_ignored() {
    let src = r#"
    let [a, b] = ["hello", 5, true];
    "#;
    infer_prog(src);
}

#[test]
fn lam_param_tuple() {
    let src = r#"
    let foo = (bar: [string, number]) => bar;
    let [a, b] = foo(["hello", 5]);
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "string");

    let b = format!("{}", ctx.lookup_value("b").unwrap());
    assert_eq!(b, "number");
}

#[test]
fn destructure_lam_param_tuple() {
    let src = r#"
    let foo = ([a, b]: [string, number]) => [a, b];
    let [a, b] = foo(["hello", 5]);
    "#;
    let (_, ctx) = infer_prog(src);

    let a = format!("{}", ctx.lookup_value("a").unwrap());
    assert_eq!(a, "string");

    let b = format!("{}", ctx.lookup_value("b").unwrap());
    assert_eq!(b, "number");
}

#[test]
fn infer_jsx() {
    let src = r#"
    type JSXElement = {};
    let point = {x: 5, y: 10};
    let msg = "world";
    let elem = <div point={point} id="point">Hello, {msg}</div>;
    "#;
    let (_, ctx) = infer_prog(src);

    let elem = format!("{}", ctx.lookup_value("elem").unwrap());
    assert_eq!(elem, "JSXElement");
}

#[test]
#[should_panic = "Unification failure"]
fn incorrect_args() {
    let src = r#"
    let add = (a, b) => a + b;
    add("hello", "world");
    "#;
    infer_prog(src);
}

#[test]
fn return_empty() {
    let src = r#"
    let foo = () => {};
    "#;
    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.lookup_value("foo").unwrap()),
        "() => undefined"
    );
}

#[test]
fn return_empty_with_body() {
    let src = r#"
    let foo = () => {
        let a = 5;
    };
    "#;
    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.lookup_value("foo").unwrap()),
        "() => undefined"
    );
}

#[test]
fn infer_if_let() {
    let src = r#"
    let p = {x: 5, y: 10};
    if (let {x, y} = p) {
        x + y;
    };
    "#;

    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.lookup_value("p").unwrap()),
        "{x: 5, y: 10}"
    );
    // Ensures we aren't polluting the outside context
    assert!(ctx.lookup_value("x").is_err());
    assert!(ctx.lookup_value("y").is_err());
}

#[test]
fn infer_if_let_with_is() {
    let src = r#"
    declare let b: string | number;
    if (let a is string = b) {
        a;
    };
    "#;

    let (_, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.lookup_value("b").unwrap()),
        "number | string"
    );
    // Ensures we aren't polluting the outside context
    assert!(ctx.lookup_value("a").is_err());
}

#[test]
fn codegen_if_let() {
    let src = r#"
    let p = {x: 5, y: 10};
    if (let {x, y} = p) {
        x + y;
    };
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
        readonly x: 5;
        readonly y: 10;
    };
    "###);
}

#[test]
fn codegen_if_let_with_rename() {
    let src = r#"
    let p = {x: 5, y: 10};
    if (let {x: a, y: b} = p) {
        a + b;
    };
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
        readonly x: 5;
        readonly y: 10;
    };
    "###);
}

#[test]
#[should_panic = "Unification failure"]
fn infer_if_let_with_type_error() {
    let src = r#"
    let p = {x: "hello", y: "world"};
    if (let {x, y} = p) {
        x + y;
    };
    "#;

    infer_prog(src);
}

#[test]
fn infer_if_let_refutable_pattern_obj() {
    let src = r#"
    let p = {x: 5, y: 10};
    if (let {x: 5, y} = p) {
        y;
    };
    "#;

    let (program, ctx) = infer_prog(src);

    assert_eq!(
        format!("{}", ctx.lookup_value("p").unwrap()),
        "{x: 5, y: 10}"
    );

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
        readonly x: 5;
        readonly y: 10;
    };
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_nested_obj() {
    let src = r#"
    let action = {type: "moveto", point: {x: 5, y: 10}};
    if (let {type: "moveto", point: {x, y}} = action) {
        x + y;
    };
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
        readonly type: "moveto";
        readonly point: {
            readonly x: 5;
            readonly y: 10;
        };
    };
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_with_disjoint_union() {
    let src = r#"
    type Point = {x: number, y: number};
    type Action = {type: "moveto", point: Point} | {type: "lineto", point: Point};
    declare let action: Action;
    if (let {type: "moveto", point: {x, y}} = action) {
        x + y;
    };
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
    declare type Action = {
        readonly type: "lineto";
        readonly point: Point;
    } | {
        readonly type: "moveto";
        readonly point: Point;
    };
    declare type Point = {
        readonly x: number;
        readonly y: number;
    };
    export declare const action: Action;
    "###);
}

#[test]
fn infer_if_let_refutable_pattern_array() {
    let src = r#"
    let p = [5, 10];
    if (let [5, y] = p) {
        y;
    };
    "#;

    let (program, ctx) = infer_prog(src);

    assert_eq!(format!("{}", ctx.lookup_value("p").unwrap()), "[5, 10]");

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

    insta::assert_snapshot!(result, @"export declare const p: readonly [5, 10];
");
}

#[test]
fn infer_if_let_refutable_pattern_nested_array() {
    let src = r#"
    let action = ["moveto", [5, 10]];
    if (let ["moveto", [x, y]] = action) {
        x + y;
    };
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

    insta::assert_snapshot!(result, @r###"export declare const action: readonly ["moveto", readonly [5, 10]];
"###);
}

#[test]
fn codegen_if_let_with_is_prim() {
    let src = r#"
    declare let b: string | number;
    if (let a is number = b) {
        a + 5;
    };
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

    insta::assert_snapshot!(result, @"export declare const b: number | string;
");
}

#[test]
fn codegen_if_let_with_is_class() {
    // NOTE: TypeScript treats classes as both types and values.
    // The type represents the type of an instance of the class.
    let src = r#"
    type Foo = {
        getNum: () => number,
    };
    type Bar = {
        getStr: () => string,
    };
    declare let foo: Foo;
    let Foo = {
        constructor: () => foo,
    };
    declare let bar: Bar;
    let Bar = {
        constructor: () => bar,
    };
    declare let b: Foo | Bar;
    if (let a is Foo = b) {
        a.getNum() + 5;
    };
    "#;

    let result = parse(src);
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
    let arr: string[] = ["hello", "world"];
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
    insta::assert_snapshot!(result, @"export declare const arr: readonly string[];
");
}
