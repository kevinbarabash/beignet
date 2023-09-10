use escalier_ast::{Program, StmtKind};
use escalier_codegen::*;
use escalier_hm::checker::Checker;
use escalier_hm::context::Context;
use escalier_hm::type_error::TypeError;
use escalier_parser::parse;

pub fn current_report_message(checker: &Checker) -> String {
    checker
        .current_report
        .diagnostics
        .iter()
        .map(|d| d.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}

fn infer(input: &str) -> String {
    let prog = parse(input).unwrap();
    let mut stmt = prog.stmts.get(0).unwrap().to_owned();
    let mut checker = Checker::default();
    let mut ctx = Context::default();
    let result = match &stmt.kind {
        StmtKind::Expr(_) => checker.infer_statement(&mut stmt, &mut ctx, true),
        _ => Err(TypeError {
            message: "unspecified error".to_string(),
        }),
    };
    match result {
        Ok(t) => checker.print_type(&t),
        Err(error) => {
            let message = error.message;
            panic!("{message}");
        }
    }
}

fn infer_prog(src: &str) -> (Program, (Context, Checker)) {
    let result = parse(src);
    let mut prog = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };
    // println!("prog = {:#?}", &prog);
    let mut checker = Checker::default();
    let mut ctx = Context::default();

    match checker.infer_program(&mut prog, &mut ctx) {
        Ok(()) => (prog, (ctx, checker)),
        Err(error) => {
            let message = error.message;
            panic!("{message}");
        }
    }
}

#[test]
fn infer_number_literal() {
    assert_eq!(infer("5"), "5");
}

// TODO: fix this tests
#[test]
#[ignore]
fn infer_lam() {
    insta::assert_snapshot!(infer("fn (x) => x"), @"<A>(x: A) -> A");
}

#[test]
fn infer_let_inside_function() {
    let src = r#"
    fn () {
        let x = 5
        return x
    }
    "#;
    assert_eq!(infer(src), "() -> 5");
}

#[test]
fn infer_op() {
    assert_eq!(infer("5 + 10"), "15");
}

#[test]
fn infer_fn_param() {
    assert_eq!(
        infer("fn (f, x) => f(x) + x"),
        "(f: (arg0: number) -> number, x: number) -> number"
    );
}

#[test]
fn infer_fn_with_param_types() {
    assert_eq!(infer("fn (a: 5, b: 10) => a + b"), "(a: 5, b: 10) -> 15");
}

#[test]
fn infer_let_fn_with_param_types() {
    let src = "let add = fn (a: 5, b: 10) => a + b";
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("add").unwrap().index);
    assert_eq!(result, "(a: 5, b: 10) -> 15");
}

// TODO: make this a recoverable error
#[test]
#[ignore]
fn infer_fn_with_incorrect_param_types() {
    let (_, (_, checker)) = infer_prog("fn (a: string, b: boolean) => a + b");

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1 - string is not a number:
    └ TypeError::UnificationError: string, number

    ESC_1 - boolean is not a number:
    └ TypeError::UnificationError: boolean, number
    "###);
}

// TODO: make this a recoverable error
#[test]
#[ignore]
fn infer_let_fn_with_incorrect_param_types() -> Result<(), TypeError> {
    let src = "let add = fn (a: string, b: boolean) => a + b";
    let (_, (ctx, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1 - string is not a number:
    └ TypeError::UnificationError: string, number

    ESC_1 - boolean is not a number:
    └ TypeError::UnificationError: boolean, number
    "###);

    let binding = ctx.values.get("add").unwrap();
    assert_eq!(
        checker.print_type(&binding.index),
        "(a: string, b: boolean) => number"
    );

    Ok(())
}

// TODO: fix this test
#[test]
#[ignore]
fn infer_fn_param_used_with_multiple_other_params() {
    insta::assert_snapshot!(
        infer("fn (f, x, y) => f(x) + f(y)"),
        @"<A>(f: (A) -> number, x: A, y: A) -> number"
    );
}

#[test]
fn infer_i_combinator() {
    let (_, (ctx, checker)) = infer_prog("let I = fn (x) => x");
    let result = checker.print_type(&ctx.values.get("I").unwrap().index);
    insta::assert_snapshot!(result, @"<A>(x: A) -> A");
}

#[test]
fn infer_k_combinator_not_curried() -> Result<(), TypeError> {
    let (program, (ctx, checker)) = infer_prog("let K = fn (x, y) => x");
    let result = checker.print_type(&ctx.values.get("K").unwrap().index);
    insta::assert_snapshot!(result, @"<A, B>(x: A, y: B) -> A");

    let result = codegen_d_ts(&program, &ctx, &checker)?;
    insta::assert_snapshot!(result, @"export declare const K: <A, B>(x: A, y: B) => A;\n");

    Ok(())
}

#[test]
fn infer_s_combinator_not_curried() {
    let (_, (ctx, checker)) = infer_prog("let S = fn (f, g, x) => f(x, g(x))");
    let result = checker.print_type(&ctx.values.get("S").unwrap().index);
    insta::assert_snapshot!(result, @"<A, C, B>(f: (arg0: A, arg1: B) -> C, g: (arg0: A) -> B, x: A) -> C");
}

#[test]
fn infer_k_combinator_curried() {
    let (_, (ctx, checker)) = infer_prog("let K = fn (x) => fn (y) => x");
    let result = checker.print_type(&ctx.values.get("K").unwrap().index);
    insta::assert_snapshot!(result, @"<A, B>(x: A) -> (y: B) -> A");
}

#[test]
fn infer_s_combinator_curried() {
    let (_, (ctx, checker)) = infer_prog("let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))");
    let result = checker.print_type(&ctx.values.get("S").unwrap().index);
    insta::assert_snapshot!(
        result,
        @"<A, C, B>(f: (arg0: A) -> (arg0: B) -> C) -> (g: (arg0: A) -> B) -> (x: A) -> C"
    );
}

#[test]
fn infer_skk() {
    let src = r#"
    let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))
    let K = fn (x) => fn (y) => x
    let I = S(K)(K)
    "#;
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("S").unwrap().index);
    insta::assert_snapshot!(result, @"<A, C, B>(f: (arg0: A) -> (arg0: B) -> C) -> (g: (arg0: A) -> B) -> (x: A) -> C");
    let result = checker.print_type(&ctx.values.get("K").unwrap().index);
    insta::assert_snapshot!(result, @"<A, B>(x: A) -> (y: B) -> A");
    let result = checker.print_type(&ctx.values.get("I").unwrap().index);
    insta::assert_snapshot!(result, @"<A>(x: A) -> A");
}

#[test]
fn infer_adding_literals_in_variables() {
    let src = r#"
    let x = 5
    let y = 10
    let z = x + y
    "#;
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("z").unwrap().index);
    assert_eq!(result, "15");
}

#[test]
fn infer_adding_numbers() {
    let src = r#"
    let x: number = 5
    let y = 10
    let z = x + y
    "#;
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("z").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_decl() -> Result<(), TypeError> {
    let src = r#"
    let foo = fn (a, b) => a + b
    let bar = "hello"
    "#;
    let (program, (ctx, checker)) = infer_prog(src);
    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    export declare const bar: "hello";
    export declare const foo: (a: number, b: number) => number;
    "###);

    Ok(())
}

#[test]
fn infer_with_subtyping() -> Result<(), TypeError> {
    let src = r#"
    let foo = fn (a, b) => a + b
    let bar = foo(5, 10)
    "#;
    let (program, (ctx, checker)) = infer_prog(src);
    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    export declare const bar: number;
    export declare const foo: (a: number, b: number) => number;
    "###);

    Ok(())
}

#[test]
fn infer_if_else_without_widening() {
    let (_, (ctx, checker)) = infer_prog("let x = if (true) { 5 } else { 5 }");
    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    // TODO: remove duplicate types from union types
    assert_eq!(result, "5 | 5");
}

#[test]
fn infer_if_else_with_widening() {
    let (_, (ctx, checker)) = infer_prog("let x = if (true) { 5 } else { 10 }");
    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "5 | 10");
}

#[test]
fn infer_value_of_let_from_a_block_return_is_undefined() {
    let (_, (ctx, checker)) = infer_prog("let x = if (true) { let a = 5 }");
    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "undefined | undefined");
}

#[test]
fn infer_only_if() {
    let src = r#"
    let x = if (true) { 
        let a = 5
        a
    }
    "#;
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "5 | undefined");
}

#[test]
fn infer_if_else_with_widening_of_top_level_vars() {
    let src = r#"
    let a = 5
    let b = 10
    let x = if (true) { a } else { b }
    "#;
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "5 | 10");
}

#[test]
fn infer_if_else_with_multiple_widenings() -> Result<(), TypeError> {
    let src = r#"
    let x = if (true) { 5 } else if (false) { 10 } else { 15 }
    "#;
    let (program, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "5 | 10 | 15");

    let result = codegen_d_ts(&program, &ctx, &checker)?;
    insta::assert_snapshot!(result, @"export declare const x: 5 | 10 | 15;\n");

    Ok(())
}

#[test]
fn infer_equal_with_numbers() {
    let (_, (ctx, checker)) = infer_prog("let cond = 5 == 10");
    let result = checker.print_type(&ctx.values.get("cond").unwrap().index);
    assert_eq!(result, "false");
}

// TODO: update definition of "!=" to be <A>(A, A) => boolean
// But how do we handle situations like `5 != 10`?
#[test]
#[ignore]
fn infer_not_equal_with_variables() {
    let (_, (ctx, checker)) = infer_prog("let neq = fn (a, b) => a != b");
    let result = checker.print_type(&ctx.values.get("neq").unwrap().index);
    insta::assert_snapshot!(result, @"<A, B>(a: A, b: B) -> boolean");
}

#[test]
fn infer_inequalities() {
    let src = r###"
    let lt = fn (a, b) => a < b
    let lte = fn (a, b) => a <= b
    let gt = fn (a, b) => a > b
    let gte = fn (a, b) => a >= b
    "###;
    let (_, (ctx, checker)) = infer_prog(src);
    assert_eq!(
        checker.print_type(&ctx.values.get("lt").unwrap().index),
        "(a: number, b: number) -> boolean"
    );
    assert_eq!(
        checker.print_type(&ctx.values.get("lte").unwrap().index),
        "(a: number, b: number) -> boolean"
    );
    assert_eq!(
        checker.print_type(&ctx.values.get("gt").unwrap().index),
        "(a: number, b: number) -> boolean"
    );
    assert_eq!(
        checker.print_type(&ctx.values.get("gte").unwrap().index),
        "(a: number, b: number) -> boolean"
    );
}

// TODO: Fix this test case
#[test]
#[ignore]
fn infer_let_rec_until() -> Result<(), TypeError> {
    let src = "let until = fn (p, f, x) => if (p(x)) { x } else { until(p, f, f(x)) }";
    let (program, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("until").unwrap().index);
    // Where is `C` coming from.  Why is `f` not returning `A`?
    // <A, C, B>(p: (arg0: A) -> boolean, f: (arg0: A) -> B, x: A) -> A | C
    insta::assert_snapshot!(result, @"<A>(p: (arg0: A) -> boolean, f: (arg0: A) -> A, x: A) -> A");

    let result = codegen_d_ts(&program, &ctx, &checker)?;
    insta::assert_snapshot!(result, @"export declare const until: <A>(p: (arg0: A) => boolean, f: (arg0: A) => A, x: A) => A;
");

    Ok(())
}

#[test]
fn infer_fib() {
    let src = r###"
    let fib = fn (n) => if (n == 0) {
        0
    } else if (n == 1) {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
    "###;

    let (_, (ctx, checker)) = infer_prog(src);
    let fib = ctx.values.get("fib").unwrap();
    assert_eq!(
        format!("{}", checker.print_type(&fib.index)),
        // TODO: unions of `number` and number literals should
        // have `number` subsume the literals.
        "(n: number) -> 0 | 1 | number"
    );
}

#[test]
fn infer_obj() {
    let src = r#"
    let point = {x: 5, y: 10, msg: "Hello, world!"}
    "#;
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("point").unwrap().index);
    assert_eq!(result, "{x: 5, y: 10, msg: \"Hello, world!\"}");
}

#[test]
fn infer_async() {
    let src = "let foo = async fn () => 10";
    let (_, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("foo").unwrap().index);
    assert_eq!(result, "() -> Promise<10, never>");
}

#[test]
fn infer_async_math() -> Result<(), TypeError> {
    let src = "let add = async fn (a, b) => await a() + await b()";
    let (program, (ctx, checker)) = infer_prog(src);
    let result = checker.print_type(&ctx.values.get("add").unwrap().index);
    assert_eq!(
        result,
        "<A, B>(a: () -> Promise<number, A>, b: () -> Promise<number, B>) -> Promise<number, A | B>"
    );

    let result = codegen_d_ts(&program, &ctx, &checker)?;
    insta::assert_snapshot!(result, @"export declare const add: <A, B>(a: () => Promise<number, A>, b: () => Promise<number, B>) => Promise<number, A | B>;
");

    Ok(())
}

#[test]
fn codegen_let_rec() -> Result<(), TypeError> {
    let src = "let f = fn () => f()";
    let (program, (ctx, checker)) = infer_prog(src);
    let (js, _) = codegen_js(src, &program);

    insta::assert_snapshot!(js, @"export const f = ()=>f();
");

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @"export declare const f: <A>() => A;\n");

    Ok(())
}

#[test]
fn codegen_if_else() -> Result<(), TypeError> {
    let src = r#"
    let cond = true
    let result = if (cond) { 5 } else { 5 }
    "#;
    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
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

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    // TODO: remove duplicates from union types
    insta::assert_snapshot!(result, @r###"
    export declare const cond: true;
    export declare const result: 5 | 5;
    "###);

    Ok(())
}

#[test]
fn codegen_object() -> Result<(), TypeError> {
    let src = "let point = {x: 5, y: 10}";
    let (program, (ctx, checker)) = infer_prog(src);
    let (js, _) = codegen_js(src, &program);

    insta::assert_snapshot!(js, @r###"
    export const point = {
        x: 5,
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    // TODO: wrap types in `Readonly<>` if the binding is immutable
    insta::assert_snapshot!(result, @r###"
    export declare const point: {
        x: 5;
        y: 10;
    };
    "###);

    Ok(())
}

#[test]
fn codegen_async_math() -> Result<(), TypeError> {
    let src = "let add = async fn (a, b) => await a() + await b()";
    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);

    insta::assert_snapshot!(js, @"export const add = async (a, b)=>await a() + await b();
");

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @"export declare const add: <A, B>(a: () => Promise<number, A>, b: () => Promise<number, B>) => Promise<number, A | B>;
");

    Ok(())
}

#[test]
fn infer_let_decl_with_type_ann() {
    let src = "let x: number = 10";
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "number");
}

// TODO: make this a recoverable error
#[test]
#[ignore]
// TODO: improve this error by checking the flags on the types before reporting
// "Unification failure".
fn infer_let_decl_with_incorrect_type_ann() {
    let src = "let x: string = 10";

    let (_, (_, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1 - 10 is not assignable to string:
    └ TypeError::UnificationError: 10, string
    "###);
}

#[test]
fn infer_declare() {
    let src = "declare let x: number";
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_expr_using_declared_var() {
    let src = r#"
    declare let x: number
    let y = x + 5
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("y").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn() {
    let src = r#"
    declare let add: fn (a: number, b: number) -> number
    let sum = add(5, 10)
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("sum").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_app_of_declared_fn_with_obj_param() {
    let src = r#"
    declare let mag: fn (p: {x: number, y: number}) -> number
    let result = mag({x: 5, y: 10})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("result").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn calling_a_fn_with_an_obj_subtype() {
    let src = r#"
    declare let mag: fn (p: {x: number, y: number}) -> number
    let result = mag({x: 5, y: 10, z: 15})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("result").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn calling_a_fn_with_an_obj_missing_a_property() {
    let src = r#"
    declare let mag: fn (p: {x: number, y: number}) -> number
    let result = mag({x: 5})
    "#;
    let (_, (_, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: 'y' is missing in {x: 5}
    "###);
}

#[test]
fn infer_literal_tuple() {
    let src = r#"let tuple = [1, "two", true]"#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("tuple").unwrap().index);
    assert_eq!(result, "[1, \"two\", true]");
}

#[test]
fn infer_tuple_with_type_annotation() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", true]"#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("tuple").unwrap().index);
    assert_eq!(result, "[number, string, boolean]");
}

#[test]
fn infer_tuple_with_type_annotation_and_extra_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", true, "ignored"]"#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("tuple").unwrap().index);
    assert_eq!(result, "[number, string, boolean]");
}

// TODO: make this a recoverable error
#[test]
#[ignore]
fn infer_tuple_with_type_annotation_and_incorrect_element() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two", 3]"#;

    let (_, (_, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1 - [1, "two", 3] is not assignable to [number, string, boolean]:
    └ TypeError::UnificationError: 3, boolean
    "###);
}

// TODO: this should be a recoverable error
#[test]
#[ignore]
fn infer_tuple_with_not_enough_elements() {
    let src = r#"let tuple: [number, string, boolean] = [1, "two"]"#;
    let (_, (_, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1 - [1, "two"] is not assignable to [number, string, boolean]:
    └ TypeError::NotEnoughElementsToUnpack
    "###);
}

#[test]
fn infer_var_with_union_type_annotation() {
    let src = r#"
    let a: number | string = 5
    let b: number | string = "ten"
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "number | string");
    let b = checker.print_type(&ctx.values.get("b").unwrap().index);
    assert_eq!(b, "number | string");
}

#[test]
fn infer_widen_tuple_return() {
    let src = r#"
    let result = fn (cond) => 
        if (cond) {
            [1, 2]
        } else {
            [true, false]
        }
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("result").unwrap().index);
    assert_eq!(result, "(cond: boolean) -> [1, 2] | [true, false]");
}

#[ignore]
#[test]
fn infer_widen_tuples_with_type_annotations() {
    let src = r#"
    let result = fn (cond) => {
        if (cond) {
            let x: [number, number] = [1, 2];
            x
        } else {
            let y: [boolean, boolean] = [true, false];
            y
        }
    }
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("result").unwrap().index);
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
    let (_, (ctx, checker)) = infer_prog(src);

    let x = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(x, "5");
    let y = checker.print_type(&ctx.values.get("y").unwrap().index);
    assert_eq!(y, "10");
}

#[test]
fn infer_member_access_on_obj_lit() {
    let src = r#"let x = {x: 5, y: 10}.x"#;
    let (_, (ctx, checker)) = infer_prog(src);

    let x = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(x, "5");
}

#[test]
fn infer_fn_using_type_decl() {
    let src = r#"
    type Point = {x: number, y: number}
    let mag = fn (p: Point) => p.x * p.x + p.y * p.y
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("mag").unwrap().index);
    assert_eq!(result, "(p: Point) -> number");
}

// TODO: infer JSX element type
#[test]
#[ignore]
fn infer_react_component() {
    let src = r#"
    type Props = {name: string}
    let Foo = fn (props: Props) {
        return <div>Hello, world</div>
    }
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("Foo").unwrap().index);
    assert_eq!(result, "(props: Props) => JSXElement");
}

#[test]
fn codegen_code_with_type_delcarations() -> Result<(), TypeError> {
    let src = r#"
    type Point = {x: number, y: number}
    let point: Point = {x: 5, y: 10}
    "#;
    let (program, (ctx, checker)) = infer_prog(src);
    let (js, _) = codegen_js(src, &program);

    insta::assert_snapshot!(js, @r###"
    ;
    export const point = {
        x: 5,
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    declare type Point = {
        x: number;
        y: number;
    };
    declare type ReadonlyPoint = {
        readonly x: number;
        readonly y: number;
    };
    export declare const point: {
        x: number;
        y: number;
    };
    "###);

    Ok(())
}

#[test]
fn infer_mem_access_with_optional_prop() {
    let src = r#"
    declare let point: {x?: number, y: number}
    let x = point.x
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "number | undefined");
}

// TODO: this should error
#[test]
#[ignore]
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
    let (_, (ctx, checker)) = infer_prog(src);

    let x = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(x, "number | undefined");
}

#[test]
fn infer_assigning_an_obj_lit_with_extra_props() {
    let src = r#"
    let point: {x: number, y: number} = {x: 5, y: 10, z: 15}
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let point = checker.print_type(&ctx.values.get("point").unwrap().index);
    assert_eq!(point, "{x: number, y: number}");
}

#[test]
fn infer_function_overloading() {
    let src = r#"
    declare let add: (fn (a: number, b: number) -> number) & (fn (a: string, b: string) -> string)
    let num = add(5, 10)
    let str = add("hello, ", "world")
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let num_result = checker.print_type(&ctx.values.get("num").unwrap().index);
    assert_eq!(num_result, "number");
    let str_result = checker.print_type(&ctx.values.get("str").unwrap().index);
    assert_eq!(str_result, "string");
}

#[test]
#[should_panic = "no valid overload for args"]
fn infer_function_overloading_with_incorrect_args() {
    let src = r#"
    declare let add: (fn (a: number, b: number) -> number) & (fn (a: string, b: string) -> string)
    let bool = add(true, false)
    "#;
    infer_prog(src);
}

#[test]
fn codegen_object_type_with_optional_property() -> Result<(), TypeError> {
    let src = r#"
    type Point = {x?: number, y: number}
    let point: Point = {y: 10}
    "#;
    let (program, (ctx, checker)) = infer_prog(src);
    let (js, _) = codegen_js(src, &program);

    insta::assert_snapshot!(js, @r###"
    ;
    export const point = {
        y: 10
    };
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    // TODO: wrap type annotations for immutable bindings in Readonly<>
    // TODO: use the type annotation for bindings when present instead of
    // the inferred type
    insta::assert_snapshot!(result, @r###"
    declare type Point = {
        x?: number;
        y: number;
    };
    declare type ReadonlyPoint = {
        readonly x?: number;
        readonly y: number;
    };
    export declare const point: {
        x?: number;
        y: number;
    };
    "###);

    Ok(())
}

#[test]
fn infer_nested_block() {
    let src = r#"
    let result = do {
        let sum = do {
            let x = 5
            let y = 10
            x + y
        }
        sum
    }"#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("result").unwrap().index);
    assert_eq!(result, "15");
}

#[test]
fn infer_block_with_multiple_non_let_lines() {
    // TODO:
    // instead of inferring that `x` should be a `number` when we see
    // `x + 0`, we should instead infer that it's a `subtype of number`.
    // that way when we reconcile it with the other inferred type of `x`
    // which is `5`, the final inferred type will be `5`.
    let src = r#"
    let result = do {
        let x = 5
        x + 0
        x
    }"#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("result").unwrap().index);
    assert_eq!(result, "5");
}

#[test]
fn codegen_block_with_multiple_non_let_lines() -> Result<(), TypeError> {
    let src = r#"
    let result = do {
        let x = 5
        x + 0
        x
    }"#;
    let (program, (ctx, checker)) = infer_prog(src);
    let (js, _) = codegen_js(src, &program);

    insta::assert_snapshot!(js, @r###"
    let $temp_0;
    {
        const x = 5;
        x + 0;
        $temp_0 = x;
    }export const result = $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @"export declare const result: 5;
");

    Ok(())
}

#[test]
fn infer_type_alias_with_param() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let foo: Foo<string>
    let bar = foo.bar
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("bar").unwrap().index);
    assert_eq!(result, "string");
}

#[test]
fn infer_fn_param_with_type_alias_with_param() {
    let src = r#"
    type Foo<T> = {bar: T}
    let get_bar = fn (foo: Foo<string>) => foo.bar
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("bar").unwrap().index);
    assert_eq!(result, "string");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_2() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: fn <T>(foo: Foo<T>) -> T
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("get_bar").unwrap().index);
    // TODO: normalize the type before inserting it into the context
    insta::assert_snapshot!(result, @"<T>(foo: Foo<T>) -> T");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_3() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: fn <T>(foo: Foo<T>) -> T
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("bar").unwrap().index);
    assert_eq!(result, "\"hello\"");
}

#[test]
fn infer_fn_param_with_type_alias_with_param_4() {
    let src = r#"
    type Foo<T> = {bar: T}
    let get_bar = fn <T>(foo: Foo<T>) => foo.bar
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("get_bar").unwrap().index);
    insta::assert_snapshot!(result, @"<T>(foo: Foo<T>) -> T");

    let result = checker.print_type(&ctx.values.get("bar").unwrap().index);
    assert_eq!(result, "\"hello\"");
}

#[test]
fn infer_with_constrained_polymorphism_success() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: fn <T: string>(foo: Foo<T>) -> T
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("bar").unwrap().index);
    assert_eq!(result, "\"hello\"");
}

// TODO: this should error
#[test]
#[ignore]
// TODO: figure out how to get the type constraints in the error message
fn infer_with_constrained_polymorphism_failiure() {
    let src = r#"
    type Foo<T> = {bar: T}
    declare let get_bar: fn <T: number>(foo: Foo<T>) -> T
    let bar = get_bar({bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1000 - Function arguments are incorrect:
    └ TypeError: type mismatch: unify("hello", number) failed
    "###);

    let result = checker.print_type(&ctx.values.get("bar").unwrap().index);
    // TODO: this should be "number"
    assert_eq!(result, "t21");
}

#[test]
fn infer_generic_type_aliases() {
    // What's the difference between these two?
    // In the first one the `<T>` on the left is being shadowed by the `<T>` on the right
    let src = r#"
    type Identity1<T> = fn <T>(foo: T) -> T
    type Identity2 = fn <T>(foo: T) -> T
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_scheme(&ctx.get_scheme("Identity1").unwrap());
    // TODO: normalize the type before inserting it into the context
    // TODO: fix this `<T><T>`
    insta::assert_snapshot!(result, @"<T><T>(foo: T) -> T");

    let result = checker.print_scheme(&ctx.get_scheme("Identity2").unwrap());
    // TODO: normalize the type before inserting it into the context
    insta::assert_snapshot!(result, @"<T>(foo: T) -> T");
}

#[test]
fn infer_destructure_all_object_properties() {
    let src = r#"
    let point = {x: 5, y: 10}
    let {x, y} = point
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "5");
    let result = checker.print_type(&ctx.values.get("y").unwrap().index);
    assert_eq!(result, "10");
}

#[test]
fn infer_destructure_some_object_properties() {
    let src = r#"
    let point = {x: 5, y: 10}
    let {x} = point
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "5");
}

#[test]
fn infer_destructure_some_object_properties_with_renaming() {
    let src = r#"
    let point = {x: 5, y: 10}
    let {x: a} = point
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(result, "5");
}

#[test]
fn infer_destructure_object_with_type_alias() {
    let src = r#"
    type Point = {x: number, y: number}
    let point: Point = {x: 5, y: 10}
    let {x, y} = point
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_inside_fn() {
    let src = r#"
    type FooBar = {foo: number, bar: string}
    let get_foo = fn (x: FooBar) {
        let {foo, bar} = x
        return foo
    }
    let foo = get_foo({foo: 5, bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("foo").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_inside_fn_2() {
    let src = r#"
    type FooBar = {foo: number, bar: string}
    let get_foo = fn (x: FooBar) {
        let {foo, bar} = x
        return foo
    }
    let foo = get_foo({foo: 5, bar: "hello"})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let result = checker.print_type(&ctx.values.get("foo").unwrap().index);
    assert_eq!(result, "number");
}

#[test]
fn infer_destructure_object_param() {
    let src = r#"
    let foo = fn ({a, b}: {a: string, b: number}) => a
    let a = foo({a: "hello", b: 5})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "string");
}

#[test]
fn infer_destructure_object_param_2() {
    let src = r#"
    let foo = fn ({a, b}: {a: string, b: number}) {
        return {a: a, b: b}
    }
    let {a, b} = foo({a: "hello", b: 5})
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "string");

    let b = checker.print_type(&ctx.values.get("b").unwrap().index);
    assert_eq!(b, "number");
}

#[test]
fn return_an_object() {
    let src = r#"
    let foo = fn () {
        return {a: "hello", b: 5}
    }
    let {a, b} = foo()
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "\"hello\"");

    let b = checker.print_type(&ctx.values.get("b").unwrap().index);
    assert_eq!(b, "5");
}

#[test]
fn object_property_shorthand() {
    let src = r#"
    let a = "hello"
    let b = 5
    let c = {a, b}
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let c = checker.print_type(&ctx.values.get("c").unwrap().index);
    assert_eq!(c, "{a: \"hello\", b: 5}");
}

#[test]
fn infer_destructuring_with_optional_properties() {
    let src = r#"
    let p: {x?: number, y: number} = {y: 10}
    let {x, y: _} = p
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let x = checker.print_type(&ctx.values.get("x").unwrap().index);
    assert_eq!(x, "number | undefined");
    assert!(ctx.values.get("y").is_none());
}

#[test]
fn infer_destructure_tuple() {
    let src = r#"
    let [a, b] = ["hello", 5]
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "\"hello\"");

    let b = checker.print_type(&ctx.values.get("b").unwrap().index);
    assert_eq!(b, "5");
}

#[test]
#[should_panic = "Expected tuple of length 3, got tuple of length 2"]
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
    let foo = fn (bar: [string, number]) => bar
    let [a, b] = foo(["hello", 5])
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "string");

    let b = checker.print_type(&ctx.values.get("b").unwrap().index);
    assert_eq!(b, "number");
}

#[test]
fn destructure_lam_param_tuple() {
    let src = r#"
    let foo = fn ([a, b]: [string, number]) => [a, b]
    let [a, b] = foo(["hello", 5])
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let a = checker.print_type(&ctx.values.get("a").unwrap().index);
    assert_eq!(a, "string");

    let b = checker.print_type(&ctx.values.get("b").unwrap().index);
    assert_eq!(b, "number");
}

// TODO: infer JSX element type
#[test]
#[ignore]
fn infer_jsx() {
    let src = r#"
    type JSXElement = {}
    let point = {x: 5, y: 10}
    let msg = "world"
    let elem = <div point={point} id="point">Hello, {msg}</div>
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    let elem = checker.print_type(&ctx.values.get("elem").unwrap().index);
    assert_eq!(elem, "JSXElement");
}

#[test]
fn incorrect_args() {
    let src = r#"
    let add = fn (a, b) => a + b
    add("hello", "world")
    "#;

    let (_, (_, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1000 - Function arguments are incorrect:
    ├ TypeError: type mismatch: unify("hello", number) failed
    └ TypeError: type mismatch: unify("world", number) failed
    "###);
}

#[test]
fn return_empty() {
    let src = r#"
    let foo = fn () {}
    "#;
    let (_, (ctx, checker)) = infer_prog(src);

    assert_eq!(
        checker.print_type(&ctx.values.get("foo").unwrap().index),
        "() -> undefined"
    );
}

#[test]
fn return_empty_with_body() {
    let src = r#"
    let foo = fn () {
        let a = 5
    }
    "#;

    let (_, (ctx, checker)) = infer_prog(src);

    let func = checker.print_type(&ctx.values.get("foo").unwrap().index);
    assert_eq!(func, "() -> undefined");
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let() {
    let src = r#"
    let p = {x: 5, y: 10}
    if (let {x, y} = p) {
        x + y
    }
    "#;

    let (_, (ctx, checker)) = infer_prog(src);

    assert_eq!(
        checker.print_type(&ctx.values.get("p").unwrap().index),
        "{x: 5, y: 10}"
    );
    // Ensures we aren't polluting the outside context
    assert!(ctx.values.get("x").is_none());
    assert!(ctx.values.get("y").is_none());
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_with_is() {
    let src = r#"
    declare let b: string | number
    if (let a is string = b) {
        a
    }
    "#;

    let (_, (ctx, checker)) = infer_prog(src);

    assert_eq!(
        checker.print_type(&ctx.values.get("b").unwrap().index),
        "number | string"
    );
    // Ensures we aren't polluting the outside context
    assert!(ctx.values.get("a").is_none());
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn codegen_if_let() -> Result<(), TypeError> {
    let src = r#"
    let p = {x: 5, y: 10}
    if (let {x, y} = p) {
        x + y
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    export const p = {
        x: 5,
        y: 10
    };
    let $temp_0;
    const $temp_1 = p;
    {
        const { x, y } = $temp_1;
        $temp_0 = x + y;
    }$temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    export declare const p: {
        readonly x: 5;
        readonly y: 10;
    };
    "###);

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn codegen_if_let_with_rename() -> Result<(), TypeError> {
    let src = r#"
    let p = {x: 5, y: 10}
    if (let {x: a, y: b} = p) {
        a + b
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    export const p = {
        x: 5,
        y: 10
    };
    let $temp_0;
    const $temp_1 = p;
    {
        const { x: a, y: b } = $temp_1;
        $temp_0 = a + b;
    }$temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    export declare const p: {
        readonly x: 5;
        readonly y: 10;
    };
    "###);

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_with_type_error() {
    let src = r#"
    let p = {x: "hello", y: "world"}
    if (let {x, y} = p) {
        x + y
    }
    "#;

    let (_, (_, checker)) = infer_prog(src);

    insta::assert_snapshot!(current_report_message(&checker), @r###"
    ESC_1 - "hello" is not a number:
    └ TypeError::UnificationError: "hello", number

    ESC_1 - "world" is not a number:
    └ TypeError::UnificationError: "world", number
    "###);
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_refutable_pattern_obj() -> Result<(), TypeError> {
    let src = r#"
    let p = {x: 5, y: 10}
    if (let {x: 5, y} = p) {
        y
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    assert_eq!(
        checker.print_type(&ctx.values.get("p").unwrap().index),
        "{x: 5, y: 10}"
    );

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    export const p = {
        x: 5,
        y: 10
    };
    let $temp_0;
    const $temp_1 = p;
    if ($temp_1.x === 5) {
        const { y } = $temp_1;
        $temp_0 = y;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    export declare const p: {
        readonly x: 5;
        readonly y: 10;
    };
    "###);

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_refutable_pattern_nested_obj() -> Result<(), TypeError> {
    let src = r#"
    let action = {type: "moveto", point: {x: 5, y: 10}}
    if (let {type: "moveto", point: {x, y}} = action) {
        x + y
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
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
        const { point: { x, y } } = $temp_1;
        $temp_0 = x + y;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"
    export declare const action: {
        readonly type: "moveto";
        readonly point: {
            readonly x: 5;
            readonly y: 10;
        };
    };
    "###);

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_refutable_pattern_with_disjoint_union() -> Result<(), TypeError> {
    let src = r#"
    type Point = {x: number, y: number}
    type Action = {type: "moveto", point: Point} | {type: "lineto", point: Point}
    declare let action: Action
    if (let {type: "moveto", point: {x, y}} = action) {
        x + y
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    ;
    ;
    ;
    let $temp_0;
    const $temp_1 = action;
    if ($temp_1.type === "moveto") {
        const { point: { x, y } } = $temp_1;
        $temp_0 = x + y;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

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

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_refutable_pattern_array() -> Result<(), TypeError> {
    let src = r#"
    let p = [5, 10]
    if (let [5, y] = p) {
        y
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    assert_eq!(
        checker.print_type(&ctx.values.get("p").unwrap().index),
        "[5, 10]"
    );

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    export const p = [
        5,
        10
    ];
    let $temp_0;
    const $temp_1 = p;
    if ($temp_1[0] === 5) {
        const [, y] = $temp_1;
        $temp_0 = y;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @"export declare const p: readonly [5, 10];
");

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn infer_if_let_refutable_pattern_nested_array() -> Result<(), TypeError> {
    let src = r#"
    let action = ["moveto", [5, 10]];
    if (let ["moveto", [x, y]] = action) {
        x + y;
    };
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
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
        $temp_0 = x + y;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @r###"export declare const action: readonly ["moveto", readonly [5, 10]];
"###);

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
fn codegen_if_let_with_is_prim() -> Result<(), TypeError> {
    let src = r#"
    declare let b: string | number
    if (let a is number = b) {
        a + 5
    }
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    ;
    let $temp_0;
    const $temp_1 = b;
    if (typeof $temp_1 === "number") {
        const a = $temp_1;
        $temp_0 = a + 5;
    }
    $temp_0;
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;

    insta::assert_snapshot!(result, @"export declare const b: number | string;
");

    Ok(())
}

// TODO: decide if we want to support `if-let`
#[test]
#[ignore]
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
    if (let a is Foo = b) {
        a.getNum() + 5;
    }
    "#;

    let result = parse(src);
    let program = match result {
        Ok(prog) => prog,
        Err(err) => {
            println!("err = {:?}", err);
            panic!("Error parsing expression");
        }
    };

    let (js, _) = codegen_js(src, &program);
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
        $temp_0 = a.getNum() + 5;
    }
    $temp_0;
    "###);
}

#[test]
fn codegen_array() -> Result<(), TypeError> {
    let src = r#"
    let arr: string[] = ["hello", "world"]
    "#;

    let (program, (ctx, checker)) = infer_prog(src);

    let (js, _) = codegen_js(src, &program);
    insta::assert_snapshot!(js, @r###"
    export const arr = [
        "hello",
        "world"
    ];
    "###);

    let result = codegen_d_ts(&program, &ctx, &checker)?;
    insta::assert_snapshot!(result, @"export declare const arr: readonly string[];
");

    Ok(())
}
