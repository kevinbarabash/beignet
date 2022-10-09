mod context;
mod infer_expr;
mod infer_fn_param;
mod infer_pattern;
mod infer_type_ann;
mod key_of;
mod substitutable;
mod unify;
mod update;
mod util;

pub mod infer;

pub use context::*;
pub use infer::*;
pub use substitutable::{Subst, Substitutable};
pub use util::{generalize_type, get_type_params, normalize, set_type_params};

#[cfg(test)]
mod tests {
    use crochet_parser::*;

    use super::*;

    fn infer(input: &str) -> String {
        let mut ctx = Context::default();
        let mut prog = parse(&format!("let x = {input};")).unwrap();
        let ctx = infer::infer_prog(&mut prog, &mut ctx).unwrap();
        get_value_type("x", &ctx)
    }

    fn infer_prog(input: &str) -> Context {
        let mut prog = parse(input).unwrap();
        let mut ctx: Context = Context::default();
        infer::infer_prog(&mut prog, &mut ctx).unwrap()
    }

    fn get_value_type(name: &str, ctx: &Context) -> String {
        match ctx.lookup_value(name) {
            Ok(t) => format!("{t}"),
            Err(_) => panic!("Couldn't find type with name '{name}'"),
        }
    }

    fn get_type_type(name: &str, ctx: &Context) -> String {
        match ctx.lookup_type(name) {
            Ok(t) => format!("{t}"),
            Err(_) => panic!("Couldn't find type with name '{name}'"),
        }
    }

    #[test]
    fn infer_i_combinator() {
        let ctx = infer_prog("let I = (x) => x;");
        assert_eq!(get_value_type("I", &ctx), "<t0>(x: t0) => t0");
    }

    #[test]
    fn infer_k_combinator() {
        let ctx = infer_prog("let K = (x) => (y) => x;");
        assert_eq!(
            get_value_type("K", &ctx),
            "<t0, t1>(x: t0) => (y: t1) => t0"
        );
    }

    #[test]
    fn infer_s_combinator() {
        let ctx = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x));");
        assert_eq!(
            get_value_type("S", &ctx),
            "<t0, t1, t2>(f: (t0) => (t1) => t2) => (g: (t0) => t1) => (x: t0) => t2"
        );
    }

    #[test]
    fn infer_skk() {
        let src = r#"
        let S = (f) => (g) => (x) => f(x)(g(x));
        let K = (x) => (y) => x;
        let I = S(K)(K);
        "#;
        let ctx = infer_prog(src);
        assert_eq!(
            get_value_type("K", &ctx),
            "<t0, t1>(x: t0) => (y: t1) => t0"
        );
        assert_eq!(
            get_value_type("S", &ctx),
            "<t0, t1, t2>(f: (t0) => (t1) => t2) => (g: (t0) => t1) => (x: t0) => t2"
        );
        assert_eq!(get_value_type("I", &ctx), "<t0>(x: t0) => t0");
    }

    #[test]
    fn infer_adding_variables() {
        let src = r#"
        let x = 5;
        let y = 10;
        let z = x + y;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("z", &ctx), "number");
    }

    #[test]
    fn infer_unary_minus() {
        let src = r#"
        let negate = (x) => -x;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("negate", &ctx), "(x: number) => number");
    }

    #[test]
    fn infer_if_else() {
        let src = r#"
        let n = 0;
        let result = if (n == 0) { 5 } else { 10 };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "10 | 5");
    }

    #[test]
    fn infer_if() {
        let src = r#"
        let n = 0;
        let result = if (n == 0) { 5; };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "undefined");
    }

    #[test]
    #[should_panic = "Consequent for 'if' without 'else' must not return a value"]
    fn infer_if_must_be_undefined() {
        let src = r#"
        let n = 0;
        let result = if (n == 0) { 5 };
        "#;
        infer_prog(src);
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
        }
        "###;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("fib", &ctx), "(n: number) => number");
    }

    #[test]
    fn infer_app_of_lam() {
        assert_eq!(infer("((x) => x)(5)"), "5");
    }

    #[test]
    fn inner_let() {
        assert_eq!(infer("() => {let x = 5; x}"), "() => 5");
    }

    #[test]
    fn inner_let_with_type_annotation() {
        assert_eq!(infer("() => {let x: number = 5; x}"), "() => number");
    }

    #[test]
    fn infer_tuple() {
        assert_eq!(infer("[5, true, \"hello\"]"), "[5, true, \"hello\"]");
    }

    #[test]
    fn basic_subtyping_assignment() {
        let ctx = infer_prog("let a: number = 5;");

        assert_eq!(get_value_type("a", &ctx), "number");
    }

    #[test]
    fn infer_destructuring_tuple() {
        let src = r#"
        let [a, b, c] = [5, true, "hello"];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "true");
        assert_eq!(get_value_type("c", &ctx), "\"hello\"");
    }

    #[test]
    fn infer_destructuring_tuple_extra_init_elems() {
        let src = r#"
        let [a, b] = [5, true, "hello"];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "true");
    }

    #[test]
    fn infer_destructuring_tuple_with_wildcard() {
        let src = r#"
        let [_, a] = [5, true];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "true");
    }

    #[test]
    #[should_panic = "Duplicate identifier in pattern"]
    fn infer_destructuring_tuple_reused_identifier() {
        let src = r#"
        let [a, a] = [5, true];
        "#;
        infer_prog(src);
    }

    #[test]
    fn infer_destructuring_nested_tuples() {
        let src = r#"
        let [[a, b], c] = [[5, true], "hello"];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "true");
        assert_eq!(get_value_type("c", &ctx), "\"hello\"");
    }

    // #[test]
    // fn infer_destructuring_function_params() {
    //     let result = infer("([x, y]) => x + y");

    //     assert_eq!(result, "([x, y]: [number, number]) => number");
    // }

    // #[test]
    // fn infer_destructuring_function_params_with_type_annotations() {
    //     let result = infer("([x, y]: [string, boolean]) => [x, y]");

    //     assert_eq!(result, "([x, y]: [string, boolean]) => [string, boolean]");
    // }

    // #[test]
    // fn infer_incomplete_destructuring_function_params_with_type_annotations() {
    //     let result = infer("([x]: [string, boolean]) => x");

    //     assert_eq!(result, "([x]: [string, boolean]) => string");
    // }

    // #[test]
    // fn destructuring_tuple_inside_lambda() {
    //     let result = infer("(p) => {let [x, y] = p; x + y}");

    //     assert_eq!(result, "(p: [number, number]) => number");
    // }

    #[test]
    #[should_panic = "not enough elements to unpack"]
    fn infer_destructuring_tuple_extra_init_elems_too_many_elements_to_unpack() {
        let src = r#"
        let [a, b, c, d] = [5, true, "hello"];
        "#;
        infer_prog(src);
    }

    #[test]
    fn infer_destructuring_tuple_with_type_annotation() {
        let src = r#"
        let [a, b, c]: [number, boolean, string] = [5, true, "hello"];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "number");
        assert_eq!(get_value_type("b", &ctx), "boolean");
        assert_eq!(get_value_type("c", &ctx), "string");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn infer_destructuring_tuple_with_incorrect_type_annotation() {
        let src = r#"
        let [a, b, c]: [number, boolean, string] = [5, "hello", true];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "number");
        assert_eq!(get_value_type("b", &ctx), "boolean");
        assert_eq!(get_value_type("c", &ctx), "string");
    }

    #[test]
    fn infer_destructuring_tuple_extra_init_elems_with_type_annotation() {
        let src = r#"
        let [a, b]: [number, boolean] = [5, true, "hello"];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "number");
        assert_eq!(get_value_type("b", &ctx), "boolean");
    }

    #[test]
    #[should_panic = "not enough elements to unpack"]
    fn infer_destructuring_tuple_extra_init_elems_too_many_elements_to_unpack_with_type_annotation()
    {
        let src = r#"
        let [a, b, c, d]: [number, boolean, string, number] = [5, true, "hello"];
        "#;
        infer_prog(src);
    }

    #[test]
    fn infer_obj() {
        assert_eq!(infer("{x:5, y: 10}"), "{x: 5, y: 10}");
    }

    #[test]
    fn infer_nested_obj() {
        assert_eq!(
            infer("{a: {b: {c: \"hello\"}}}"),
            "{a: {b: {c: \"hello\"}}}"
        );
    }

    #[test]
    fn destructure_obj() {
        let ctx = infer_prog("let {x, y} = {x: 5, y: 10};");

        assert_eq!(get_value_type("x", &ctx), "5");
        assert_eq!(get_value_type("y", &ctx), "10");
    }

    #[test]
    fn destructure_obj_with_optional() {
        let src = r#"
        declare let point: {x: number, y: number, z?: number};
        let {x, y, z} = point;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("z", &ctx), "number | undefined");
    }

    #[test]
    fn destructure_obj_with_renaming() {
        let src = "let {x: a, y: b} = {x: 5, y: 10};";
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "10");
        assert!(ctx.lookup_value("x").is_err());
        assert!(ctx.lookup_value("y").is_err());
    }

    #[test]
    #[should_panic = "Duplicate identifier in pattern"]
    fn infer_destructuring_obj_reused_identifier() {
        let src = "let {x: a, y: a} = {x: 5, y: 10};";
        infer_prog(src);
    }

    #[test]
    fn nested_destructure_obj() {
        let ctx = infer_prog("let {a: {b: {c}}} = {a: {b: {c: \"hello\"}}};");

        assert!(ctx.lookup_value("a").is_err());
        assert!(ctx.lookup_value("b").is_err());
        assert_eq!(get_value_type("c", &ctx), "\"hello\"");
    }

    #[test]
    fn partial_destructure_obj() {
        let ctx = infer_prog("let {x} = {x: 5, y: 10}");

        assert_eq!(get_value_type("x", &ctx), "5");
    }

    #[test]
    fn partial_destructure_disjoint_union_common_property() {
        let src = r#"
        declare let obj: {
            type: "foo",
            same: string,
            diff: boolean,
        } | {
            type: "bar",
            same: string,
            diff: number,
        };
        let {same, diff} = obj;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("same", &ctx), "string");
        assert_eq!(get_value_type("diff", &ctx), "boolean | number");
    }

    #[test]
    fn partial_destructure_disjoint_union_common_optional_property() {
        let src = r#"
        declare let obj: {type: "foo", value?: string} | {type: "bar", value?: number};
        let {value} = obj;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("value", &ctx), "number | string | undefined");
    }

    // TODO: In order for this to work, we need custom handling for unifying
    // a union of objects with an object.
    #[test]
    #[ignore]
    fn partial_destructure_disjoint_union_uncommon_property() {
        let src = r#"
        declare let obj: {type: "foo", value: string} | {type: "bar"};
        let {value} = obj;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("value", &ctx), "string | undefined");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn missing_property_when_destructuring() {
        infer_prog("let {foo} = {x: 5, y: 10};");
    }

    #[test]
    fn obj_assignment() {
        let ctx = infer_prog("let p = {x: 5, y: 10};");

        assert_eq!(get_value_type("p", &ctx), "{x: 5, y: 10}");
    }

    #[test]
    fn obj_assignment_shorthand() {
        let src = r#"
        let x = 5;
        let y = 10;
        let p = {x, y};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("p", &ctx), "{x: 5, y: 10}");
    }

    #[test]
    #[should_panic]
    fn obj_assignment_shorthand_missing_variable() {
        let src = r#"
        let x = 5;
        let p = {x, y};
        "#;

        infer_prog(src);
    }

    #[test]
    fn obj_assignment_with_type_annotation() {
        let ctx = infer_prog("let p: {x: number, y: number} = {x: 5, y: 10};");

        assert_eq!(get_value_type("p", &ctx), "{x: number, y: number}");
    }

    #[test]
    fn obj_assignment_with_type_annotation_extra_properties() {
        let ctx = infer_prog("let p: {x: number, y: number} = {x: 5, y: 10, z: 15};");

        assert_eq!(get_value_type("p", &ctx), "{x: number, y: number}");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn obj_assignment_with_type_annotation_missing_properties() {
        infer_prog("let p: {x: number, y: number} = {x: 5};");
    }

    #[test]
    fn infer_assigning_to_obj_with_optional_props() {
        let src = r#"
        let p: {x?: number, y: number} = {y: 10};
        let x = p.x;
        "#;
        let ctx = infer_prog(src);

        let x = format!("{}", ctx.lookup_value("x").unwrap());
        assert_eq!(x, "number | undefined");
    }

    #[test]
    fn obj_param_destructuring() {
        assert_eq!(
            infer("({x, y}) => x + y"),
            "({x, y}: {x: number, y: number}) => number"
        );
    }

    #[test]
    fn obj_param_destructuring_with_renaming() {
        assert_eq!(
            infer("({x: p, y: q}) => p + q"),
            "({x: p, y: q}: {x: number, y: number}) => number"
        );
    }

    #[test]
    fn obj_param_destructuring_with_type_annotation() {
        assert_eq!(
            infer("({x, y}: {x: 5, y: 10}) => x + y"),
            "({x, y}: {x: 5, y: 10}) => number"
        );
    }

    #[test]
    fn obj_param_partial_destructuring_with_type_annotation() {
        assert_eq!(
            infer("({a}: {a: string, b: boolean}) => a"),
            "({a}: {a: string, b: boolean}) => string"
        );
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn obj_destructuring_with_type_annotation_missing_param() {
        infer("({c}: {a: string, b: boolean}) => c");
    }

    #[test]
    fn destructuring_inside_lambda() {
        assert_eq!(
            infer("(p) => {let {x, y} = p; x + y}"),
            "(p: {x: number, y: number}) => number"
        );
    }

    #[test]
    fn infer_if_let() {
        let src = r#"
        let p = {x: 5, y: 10};
        let sum = if (let {x, y} = p) {
            x + y
        } else {
            0
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "number");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
        assert!(ctx.lookup_value("y").is_err());
    }

    #[test]
    fn infer_if_let_without_else_no_return() {
        let src = r#"
        let p = {x: 5, y: 10};
        if (let {x, y} = p) {
            x + y;
        };
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Consequent for 'if' without 'else' must not return a value"]
    fn infer_if_let_without_else_errors_with_return() {
        let src = r#"
        let p = {x: 5, y: 10};
        if (let {x, y} = p) {
            x + y
        };
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_if_let_chaining() {
        let src = r#"
        let p = {x: 5, y: 10};
        let c = {a: 0, b: 1};
        let sum = if (let {x, y} = p) {
            x
        } else if (let {a, b} = c) {
            b
        } else {
            0
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "0 | 1 | 5");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
        assert!(ctx.lookup_value("y").is_err());
    }

    #[test]
    fn infer_if_let_inside_lambda() {
        let src = r#"
        let add = (p) => {
            if (let {x, y} = p) {
                x + y
            } else {
                0
            }
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("add", &ctx),
            "(p: {x: number, y: number}) => number"
        );
    }

    #[test]
    fn infer_if_let_refutable_is() {
        let src = r#"
        declare let a: string | number;
        let sum = if (let x is number = a) {
            x + 5
        } else {
            0
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "number");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
    }

    #[test]
    fn infer_if_let_refutable_is_with_unreachable_code() {
        let src = r#"
        declare let a: string | number;
        let result = if (let x is number = a) {
            x + 5
        } else if (let y is string = a) {
            y
        } else {
            true
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number | string | true");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
        assert!(ctx.lookup_value("y").is_err());
    }

    #[test]
    #[should_panic = "Can't unify string with number"]
    fn infer_if_let_refutable_is_unification_failure() {
        let src = r#"
        declare let a: string | number;
        let sum = if (let x is string = a) {
            x + 5
        };
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_if_let_refutable_is_inside_obj() {
        let src = r#"
        declare let foo: {bar: string | number};
        let sum = if (let {bar: x is number} = foo) {
            x + 5
        } else {
            0
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "number");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn infer_if_let_is_string_with_literal() {
        // NOTE: This doesn't unify because `string` is not a subtype
        // of the string literal "hello".
        // TODO: introduce Variant::Is(Type) so that we can allow this
        // to unify.  The resulting type of `a` should be the "hello"
        // literal in this case, but more generally it should be a subtype
        // of string that matches the expression being matched against.
        let src = r#"
        let tuple = ["hello", "world"];
        if (let [a is string, b] = tuple) {
            b;
        };
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn infer_if_let_refutable_is_inside_obj_incorrect_type() {
        let src = r#"
        declare let foo: {bar: string};
        let result = if (let {bar: x is number} = foo) {
            x
        } else {
            0
        };
        "#;

        infer_prog(src);
    }

    // TODO: figure out what to do when there are multiple matches... it should be the same
    // thing that happens when destructuring a union of objects (or union of tuples).
    #[test]
    fn infer_if_let_disjoint_union() {
        let src = r#"
        declare let action: {type: "foo", num: number} | {type: "bar", str: string};
        let result = if (let {type: "foo", num} = action) {
            num + 5
        } else {
            0
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
    }

    #[test]
    fn infer_if_let_disjoint_union_chaining() {
        let src = r#"
        declare let action: {type: "foo", num: number} | {type: "bar", str: string};
        let result = if (let {type: "foo", num} = action) {
            num + 5
        } else if (let {type: "bar", str} = action) {
            str
        } else {
            true
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number | string | true");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn infer_if_let_disjoint_union_no_matches() {
        let src = r#"
        declare let action: {type: "foo", num: number} | {type: "bar", str: string};
        let result = if (let {type: "bar", num} = action) {
            num
        } else {
            0
        };
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Can't unify string with number"]
    fn infer_if_let_disjoint_union_incorrect_match() {
        let src = r#"
        declare let action: {type: "foo", num: number} | {type: "bar", str: string};
        let result = if (let {type: "bar", str} = action) {
            str + 5
        } else {
            0
        };
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_if_let_refutable_is_inside_array() {
        let src = r#"
        declare let point: [string | number, string | number]
        let result = if (let [x is number, y is number] = point) {
            x + y
        } else {
            0
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");

        // Ensures we aren't polluting the outside context
        assert!(ctx.lookup_value("x").is_err());
        assert!(ctx.lookup_value("y").is_err());
    }

    // TODO: handle refutable patterns in if-else
    // TODO: handle irrefutable patterns with union types
    // e.g. given the following type:
    // foo: {a: number, b: number} | {x: string, y: string}
    // or
    // bar: {a: number, b: number} | {a: string, a: string}
    // if we do:
    // if let {a, b} = foo { ... }
    // a and b will both be numbers
    // if let {a, b} = bar { ... }
    // a and b will both be number | string

    #[test]
    fn infer_let_ignore_result() {
        assert_eq!(infer("() => {let _ = 5; 10}"), "() => 10");
    }

    #[test]
    #[ignore]
    fn infer_let_wildcard() {
        // TODO: decide if we really want `_` identifiers to be
        // ignored everywhere.
        let src = r#"
        let _ = 5;
        "#;

        let ctx = infer_prog(src);

        assert!(ctx.lookup_value("_").is_err());
    }

    #[test]
    fn infer_expression_statements() {
        assert_eq!(infer("() => {5; 10}"), "() => 10");
    }

    #[test]
    fn type_decl() {
        let src = r#"
        type Point = {x: number, y: number};
        let p: Point = {x: 5, y: 10};
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("p", &ctx), "Point");
    }

    #[test]
    fn external_decl() {
        let src = r#"
        type Point = {x: number, y: number};
        declare let p: Point;
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("p", &ctx), "Point");
    }

    #[test]
    fn lambda_with_type_param() {
        let src = r#"
        let fst = <T>(a: T, b: T): T => {
            a
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("fst", &ctx), "<t0>(a: t0, b: t0) => t0");
    }

    #[test]
    fn lambda_with_multiple_type_params() {
        let src = r#"
        let fst = <A, B>(a: A, b: B): A => {
            a
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("fst", &ctx), "<t0, t1>(a: t0, b: t1) => t0");
    }

    #[test]
    fn calling_a_generic_function() {
        let src = r#"
        let fst = <A, B>(a: A, b: B): A => {
            a
        };
        let result = fst(5, 10);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "5");
    }

    #[test]
    fn calling_lambda_returning_generic_lambda() {
        let src = r#"
        let run = () => (a, b) => a;
        let fst = run();
        let x = fst(5, 10);
        let y = fst(true, false);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("fst", &ctx), "<t0, t1>(a: t0, b: t1) => t0");
        assert_eq!(get_value_type("x", &ctx), "5");
        assert_eq!(get_value_type("y", &ctx), "true");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn calling_generic_lambda_inside_lambda() {
        let src = r#"
        let run = () => {
            let fst = (a, b) => a;
            [fst(5, 10), fst(true, false)]
        };
        let result = run();
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "[5, true]");
    }

    #[test]
    fn call_fn_with_optional_param() {
        let src = r#"
        let plus_one = (a, b?) => a + 1;
        let result = plus_one(5);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
        assert_eq!(
            get_value_type("plus_one", &ctx),
            "<t0>(a: number, b?: t0) => number"
        );
    }

    #[test]
    fn optional_param_with_type_annotation_inferred_as_union_with_undefined() {
        let src = r#"
        let foo = (a?: string) => a;
        let result1 = foo();
        let result2 = foo("hello");
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("foo", &ctx),
            "(a?: string) => string | undefined"
        );
        assert_eq!(get_value_type("result1", &ctx), "string | undefined");
        assert_eq!(get_value_type("result2", &ctx), "string | undefined");
    }

    #[test]
    fn optional_param_type_inferred_as_union_with_undefined() {
        let src = r#"
        let foo = (a?: number) => a;
        let bar = (a?) => foo(a);
        let result1 = bar();
        let result2 = bar(5);
        "#;
        let ctx = infer_prog(src);

        // TODO: bar should be inferred as `(a?: number) => number | undefined`
        // This is likely because foo's `a` param's type is `number | undefined`.
        // This suggests that we should introduce a `Optional` type into the type
        // system that can be differentiated from `T | undefined`.
        assert_eq!(
            get_value_type("bar", &ctx),
            "(a?: number | undefined) => number | undefined"
        );
        assert_eq!(get_value_type("result1", &ctx), "number | undefined");
        assert_eq!(get_value_type("result2", &ctx), "number | undefined");
    }

    #[test]
    fn call_fn_with_optional_and_rest_params() {
        let src = r#"
        let plus_one = (a, b?, ...c) => a + 1;
        let result = plus_one(5);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
        assert_eq!(
            get_value_type("plus_one", &ctx),
            "<t0, t1>(a: number, b?: t0, ...c: t1[]) => number"
        );
    }

    #[test]
    fn call_fn_with_optional_and_rest_params_all_args() {
        let src = r#"
        let plus_one = (a, b?: string, ...c: boolean[]) => a + 1;
        let result = plus_one(5, "hello", true, false);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
        assert_eq!(
            get_value_type("plus_one", &ctx),
            "(a: number, b?: string, ...c: boolean[]) => number"
        );
    }

    #[test]
    #[should_panic = "Not enough args provided"]
    fn call_fn_with_rest_param_with_too_few_args() {
        let src = r#"
        let add = (a, b, ...c: number[]) => a + b;
        let add5 = add(5);
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_optional_param_type() {
        let src = r#"
        let plus_one = (a, b?) => {
            match (b) {
                c is string -> c,
                _ -> a + 1
            }
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("plus_one", &ctx),
            "(a: number, b?: string) => number | string"
        );
    }

    #[test]
    fn infer_from_pattern_matching() {
        let src = r#"
        let foo = (arg) => {
            match (arg) {
                {x: x is number, y: y is number} -> x + y,
                {msg: msg is string} -> msg
            }
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("foo", &ctx),
            "(arg: {x: number, y: number}) => number | string"
        );
    }

    #[test]
    fn lambda_with_explicit_types() {
        let src = r#"
        let add = (a: number, b: number): number => {
            a + b
        };
        "#;

        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("add", &ctx),
            "(a: number, b: number) => number"
        );
    }

    #[test]
    #[should_panic = "Can't unify number with string"]
    fn lambda_with_incorrect_return_type() {
        let src = r#"
        let add = (a: number, b: number): string => {
            a + b
        };
        "#;

        infer_prog(src);
    }

    // TODO: make this test case return an error
    #[test]
    #[should_panic = "Can't unify string with number"]
    fn lambda_with_incorrect_param_type() {
        let src = r#"
        let add = (a: number, b: string): number => {
            a + b
        };
        "#;

        infer_prog(src);
    }

    #[test]
    fn call_lam_with_subtypes() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let sum = add(5, 10);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "number");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn call_lam_with_wrong_types() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let sum = add("hello", true);
        "#;

        infer_prog(src);
    }

    #[test]
    fn call_lam_with_extra_params() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let sum = add(5, 10, "hello");
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "number");
    }

    #[test]
    #[should_panic = "Not enough args provided"]
    fn call_lam_with_too_few_params() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let sum = add(5);
        "#;

        infer_prog(src);
    }

    #[test]
    fn pass_callback_with_too_few_params() {
        let src = r#"
        declare let fold_num: (cb: (a: number, b: number) => boolean, seed: number) => number;
        let result = fold_num((x) => true, 0);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    fn pass_callback_whose_params_are_supertypes_of_expected_callback() {
        let src = r#"
        declare let fold_num: (cb: (a: 5, b: 10) => boolean, seed: number) => number;
        let result = fold_num((x: number, y: number) => true, 0);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    #[should_panic = "Couldn't unify lambdas"]
    fn pass_callback_with_too_many_params() {
        // This is not allowed because `fold_num` can't provide all of the params
        // that the callback is expecting and it would result in partial application
        // when the `fold_num` is not expecting it.  While it's possible to conceive
        // of a scenario where a callback returns a function and a partially applied
        // calback results in correct function, allowing this in the type checker
        // is bound to result in confusing and hard to understand code.
        let src = r#"
        declare let fold_num: (cb: (a: number, b: number) => boolean, seed: number) => number;
        let result = fold_num((x, y, z) => true, 0);
        "#;

        infer_prog(src);
    }

    // TODO: TypeScript takes the union of the params when unifying them
    #[test]
    #[ignore]
    fn call_generic_lam_with_subtypes() {
        let src = r#"
        declare let add: <T>(a: T, b: T) => T;
        let sum = add(5, 10);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "5 | 10");
    }

    #[test]
    fn infer_generic_lam() {
        // TODO: figure out how to handle parametric functions like this
        let src = r#"
        let add = <T>(x: T, y: T): T => {
            x + y
        };
        let sum = add(5, 5);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("add", &ctx),
            "(x: number, y: number) => number"
        );
        assert_eq!(get_value_type("sum", &ctx), "number");
    }

    #[test]
    fn spread_param_tuple() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let args = [5, 10];
        let result = add(...args);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    fn spread_param_tuple_with_extra_elements() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let args = [5, 10, 15];
        let result = add(...args);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    fn spread_multiple_param_tuples() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let args1 = [5];
        let args2 = [10];
        let result = add(...args1, ...args2);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn spread_param_tuples_with_incorrect_types() {
        let src = r#"
        declare let add: (a: number, b: number) => number;
        let args = ["hello", true];
        let result = add(...args);
        "#;

        infer_prog(src);
    }

    #[test]
    fn rest_param() {
        let src = r#"
        let fst = (a: number, ...b: number[]) => a;
        let result = fst(5, 10, 15);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("fst", &ctx),
            "(a: number, ...b: number[]) => number"
        );
        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    fn rest_param_with_arg_spread() {
        // TODO: handle the spread directly in infer_expr()
        let src = r#"
        let fst = (a: number, ...b: number[]) => a;
        let nums = [10, 15];
        let result = fst(5, ...nums);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn rest_param_with_arg_spread_and_incorrect_type() {
        // TODO: handle the spread directly in infer_expr()
        let src = r#"
        let fst = (a: number, ...b: number[]) => a;
        let mixed = [10, "hello"];
        let result = fst(5, ...mixed);
        "#;

        infer_prog(src);
    }

    #[test]
    fn empty_rest_param() {
        let src = r#"
        let fst = (a: number, ...b: number[]) => a;
        let result = fst(5);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn rest_param_with_incorrect_arg_type() {
        let src = r#"
        let fst = (a: number, ...b: number[]) => a;
        let result = fst(5, 10, "hello");
        "#;

        infer_prog(src);
    }

    #[test]
    fn rest_param_decl() {
        let src = r#"
        declare let add: (a: number, ...b: number[]) => number;
        let result = add(5, 10, 15);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "number");
    }

    #[test]
    fn infer_member_access() {
        let src = r#"
        let p = {x: 5, y: 10};
        let x = p.x;
        let y = p.y;
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("x", &ctx), "5");
        assert_eq!(get_value_type("y", &ctx), "10");
    }

    #[test]
    fn infer_optional_member() {
        let src = r#"
        declare let obj: {a: string, b?: number};
        let a = obj.a;
        let b = obj.b;
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "string");
        assert_eq!(get_value_type("b", &ctx), "number | undefined");
    }

    #[test]
    fn infer_member_access_with_string_literal_key() {
        let src = r#"
        let p = {x: 5, y: 10};
        let z = p["x"];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("z", &ctx), "5");
    }

    #[test]
    fn infer_member_access_with_string_key() {
        let src = r#"
        let p = {x: 5, y: 10};
        declare let key: string;
        let z = p[key];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("z", &ctx), "10 | 5 | undefined");
    }

    #[test]
    fn infer_nested_member_access() {
        let src = r#"
        let obj = {a: {b: {c: "hello"}}};
        let c = obj.a.b.c;
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("c", &ctx), "\"hello\"");
    }

    #[test]
    fn infer_calling_method_on_obj() {
        let src = r#"
        let obj = {add: (a, b) => a + b};
        let sum = obj.add(5, 10);
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum", &ctx), "number");
    }

    #[test]
    fn infer_combined_member_acces_and_method_call_result() {
        let src = r#"
        type Point = {x: number, y: number};
        let obj = {add: (p: Point, q: Point) => {
            let result = {x: p.x + q.x, y: p.y + q.y};
            result
        }};
        let p = {x: 5, y: 10};
        let q = {x: 0, y: 1};
        let sum_x = obj.add(p, q).x;
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("sum_x", &ctx), "number");
    }

    #[test]
    #[should_panic = "Object type doesn't contain key z."]
    fn infer_missing_member() {
        let src = r#"
        let p = {x: 5, y: 10};
        let z = p.z;
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Object type doesn't contain key z."]
    fn infer_missing_member_str_lit() {
        let src = r#"
        let p = {x: 5, y: 10};
        let z = p["z"];
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "true is an invalid key for object types"]
    fn infer_member_with_invalid_literal_key() {
        let src = r#"
        let p = {x: 5, y: 10};
        let z = p[true];
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "boolean is an invalid key for object types"]
    fn infer_member_with_invalid_primitive_key() {
        let src = r#"
        let p = {x: 5, y: 10};
        declare let key: boolean;
        let z = p[key];
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "{x: 5, y: 10} is an invalid key for object types"]
    fn infer_member_with_invalid_key_of_other_type() {
        let src = r#"
        let p = {x: 5, y: 10};
        let z = p[p];
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_number_literal_index_on_tuple() {
        let src = r#"
        let tuple = [5, "hello", true];
        let fst = tuple[0];
        let snd = tuple[1];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("fst", &ctx), "5");
        assert_eq!(get_value_type("snd", &ctx), "\"hello\"");
    }

    #[test]
    fn infer_number_primitive_index_on_tuple() {
        let src = r#"
        let tuple = [5, "hello", true];
        let index: number = 0;
        let elem = tuple[index];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("elem", &ctx),
            "\"hello\" | 5 | true | undefined"
        );
    }

    #[test]
    #[should_panic = "4 is out of bounds for [5, \\\"hello\\\", true]"]
    fn infer_number_index_on_tuple_out_of_bounds() {
        let src = r#"
        let tuple = [5, "hello", true];
        tuple[4];
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "true is an invalid indexer for tuple types"]
    fn infer_invalid_literal_index_on_tuple() {
        let src = r#"
        let tuple = [5, "hello", true];
        tuple[true];
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "boolean is an invalid indexer for tuple types"]
    fn infer_invalid_primitive_index_on_tuple() {
        let src = r#"
        let tuple = [5, "hello", true];
        declare let index: boolean;
        tuple[index];
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "[5, \\\"hello\\\", true] is an invalid indexer for tuple types"]
    fn infer_invalid_other_index_type_on_tuple() {
        let src = r#"
        let tuple = [5, "hello", true];
        tuple[tuple];
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_elem_access_on_array() {
        let src = r#"
        type ReadonlyArray<T> = {
            [key: number]: T;
        }
        let array: number[] = [1, 2, 3];
        let fst = array[0];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_value_type("fst", &ctx), "number | undefined");
    }

    #[test]
    fn destructure_obj_with_rest() {
        let src = r#"
        declare let point: {x: number, y: number, z?: number};
        let {z, ...rest} = point;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("rest", &ctx), "{x: number, y: number}");
    }

    #[test]
    #[should_panic = "Maximum one rest pattern allowed in object patterns"]
    fn destructure_obj_with_rest_undecidable() {
        let src = r#"
        declare let point: {x: number, y: number, z?: number};
        let {z, ...p, ...q} = point;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("q", &ctx), "{x: number, y: number}");
    }

    #[test]
    fn spread_an_object() {
        let src = r#"
        declare let point: {x: number, y: number};
        let point_3d = {...point, z: 15};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("point_3d", &ctx),
            "{x: number, y: number, z: 15}"
        );
    }

    #[test]
    fn spread_multiple_objects_no_overlap() {
        let src = r#"
        let foo = {a: true, b: "hello"};
        let bar = {x: 5, y: 10};
        let obj = {...foo, ...bar};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("obj", &ctx),
            "{a: true, b: \"hello\", x: 5, y: 10}"
        );
    }

    #[test]
    fn spread_multiple_objects_with_overlap() {
        let src = r#"
        let foo = {a: true, b: "hello"};
        let bar = {b: 5, c: false};
        let obj = {...foo, ...bar};
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("obj", &ctx),
            "{a: true, b: \"hello\" & 5, c: false}"
        );
    }

    #[test]
    fn infer_obj_from_spread() {
        let src = r#"
        type Point = {x: number, y: number, z: number};
        declare let mag: (p: Point) => number;
        let mag_2d = (p) => {
            mag({...p, z: 0})
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("mag_2d", &ctx),
            "(p: {x: number, y: number}) => number"
        );
    }

    #[test]
    #[should_panic = "Unification is undecidable"]
    fn infer_obj_from_spread_undecidable() {
        let src = r#"
        type Point = {x: number, y: number, z: number};
        declare let mag: (p: Point) => number;
        let mag_2d = (p, q) => {
            mag({...p, ...q, z: 0})
        };
        "#;
        infer_prog(src);
    }

    #[test]
    fn infer_tuple_rest_at_end() {
        let src = r#"
        let tuple = [5, "hello", true];
        let [a, ...b] = tuple;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "[\"hello\", true]");
    }

    #[test]
    fn infer_tuple_rest_at_start() {
        let src = r#"
        let tuple = [5, "hello", true];
        let [...a, b] = tuple;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "[5, \"hello\"]");
        assert_eq!(get_value_type("b", &ctx), "true");
    }

    #[test]
    fn infer_tuple_rest_in_middle() {
        let src = r#"
        let tuple = [5, "hello", true];
        let [a, ...b, c] = tuple;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "[\"hello\"]");
        assert_eq!(get_value_type("c", &ctx), "true");
    }

    #[test]
    fn infer_tuple_empty_rest() {
        let src = r#"
        let tuple = [5, true];
        let [a, ...b, c] = tuple;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("a", &ctx), "5");
        assert_eq!(get_value_type("b", &ctx), "[]");
        assert_eq!(get_value_type("c", &ctx), "true");
    }

    #[test]
    #[should_panic = "not enough elements to unpack"]
    fn infer_tuple_rest_no_enough_elements_to_unpack() {
        let src = r#"
        let tuple = [5];
        let [a, ...b, c] = tuple;
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Only one rest pattern is allowed in a tuple"]
    fn infer_tuple_more_than_one_rest() {
        let src = r#"
        let tuple = [5, "hello", true];
        let [a, ...b, ...c, d] = tuple;
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_spread_tuple_at_end() {
        let src = r#"
        let a = 5;
        let b = ["hello", true];
        let tuple = [a, ...b];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tuple", &ctx), "[5, \"hello\", true]");
    }

    #[test]
    fn infer_spread_tuple_literal() {
        let src = r#"
        let a = 5;
        let tuple = [a, ...["hello", true]];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tuple", &ctx), "[5, \"hello\", true]");
    }

    #[test]
    fn infer_nested_spread_tuple_literal() {
        let src = r#"
        let a = 5;
        let tuple = [a, ...["hello", ...[true]]];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tuple", &ctx), "[5, \"hello\", true]");
    }

    #[test]
    fn infer_spread_tuple_at_start() {
        let src = r#"
        let a = 5;
        let b = ["hello", true];
        let tuple = [...b, a];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tuple", &ctx), "[\"hello\", true, 5]");
    }

    #[test]
    fn infer_array_destructure_after_spread() {
        let src = r#"
        let a = 5;
        let b = ["hello", true];
        let [x, ...y] = [...b, a];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("x", &ctx), "\"hello\"");
        assert_eq!(get_value_type("y", &ctx), "[true, 5]");
    }

    #[test]
    fn infer_spread_tuple_in_middle() {
        let src = r#"
        let a = 5;
        let b = ["hello"];
        let c = true;
        let tuple = [a, ...b, c];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tuple", &ctx), "[5, \"hello\", true]");
    }

    #[test]
    fn infer_multiple_tuple_spreads() {
        let src = r#"
        let a = [5, 10];
        let b = ["hello", true];
        let tuple = [...a, ...b];
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tuple", &ctx), "[5, 10, \"hello\", true]");
    }

    #[test]
    #[should_panic = "Can only spread tuple types inside a tuple"]
    fn spread_non_tuple_type_should_fail() {
        let src = r#"
        let p = {x: 5, y: 10};
        let b = ["hello", true];
        let tuple = [...p, ...b];
        "#;
        infer_prog(src);
    }

    #[test]
    fn call_overloaded_function() {
        let src = r#"
        declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string);
        let str = add("hello", "world");
        let num = add(5, 10);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("str", &ctx), "string");
        assert_eq!(get_value_type("num", &ctx), "number");
    }

    #[test]
    #[should_panic = "Couldn't unify lambda with intersection"]
    fn call_overloaded_function_with_wrong_params() {
        let src = r#"
        declare let add: ((a: number, b: number) => number) & ((a: string, b: string) => string);
        add("hello", 10);
        "#;

        infer_prog(src);
    }

    #[test]
    fn async_await() {
        let src = r#"
        let add_async = async (a, b) => {
            await a + await b
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("add_async", &ctx),
            "(a: Promise<number>, b: Promise<number>) => Promise<number>"
        );
    }

    #[test]
    fn async_doesnt_rewrap_return_promise() {
        let src = r#"
        let passthrough = async (x: Promise<string>) => x;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("passthrough", &ctx),
            "(x: Promise<string>) => Promise<string>"
        );
    }

    #[test]
    #[should_panic = "Can't use `await` inside non-async lambda"]
    fn await_only_works_in_async_functions() {
        let src = r#"
        let add_async = (a, b) => {
            await a + await b
        };
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Can't use `await` inside non-async lambda"]
    fn await_only_works_in_async_functions_nested() {
        let src = r#"
        let add_async = async (a, b) => {
            let inner = (x, y) => {
                await x + await y
            };
            await inner(a, b)
        };
        "#;

        infer_prog(src);
    }

    #[test]
    fn await_works_in_nested_async_functions() {
        let src = r#"
        let add_async = async (a, b) => {
            let inner = async (x, y) => {
                await x + await y
            };
            await inner(a, b)
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("add_async", &ctx),
            "(a: Promise<number>, b: Promise<number>) => Promise<number>"
        );
    }

    #[test]
    fn jsx_element() {
        let src = r#"
        let elem = <div>Hello, world!</div>;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("elem", &ctx), "JSXElement");
    }

    #[test]
    fn jsx_custom_element() {
        let src = r#"
        let Foo = () => <div>Hello, world!</div>;
        let elem = <Foo />;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("elem", &ctx), "JSXElement");
    }

    #[test]
    fn jsx_custom_element_with_props() {
        let src = r#"
        type Props = {msg: string};
        let Foo = (props: Props) => <div>{props.msg}</div>;
        let elem = <Foo msg="Hello, world!" />;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("elem", &ctx), "JSXElement");
    }

    // TODO: disallow extra props
    #[test]
    fn jsx_custom_element_with_extra_props() {
        let src = r#"
        type Props = {msg: string};
        let Foo = (props: Props) => <div>{props.msg}</div>;
        let elem = <Foo msg="Hello, world!" bar={true} />;
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("elem", &ctx), "JSXElement");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn jsx_custom_element_with_incorrect_props() {
        let src = r#"
        type Props = {msg: string};
        let Foo = (props: Props) => <div>{props.msg}</div>;
        let elem = <Foo msg={5} />;
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn jsx_custom_element_with_missing_prop() {
        let src = r#"
        type Props = {msg: string};
        let Foo = (props: Props) => <div>{props.msg}</div>;
        let elem = <Foo />;
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Can't find value type: Bar"]
    fn jsx_custom_element_not_found() {
        let src = r#"
        let Foo = () => <div>Hello, world!</div>;
        let elem = <Bar />;
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Component must be a function"]
    fn jsx_custom_element_not_a_function() {
        let src = r#"
        let Foo = "hello, world";
        let elem = <Foo />;
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn jsx_custom_element_incorrect_return() {
        let src = r#"
        let Foo = () => {x: 5, y: 10};
        let elem = <Foo />;
        "#;

        infer_prog(src);
    }

    #[test]
    fn infer_fn_based_on_multiple_different_calls() {
        let src = r#"let h = (f, x, y) => f(x) + f(y);"#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("h", &ctx),
            "<t0>(f: (t0) => number, x: t0, y: t0) => number"
        );
    }

    #[test]
    fn infer_template_literal_as_string() {
        let src = r#"let str = `hello, "world"!`;"#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("str", &ctx), "string");
    }

    #[test]
    fn infer_template_literal_with_expressions_as_string() {
        let src = r#"let str = `hello, ${true}!`;"#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("str", &ctx), "string");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn detect_type_errors_inside_template_literal_expressions() {
        let src = r#"let str = `hello, ${5 + true}!`;"#;

        infer_prog(src);
    }

    #[test]
    fn assign_empty_tuple_to_array() {
        let src = r#"let arr: string[] = [];"#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("arr", &ctx), "string[]");
    }

    #[test]
    fn assign_tuple_with_values_to_array() {
        let src = r#"let arr: string[] = ["hello", "world"];"#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("arr", &ctx), "string[]");
    }

    #[test]
    fn pass_tuple_as_array_param() {
        let src = r#"
        declare let concat: (str_arr: string[]) => string;
        let result = concat(["hello", "world"]);
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "string");
    }

    #[test]
    #[should_panic = "Unification failure"]
    fn assign_tuple_with_to_array_with_incompatible_types() {
        let src = r#"let arr: string[] = ["hello", 5];"#;

        infer_prog(src);
    }

    #[test]
    fn basic_pattern_matching() {
        let src = r#"
        declare let count: number;
        let result = match (count) {
            0 -> "none",
            1 -> "one",
            2 -> "a couple",
            n if (n < 5) -> "a few",
            _ -> "many"
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(
            get_value_type("result", &ctx),
            r#""a couple" | "a few" | "many" | "none" | "one""#
        );
    }

    #[test]
    fn pattern_matching_with_disjoint_union() {
        let src = r#"
        type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string};
        declare let event: Event;
        let result = match (event) {
            {type: "mousedown", x, y} -> `mousedown: (${x}, ${y})`,
            {type: "keydown", key} -> key
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "string");
    }

    #[test]
    #[should_panic = "Can't unify number with string"]
    fn pattern_matching_with_disjoint_union_incorrect_result_type() {
        // The return type of a `match` expression is the union of all of the return types of
        // each of the arms.  If you want to ensure that all arms return a common type then you'll
        // need to add an explicit type annotation to the variable that the match is being assigned
        // to.
        let src = r#"
        type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string};
        declare let event: Event;
        let result: string = match (event) {
            {type: "mousedown", x, y} -> x + y,
            {type: "keydown", key} -> key
        };
        "#;

        infer_prog(src);
    }

    #[test]
    #[should_panic = "Can't unify number with string"]
    fn pattern_matching_with_disjoint_union_does_not_match_fn_return_type() {
        // The return type of a `match` expression is the union of all of the return types of
        // each of the arms.  If you want to ensure that all arms return a common type then you'll
        // need to add an explicit type annotation to the variable that the match is being assigned
        // to.
        let src = r#"
        type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string};
        declare let event: Event;
        let print = (event: Event): string => {
            match (event) {
                {type: "mousedown", x, y} -> x + y,
                {type: "keydown", key} -> key
            }
        };
        "#;

        infer_prog(src);
    }

    #[test]
    fn if_let_disjoint_union() {
        let src = r#"
        type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string};
        declare let event: Event;
        let result = if (let {type: "mousedown", x, y} = event) {
            `mousedown: (${x}, ${y})`
        } else if (let {type: "keydown", key} = event) {
            key
        } else {
            ""
        };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("result", &ctx), "string");
    }

    #[test]
    fn recursive_data_type() {
        let src = r#"
        type Tree = {type: "tree", children: Tree[] } | {type: "leaf"};
        let tree: Tree = {type: "tree", children: [{type: "leaf"}, {type: "leaf"}] };
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_value_type("tree", &ctx), "Tree");
    }

    #[test]
    fn keyof() {
        let src = r#"
        type Point = {
            x: number,
            y: number,
        };
        type CoordName = keyof Point;

        let x: CoordName = "x";
        let y: CoordName = "y";
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_type_type("CoordName", &ctx), "keyof Point");
    }

    #[test]
    fn test_typeof() {
        let src = r#"
        let foo = {bar: "baz"};
        
        type Foo = typeof foo;
        type FooBar = typeof foo.bar;
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_type_type("Foo", &ctx), r#"{bar: "baz"}"#);
        assert_eq!(get_type_type("FooBar", &ctx), r#""baz""#);
    }

    #[test]
    fn test_indexed_access() {
        let src = r#"        
        type FooBar = {foo: number, bar: string};
        type Foo = FooBar["foo"];
        type BarKey = "bar";
        type Bar = FooBar[BarKey];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_type_type("Foo", &ctx), "number");
        assert_eq!(get_type_type("Bar", &ctx), "string");
    }

    #[test]
    fn test_nested_indexed_access() {
        let src = r#"        
        type Nested = {a: {b: {c: string}}};
        type B = Nested["a"]["b"];
        type C = Nested["a"]["b"]["c"];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_type_type("B", &ctx), "{c: string}");
        assert_eq!(get_type_type("C", &ctx), "string");
    }

    #[test]
    fn test_indexed_access_with_indexer_elements() {
        let src = r#"
        type ReadonlyArray<T> = {
            [key: string]: T
        }
        type MyRecord = {[key: string]: number};
        type MyArray = boolean[];
        type RecVal = MyRecord["foo"];
        type ArrVal = MyArray["bar"];
        "#;

        let ctx = infer_prog(src);

        assert_eq!(get_type_type("RecVal", &ctx), "number");
        assert_eq!(get_type_type("ArrVal", &ctx), "boolean");
    }

    #[test]
    fn infer_ident_inside_lam() {
        let src = "let add = (a, b) => a + b;";

        let mut prog = parse(src).unwrap();
        let mut ctx: Context = Context::default();
        infer::infer_prog(&mut prog, &mut ctx).unwrap();

        insta::assert_debug_snapshot!(prog);
    }
}
