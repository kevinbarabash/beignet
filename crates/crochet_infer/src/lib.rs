mod constraint_solver;
mod context;
pub mod infer;
mod infer_expr;
mod infer_lambda;
mod infer_mem;
mod infer_pattern;
mod infer_prog;
mod infer_type_ann;
mod substitutable;
pub mod types;
mod util;

pub use context::*;
pub use infer_expr::*;
pub use infer_prog::*;

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;
    use crochet_parser::*;

    use super::*;

    fn infer(input: &str) -> String {
        let ctx = Context::default();
        let expr = expr_parser().parse(input).unwrap();
        let scheme = infer::infer_expr(&ctx, &expr).unwrap();
        format!("{scheme}")
    }

    fn infer_prog(input: &str) -> Context {
        let prog = parser().parse(input).unwrap();
        infer::infer_prog(&prog).unwrap()
    }

    fn get_type(name: &str, ctx: &Context) -> String {
        let t = ctx.values.get(name).unwrap();
        format!("{t}")
    }

    #[test]
    fn infer_i_combinator() {
        let ctx = infer_prog("let I = (x) => x");
        assert_eq!(get_type("I", &ctx), "<t0>(t0) => t0");
    }

    #[test]
    fn infer_k_combinator() {
        let ctx = infer_prog("let K = (x) => (y) => x");
        assert_eq!(get_type("K", &ctx), "<t0, t1>(t0) => (t1) => t0");
    }

    #[test]
    fn infer_s_combinator() {
        let ctx = infer_prog("let S = (f) => (g) => (x) => f(x)(g(x))");
        assert_eq!(
            get_type("S", &ctx),
            "<t0, t1, t2>((t0) => (t1) => t2) => ((t0) => t1) => (t0) => t2"
        );
    }

    #[test]
    fn infer_skk() {
        let src = r#"
        let S = (f) => (g) => (x) => f(x)(g(x))
        let K = (x) => (y) => x
        let I = S(K)(K)
        "#;
        let ctx = infer_prog(src);
        assert_eq!(get_type("K", &ctx), "<t0, t1>(t0) => (t1) => t0");
        assert_eq!(
            get_type("S", &ctx),
            "<t0, t1, t2>((t0) => (t1) => t2) => ((t0) => t1) => (t0) => t2"
        );
        assert_eq!(get_type("I", &ctx), "<t0>(t0) => t0");
    }

    #[test]
    fn infer_adding_variables() {
        let src = r#"
        let x = 5
        let y = 10
        let z = x + y
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_type("z", &ctx), "number");
    }

    #[test]
    fn infer_if_else() {
        let src = r#"
        let n = 0
        let result = if n == 0 { 5 } else { 10 }
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_type("result", &ctx), "5 | 10");
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
        let ctx = infer_prog(src);

        assert_eq!(get_type("fib", &ctx), "(number) => number");
    }

    #[test]
    fn infer_app_of_lam() {
        assert_eq!(infer("((x) => x)(5)"), "5");
    }

    #[test]
    fn infer_tuple() {
        assert_eq!(infer("[5, true, \"hello\"]"), "[5, true, \"hello\"]");
    }

    #[test]
    fn infer_destructuring_tuple() {
        let src = r#"
        let [a, b, c] = [5, true, "hello"]
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_type("a", &ctx), "5");
        assert_eq!(get_type("b", &ctx), "true");
        assert_eq!(get_type("c", &ctx), "\"hello\"");
    }

    #[test]
    #[ignore]
    fn infer_destructuring_tuple_with_type_annotation() {
        let src = r#"
        let [a, b, c]: [number, boolean, string] = [5, true, "hello"]
        "#;
        let ctx = infer_prog(src);

        assert_eq!(get_type("a", &ctx), "5");
        assert_eq!(get_type("b", &ctx), "true");
        assert_eq!(get_type("c", &ctx), "\"hello\"");
    }
}
