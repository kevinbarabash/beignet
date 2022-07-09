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

    #[test]
    fn infer_i_combinator() {
        assert_eq!(infer("(x) => x"), "<t0>(t0) => t0");
    }

    #[test]
    fn infer_k_combinator() {
        assert_eq!(infer("(x, y) => x"), "<t0, t1>(t0, t1) => t0");
    }

    #[test]
    fn infer_s_combinator() {
        assert_eq!(
            infer("(f, g, x) => f(x, g(x))"),
            "<t0, t1, t2>((t0, t1) => t2, (t0) => t1, t0) => t2"
        );
    }

    #[test]
    fn infer_app_of_lam() {
        assert_eq!(infer("((x) => x)(5)"), "5");
    }
}
