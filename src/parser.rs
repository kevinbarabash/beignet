use chumsky::prelude::*;

use super::literal::Literal;
use super::syntax::{BinOp, BindingIdent, Expr};

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    let num = text::int(10)
        .map(|s: String| Expr::Lit(Literal::Num(s)))
        .padded();

    let str_ = just("\"")
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|s: String| Expr::Lit(Literal::Str(s)));

    let lit = num.or(str_);

    let expr = recursive(|expr| {
        let atom = num
            .or(expr.clone().delimited_by(just("("), just(")")))
            .or(ident.map(Expr::Ident))
            .or(lit);

        let product = atom
            .clone()
            .then(
                just("*")
                    .padded()
                    .to(partial!(Expr::Op => BinOp::Mul, _, _) as fn(_, _) -> _)
                    .or(just("/")
                        .padded()
                        .to(partial!(Expr::Op => BinOp::Div, _, _) as fn(_, _) -> _))
                    .then(atom.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op_con, rhs)| op_con(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                just("+")
                    .padded()
                    .to(partial!(Expr::Op => BinOp::Add, _, _) as fn(_, _) -> _)
                    .or(just("-")
                        .padded()
                        .to(partial!(Expr::Op => BinOp::Sub, _, _) as fn(_, _) -> _))
                    .then(product.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op_con, rhs)| op_con(Box::new(lhs), Box::new(rhs)));

        let app = atom
            .clone()
            .then(
                expr.clone()
                    .padded()
                    .separated_by(just(","))
                    .allow_trailing()
                    .delimited_by(just("("), just(")")),
            )
            .map(|(func, args)| Expr::App(Box::new(func), args));

        let param_list = ident
            .map(BindingIdent::Ident)
            .padded()
            .separated_by(just(","))
            .allow_trailing()
            .delimited_by(just("("), just(")"));

        let lam = param_list
            .then_ignore(just("=>").padded())
            .then(expr.clone())
            .map(|(args, body)| Expr::Lam(args, Box::new(body), false));

        let r#let = just("let")
            .ignore_then(ident)
            .then_ignore(just("=").padded())
            .then(expr.clone())
            .then_ignore(just("in").padded())
            .then(expr.clone())
            .map(|((name, value), body)| Expr::Let(name, Box::new(value), Box::new(body)));

        app.or(lam).or(r#let).or(sum)
    });

    expr.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(s: &str) -> String {
        format!("{:?}", parser().parse(s).unwrap())
    }

    #[test]
    fn fn_with_multiple_params() {
        assert_eq!(
            parse("(a, b) => c"),
            "Lam([Ident(\"a\"), Ident(\"b\")], Ident(\"c\"), false)"
        );
    }

    #[test]
    fn fn_returning_num_literal() {
        assert_eq!(parse("() => 10"), "Lam([], Lit(Num(\"10\")), false)");
    }

    #[test]
    fn fn_returning_str_literal() {
        assert_eq!(
            parse("(a) => \"hello\""),
            "Lam([Ident(\"a\")], Lit(Str(\"hello\")), false)"
        );
    }

    #[test]
    fn app_with_no_args() {
        assert_eq!(parse("foo()"), "App(Ident(\"foo\"), [])");
    }

    #[test]
    fn app_with_multiple_args() {
        assert_eq!(
            parse("foo(a, b)"),
            "App(Ident(\"foo\"), [Ident(\"a\"), Ident(\"b\")])"
        );
    }

    #[test]
    fn app_with_multiple_lit_args() {
        assert_eq!(
            parse("foo(10, \"hello\")"),
            "App(Ident(\"foo\"), [Lit(Num(\"10\")), Lit(Str(\"hello\"))])"
        );
    }

    #[test]
    fn atom_number() {
        assert_eq!(parse("10"), "Lit(Num(\"10\"))");
    }

    #[test]
    fn atom_string() {
        assert_eq!(parse("\"hello\""), "Lit(Str(\"hello\"))");
    }

    #[test]
    fn simple_let() {
        assert_eq!(
            parse("let x = 5 in x"),
            "Let(\"x\", Lit(Num(\"5\")), Ident(\"x\"))"
        );
    }

    #[test]
    fn nested_let() {
        assert_eq!(
            parse("let x = 5 in let y = 10 in z"),
            "Let(\"x\", Lit(Num(\"5\")), Let(\"y\", Lit(Num(\"10\")), Ident(\"z\")))",
        );
    }

    #[test]
    fn add_sub_operations() {
        assert_eq!(
            parse("1 + 2 - 3"),
            "Op(Sub, Op(Add, Lit(Num(\"1\")), Lit(Num(\"2\"))), Lit(Num(\"3\")))",
        );
    }

    #[test]
    fn mul_div_operations() {
        assert_eq!(
            parse("x * y / z"),
            "Op(Div, Op(Mul, Ident(\"x\"), Ident(\"y\")), Ident(\"z\"))",
        );
    }

    #[test]
    fn operator_precedence() {
        assert_eq!(
            parse("a + b * c"),
            "Op(Add, Ident(\"a\"), Op(Mul, Ident(\"b\"), Ident(\"c\")))",
        );
    }

    #[test]
    fn specifying_operator_precedence_with_parens() {
        assert_eq!(
            parse("(a + b) * c"),
            "Op(Add, Op(Mul, Ident(\"a\"), Ident(\"b\")), Ident(\"c\"))",
        );
    }
}
