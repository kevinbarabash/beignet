use chumsky::prelude::*;

use crate::literal::Literal;

use super::syntax::{BindingIdent, Expr};

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

        let app = atom.clone()
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

        app.or(lam).or(ident.map(Expr::Ident)).or(atom)
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
        assert_eq!(parse("foo(a, b)"), "App(Ident(\"foo\"), [Ident(\"a\"), Ident(\"b\")])");
    }

    #[test]
    fn app_with_multiple_lit_args() {
        assert_eq!(parse("foo(10, \"hello\")"),
        "App(Ident(\"foo\"), [Lit(Num(\"10\")), Lit(Str(\"hello\"))])");
    }

    #[test]
    fn atom_number() {
        assert_eq!(parse("10"), "Lit(Num(\"10\"))");
    }

    #[test]
    fn atom_string() {
        assert_eq!(parse("\"hello\""), "Lit(Str(\"hello\"))");
    }
}
