pub fn add(left: usize, right: usize) -> usize {
    left + right
}

mod expr;
mod expr_parser;
mod lexer;
mod scanner;
mod source_location;
mod token;

pub use expr_parser::parse_expr;
pub use lexer::Lexer;

#[cfg(test)]
mod tests {
    use crate::expr_parser::parse_expr;
    use crate::lexer::Lexer;

    #[test]
    fn lex_identifiers() {
        let mut lexer = Lexer::new("abc _a0 123");

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::Identifier("abc".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            crate::token::TokenKind::Identifier("_a0".to_string())
        );
    }

    #[test]
    fn lex_numbers() {
        let mut lexer = Lexer::new("123 1.23");

        let tokens = lexer.lex();

        assert_eq!(
            tokens[0].kind,
            crate::token::TokenKind::Number("123".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            crate::token::TokenKind::Number("1.23".to_string())
        );
    }

    #[test]
    #[should_panic = "Unexpected character: '.'"]
    fn lex_number_multiple_decimals_error() {
        let mut lexer = Lexer::new("1.2.3");

        lexer.lex();
    }

    #[test]
    fn lex_comparison_ops() {
        let mut lexer = Lexer::new("> >= < <= == !=");

        let tokens = lexer.lex();

        assert_eq!(tokens[0].kind, crate::token::TokenKind::GreaterThan);
        assert_eq!(tokens[1].kind, crate::token::TokenKind::GreaterThanOrEqual);
        assert_eq!(tokens[2].kind, crate::token::TokenKind::LessThan);
        assert_eq!(tokens[3].kind, crate::token::TokenKind::LessThanOrEqual);
        assert_eq!(tokens[4].kind, crate::token::TokenKind::Equals);
        assert_eq!(tokens[5].kind, crate::token::TokenKind::NotEquals);
    }

    #[test]
    #[should_panic = "Unexpected character: '!'"]
    fn lex_unexpected_exclamation() {
        let mut lexer = Lexer::new("1 ! 2");

        lexer.lex();
    }

    #[test]
    fn parse_simple_addition() {
        let mut lexer = Lexer::new("1 + 2 + 3");
        let tokens = lexer.lex();

        let ast = parse_expr(tokens);
        eprintln!("{:#?}", ast);
    }

    #[test]
    fn parse_addition_and_subtraction() {
        let mut lexer = Lexer::new("1 + 2 - 3 + 4");
        let tokens = lexer.lex();

        let ast = parse_expr(tokens);
        eprintln!("{:#?}", ast);
    }

    #[test]
    fn parse_unary_operators() {
        let mut lexer = Lexer::new("--a - +b");
        let tokens = lexer.lex();

        let ast = parse_expr(tokens);
        eprintln!("{:#?}", ast);
    }
}
