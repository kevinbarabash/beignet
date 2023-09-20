use escalier_ast::*;

use crate::parse_error::ParseError;
use crate::parser::*;
use crate::token::*;

impl<'a> Parser<'a> {
    pub fn parse_script(&mut self) -> Result<Script, ParseError> {
        let mut stmts = Vec::new();
        while self.peek().unwrap_or(&EOF).kind != TokenKind::Eof {
            // TODO: attach comments to AST nodes
            if let TokenKind::Comment(_) = &self.peek().unwrap_or(&EOF).kind {
                self.next(); // consumes the comment
                continue;
            }
            stmts.push(self.parse_stmt()?);
        }
        Ok(Script { stmts })
    }
}
