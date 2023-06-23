use crate::identifier::Ident;
use crate::jsx::*;
use crate::parser::*;
use crate::source_location::*;
use crate::token::{Token, TokenKind};

impl<'a> Parser<'a> {
    pub fn parse_jsx_element(&mut self) -> JSXElement {
        assert_eq!(self.scanner.pop(), Some('<'));
        let name_token = self.lex_ident_or_keyword();
        let name = match name_token.kind {
            TokenKind::Identifier(name) => JSXElementName::Ident(Ident {
                name,
                loc: SourceLocation {
                    start: name_token.loc.start,
                    end: name_token.loc.end,
                },
            }),
            _ => panic!("Expected identifier or keyword"),
        };

        let mut attrs = vec![];
        let mut self_closing = false;

        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '/' => {
                    self.scanner.pop();
                    assert_eq!(self.scanner.pop(), Some('>'));
                    self_closing = true;
                    break;
                }
                '>' => {
                    self.scanner.pop();
                    break;
                }
                ' ' => {
                    self.scanner.pop();
                }
                _ => {
                    attrs.push(self.parse_jsx_attribute());
                }
            }
        }

        let opening = JSXOpeningElement {
            name,
            attrs,
            self_closing,
        };

        let mut children = vec![];

        let closing = if self_closing {
            None
        } else {
            children = self.parse_jsx_children();

            assert_eq!(self.scanner.pop(), Some('<'));
            assert_eq!(self.scanner.pop(), Some('/'));
            let end_name = self.lex_ident_or_keyword();
            assert_eq!(self.scanner.pop(), Some('>'));

            Some(JSXClosingElement {
                name: match end_name.kind {
                    TokenKind::Identifier(name) => JSXElementName::Ident(Ident {
                        name,
                        loc: SourceLocation {
                            start: end_name.loc.start,
                            end: end_name.loc.end,
                        },
                    }),
                    _ => panic!("Expected identifier or keyword"),
                },
            })
        };

        JSXElement {
            opening,
            children,
            closing,
        }
    }

    pub fn parse_jsx_fragment(&mut self) -> JSXFragment {
        assert_eq!(self.scanner.pop(), Some('<'));
        assert_eq!(self.scanner.pop(), Some('>'));
        let children = self.parse_jsx_children();
        assert_eq!(self.scanner.pop(), Some('<'));
        assert_eq!(self.scanner.pop(), Some('/'));
        assert_eq!(self.scanner.pop(), Some('>'));

        todo!();
        // JSXFragment { children }
    }

    pub fn parse_jsx_attribute(&mut self) -> JSXAttr {
        let name = self.lex_ident_or_keyword();

        let name = match name.kind {
            TokenKind::Identifier(name) => name,
            _ => panic!("Unexpected token"),
        };

        if let Some('=') = self.scanner.peek(0) {
            self.scanner.pop();
        } else {
            return JSXAttr { name, value: None };
        }

        match self.scanner.peek(0).unwrap() {
            '"' => {
                let value = self.lex_string();
                let value = match value.kind {
                    TokenKind::StrLit(value) => value,
                    _ => panic!("Unexpected token"),
                };

                JSXAttr {
                    name,
                    value: Some(JSXAttrValue::Str(value)),
                }
            }
            '{' => {
                self.scanner.pop();

                self.brace_counts.push(0);
                let expr = self.parse_expr();
                self.brace_counts.pop();

                JSXAttr {
                    name,
                    value: Some(JSXAttrValue::ExprContainer(JSXExprContainer {
                        expr: Box::new(expr),
                    })),
                }
            }
            _ => panic!("Unexpected character"),
        }
    }

    pub fn parse_jsx_children(&mut self) -> Vec<JSXElementChild> {
        let mut children = vec![];

        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '<' => {
                    // TODO: skip of whitespace
                    if self.scanner.peek(1) == Some('/') {
                        break;
                    } else {
                        let elem = self.parse_jsx_element();
                        children.push(JSXElementChild::Element(Box::new(elem)));
                    }
                }
                '{' => {
                    self.scanner.pop();

                    self.brace_counts.push(0);
                    let expr = self.parse_expr();
                    self.brace_counts.pop();

                    children.push(JSXElementChild::ExprContainer(JSXExprContainer {
                        expr: Box::new(expr),
                    }));
                }
                _ => {
                    let text = self.parse_jsx_text();
                    children.push(JSXElementChild::Text(text));
                }
            }
        }

        children
    }

    pub fn parse_jsx_text(&mut self) -> JSXText {
        let start = self.scanner.position();

        let mut value = String::new();

        while !self.scanner.is_done() {
            match self.scanner.peek(0).unwrap() {
                '{' => {
                    break;
                }
                '<' => {
                    break;
                }
                _ => {
                    value.push(self.scanner.pop().unwrap());
                }
            }
        }

        JSXText {
            value,
            // loc: SourceLocation {
            //     start,
            //     end: self.scanner.position(),
            // },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_jsx_element() {
        let mut parser = Parser::new(r#"<Foo bar="baz" qux></Foo>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn parse_self_closing_jsx_element() {
        let mut parser = Parser::new(r#"<Foo bar="baz" qux />"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn parse_jsx_element_with_children_text() {
        let mut parser = Parser::new(r#"<h1>Hello, world!</h1>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn parse_jsx_element_with_text_and_exprs() {
        let mut parser = Parser::new(r#"<h1>Hello, {name}!</h1>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn parse_jsx_element_with_children_elements() {
        let mut parser = Parser::new(r#"<ul><li>one</li><li>two</li></ul>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }
}
