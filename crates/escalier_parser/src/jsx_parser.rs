use crate::identifier::Ident;
use crate::jsx::*;
use crate::parser::*;
use crate::source_location::*;
use crate::token::{TokenKind, EOF};

impl<'a> Parser<'a> {
    pub fn parse_jsx_element(&mut self) -> JSXElement {
        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::LessThan);
        let name_token = self.lex_ident_or_keyword();
        let name = match name_token.kind {
            TokenKind::Identifier(name) => JSXElementName::Ident(Ident {
                name,
                span: name_token.span,
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

            let start = self.scanner.cursor();

            assert_eq!(self.scanner.pop(), Some('<'));
            assert_eq!(self.scanner.pop(), Some('/'));
            let end_name = self.lex_ident_or_keyword();
            assert_eq!(self.scanner.pop(), Some('>'));

            let end = self.scanner.cursor();

            Some(JSXClosingElement {
                name: match end_name.kind {
                    TokenKind::Identifier(name) => JSXElementName::Ident(Ident {
                        name,
                        span: Span { start, end },
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
        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::LessThan);
        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::GreaterThan
        );

        let children = self.parse_jsx_children();

        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::LessThan);
        assert_eq!(self.next().unwrap_or(EOF.clone()).kind, TokenKind::Divide);
        assert_eq!(
            self.next().unwrap_or(EOF.clone()).kind,
            TokenKind::GreaterThan
        );

        JSXFragment {
            opening: JSXOpeningFragment {},
            children,
            closing: JSXClosingFragment {},
        }
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
                    if self.scanner.peek(1) == Some('/') {
                        break;
                    } else if self.scanner.peek(1) == Some('>') {
                        let fragment = self.parse_jsx_fragment();
                        children.push(JSXElementChild::Fragment(Box::new(fragment)));
                    } else {
                        let element = self.parse_jsx_element();
                        children.push(JSXElementChild::Element(Box::new(element)));
                    }
                }
                '{' => {
                    self.scanner.pop(); // consumes '{'

                    self.brace_counts.push(0);
                    let expr = self.parse_expr();
                    self.brace_counts.pop();

                    self.scanner.pop(); // consumes '}'

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
        let start = self.scanner.cursor();

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
            span: Span {
                start,
                end: self.scanner.cursor(),
            },
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

    #[test]
    fn parse_jsx_fragment() {
        let mut parser = Parser::new(r#"<><span>Hello, </span><span>world!</span></>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn parse_jsx_nested_fragments() {
        let mut parser = Parser::new(r#"<>a<>{b}{c}</>d</>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }

    #[test]
    fn parse_jsx_props_dot_children() {
        let mut parser = Parser::new(r#"<div>{a+b}</div>"#);

        let jsx_elem = parser.parse_jsx_element();

        insta::assert_debug_snapshot!(jsx_elem);
    }
}
