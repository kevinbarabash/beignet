pub mod decl;
pub mod expr;
pub mod jsx;
pub mod pattern;
pub mod type_params;
pub mod type_ann;
pub mod util;

pub use decl::decl_parser;
pub use expr::expr_parser;

use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::text::Padded;

use crochet_ast::*;

pub fn just_with_padding(inputs: &str) -> Padded<Just<char, &str, Simple<char>>> {
    just(inputs).padded()
}

pub fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    let program = choice((
        decl_parser(),
        expr_parser().map_with_span(|expr, span: Span| Statement::Expr { expr, span }),
    ))
    .padded()
    .repeated()
    .map(|body| Program { body });

    program.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Program {
        parser().parse(input).unwrap()
    }

    #[test]
    fn literals() {
        insta::assert_debug_snapshot!(parse("10"));
        insta::assert_debug_snapshot!(parse("1.23"));
        insta::assert_debug_snapshot!(parse("\"hello\""));
    }

    #[test]
    fn template_literals() {
        insta::assert_debug_snapshot!(parse("`Hello, world`"));
        insta::assert_debug_snapshot!(parse("`Hello, ${name}`"));
        insta::assert_debug_snapshot!(parse("`(${x}, ${y})`"));
        insta::assert_debug_snapshot!(parse("`Hello, \"world\"`"));
        insta::assert_debug_snapshot!(parse("`foo ${`bar ${baz}`}`"));
        insta::assert_debug_snapshot!(parse("sql`SELECT * FROM ${table} WHERE id = ${id}`"));
    }

    #[test]
    fn operations() {
        insta::assert_debug_snapshot!(parse("1 + 2 - 3"));
        insta::assert_debug_snapshot!(parse("x * y / z"));
        insta::assert_debug_snapshot!(parse("(a + b) * c"));
        insta::assert_debug_snapshot!(parse("a == b"));
        insta::assert_debug_snapshot!(parse("a != b"));
        insta::assert_debug_snapshot!(parse("a > b"));
        insta::assert_debug_snapshot!(parse("a >= b"));
        insta::assert_debug_snapshot!(parse("a < b"));
        insta::assert_debug_snapshot!(parse("a <= b"));
        insta::assert_debug_snapshot!(parse("let cond = a != b"));
    }

    #[test]
    fn function_definition() {
        insta::assert_debug_snapshot!(parse("(a, b) => c"));
        insta::assert_debug_snapshot!(parse("() => 10"));
        insta::assert_debug_snapshot!(parse("(a) => \"hello\""));
    }

    #[test]
    fn async_await() {
        insta::assert_debug_snapshot!(parse("async () => 10"));
        insta::assert_debug_snapshot!(parse("let foo = async () => { await 10 }"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await a + await b"));
        insta::assert_debug_snapshot!(parse("let foo = async () => await bar()"));
    }

    #[test]
    fn function_application() {
        insta::assert_debug_snapshot!(parse("foo()"));
        insta::assert_debug_snapshot!(parse("foo(a, b)"));
        insta::assert_debug_snapshot!(parse("foo(10, \"hello\")"));
        insta::assert_debug_snapshot!(parse("f(x)(g(x))"));
    }

    #[test]
    fn declarations() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("let x = (a, b) => a + b"));
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; x}"));
        insta::assert_debug_snapshot!(parse("let rec f = () => f()")); // recursive
    }

    #[test]
    fn top_level_expressions() {
        insta::assert_debug_snapshot!(parse("a + b"));
        insta::assert_debug_snapshot!(parse("123\n\"hello\""));
    }

    #[test]
    fn if_else() {
        insta::assert_debug_snapshot!(parse("if true { 5 } else { 10 }"));
        insta::assert_debug_snapshot!(parse("if a { 5 } else if b { 10 } else { 20 }"));
    }

    #[test]
    fn objects() {
        insta::assert_debug_snapshot!(parse("{x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let obj = {x, y}"));
        insta::assert_debug_snapshot!(parse("let obj = {a, b, ...others}"));
    }

    #[test]
    fn jsx() {
        insta::assert_debug_snapshot!(parse("<Foo>Hello</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{bar}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>Hello {world}!</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>{<Bar>{baz}</Bar>}</Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo bar={baz} />"));
        insta::assert_debug_snapshot!(parse("<Foo msg=\"hello\" bar={baz}></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo><Bar>{baz}</Bar></Foo>"));
        insta::assert_debug_snapshot!(parse("<Foo>hello<Bar/>{world}<Baz/></Foo>"));
        insta::assert_debug_snapshot!(parse(
            "let elem = <div point={point} id=\"point\">Hello, {msg}</div>"
        ));
    }

    #[test]
    #[should_panic="JSX head and tail elements must match"]
    fn jsx_head_and_tail_must_match() {
        parse("<Foo>Hello</Bar>");
    }

    #[test]
    fn type_annotations() {
        insta::assert_debug_snapshot!(parse("let x: number = 5"));
        insta::assert_debug_snapshot!(parse("let msg: string = \"hello\""));
        insta::assert_debug_snapshot!(parse("let add = (a: number, b: number) => a + b"));
        insta::assert_debug_snapshot!(parse("let p: Point = {x: 5, y: 10}"));
        insta::assert_debug_snapshot!(parse("let FOO: \"foo\" = \"foo\""));
    }

    #[test]
    fn decls() {
        insta::assert_debug_snapshot!(parse("let x = 5"));
        insta::assert_debug_snapshot!(parse("   let x = 5")); // with leading whitespace
        insta::assert_debug_snapshot!(parse("declare let x: number"));
        insta::assert_debug_snapshot!(parse("declare let foo: Foo<string>"));
    }

    #[test]
    fn tuples() {
        insta::assert_debug_snapshot!(parse("let x = []"));
        insta::assert_debug_snapshot!(parse("let x = [1, 2, 3]"));
        insta::assert_debug_snapshot!(parse("let x = [1, [a, b]]"));
        insta::assert_debug_snapshot!(parse("let foo = () => [a, b]"));
    }

    #[test]
    fn member_access() {
        insta::assert_debug_snapshot!(parse("a.b.c"));
        insta::assert_debug_snapshot!(parse("foo.bar()"));
        insta::assert_debug_snapshot!(parse("p.x * p.x + p.y * p.y"));
        insta::assert_debug_snapshot!(parse("foo().bar()"));
    }

    #[test]
    fn type_decls() {
        insta::assert_debug_snapshot!(parse("type Num = number"));
        insta::assert_debug_snapshot!(parse("type Point = {x: number, y: number}"));
        insta::assert_debug_snapshot!(parse("type Foo<T> = {bar: T}"));
        insta::assert_debug_snapshot!(parse("type Foo<T extends string> = {bar: T}"));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T = "foo"> = {bar: T}"#));
        insta::assert_debug_snapshot!(parse(r#"type Foo<T extends string = "foo"> = {bar: T}"#));
    }

    #[test]
    fn blocks() {
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; x}"));
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; let y = 10; x + y}"));
        insta::assert_debug_snapshot!(parse("{let x = 5; let y = 10; x + y}"));
        insta::assert_debug_snapshot!(parse("{let sum = {let x = 5; let y = 10; x + y}; sum}"));
        insta::assert_debug_snapshot!(parse("let foo = {let x = 5; console.log(x); x}"));
        insta::assert_debug_snapshot!(parse("let foo = {console.log(x); x}"));
    }

    #[test]
    fn destructuring() {
        insta::assert_debug_snapshot!(parse("let {x, y} = point"));
        insta::assert_debug_snapshot!(parse("let {a, b, ...rest} = letters"));
        insta::assert_debug_snapshot!(parse("let {p0: {x, y}, p1: {x, y}} = line"));
        insta::assert_debug_snapshot!(parse("let [a, b, ...rest] = letters"));
        insta::assert_debug_snapshot!(parse("let [foo, ...[bar, ...rest]] = baz"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]) => a"));
        insta::assert_debug_snapshot!(parse("let foo = ([a, b]: [string, number]) => a"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}) => b"));
        insta::assert_debug_snapshot!(parse("let foo = ({a, b}: {a: string, b: number}) => b"));
        // TODO: assigning defaults
        // TODO: type annotations
        // TODO: function params
        // TODO: disallowed patterns, e.g. top-level rest, non-top-level type annotations
    }

    #[test]
    fn array_spread() {
        insta::assert_debug_snapshot!(parse("let tuple = [...a, b]"));
        insta::assert_debug_snapshot!(parse("let tuple = [a, ...b]"));
        insta::assert_debug_snapshot!(parse("let tuple = [1, ...[2, 3]]"));
    }

    #[test]
    #[should_panic="Only one rest is allowed in an object pattern"]
    fn multiple_rests_is_invalid() {
        insta::assert_debug_snapshot!(parse("let {z, ...p, ...q} = point"));
    }

    #[test]
    #[should_panic="Rest should come last in object pattern"]
    fn rest_that_isnt_last_is_invalid() {
        insta::assert_debug_snapshot!(parse("let {...p, z} = point"));
    }

    #[test]
    fn types() {
        insta::assert_debug_snapshot!(parse("let get_bar = <T>(foo: Foo<T>) => foo.bar"));
        insta::assert_debug_snapshot!(parse("declare let get_bar: (Foo) => T"));
    }

    #[test]
    fn if_let() {
        insta::assert_debug_snapshot!(parse("if let {x, y} = p { x + y; }"));
        insta::assert_debug_snapshot!(parse("if let {x: a, y: b} = p { a + b; }"));
        insta::assert_debug_snapshot!(parse("if let {x: 5, y} = p { y; }"));
        // TODO: this should probably error since there's consequent
        insta::assert_debug_snapshot!(parse("if let a is string = value"));
        insta::assert_debug_snapshot!(parse("if let {x: 5, y} = p { y; }"));
        insta::assert_debug_snapshot!(parse("if let {x: 5, y} = p { y } else { 0 }"));
        insta::assert_debug_snapshot!(parse("if let {x: 5, y} = p { y; } else if let {x} = p { x; }"));
    }
}
