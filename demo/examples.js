export const basics = [
  "let add = (a, b) => a + b",
  "let sub = (a, b) => a - b",
  "let foo = (f, x) => f(x) + x",
  "",
  "let baz = if (true) {",
  "  let x = 5;",
  "  let y = 10;",
  "  x + y",
  "} else {",
  "  10",
  "}",
  "",
  "type Point = {x: number, y: number}",
  "let point: Point = {x: 5, y: 10}",
].join("\n");

export const asyncAwait = [
  "let add = async (a, b) => await a() + await b()",
].join("\n");

export const jsx = [
  "type JSXElement = {}",
  'let msg = "world"',
  'let elem = <div point={point} id="point">Hello, {msg}</div>',
].join("\n");

export const fibonacci = [
  "let rec fib = (n) => if (n == 0) {",
  "  0",
  "} else if (n == 1) {",
  "  1",
  "} else {",
  "  fib(n - 1) + fib(n - 2)",
  "}",
].join("\n");

export const functionOverloading = [
  "declare let add: ((number, number) => number) & ((string, string) => string)",
  "",
  "let num = add(5, 10)",
  'let str = add("hello, ", "world")',
].join("\n");
