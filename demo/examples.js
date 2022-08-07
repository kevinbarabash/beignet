export const basics = `
let add = (a, b) => a + b
let sub = (a, b) => a - b
let foo = (f, x) => f(x) + x

let baz = if (true) {
  let x = 5;
  let y = 10;
  x + y
} else {
  10
}

type Point = {x: number, y: number}
let point: Point = {x: 5, y: 10}
`;

export const asyncAwait = `
let add = async (a, b) => await a() + await b()
`;

export const jsx = `
type JSXElement = {}
let msg = "world"
let elem = <div point={point} id="point">Hello, {msg}</div>
`;

export const fibonacci = `
let rec fib = (n) => if (n == 0) {
  0
} else if (n == 1) {
  1
} else {
  fib(n - 1) + fib(n - 2)
}
`;

export const functionOverloading = `
declare let add: ((number, number) => number) & ((string, string) => string)

let num = add(5, 10)
let str = add("hello, ", "world")
`;

export const ifLetElse = `
declare let a: string | number
let result = if let x is number = a {
    x + 5
} else if let y is string = a {
    y
} else {
    true
}
`;

export const basicPatternMatching = `
declare let count: number
let result = match count {
    0 => "none",
    1 => "one",
    2 => "a couple",
    n if n < 5 => "a few",
    _ => "many",
}
`;

export const disjointUnionPatternMatching = `
type Event = {type: "mousedown", x: number, y: number} | {type: "keydown", key: string}
declare let event: Event
let result = match event {
    {type: "mousedown", x, y} => \`mousedown: (\${x}, \${y})\`,
    {type: "keydown", key} => key,
}
`;
