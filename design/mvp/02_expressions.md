# 02 Expressions

JavaScript is a very statement-heavy language. As a result, things often end up
needing to be more verbose than would otherwise be necessary. Crochet redefines
the following statements as expressions:

- `if`-`else`
- `try`-`catch`
- `throw` ([TC39 Prospoals](https://github.com/tc39/proposal-throw-expressions))

## `if`-`else`

The result of an `if`-`else` in Crochet is defined as the last expression in
each block. `if`-`else` can be chained and is much more readable than chained
ternaries. This obviates the need for ternary expressions.

```ts
let result = if (condition1) {
  console.log("calling foo()");
  foo()
} else if (condition2) {
  console.log("calling bar()");
  bar()
} else {
  console.log("calling baz()");
  baz()
};
```

## `try`-`catch`

This works similarly for `try`-`catch`.

```ts
let point = try {
  console.log("parsing input");
  JSON.parse(input)
} catch (_) {
  console.log("parsing failed");
  {x: 0, y: 0}
};
```

NOTE: `finally` can be used, but a value cannot be return from this block.

## `throw`

Being able to use `throw` as an expression can be useful in situations where an
expression is expected. One such case is an arrow function that returns an
expression.

```ts
let throwMyError = (message) => throw new MyError(message);
```

This will also come in handy later when we look at [Pattern Matching](05_pattern_matching.md).

## `do` Expressions

This allows you to introduce a new scope to enacapsulate computation. It's
similar to an IIFE (immediately invoked function expression) without the cost of
function call.

```ts
// foo.crochet
let foo = do {
    let bar = getBar();
    console.log(`bar = ${bar}`);
    bar
};

// foo.js
let foo;
{
    let bar = getBar();
    console.log(`bar = ${bar}`);
    foo = bar;
}
```

The Crochet implementation of this is very similar to the [TC39
Proposal](https://github.com/tc39/proposal-do-expressions).

If the last statement within the block ends with a semicolon then value of
the `do` expression is `undefined`. This mimics Rust's behavior with
`undefined` standing in for `()`.

```ts
// foo.crochet
let foo = do {
    let bar = getBar();
    console.log(`bar = ${bar}`);
    bar;
};

// foo.js
let foo;
{
    let bar = getBar();
    console.log(`bar = ${bar}`);
    bar;
}
```
