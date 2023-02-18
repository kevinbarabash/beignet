# 02 Expressions

JavaScript is a very statement-heavy language. As a result, things often end up
needing to be more verbose than would otherwise be necessary. Escalier redefines
the following statements as expressions:

- `if`-`else`
- `try`-`catch`
- `throw` ([TC39 Prospoals](https://github.com/tc39/proposal-throw-expressions))

## `if`-`else`

The result of an `if`-`else` in Escalier is defined as the last expression in
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

The type of the `if`-`else` is the union of the consequent and alternative
blocks, e.g.

```ts
let result = if (cond) { 5 } else { 10 }; // `result` has type `5 | 10`
```

When using `if` without an `else` block, it will be inferred as `T | undefined`
where `T` is the type of the consequent, e.g.

```ts
let result  = if (cond) { 5 };  // `result` has type `5 | undefined`
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

The Escalier implementation of this is very similar to the [TC39
Proposal](https://github.com/tc39/proposal-do-expressions). One notable
difference is that `let` and `if` can appear last within a block.

The `;` is optional on the last statement in a block. If it's omitted, the
statement will be parsed as an expression. This means that a bunch of things
that aren't currently expressions (looping constructs, class declarations, etc)
are not allowed to come last.

TODO:

- make more things expressions
- look at changing the parser so that we can allow statements to appear last
  without having the place a `;` after each one (maybe we can use `;` as a
  separator between statements within a block - what does this mean for
  top-level parsing that isn't in a block?)
