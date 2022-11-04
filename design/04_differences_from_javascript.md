# 04 Differences From JavaScript

## Semicolons

- semicolons are not automatically inserted
- semicolons are required in between statements

## `let`-binding

The behavior of `let` has been modified so that it:

- allows redeclarations (like `var`)
- doesn't allow reassignment (like `const`) without redeclaration

**Crochet**

```typescript
let x = "hello";
let x = x.length; // x is now a number
```

## `if`-`let`

This syntax combines some sort of runtime type checking along with the
introducing of a new binding for the narrowed type. This is used in place of
type refinements you'd normally use in static type checkers for Javascript. An
example of this was shown in the "No Type Refinements" sub-section earlier in
the doc. Here's another example:

**TypeScript**

```typescript
declare const foo: number | string;
if (typeof foo === "number") {
  // `foo` is just a `number` in this scope
} else if (typeof foo === "string") {
  // `foo` is just a `string` in this scope
}
// `foo` is still has type `number | string` in this scope
```

**Crochet**

```typescript
declare let foo: number | string;
if (let n is number = foo) {
    // `n` is a `number`, `foo` still has type `number | string`
} else if (let s is string = foo) {
    // `s` is a `string`, `foo` still has type `number | string`
}
```

The syntax also supports the following pattern kinds:

- tuple and object destructuring
- literals, e.g. `"mousedown"`
- `is` checks with either a primitive or a class
- variables
- `_` checks for the presence of a variable/property, but doesn't introduce a
  binding for it

Patterns can either refutable or irrefutable. A pattern is said to be refutable
if it contains at least one of the following pattern kinds: literals or `is`.
While `if`-`let` can be used with both types of patterns, it's really only
useful with refutable patterns. `let` declarations on the other hand can only
use irrefutable patterns.

## Pattern Matching

Pattern matching is like a `switch`-`case` but you can match against structured
data and the case statements can use the same patterns that are use with
`if`-`let`. Here's one example of what this looks like in Crochet:

**Crochet**

```
type Event =
  | {type: "mousedown", x: number, y: number}
  | {type: "keydown", key: string}
  ;

declare let event: Event;

let result = match (event) {
    {type: "mousedown", x, y} -> \`mousedown: (\${x}, \${y})\`,
    {type: "keydown", key} -> \`keydown: \${key}\`
};
```

The reason for using `->` instead of `=>` was to differentiate the arms of the
`match` expression from arrow functions. The latter introduces a new stack frame
whereas the former does not.

While there is a [TC39
Proposal](https://github.com/tc39/proposal-pattern-matching) for pattern
matching, it feels overly complex. This is why an alternate syntax was adopted.

NOTE: Pattern matching is syntactic sugar for `if`-`let`-`else`. The example
above can be rewritten as the following:

**Crochet**

```
type Event =
  | {type: "mousedown", x: number, y: number}
  | {type: "keydown", key: string}
  ;

declare let event: Event;

let result = if (let {type: "mousedown", x, y} = e) {
    \`mousedown: (\${x}, \${y})\`
} else if (let {type: "keydown", key} = e) {
    \`keydown: \${key}\
};
```

## `do` Expressions

This allows you to introduce a new scope to enacapsulate computation. It's
similar to an IIFE (immediately invoked function expression) without the cost of
function call.

**Crochet**

```typescript
let foo = do {
    let bar = getBar();
    console.log(`bar = ${bar}`);
    bar
};
// `foo` is defined and has value of `bar` before it went out of scope
// `bar` is no longer in scope
```

The Crochet implementation of this is very similar to the [TC39
Proposal](https://github.com/tc39/proposal-do-expressions).

NOTE: If the last statement within the block ends with a semicolon then value of
the `do`-expression is `undefined`, e.g.

**Crochet**

```typescript
let foo = do {
    let bar = getBar();
    console.log(`bar = ${bar}`);
    bar;
};
// `foo` is in scope, but its value is set to `undefined`
```

This mimics Rust's behavior with `undefined` standing in for `()`.

## More Expressions (fewer statements)

JavaScript is a very statement-heavy language. As a result, things often end up
needing to be more verbose than would otherwise be necessary. Crochet redefines
the following statements as expressions:

- `if`-`else`
- `throw` ([TC39 Prospoals](https://github.com/tc39/proposal-throw-expressions))
- `try`

The result of an `if`-`else` in Crochet is defined as the last expression in
each block, similar to how `do`-expressions work, e.g.

```typescript
let if (condition) {
    const result = foo();
    console.log("result = ", result);
    result
} else {
    const result = bar();
    console.log("result = ", result);
    result
};
```

## Removed Syntax

### Legacy Syntax

- `with`: no one should be using this
- `eval`: no one should be using this
- `ternary`: superseded by `if`-`else` bing converted to an expression
- `switch`-`case`: superseded by pattern matching
- `function`: arrow syntax is used exclusively. This will make supporting
  generators interesting.

### `const` and `var`

All variables are declared using `let` or `let mut`, see
[Mutability](02_mutability.md).

### Default imports/exports

Default imports/exports make it difficult to maintain consistent naming of
variables across large projects. You'll still be able to import `default` using
a named import to support TypeScript interop.
