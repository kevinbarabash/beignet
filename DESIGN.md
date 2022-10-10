# Crochet Design

This document describes the Crochet language as we would like it to be. The language is currently
being implemented so many of these features do not exist yet or not work exactly as describe here.

The main goals of Crochet are:

- tight interop with TypeScript
- improved type safety and type inference
- extensibility
- address some JavaScript pain points
- support developer expectations

## TypeScript Interop

There are a number of compile to JavaScript languages out there, but all of them struggle with
writing bindings for existing libraries. Crochet aims to address this issue by:

- implementing a type system that is compatible with TypeScript's
- parsing .d.ts files and interpreting them as types in Crochet's type system
- outputting .d.ts files for each .crochet source file

This allows for automatic, bidirectional, and typesafe interop with TypeScript. The intent
is to facilitate the following use cases:

- new Crochet projects that want to use existing libraries with TypeScript type definitions
- existing TypeScript projects that want to use Crochet libraries
- existing TypeScript projects that want to use Crochet for new development without having to
  migrate existing TypeScript code

One thing we'd like to eventually have is a script to convert .ts files to .crochet, but this
will likely be quite difficult to implement for all JavaScript/TypeScript constructs.

## Improved Type Safety and Type Inference

### No `any` type

Instead, any function that relies on an `any` type will be turned into a generic parameter, e.g.

**TypeScript**

```typescript
interface JSON {
  parse(text: string): any;
}
```

will be converted

**Crochet**

```typescript
interface JSON {
  parse<t0>(text: string): t0;
}
```

Whenever `JSON.parse()` is called its return type will be inferred as a new type variable.
This type variable will then be unified with other expressions within the program. While
this won't detect issues with a mismatch between the parsed type and its uses, it will detect
inconsistencies between the uses.

**Crochet**

```typescript
let p = parse("{x: 5, y: 10}");
let q = parse("{x: 1, y: -1}");
let dist = q - p; // also typechecks, but is invalid

let a = parse("10");
let succ = a + 1; // typechecks and is valid
let length = a.length; // does not typecheck since the previous line inferred `a` as a number
```

### Accessing Indexer returns `T | undefined`

TypeScript assumes that all keys exist on objects with indexers. If you index an element in an
array typed as `number[]` the value you get back will be typed as `number` regardless of whether
that element exists in the array or not. Crochet on the other hand will type the value as
`number | undefined`.

### Literals are inferred as if TypeScript's `as const` was being used

JavaScript APIs make heavy use of string literals in a way that affects the types of things.
As an example `document.createElement(tag)` will return different types depending on the value
of `tag`. By inferring the types of tuples and objects as if we were using `as const` we maintain
the values of any literals within these containers which decreases how often we have to fallback
to a more general type.

**TypeScript**

```typescript
let point: {x: 5, y: 10}; // {x: number, y: number}
let point: {x: 5, y: 10} as const; // {x: 5, y: 10}
```

**Crochet**

```typescript
let point: { x: 5; y: 10 }; // {x: 5, y: 5}
```

### Everything is `readonly` by default

In TypeScript everything is mutable by default. Immutability in code has a number of benefits.
In the context of TypeScript, immutability avoids issues with variance. It's very easy to construct
programs that aren't typesafe, e.g.

**TypeScript**

```typescript
class Animal {}
class Dog extends Animal {}
class Cat extends Animal {}

const processAnimals = (animals: Animal[]): void => {
  animals.push(new Dog());
};
declare const cats: Cat[];

processAnimals(cats); // not an error, but now `cats` has a `Dog` in it
```

Crochet will prevent this by doing the following:

- only allow covariance (sub-types of params to be passed as arg) when the params are `readonly`
- only allow invariance (exact type matches) when the params are mutable
- default everything to readonly and use the new `mutable` keyword to specify when things are
  not `readonly` (there is no `readonly` keyword in Crochet)

Currently Crochet's type system only support immutable types. In order to support mutable types
we'll need to implement a version of `unify()` that doesn't allow sub-types to be used.

### Exception Tracking and Type Exceptions

If a developer wants to be defensive and catch these, it can difficult to know which functions
throw which errors. [hegel.js](https://hegel.js.org), another static type checker for JavaScript,
includes a `$Throws<>` type for tracking which exceptions a function can throw. If a function
calls another function which throws an exception without catching it, we will infer it to also be
throwing the same exception.

Tracking exceptions means that we'd also be able to know what type (or types) a caught exception
could be. This would allow us to increase the type safety of code in `catch` blocks.

### Rethinking Type Refinements

Type refinement is a concept that a lot of developers struggle with when using static type checkers
with JavaScript. TypeScript has made some improvements in this area recently to allow developers
to do refinements more naturally, e.g.

**Current TypeScript**

```typescript
type MyEvent =
  | { type: "mousedown"; x: number; y: number }
  | { type: "keydown"; key: string };

declare const e: MyEvent;
const { type } = e;
if (type === "mousedown") {
  const { x, y } = e;
  // do stuff with `x` and `y`
}
```

Previously (and still in Flow), you'd have to make sure that if condition expression included
the variable you wanted to refine.
**Old TypeScript**

```typescript
type MyEvent =
  | { type: "mousedown"; x: number; y: number }
  | { type: "keydown"; key: string };

declare const e: MyEvent;
if (e.type === "mousedown") {
  const { x, y } = e;
  // `x` and `y` are `number`s in this scope
} else if (e.type === "keydown") {
  const { key } = e;
  // `key` is a `string` in this scope
}
```

Flow complicated things further by invalidating type refinements in after any function call. While
it's possible for a function call to invalidate a refinement, the vast majority of time this never
actually happens.

Even though TypeScript supports writing JavaScript in a more idiomatic way than Flow, type
refinements can still be confusing. This stems from the fact that the type of a variable changes
in different scopes. This differs from almost all other statically type languages.

Crochet takes a different approach. Instead of refining the type of an existing variable, a new
variable binding is introduced when determining a variable's type, e.g.

**Crochet**

```typescript
type MyEvent =
  | {type: "mousedown", x: number, y: number}
  | {type: "keydown", key: string}
  ;

declare const e: MyEvent;
if (let {type: "mousedown", x, y} = e) {
    // `x` and `y` are `number`s in this scope
} else if (let {type: "keydown", key} = e) {
    // `key` is a `string` in this scope
}
```

See the "if-let" section below for more details on this syntax.

### Improved Inference

TypeScript infers parameters types as `any` if no explicit type is provided. Crochet is able to
infer parameter types based on how they are used in the function, e.g.

**TypeScript**

```typescript
let add = (a, b) => a + b; // inferred as (a: any, b: any) => any
```

**Crochet**

```typescript
let add = (a, b) => a + b; // inferred as (a: number, b: number) => any
```

NOTE: `+` in Crochet can only be used with numbers. For string concatenation use `.join()` on
arrays/tuples or template literals.

## Extensibility

### Extensible Type System

There are lots of APIs that make use of data within strings, e.g. regexes, `document.createElement`,
`document.querySelector`, `react-router`, etc. TypeScript is able to infer types from some of these
APIs such as `createElement` and `querySelector`. This capability is likely to expand over time
now that TypeScript has conditional types, template literal types, and `infer` . That being said,
implementing type inference of this data within TypeScript's type system is complicated and much
slower than it could be.

Type inference in some cases may require access to external data, e.g. inferring the type of GraphQL
queries requires accessing a schema. There are plugins for VSCode that provide editor support for
GraphQL and SQL. These plugins provide validation of queries in the editor and can also generate
type definitions for the queries. It's a workable solution, but it results in a lot of generated
files that need to be kept up to date and may result in merge conflicts with other generated types.

Crochet will solve these issues by providing a plugin API for the type system that can:

- infer structured types based on literals (strings, regexes, tagged template literals)
- access external resources (file system, network, etc.) to support this type inference

### Linting

Crochet can provide a typed AST to a future linter. This would allow us to craft more powerful rules
than is currently possible with `eslint`.

### Code Highlighting

We can also leverage a typed AST to support better code highlighting. `rust-analyzer` provides
syntax highlighting that differentiates mutable and immutable variables. We could do the same
with Crochet.

### Hygienic Macros

TODO

## Differences for JavaScript

### Semicolons

- semicolons are not automatically inserted
- semicolons are required in between statements

### `let`-binding

The behavior of `let` has been modified so that it:

- allows redeclarations (like `var`)
- doesn't allow reassignment (like `const`) without redeclaration

**Crochet**

```typescript
let x = "hello";
let x = x.length; // x is now a number
```

### `if`-`let`

This syntax combines some sort of runtime type checking along with the introducing of a new
binding for the narrowed type. This is used in place of type refinements you'd normally use in
static type checkers for Javascript. An example of this was shown in the "No Type Refinements"
sub-section earlier in the doc. Here's another example:

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
- `_` checks for the presence of a variable/property, but doesn't introduce a binding for it

Patterns can either refutable or irrefutable. A pattern is said to be refutable if it contains
at least one of the following pattern kinds: literals or `is`. While `if`-`let` can be used with
both types of patterns, it's really only useful with refutable patterns. `let` declarations on
the other hand can only use irrefutable patterns.

### Pattern Matching

Pattern matching is like a `switch`-`case` but you can match against structured data and
the case statements can use the same patterns that are use with `if`-`let`. Here's one example of
what this looks like in Crochet:

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

NOTE: Pattern matching is syntactic sugar for `if`-`let`-`else`. The example above can be
rewritten as the following:

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

The syntax also supports:

- `if` guards on patterns
- `is` patterns to match primitives and class, e.g. `variable is string` or `variable is HTMLElement`
- `variable` will match anything and bind it to the variable
- `_` will match anything without binding

The reason for using `->` instead of `=>` was to differentia

While there is a [TC39 Proposal](https://github.com/tc39/proposal-pattern-matching) for pattern
matching, it feels overly complex. This is why an alternate syntax was adopted.

### `do` Expressions

This allows you to introduce a new scope to enacapsulate computation. It's similar to an IIFE
(immediately invoked function expression) without the cost of function call.

**Crochet**

```typescript
let foo = (bar: string) {
    let baz = do {
        let ucBar = bar.toUpperCase();
        let ucBaz = ucBar.replace("R", "Z");
        let baz = ucBaz.toLowerCase();
        console.log(`baz = ${baz}`);
        baz // this is the result of this do-expression
    };

    // ... do something with baz
}
```

The Crochet implementation of this is very similar to the [TC39 Proposal](https://github.com/tc39/proposal-do-expressions).

NOTE: If the last statement within the block ends with a semicolon then value of the `do`-expression
is `undefined`, e.g.

**Crochet**

```typescript
let foo = do {
    let bar = getBar();
    console.log(`bar = ${bar}`);
    bar;
}; // foo is `undefined`
```

This mimics Rust's behavior.

### More Expressions (fewer statements)

JavaScript is a very statement-heavy language. As a result, things often end up needing to be
more verbose than would otherwise be necessary. Crochet redefines the following statements as
expressions:

- `if`-`else`
- `throw` ([TC39 Prospoals](https://github.com/tc39/proposal-throw-expressions))
- `try`

The result of an `if`-`else` in Crochet is defined as the last expression in each block, similar
to how `do`-expressions work, e.g.

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

### Removed Syntax

#### Legacy Syntax

- `with`: no one should be using this
- `eval`: no one should be using this
- `ternary`: superseded by `if`-`else` bing converted to an expression
- `switch`-`case`: superseded by pattern matching
- `function`: arrow syntax is used exclusively. This will make supporting generators interesting.

#### `const` and `var`

All variables are declared using `let`.

#### Default imports/exports

Default imports/exports make it difficult to maintain consistent naming of variables across
large projects. You'll still be able to import `default` using a named import to support TypeScript
interop.

### Autobinding

One of the confusing aspects of using JavaScript is passing methods as callbacks. To avoid this
confusion when a method is passed as a callback, Crochet will autobind it.

## Developer Expectations

### LSP (Language Server Protocol) Support

- show types and docstring
- typesafe renaming across multiple files

### Sourcemaps

Good support for sourcemaps will have the following benefits:

- ease of debuggability
- support other tooling (test runners, coverage reporters, etc.)

### Modern JavaScript Features

- optional chaining
- nullish coalescing
- JSX support (already codegen'd, but type-checking is incorrect)
- async/await (already supported)
- iterators
- classes
- generators
- decorators

## Bonus Features

There are lots of great features from other languages that we can bring to Crochet to make it even
more of a joy to use.

### Typed holes

This is a feature from Haskell/PureScript. It allows you to prefix an identifier with `?` and it
will return what its type should be in order for the code to typecheck. In Haskell/PureScript this
can be used to get a list of suggestions for expressions to replace the typed hole with.

### Ranges

Range types could be very handy for working with arrays safely without having to check if each
element is defined or not.

Additionally, we could adopt Rust's syntax (which is very similar to Python's syntax) for slices
as syntactic sugar for calls to `.slice()`, e.g.

**Crochet**

```typescript
let tuple = [1, 2, 3, 4, 5];

let slice1 = [:3];  // tuple.slice(0, 3);  === [1, 2, 3]
let slice2 = [1:3]; // tuple.slice(1, 3);  === [2, 3]
let slice3 = [1:];  // tuple.slice(1);     === [2, 3, 4, 5]
let slice4 = [:-1]; // tuple.slice(0, -1); === [1, 2, 3, 4]
let slice5 = [:];   // tuple.slice();      === [1, 2, 3, 4, 5]
```

### Pipeline Operator?

TODO, see [TC39 Proposal](https://github.com/tc39/proposal-pipeline-operator).

### Const Expressions

If an expression is composed of literals and operations that can be computed at compile time, e.g.
math operators, `.length` on string literals, etc. then we can compute the result of the expression
at compile time.

### Pure/Side-Effect Tracking

TODO
