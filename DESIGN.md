# Crochet Design

This document describes the Crochet language as we would like it to be. The language is currently
being implemented so many of these features do not exist yet or not work exactly as describe here.

The main goals of Crochet are:

- tight interop with TypeScript
- improve the type safety of TypeScript
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

## Improved Type Safety

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

### Typed + Tracked Exceptions

If a developer wants to be defensive and catch these, it can difficult to know which functions
throw which errors. [hegel.js](https://hegel.js.org), another static type checker for JavaScript
that include a `$Throws<>` type for tracking which exceptions a function can throw. If a function
calls another function which throws an exception without catching it, we will infer it to also be
throwing the same exception.

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

### Pattern Matching

Pattern matching is like a `switch`-`case` but you can match against structured data and
the case statements can also be structured. Here's one example of what this looks like
in Crochet:

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

All variables are declared using `let`. A variable can be redeclared with the same scope. This
shadows the previous declaration. This is possible because all block statements are lambdas.

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
