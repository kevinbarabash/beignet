# 03 Type Safety

## Everything is `readonly` by default

In TypeScript everything is mutable by default. Immutability in code has a
number of benefits. In the context of TypeScript, immutability avoids issues
with variance. It's very easy to construct programs that aren't typesafe, e.g.

**TypeScript**

```ts
class Animal {}
class Dog extends Animal {}
class Cat extends Animal {}

const processAnimals = (animals: Animal[]): void => {
  animals.push(new Dog());
};
declare const cats: Cat[];

processAnimals(cats); // not an error, but now `cats` has a `Dog` in it
```

Crochet prevents this by doing the following:

- Only allow covariance (sub-types of params to be passed as arg) when the
  params are `readonly`
- Only allow invariance (exact type matches) when the params are mutable
- Default everything to readonly and use the new `mut` keyword to specify
  when things are mutable, see [Mutability](06_mutability.md).

## Literals are inferred as if TypeScript's `as const` was being used

JavaScript APIs make heavy use of string literals in a way that affects the
types of things. As an example `document.createElement(tag)` will return
different types depending on the value of `tag`. By inferring the types of
tuples and objects as if we were using `as const` we maintain the values of any
literals within these containers which decreases how often we have to fallback
to a more general type.

**TypeScript**

```ts
let point: {x: 5, y: 10}; // {x: number, y: number}
let point: {x: 5, y: 10} as const; // {x: 5, y: 10}
```

**Crochet**

```ts
let point: { x: 5; y: 10 }; // {x: 5, y: 5}
```

## Accessing Indexer returns `T | undefined`

TypeScript assumes that all keys exist on objects with indexers. If you index an
element in an array typed as `number[]` the value you get back will be typed as
`number` regardless of whether that element exists in the array or not. Crochet
on the other hand will type the value as `number | undefined`.

```ts
type Record<K, V> = {
  [key: K]: V;
};

declare let cache: Record<string, string>;
declare let numbers: number[];

let value = cache["foo"]; // `value` has type `string | undefined`
let element = numbers[10]; // `element` has type `number | undefined`
```

The plan is to introduce range types post-MVP so that array elements can be
accessed safely without to check if the accessed index is undefined or not.

## Exception Tracking and Typed Exception

If a developer wants to be defensive and catch these, it can difficult to know
which functions throw which errors. [hegel.js](https://hegel.js.org), another
static type checker for JavaScript, includes a `$Throws<>` type for tracking
which exceptions a function can throw. If a function calls another function
which throws an exception without catching it, we will infer it to also be
throwing the same exception.

Tracking exceptions means that we'd also be able to know what type (or types) a
caught exception could be. This would allow us to increase the type safety of
code in `catch` blocks.

## Improved Inference

TypeScript infers parameters types as `any` if no explicit type is provided.
Crochet is able to infer parameter types based on how they are used in the
function, e.g.

**TypeScript**

```ts
let add = (a, b) => a + b; // inferred as (a: any, b: any) => any
```

**Crochet**

```ts
let add = (a, b) => a + b; // inferred as (a: number, b: number) => number
```

NOTE: `+` in Crochet can only be used with numbers. For string concatenation use
template literals.

## No `any` type

Instead, any function that relies on an `any` type will be turned into a generic
parameter, e.g.

**TypeScript**

```ts
interface JSON {
  parse(text: string): any;
}
```

will be converted to

**Crochet**

```ts
interface JSON {
  parse<t0>(text: string): t0;
}
```

Whenever `JSON.parse()` is called its return type will be inferred as a new type
variable. This type variable will then be unified with other expressions within
the program. While this won't detect issues with a mismatch between the parsed
type and its uses, it will detect inconsistencies between the uses of the
returned variable.

```ts
let a = parse("10");
let succ = a + 1; // typechecks and is valid
let length = a.length; // does not typecheck since the previous line inferred `a` as a number

let p = parse("{x: 5, y: 10}");
let q = parse("{x: 1, y: -1}");
let dist = q - p; // also typechecks, but is invalid
```

This is only a slight improvement over `any` which simply
opts out of type checking altogether.

Questions:

- How do we get well-typed data back from parsing a JSON string?
