# 06 Mutability

In order for Crochet to have tight interop with TypeScript, it needs to support
mutability. TypeScript assumes that everything is mutiple unless otherwise
indicated.

## TypeScript's Approach

TypeScript provides different syntax for indicating whether a variable can be
reassigned (`let` vs `const`) and whether the data being referenced by the
variable can be mutated.

**TypeScript**

```typescript
// The variable can't be reassigned and the array assigned to it can't be
// modified.
const array: readonly number[];

// The variable can be reassigned, but arrays assigned to it can't be modified
// by using this variable.  It's possible to assign a mutable array to a
// readonly reference and update the array via the mutable binding.
let array: readonly number[];

// The variable can't be reassigned, but it can be used to mutate the array it
// references.
const array: number[];

// The variable can be reassigned and it can be used to mutate the array it
// references.
let array: number[];
```

There a number of different ways to indicate the readonly-ness of an array. The
following are equivalent:

**TypeScript**

```typescript
type T = readonly number[];
type T = ReadonlyArray<number>;
```

An object can also be readonly, but the `readonly` modifier doesn't work with
object types. Instead, either all properties must be marked as `readonly` or the
object can be wrapped in the `Readonly<T>` utility type. The following are
equivalent:

**TypeScript**

```typescript
type T = {
  readonly x: number;
  readonly y: number;
};
type T = Readonly<{
  readonly x: number;
  readonly y: number;
}>;
```

Confusingly the following type are not equivalent:

**TypeScript**

```typescript
type T = ReadonlySet<number>;
type U = Readonly<Set<number>>;
```

The reason being is that the `Readonly<T>` utility type only updates mutable
properties to be `readonly`. It does **not** remove methods that mutate the
caller. The `ReadonlySet<T>` type is in fact a separate interface that
contains only those methods from `Set<T>` that don't mutate the caller. The
indexer property on `ReadonlySet<T>` is also set to be `readonly`.

Interestingly, TypeScript appears to special case `ReadonlyArray<T>` and
`ReadOnly<Array<T>>` to both be equivalent to `readonly T[]`.

There are a few other paired interfaces like this:

- `Map<K, V>` and `ReadonlyMap<K, V>`
- `Set<T>` and `ReadonlySet<T>`

The reason for this is that TypeScript doesn't have a way to mark methods as
mutating or not.

## Crochet's Approach

Crochet removes TypeScript's `const` and `readonly` keywords, `Readonly<T>`
utility type, and readonly interface variants (`ReadonlyArray<T>`,
`ReadonlyMap<K, V>`, and `ReadonlySet<T>`) and replaces them with a single `mut`
keyword. Below we describe various uses of `mut`.

### `let mut`

Variables that can be re-assigned and declared using `let mut`.

**Crochet (TypeScript in comments)**

```typescript
let message = "hello";              // const message = "hello" as const;
let message: string = "hello";      // const message = "hello";
let mut message = "hello";          // let message = "hello";
let mut message: string = "hello";  // let message = "hello";
```

Notes:

- `"hello"` is widened to `string` in `let mut message = "hello"` even though
  the type wasn't specified since a mutable variable that can only be assigned
  a single values isn't very useful.

### Object types

Crochet has object types which are similar to object types in TypeScript but
with the follow differences:

- getters/setters are not allowed
- methods are not allowed (but values can be functions)
- properties cannot be reassigned by default
- properties can only be reassigned using a mutable reference to the object

Mutable references are specified using the `mut` keyword as shown in the
following example.

**Crochet**

```typescript
type Point = {      // type Point {
  x: number;        //   readonly x: number;
  y: number;        //   readonly y: number;
};                  // };

let p: Point = {x: 5, y: 10};
p.x = 25;           // ERROR: `p` is an immutable reference so we can't use it
                    // to modify any properties

let q: mut Point = {x: 5, y: 10};
q.x = 25;           // OK: because `q` is a mutable reference

let r: Point = q;   // OK: assigning a mutable reference to an immutable one is
                    // allowed

q.x = 50;           // NOTE: r.x is now 50 since the the object it references
                    // has been modified
```

While these limitations may feel quite restrictive, they are sufficient for
dealing with plain old JavaScript objects (POJOs).

Questions:

- Should `mut T` only affect the assignability of properties immediately on `T`
  or also properties on sub-objects?

### Interfaces

Not all objects we want to interact with are POJOs. Interfaces provides a way
to model objects with getters/setters and methods. An interface can use the
`mut` keyword to specify whether a property is "mutable" or a method is
mutating.

A property is said to be "mutable" if we allow it to be reassigned a different
value of the property's declared type (or a subtype of it).

A method is said to be "mutating" if any of the following are true:

- it modifies a property on `this`
- it calls another "mutating" method on `this`

A mutable reference of an instance conforming to a given interface can call
all of its methods (including the mutating ones) and reassign any of its mutable
properties. It does **not** allow immutable properties to be reassigned.

**Crochet (TypeScript in comments)**

```typescript
interface Greeter {             // interface Greeter {
  message: string;              //   readonly message: string;
  mut name: string;             //   name: string;
  mut updateName(name: string); //   updateName(name: string);
  printGreeting();              //   printGreeting();
}                               // }
                                //
                                // interface ReadonlyGreeter {
                                //   readonly message: string;
                                //   readonly name: string;
                                //   printGreeting();
                                // }

let greeter: Gretter;           // const greeter: ReadonlyGreeter;
let greeter: mut Greeter;       // const greeter: Greeter;
let mut greeter: Greeter;       // let greeter: ReadonlyGreeter;
let mut greeter: mut Greeter;   // let greeter: Greeter;
```

Notes:

- the TypeScript version of this code requires two interfaces. This is because
  TypeScript doesn't have a way to differentiate between mutating and
  non-mutating methods.

## Making Interop Work

Crochet uses .d.ts to import types from and export types to TypeScript. We do
not generate Crochet types on disk from TypeScript, instead they appear in
memory only.

### `let` and `const`

When outputting a .d.ts file, each top-level Crochet declaration is
automatically exported as a named export. `let` in Crochet is equivalent to
`const` in TypeScript and `let mut` is equivalent to `let`. When Crochet reads
a .d.ts file it does the reverse conversion.

```typescript
// example.crochet
let foo = "foo";
let mut bar = "bar";

// example.js
export const foo = "foo";
export let bar = "bar";

// example.d.ts
declare const foo: "foo";
declare let bar: "bar";
```

### Object Types

When import an object type from a .d.ts file, it checks whether all of the
properties are marked as `readonly` or none are. If there's some are `readonly`
and some are not then Crochet will end up converting the type to an interface.

The happy path is when all of the properties in the TypeScript object type are
`readonly`. We can create a Crochet type with the same name and have the two
types line up exactly.

```typescript
// point.d.ts
type Point = {
  readonly x: number;
  readonly y: number;
};

// corresponding Crochet type (in-memory)
type Point = {
  x: number;
  y: number;
};
```

If a Crochet function accepts a `mut Point`, we need a way to export that
function's type to TypeScript while re-using the existing TypeScript definition
of `Point`:

```typescript
// point_utils.crochet
let scalePoint = (point: mut Point, scaleFactor: number): void => ...
let magnitude = (point: Point): number => ...

// point_utils.d.ts
export declare let scalePoint = (point: Mutable<Point>, scaleFactor: number): void;
export declare let magnitude = (point: Point): number;
```

If none of an object's properties are `readonly` in the TypeScript type then
we have to do more work to map the TypeScript type to the corresponding Crochet
type.

```typescript
// point.d.ts
type Point = {
  x: number;
  y: number;
};

// the Crochet type (in-memory) corresponds to `Readonly<Point>` in TypeScript
type Point = {
  x: number;
  y: number;
};
```

If we define the following Crochet functions, we'll output the following .d.ts
file (re-using the existing TypeScript definition of `Point`):

```typescript
// point_utils.crochet
let scalePoint = (point: mut Point, scaleFactor: number) => ...
let magnitude = (point: Point): number => ...

// point_util.d.ts
export declare let scalePoint = (point: Point, scaleFactor: number): void;
export declare let magnitude = (point: Point): number;
```

Questions:

- What about an object type that is more than one level deep?
- What about anonymous object types?

Answer:

I think it's okay to treat them like interfaces.

### Interfaces

When Crochet reads a .d.ts file, it will merge readonly and non-readonly
interfaces into a single interface type. In the case of `Array<T>` and
`ReadonlyArray<T>` it would construct the following type:

**Crochet**

```typescript
type Array<T> = {
  mut [key: number]: T;
  length: number;
  map(cb: (elem: T, index: number, array: T[])): mut U[],
  mut map(cb: (elem: T, index: number, array: mut T[])): mut U[],
  mut sort(): mut T[],
  // ...
};
```

Notes:

- `map` is overloaded with mutating and non-mutating versions
- `map` returns a mutable array in both versions. To get a non-mutable reference
  to the result you'd have to do `let result: T[] = input.map(cb);`.
- We should be able to output a Crochet interface to the corresponding mutable
  and immutable (`Readonly` prefixed) types and then read those TypeScript types
  in to reconstruct the original Crochet interface.

### Literals

Array and object literals require special treatement. We'd like `[1, 2, 3]` to
be inferred as a non-mutable tuple when assigning it to a variable, but we also
want to be able to call mutable methods (or pass the tuple to a function
accepting a mutable array), e.g.

```typescript
let tuple = [1, 2, 3];               // inferred as a non-mutable tuple
let array = [1, 2, 3].sort();        // inferred as a mutable array

let array: number[] = [1, 2, 3];     // OK: the tuple is a sub-type of number[]
let array: mut number[] = [1, 2, 3]; // OK: because there are no other refrences to the tuple

let sort = <T>(values: mut T[]): T[] => values.sort();
let array = sort([1, 2, 3]);

let tuple = [1, 2, 3];
sort(tuple); // ERROR: passing a variable containing a non-mutable type is not allowed
let array: number[] = [1, 2, 3];
sort(array); // ERROR: passing a variable containing a non-mutable type is not allowed
```

While array spreading is equivalent to calling `.concat()`, the inferred type is
different.

```typescript
let array = [1, 2, 3].concat([4, 5, 6]); // inferred as `number[]`
let tuple = [1, 2, 3, ...[4, 5, 6]]; // inferred as `[1, 2, 3, 4, 5, 6]`
```

Object literals are inferred as non-mutable types, but can also be passed to
functions with mutable parameters, e.g.

```typescript
type Point = { x: number; y: number };

let scale = (scale: number, point: mut Point) => {
  point.x *= scale;
  point.y *= scale;
};

scale({x: 5, y: 10});  // allowed, but isn't very useful

let point = {x: 5, y: 10}; // inferred as {x: 5, y: 10}
scale(point); // ERROR: variables pointing to non-mutable Point sub-types are not allowed

let point: mut Point = {x: 5, y: 10}; // OK: the object literal is a sub-type of Point
let point: Point = {x: 5, y: 10};     // OK: because there are no other refrences to the object
```
