# Mutability

In order for Crochet to have tight interop with TypeScript, it needs to support mutability.
TypeScript assumes that everything is mutiple unless otherwise indicated.

## TypeScript's Approach

TypeScript provides different syntax for indicating whether a variable can be reassigned
(`let` vs `const`) and whether the data being referenced by the variable can be mutated.

**TypeScript**

```typescript
// The variable can't be reassigned and the array assigned to it can't be modified.
const array: readonly number[];

// The variable can be reassigned, but arrays assigned to it can't be modified by
// using this variable.  It's possible to assign a mutable array to a readonly reference
// and update the array via the mutable binding.
let array: readonly number[];

// The variable can't be reassigned, but it can be used to mutate the array it references.
const array: number[];

// The variable can be reassigned and it can be used to mutate the array it references.
let array: number[];
```

There a number of different ways to indicate the readonly-ness of an array. The following are
equivalent:

**TypeScript**

```typescript
type T = readonly number[];
type T = ReadonlyArray<number>;
```

An object can also be readonly, bu the `readonly` modifier doesn't work with object types.
Instead, either all properties must be marked as `readonly` or the object can be wrapped in
the `Readonly<T>` utility type. The following are equivalent:

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
type T = ReadonlyArray<number>;
type U = Readonly<Array<number>>;
```

The reason being is that the `Readonly<T>` utility type only updates mutable properties to be `readonly`.
It does **not** remove methods that mutate the caller. The `ReadonlyArray<T>` type is in fact a separate
interface that contains only those methods from `Array<T>` that don't mutate the caller. The indexer
property on `ReadonlyArray<T>` is also set to be `readonly`.

There are a few other paired interfaces like this:

- `Map<K, V>` and `ReadonlyMap<K, V>`
- `Set<T>` and `ReadonlySet<T>`

The reason for this is that TypeScript doesn't have a way to mark methods as mutating or not.

## Crochet's Approach

Crochet removes TypeScript's `const` and `readonly` keywords, `Readonly<T>` utility type, and
readonly interface variants (`ReadonlyArray<T>`, `ReadonlyMap<K, V>`, and `ReadonlySet<T>`) and
replaces them with a single `mut` keyword. The new keyword serves the following purposed:

**Crochet (TypeScript in comments)**

```typescript
let array: number[]; // const array: readonly number[];
let array: mut number[]; // const array: number[];
let mut array: number[]; // let array: readonly number[];
let mut array: mut number[]; // let array: number[];

let point: Point; // const point: Readonly<Point>;
let point: mut Point; // const point: Point;

let set: Set<string>; // const set: ReadonlySet<string>;
let set: mut Set<string>; // const set: Set<string>;
```

The `mut` keyword can also appear wherever a new variable binding is introduced, i.e.
destructuring patterns in variable declarations, pattern matching, and function params.

**Crochet**

```typescript
let {x, mut y, z: mut w} = point3d;
let {x, mut y = 5, z: mut w = 10} = point3d;
let [a, mut b, mut c = "c", ...rest] = letters;
```

This has the benefit of being able to introduce a mix of mutable and immutable references
when destructring objects and arrays. In TypeScript (and JavaScript), you'd need to have
separate declarations for `let` and `const`.

Crochet will also allow methods in classes and interfaces to be marked as "mutating". It's
important to note here that `mut` doesn't mean "mutable" like in the other uses. A method
marked with `mut` can't be reassigned to point to a different implementation. Instead, it
means that the method may mutate instance properties marked as `mut`.

**Crochet**

```typescript
class Greeter {
  message: string, // readonly, can only be set once by the constructor
  mut name: string,

  constructor(message: string, name: string) {
    this.message = message;
    this.name = name;
  }

  mut updateName(name: string) {
    this.name = name;
  }

  printGreeting(): string {
    console.log(`${this.message}, ${this.name}`);
  }
}
```

This class is equivalent to the following TypeScript interfaces:

**TypeScript**

```typescript
interface Greeter {
  readonly message: string;
  name: string;
  updateName(name: string);
  updateMessage(name);
}

interface ReadonlyGreeter {
  readonly message: string;
  readonly name: string;
  updateMessage(name);
}
```

References to Crochet classes and inferaces work in the following way:

**Crochet (TypeScript in comments):**

```typescript
let greeter = new Greeter("Hello", "world"); // by default non-mutable references are inferred
let greeter: Gretter; // const greeter: ReadonlyGreeter;
let greeter: mut Greeter; // const greeter: Greeter;
let mut greeter: Greeter; // let greeter: ReadonlyGreeter;
let mut greeter: mut Greeter; // let greeter: Greeter;
```

Notes:

- mutating methods cannot mutate immutable properties
- non-mutating methods cannot mutate any properties
- non-mutating methods cannot call mutating methods
- non-mutating methods can call mutating methods and mutate properties on other objects

**Crochet**

```typescript
class Greeter {
  greetMany(names: string[]) {
    // It's okay to call `.sort()` within `greetMany` even though
    // `greetMany` isn't mutating.
    let sortedNames = [...names].sort();
    for (const name of sortedNames) {
      console.log(`${this.greeting}, ${name}`);
    }
  }
}
```

## Making Interop Work

### `let` and `const`

TODO

### Interfaces and Classes

Crochet classes will be exported as a `class` in a .js file and two interfaces in the corresponding
.d.ts. In the case of the `Greeter` class , the .d.ts file would contain both `Greeter` and
`ReadonlyGreeter` interfaces shown above.

When Crochet reads a .d.ts file, it will merge readonly and non-readonly interfaces into a single
interface type. In the case of `Array<T>` and `ReadonlyArray<T>` it would construct the following
type:

**Crochet**

```typescript
type Array<T> = {
  mut [key: number]: T;
  length: number;
  map(cb: (elem: T, index: number, array: T[])): mut U[],
  mut map(cb: (elem: T, index: number, array: mut T[])): mut U[],
  mut sort(): mut T[],
  // et cetera
};
```

Notes:

- `map` is overloaded with mutating and non-mutating versions
- `map` returns a mutable array in both versions. To get a non-mutable reference to
  the result you'd have to do `let result: T[] = input.map(cb);`.<br/>
  **Question:** Given that array literals (aka tuples) are inferred as non-mutable, but
  can also be passed to functions expecting mutable arrays (when passing the literal directly).
  Should return types behave similarly to how literals behave in this regard (but in reverse)?

### Mutable References

The following Crochet code will be converted to TypeScript as follows:

**Crochet (TypeScript in comments)**

```typescript
const array: mut number[];  // const array: number[];
const array: number[];      // const array: readonly number[];
const point: mut Point;     // const point: Point;
const point: Point;         // const point: Readonly<Point>; (Point imported from .d.ts)
const point: Point;         // const point: ReadonlyPoint; (Point declared in Crochet)
const set: mut Set<number>; // const set: Set<number>;
const set: Set<number>;     // const set: ReadonlySet<number>;
```
