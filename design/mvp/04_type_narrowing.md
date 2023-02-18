# 04 Type Narrowing

Type narrowing is a concept that a lot of developers struggle with when using
static type checkers with JavaScript. TypeScript has made some improvements in
this area recently to allow developers to do narrowing more naturally, e.g.

**Current TypeScript**

```typescript
type MyEvent =
  | { type: "mousedown"; x: number; y: number }
  | { type: "keydown"; key: string };

declare const event: MyEvent;

const { type } = event;

if (type === "mousedown") {
  const { x, y } = event; // `x` and `y` are `number`s in this scope
} else if (type === "keydown") {
  const { key } = event; // `key` is a `string` in this scope
}
```

Previously (and still in Flow), you'd have to make sure that if condition
expression included the variable you wanted to narrow. **Old TypeScript**

```ts
type MyEvent =
  | { type: "mousedown"; x: number; y: number }
  | { type: "keydown"; key: string };

declare const event: MyEvent;

if (event.type === "mousedown") {
  const { x, y } = event; // `x` and `y` are `number`s in this scope
} else if (event.type === "keydown") {
  const { key } = event; // `key` is a `string` in this scope
}
```

Flow complicated things further by invalidating type narrowing in after any
function call. While it's possible for a function call to invalidate narrowing,
the vast majority of time this never actually happens.

Even though TypeScript supports writing JavaScript in a more idiomatic way than
Flow, type narrowing can still be confusing. This stems from the fact that the
type of a variable changes in different scopes. This differs from almost all
other statically type languages.

## `if`-`let`

Escalier takes a different approach. Instead of narrowing the type of an existing
variable, a new variable binding is introduced when determining a variable's
type, e.g.

```ts
type MyEvent =
  | {type: "mousedown", x: number, y: number}
  | {type: "keydown", key: string}
  ;

declare const event: MyEvent;

if (let {type: "mousedown", x, y} = event) {
  // `x` and `y` are `number`s in this scope
  // `event` is still of type `MyEvent`
} else if (let {type: "keydown", key} = event) {
  // `key` is a `string` in this scope
  // `event` is still of type `MyEvent`
}
```

There are a number of different patterns that can be used with `if`-`let`:

- tuple and object destructuring
- `is` checks with either a primitive or a class
- instance destructuring (combines an `is` check with object destructuring)
- literals, e.g. `"hello"`, `5`, `true`, `null`, `undefined`
- variables (binds the matched value to the variable)
- `_` checks for the presence of a variable/property, but doesn't introduce a
  binding for it

Here's an example using a couple of `is` patterns.

```ts
// is_pattern.esc
declare let foo: number | Set<number>;

if (let num is number = foo) {
  // `num` is a `number`
  // `foo` still has type `number | Set<number>`
} else if (let set is Set = foo) {
  // `set` is an instance of `Set`
  // `foo` still has type `number | Set<number>`
}

// is_pattern.js
if (typeof foo === "number") {
  const num = foo;
} else if (foo instanceof Set) {
  const set = foo;
}
```

There may be situations where you want to destructure an object based on what
class it is. This can be done in the following way:

```ts
// instance_pattern.esc
declare const event: MouseEvent | KeyboardEvent;

if (let MouseEvent { pageX, pageY } = event) {
  // do something with pageX and pageY
} else if (let KeyboardEvent { keyCode } = event) {
  // do something with keyCode
}

// instance_pattern.js
if (event instanceof MouseEvent) {
  const { pageX, pageY } = event;
  // do something with pageX and pageY
} else if (event instanceof KeyboardEvent) {
  const { keyCode } = event;
  // do something with keyCode
}
```

Patterns can either refutable or irrefutable. A pattern is said to be refutable
if it contains at least one of the following pattern kinds:

- literals
- `is`
- instance patterns
- an object or array that contains one of the above patterns

While `if`-`let` can be used with both types of patterns, it's really only
useful with refutable patterns.

`let` declarations on the other hand can only use irrefutable patterns, e.g.

```ts
let { x, y } = point;
```

Questions:

- What happens with types like `Set<T> | Set<U>`? How can we tell them apart?
